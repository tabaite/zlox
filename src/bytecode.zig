// This is the *old* code generation backend that was called
// directly from the parser and generated quite poor quality bytecode
// (albeit without the added step of an AST).

const prelude = @import("prelude.zig");
const std = prelude.std;
// circular imports are allowed!!!
const parsing = @import("parsing.zig");
const context = @import("context.zig");
const ztracy = @import("ztracy");

const Stack = prelude.Stack;
const Context = context.Context;
const Allocator = std.mem.Allocator;

pub const MAX_ARGS = 128;

// function calls:
// use new pushArg instruction to push instructions into a buffer
// when calling, we put the number of pushed args onto the callstack
// and push that set of values
// when returning, we pop the number of popped args

// How literals work:
// Each operation has a component that displays the type of the arguments (literal OR handle).
// We store this information in the HandledOperand type.
// For strings, we push the index of the string within the ROM and the length onto the stack, then return a handle to that item.
// For bools, we consider it to be true if the number is not 0, and false if it is 0 (similar to C)
// The first qword is the index into rom, and the second is the length.

// How the destination part of instructions work:
// We don't want to push something EVERY time we do some basic arithmetic,
// so the parser will EXPLICITLY provide a handle for us to put things into,
// rather than us assuming anything. This is because the parser has information we don't.

// Types are erased from bytecode, we check them at compile time.
// These are only relevant for some operations, some others interpret this their own way.
pub const ArgTypes = enum(u2) {
    // if the right bit is set, it's a literal
    // if the left bit is set, it's a literal
    bothHandle = 0,
    handleALiteralB = 1,
    literalAHandleB = 2,
    bothLiteral = 3,
};

pub const OpCode = enum(u30) {
    // Just for now!
    noop,
    // If the argument type is a handle, we push 0, and then assign the value of the handle to the new handle.
    // A: Item to be pushed, B: Unused, Dest: Unused, Arg Type: Used for A
    pushItem,
    // If the argument type is a handle, we push 0, and then assign the value of the handle to the new handle.
    // A: Item to be pushed, B: Unused, Dest: Unused, Arg Type: Used for A
    pushArgument,
    // If the argument type is a handle, we push 0, and then assign the value of the handle to the new handle.
    // A: Item to be pushed, B: Unused, Dest: Unused, Arg Type: Used for A
    pop,
    // A: Number to be negated, B: Unused, Dest: Where to store the result, Arg Type: Used for A
    negateNumber,
    // A: Bool to be negated, B: Unused, Dest: Where to store the result, Arg Type: Used for A
    negateBool,

    // A, B: Operands, Dest: Where to store the result, Arg Type: Used for A and B
    add,
    subtract,
    multiply,
    modulo,
    divide,
    eq,
    neq,
    greater,
    ge,
    less,
    le,
    bOr,
    bAnd,

    // A: Location of function to call, B, Dest, Arg Type: Unused
    call,
    // All unused
    ret,

    // A: Item to move (literal or handle), B: Unused, Dest: Where to move to, Arg Type: Used for A
    move,
};

test "correct sizing for instructions" {
    const assert = std.testing.expect;
    try assert(@sizeOf(Operation) == 4);
    try assert(@sizeOf(Instruction) == 24);
}

pub const Operation = packed struct {
    argType: ArgTypes,
    op: OpCode,
};

pub const Handle = u32;
pub const Type = enum(u32) {
    nil,
    number,
    numberLit,
    string,
    bool,
    boolLit,
    // This is for when we bump into an error and still
    // have to use a value, but don't want to introduce
    // a potentially confusing IncompatibleType error.
    // This coerces into any type.
    errorType,
    // this is for user classes, but i'll deal with that later
    // _,

    pub fn asString(self: Type) []const u8 {
        return switch (self) {
            .nil => "void",
            .number, .numberLit => "number",
            .string => "string",
            .bool, .boolLit => "bool",
            .errorType => "error type (any)",
        };
    }
};

// Any declaration where the type is not known, or where the type cannot be inferred from its
// initial value is not allowed.
pub const NewVariableTypeInfo = union(enum) {
    fromValue: HandledOperand,
    provided: struct {
        type: Type,
        initial: ?HandledOperand,
    },
};

pub const ArgInfo = struct {
    name: []u8,
    type: Type,
};

// Operand, but with a type. Not used in bytecode, but rather for variable tracking and pushing operations.
pub const HandledOperand = struct {
    operand: RawOperand,
    type: Type,

    pub const NIL: HandledOperand = .{ .operand = .NULL_HANDLE, .type = .nil };
    pub const ERR: HandledOperand = .{ .operand = .NULL_HANDLE, .type = .errorType };
};

pub const RawOperand = packed struct {
    item: u64,
    pub const NULL_HANDLE: RawOperand = .{ .item = 0 };
    pub const RET_HANDLE: RawOperand = .{ .item = 1 };
};

// The operand will be treated differently depending on the operation.
pub const Instruction = struct {
    a: RawOperand,
    b: RawOperand,
    dest: Handle,
    op: Operation,
};

pub const Program = struct {
    instructions: []Instruction,
    entryPoint: usize,
};

const ScopeExtent = struct {
    numVars: usize = 0,
    numItems: usize = 0,
};
const ScopeExtentStack = Stack(ScopeExtent, 32767);
const ScopeNamesStack = Stack([]u8, 32767);

const CurrentFunctionContext = struct {
    name: []u8,
    args: []ArgInfo,
    start: usize,
    retType: Type,
    returnsOnAllPaths: bool,
};

/// That's right. Running out of memory IS a fatal error and you can't change my mind.
/// It's not like the old code did anything except bubble the error up to main anyways.
/// This will allow us to operate with errors as "interupts" without the error space
/// being polluted by OutOfMemory.
pub fn allocatorMust(T: type, result: Allocator.Error!T) T {
    return result catch @panic("Interpreter backend ran out of memory.");
}

pub const BytecodeGenerator = struct {
    const FunctionType = struct {
        args: []ArgInfo,
        start: usize,
        retType: Type,
    };
    allocator: Allocator,
    // We use 2 stacks to track the state of our variables.
    // scopeExtentStack tracks the amount of variables that are pushed in a scope.
    // scopeNamesStack tracks the actual names that are declared.
    // When a scope is entered, we push a new number onto scopeExtentStack.
    // When a variable is declared, we add 1 to the number on the top of scopeExtentStack.
    // When the scope exits, pop scopeExtentStack, and deregister that amount of variables from the top of scopeNamesStack.
    scopeExtentStack: ScopeExtentStack,
    scopeNamesStack: ScopeNamesStack,
    variableRegistry: std.StringHashMapUnmanaged(HandledOperand),
    functionRegistry: std.StringHashMapUnmanaged(FunctionType),
    bytecodeList: std.ArrayListUnmanaged(Instruction),
    stringBuffer: std.ArrayListUnmanaged(u8),
    // Tracks how high the stack is currently in RawOperands.
    stackHeight: u32,
    // Because the first "instruction" is the entry point, 0 is used here to represent none.
    entryPoint: enum(u32) { none = 0, _ },
    currentFunction: ?CurrentFunctionContext,

    /// TODO: fix
    /// Returns null if no entry point available to actually run program.
    pub fn finalize(self: *BytecodeGenerator, ctx: Context) ?Program {
        if (self.entryPoint == .none) {
            ctx.pushError(.mainFunctionNotDeclared);
            return null;
        }
        return .{ .entryPoint = @as(usize, @intFromEnum(self.entryPoint)) - 1, .instructions = self.bytecodeList.items };
    }

    pub fn init(allocator: Allocator) !BytecodeGenerator {
        const tracyZone = ztracy.ZoneN(@src(), "init bytecode generator");
        defer tracyZone.End();
        return BytecodeGenerator{
            .currentFunction = null,
            .allocator = allocator,
            .scopeExtentStack = try .init(allocator),
            .scopeNamesStack = try .init(allocator),
            .bytecodeList = std.ArrayListUnmanaged(Instruction){},
            .stringBuffer = std.ArrayListUnmanaged(u8){},
            .variableRegistry = std.StringHashMapUnmanaged(HandledOperand).empty,
            .functionRegistry = std.StringHashMapUnmanaged(FunctionType).empty,
            // 0: Null handle, 1: Return handle
            .stackHeight = 2,
            .entryPoint = .none,
        };
    }
    pub fn deinit(self: *BytecodeGenerator) void {
        self.scopeExtentStack.deinit(self.allocator);
        self.scopeNamesStack.deinit(self.allocator);
        self.variableRegistry.deinit(self.allocator);
        const iter = self.functionRegistry.keyIterator();
        for (0..iter.len) |i| {
            // We're not too worried about the key being dropped before this is called.
            // IN THEORY the source file should live the entire duration of the program.
            self.allocator.free(self.functionRegistry.get(iter.items[i].?));
        }
        self.functionRegistry.deinit(self.allocator);
        self.bytecodeList.deinit(self.allocator);
        self.stringBuffer.deinit(self.allocator);
    }

    pub fn enterFunction(self: *BytecodeGenerator, ctx: Context, name: []u8, args: []ArgInfo, retType: Type) void {
        const tracyZone = ztracy.ZoneN(@src(), "bytecode enter function");
        defer tracyZone.End();

        if (self.currentFunction != null) {
            @panic("Nested function parsed! This should not happen due to parsing restrictions");
        }
        if (std.mem.eql(u8, name, "main")) {
            if (args.len != 0) {
                ctx.pushError(.{ .mainFunctionCannotHaveArguments = .{ .numArgsFound = @truncate(args.len) } });
            }
            if (retType != .nil) {
                ctx.pushError(.{ .mainFunctionCannotHaveReturnType = .{ .foundType = retType } });
            }
            self.entryPoint = @enumFromInt(self.bytecodeList.items.len + 1);
        }
        for (0..args.len) |i| {
            const argZone = ztracy.ZoneN(@src(), "bytecode process function arg");
            defer argZone.End();

            const arg = args[i];
            allocatorMust(void, self.variableRegistry.put(
                self.allocator,
                arg.name,
                .{ .type = arg.type, .operand = .{ .item = @as(u64, @intCast(self.stackHeight)) + @as(u64, @intCast(i)) } },
            ));
        }
        // "push" args, we add one so that the handles resume usage after the args
        self.stackHeight += @truncate(args.len);

        // When the function exits, then we will release this memory.
        const argsDuped = allocatorMust([]ArgInfo, self.allocator.dupe(ArgInfo, args));
        const func: CurrentFunctionContext = .{
            .start = self.bytecodeList.items.len,
            .args = argsDuped,
            .name = name,
            .retType = retType,
            .returnsOnAllPaths = false,
        };
        self.currentFunction = func;
    }

    pub fn exitFunction(self: *BytecodeGenerator, ctx: Context) void {
        const tracyZone = ztracy.ZoneN(@src(), "bytecode exit function");
        defer tracyZone.End();

        defer self.currentFunction = null;
        if (self.currentFunction) |f| {
            if (!f.returnsOnAllPaths) {
                if (f.retType != .nil) {
                    std.debug.print("NOT ALL CODE PATHS IN FUNCTION {s} RETURN\n", .{f.name});
                } else {
                    self.insertFunctionReturn(ctx, .NIL);
                }
            }
            for (f.args) |arg| {
                _ = self.variableRegistry.remove(arg.name);
            }
            // We know the scope for the variables will be cleaned up before this, so it's okay
            self.stackHeight -= @truncate(f.args.len);

            const func: FunctionType = .{ .args = f.args, .retType = f.retType, .start = f.start };
            allocatorMust(void, self.functionRegistry.put(self.allocator, f.name, func));
        }
    }

    pub fn enterScope(self: *BytecodeGenerator) void {
        const tracyZone = ztracy.ZoneN(@src(), "bytecode enter scope");
        defer tracyZone.End();

        self.scopeExtentStack.push(.{});
    }

    pub fn exitScope(self: *BytecodeGenerator) void {
        const tracyZone = ztracy.ZoneN(@src(), "bytecode exit scope");
        defer tracyZone.End();

        const num: ScopeExtent = self.scopeExtentStack.pop() orelse .{};
        for (0..num.numVars) |_| {
            const name = self.scopeNamesStack.pop() orelse break;
            _ = self.variableRegistry.remove(name);
        }
        for (0..num.numItems) |_| {
            self.popFromStack();
        }
    }

    pub fn callFunction(self: *BytecodeGenerator, ctx: Context, name: []u8, args: []HandledOperand) HandledOperand {
        const tracyZone = ztracy.ZoneN(@src(), "bytecode insert fn call");
        defer tracyZone.End();

        const func = self.functionRegistry.get(name) orelse {
            ctx.pushError(.{ .functionNotDefined = .{ .name = name } });
            return .ERR;
        };

        const argLen = l: {
            if (args.len != func.args.len) {
                ctx.pushError(.{ .incorrectNumberOfArguments = .{ .numExpected = @truncate(func.args.len), .numFound = @truncate(args.len) } });
                break :l @min(args.len, func.args.len);
            } else {
                break :l args.len;
            }
        };

        for (0..argLen) |i| {
            const arg = args[i];
            const argDecayedType = switch (arg.type) {
                .boolLit => .bool,
                .numberLit => .number,
                else => |s| s,
            };
            const argType: ArgTypes = switch (arg.type) {
                .boolLit, .numberLit => .literalAHandleB,
                else => .handleALiteralB,
            };
            const defArg = func.args[i];

            if (argDecayedType != defArg.type) {
                ctx.pushError(.{ .argumentTypeIncorrect = .{ .found = argDecayedType, .expected = defArg.type } });
            }

            allocatorMust(void, self.bytecodeList.append(self.allocator, Instruction{
                .op = .{ .argType = argType, .op = .pushArgument },
                .a = arg.operand,
                .b = .NULL_HANDLE,
                .dest = 0,
            }));
        }
        allocatorMust(void, self.bytecodeList.append(self.allocator, Instruction{
            .op = .{ .argType = .bothHandle, .op = .call },
            .a = .{ .item = @intCast(func.start) },
            .b = .NULL_HANDLE,
            .dest = 0,
        }));
        return HandledOperand{ .type = func.retType, .operand = .RET_HANDLE };
    }

    pub fn registerVariable(self: *BytecodeGenerator, ctx: Context, name: []u8, typeInfo: NewVariableTypeInfo) HandledOperand {
        const tracyZone = ztracy.ZoneN(@src(), "bytecode define variable");
        defer tracyZone.End();

        const res = allocatorMust(std.StringHashMapUnmanaged(HandledOperand).GetOrPutResult, self.variableRegistry.getOrPut(self.allocator, name));
        if (!res.found_existing) {
            self.scopeNamesStack.push(name);
            const handle = self.pushOperand(ctx, name, typeInfo);
            const scopeVarCount = self.scopeExtentStack.top();
            if (scopeVarCount != null) {
                (scopeVarCount orelse unreachable).numVars += 1;
            }
            res.value_ptr.* = handle;
            return handle;
        }
        ctx.pushError(.{ .functionAlreadyDefined = .{ .name = name } });
        return .ERR;
    }

    pub fn getVariable(self: *BytecodeGenerator, ctx: Context, name: []u8) HandledOperand {
        const tracyZone = ztracy.ZoneN(@src(), "bytecode get variable");
        defer tracyZone.End();

        return self.variableRegistry.get(name) orelse {
            ctx.pushError(.{ .variableNotDefined = .{ .name = name } });
            return .ERR;
        };
    }

    pub fn updateVariable(self: *BytecodeGenerator, ctx: Context, name: []u8, new: HandledOperand) HandledOperand {
        const tracyZone = ztracy.ZoneN(@src(), "bytecode insert variable update");
        defer tracyZone.End();

        const handle = self.variableRegistry.getPtr(name) orelse {
            ctx.pushError(.{ .variableNotDefined = .{ .name = name } });
            return .ERR;
        };
        handle.type = switch (new.type) {
            .boolLit => .bool,
            .numberLit => .number,
            else => |s| s,
        };
        return self.moveOperand(new, handle.*);
    }

    pub fn insertFunctionReturn(self: *BytecodeGenerator, ctx: Context, val: HandledOperand) void {
        const tracyZone = ztracy.ZoneN(@src(), "bytecode insert fn return");
        defer tracyZone.End();

        const f = self.currentFunction orelse return;
        self.currentFunction.?.returnsOnAllPaths = true;
        if (val.type != f.retType) {
            ctx.pushError(.{ .incompatibleTypeReturn = .{ .expectedType = f.retType, .foundType = val.type } });
            return;
        }
        if (f.retType != .nil) {
            const ret = HandledOperand{ .type = f.retType, .operand = RawOperand.RET_HANDLE };
            _ = self.moveOperand(val, ret);
        }
        allocatorMust(void, self.bytecodeList.append(self.allocator, Instruction{ .op = .{ .op = .ret, .argType = .bothHandle }, .a = .NULL_HANDLE, .b = .NULL_HANDLE, .dest = 0 }));
    }

    pub fn moveOperand(self: *BytecodeGenerator, item: HandledOperand, dest: HandledOperand) HandledOperand {
        const tracyZone = ztracy.ZoneN(@src(), "bytecode insert move");
        defer tracyZone.End();

        switch (dest.type) {
            .boolLit, .numberLit => @panic("Trying to move into a literal..? (this should not happen due to parsing)"),
            else => {},
        }
        const argType: ArgTypes = switch (item.type) {
            .boolLit, .numberLit => .bothLiteral,
            else => .bothHandle,
        };
        const retType: Type = switch (item.type) {
            .boolLit => .bool,
            .numberLit => .number,
            else => |s| s,
        };
        const ins: Instruction = .{ .op = .{ .argType = argType, .op = .move }, .a = item.operand, .b = RawOperand.NULL_HANDLE, .dest = @truncate(dest.operand.item) };
        allocatorMust(void, self.bytecodeList.append(self.allocator, ins));
        return .{ .type = retType, .operand = dest.operand };
    }

    pub fn popFromStack(self: *BytecodeGenerator) void {
        const tracyZone = ztracy.ZoneN(@src(), "bytecode insert pop");
        defer tracyZone.End();

        self.stackHeight -= 1;
        allocatorMust(void, self.bytecodeList.append(self.allocator, .{ .op = .{ .op = .pop, .argType = .bothHandle }, .a = .{ .item = 0 }, .b = .{ .item = 0 }, .dest = 0 }));
    }
    // name is only used for debugging currently
    pub fn pushOperand(self: *BytecodeGenerator, ctx: Context, debugName: []u8, info: NewVariableTypeInfo) HandledOperand {
        const tracyZone = ztracy.ZoneN(@src(), "bytecode insert push item");
        defer tracyZone.End();

        // This can store any (built-in) type.
        const variableSize = 1;
        const InitializeInformation = struct { value: HandledOperand, type: Type };
        const typeInfo: InitializeInformation = switch (info) {
            .provided => |t| .{ .value = t.initial orelse zero: {
                switch (t.type) {
                    .string => break :zero self.newStringLit(""),
                    else => |ty| break :zero .{ .operand = .{ .item = 0 }, .type = ty },
                }
            }, .type = t.type },
            .fromValue => |h| .{ .value = h, .type = h.type },
        };
        // "Decay" literal types into regular ones.
        const decayedType: Type = switch (typeInfo.type) {
            .numberLit => .number,
            .boolLit => .bool,
            else => |t| t,
        };
        // "Decay" literal types into regular ones.
        const decayedValueType: Type = switch (typeInfo.value.type) {
            .numberLit => .number,
            .boolLit => .bool,
            else => |t| t,
        };
        if (decayedType != decayedValueType) {
            ctx.pushError(.{ .incompatibleTypeInitialValue = .{ .expectedType = decayedType, .foundType = decayedValueType } });
            return .ERR;
        }
        return h: switch (typeInfo.type) {
            .string => {
                const strHandle = typeInfo.value.operand.item;

                std.debug.print("( REGISTER \"{s}\" STRING_HANDLE({d}) )\n", .{ debugName, strHandle });

                break :h .{ .operand = .{ .item = strHandle }, .type = .string };
            },
            .nil => HandledOperand.NIL,
            .errorType => HandledOperand.ERR,
            else => {
                {
                    const top = self.scopeExtentStack.top();
                    if (top != null) {
                        (top orelse unreachable).numItems += 1;
                    }
                }

                const arg: ArgTypes = switch (typeInfo.value.type) {
                    .numberLit, .boolLit => .bothLiteral,
                    else => .bothHandle,
                };
                const n = typeInfo.value.operand.item;

                const start = self.stackHeight;

                // Dest is unused, but we set it to the stack height just for convenience purposes
                const variable = Instruction{ .op = .{ .argType = arg, .op = .pushItem }, .a = .{ .item = n }, .b = .{ .item = variableSize }, .dest = self.stackHeight };
                self.stackHeight += variableSize;
                allocatorMust(void, self.bytecodeList.append(self.allocator, variable));

                break :h .{ .operand = .{ .item = @as(u64, start) }, .type = decayedType };
            },
        };
    }

    pub fn pushBinaryOperation(self: *BytecodeGenerator, ctx: Context, op: parsing.BinaryExprType, a: HandledOperand, b: HandledOperand) HandledOperand {
        const tracyZone = ztracy.ZoneN(@src(), "bytecode insert binary op");
        defer tracyZone.End();

        const InsInfo = struct {
            op: Operation,
            dest: HandledOperand,
        };
        const OpInfo = struct {
            op: OpCode,
            argType: Type,
            retType: Type,
        };
        const res: InsInfo = r: {
            const info: OpInfo = switch (op) {
                .add => .{ .op = .add, .argType = .number, .retType = .number },
                .subtract => .{ .op = .subtract, .argType = .number, .retType = .number },
                .multiply => .{ .op = .multiply, .argType = .number, .retType = .number },
                .modulo => .{ .op = .modulo, .argType = .number, .retType = .number },
                .divide => .{ .op = .divide, .argType = .number, .retType = .number },
                .notEquality => .{ .op = .neq, .argType = .number, .retType = .bool },
                .equality => .{ .op = .eq, .argType = .number, .retType = .bool },
                .greater => .{ .op = .greater, .argType = .number, .retType = .bool },
                .greaterEqual => .{ .op = .ge, .argType = .number, .retType = .bool },
                .less => .{ .op = .less, .argType = .number, .retType = .bool },
                .lessEqual => .{ .op = .le, .argType = .number, .retType = .bool },
                .bOr => .{ .op = .bOr, .argType = .bool, .retType = .bool },
                .bAnd => .{ .op = .bAnd, .argType = .bool, .retType = .bool },
            };
            const aTypeDecayed: Type = switch (a.type) {
                .errorType => info.argType,
                .numberLit => .number,
                .boolLit => .bool,
                else => |t| t,
            };
            const bTypeDecayed: Type = switch (b.type) {
                .errorType => info.argType,
                .numberLit => .number,
                .boolLit => .bool,
                else => |t| t,
            };

            if (aTypeDecayed != info.argType or bTypeDecayed != info.argType) {
                ctx.pushError(.{ .incompatibleTypeBinary = .{ .operation = op, .lhsType = a.type, .rhsType = b.type } });
                return .ERR;
            }

            var argFlag = @intFromEnum(ArgTypes.bothHandle);

            argFlag |= switch (a.type) {
                .numberLit, .boolLit => @intFromEnum(ArgTypes.literalAHandleB),
                else => @intFromEnum(ArgTypes.bothHandle),
            };
            argFlag |= switch (b.type) {
                .numberLit, .boolLit => @intFromEnum(ArgTypes.handleALiteralB),
                else => @intFromEnum(ArgTypes.bothHandle),
            };

            const newVar: NewVariableTypeInfo = .{ .provided = .{ .type = info.retType, .initial = null } };
            const dest = self.pushOperand(ctx, @constCast("TEMP TEMP TEMP TEMP"), newVar);
            break :r .{ .op = .{ .op = info.op, .argType = @as(ArgTypes, @enumFromInt(argFlag)) }, .dest = dest };
        };
        const item = Instruction{ .op = res.op, .a = a.operand, .b = b.operand, .dest = @truncate(res.dest.operand.item) };
        allocatorMust(void, self.bytecodeList.append(self.allocator, item));
        return res.dest;
    }

    pub fn pushUnaryOperation(self: *BytecodeGenerator, ctx: Context, op: parsing.UnaryExprType, a: HandledOperand) HandledOperand {
        const tracyZone = ztracy.ZoneN(@src(), "bytecode insert unary op");
        defer tracyZone.End();

        const res: Operation = switch (op) {
            .negate => switch (a.type) {
                .number => .{ .op = .negateNumber, .argType = .bothHandle },
                .numberLit, .errorType => .{ .op = .negateNumber, .argType = .bothLiteral },
                else => {
                    ctx.pushError(.{ .incompatibleTypeUnary = .{ .operation = op, .foundType = a.type } });
                    return .ERR;
                },
            },
            .negateBool => switch (a.type) {
                .bool => .{ .op = .negateBool, .argType = .bothHandle },
                .boolLit, .errorType => .{ .op = .negateBool, .argType = .bothLiteral },
                else => {
                    ctx.pushError(.{ .incompatibleTypeUnary = .{ .operation = op, .foundType = a.type } });
                    return .ERR;
                },
            },
        };
        const dest = self.pushOperand(ctx, @constCast("TEMP TEMP TEMP TEMP"), .{ .provided = .{ .type = a.type, .initial = a } });
        const item = Instruction{ .op = res, .a = a.operand, .b = .NULL_HANDLE, .dest = @truncate(dest.operand.item) };
        allocatorMust(void, self.bytecodeList.append(self.allocator, item));
        return dest;
    }
    pub fn newStringLit(self: *BytecodeGenerator, string: []u8) HandledOperand {
        const tracyZone = ztracy.ZoneN(@src(), "bytecode allocate new string lit");
        defer tracyZone.End();

        // allocate shit ig
        const strStart = self.stringBuffer.items.len;
        allocatorMust(void, self.stringBuffer.appendSlice(self.allocator, string));

        const start = self.stackHeight;
        self.stackHeight += 2 * @sizeOf(u64);
        const ptr = Instruction{ .op = .{ .argType = .bothHandle, .op = .pushItem }, .a = .{ .item = @bitCast(strStart) }, .b = .{ .item = 8 }, .dest = 0 };
        allocatorMust(void, self.bytecodeList.append(self.allocator, ptr));
        const len = Instruction{ .op = .{ .argType = .bothHandle, .op = .pushItem }, .a = .{ .item = @bitCast(string.len) }, .b = .{ .item = 8 }, .dest = 0 };
        allocatorMust(void, self.bytecodeList.append(self.allocator, len));

        const top = self.scopeExtentStack.top();
        if (top) |t| {
            t.numItems += 2;
        }

        return .{ .operand = .{ .item = @as(u64, start) }, .type = .string };
    }
    pub fn newNumberLit(number: f64) HandledOperand {
        return HandledOperand{ .operand = .{ .item = @bitCast(number) }, .type = .numberLit };
    }
    pub fn newBoolLit(boolean: bool) HandledOperand {
        return HandledOperand{ .operand = .{ .item = @as(u64, @intFromBool(boolean)) }, .type = .boolLit };
    }
    pub fn newNilLit() HandledOperand {
        return .{ .operand = RawOperand.NULL_HANDLE, .type = .nil };
    }
};

pub fn printInstruction(ins: Instruction, out: *std.Io.Writer) !void {
    switch (ins.op.op) {
        .move => switch (ins.op.argType) {
            .bothHandle, .handleALiteralB => try out.print("( MOV HANDLE({d}) ", .{ins.a.item}),
            .bothLiteral, .literalAHandleB => try out.print("( MOV LIT(ASNUM({d}), ASBOOL({s}), ASUINT({d})) ", .{ @as(f64, @bitCast(ins.a.item)), if (ins.a.item != 0) "TRUE" else "FALSE", ins.a.item }),
        },
        .noop => _ = try out.write("( NOP "),
        .call => try out.print("( CAL {d} ", .{ins.a.item}),
        .ret => _ = try out.write("( RET "),
        .negateBool => switch (ins.op.argType) {
            .bothHandle, .handleALiteralB => try out.print("( NOT HANDLE({d}) ", .{ins.a.item}),
            .bothLiteral, .literalAHandleB => try out.print("( NOT LIT({s}) ", .{if (ins.a.item != 0) "TRUE" else "FALSE"}),
        },
        .negateNumber => switch (ins.op.argType) {
            .bothHandle, .handleALiteralB => try out.print("( NEG HANDLE({d}) ", .{ins.a.item}),
            .bothLiteral, .literalAHandleB => try out.print("( NEG LIT({d}) ", .{@as(f64, @bitCast(ins.a.item))}),
        },
        // types are erased so yeah
        .pushItem => switch (ins.op.argType) {
            .bothHandle, .handleALiteralB => try out.print("( PSH HANDLE({d}) ", .{ins.a.item}),
            .bothLiteral, .literalAHandleB => try out.print("( PSH LIT(ASNUM({d:.4}), ASBOOL({s}), ASUINT({d})) ", .{ @as(f64, @bitCast(ins.a.item)), if (ins.a.item != 0) "TRUE" else "FALSE", ins.a.item }),
        },
        .pushArgument => switch (ins.op.argType) {
            .bothHandle, .handleALiteralB => try out.print("( ARG HANDLE({d}) ", .{ins.a.item}),
            .bothLiteral, .literalAHandleB => try out.print("( ARG LIT(ASNUM({d:.4}), ASBOOL({s}), ASUINT({d})) ", .{ @as(f64, @bitCast(ins.a.item)), if (ins.a.item != 0) "TRUE" else "FALSE", ins.a.item }),
        },
        else => {
            const name = switch (ins.op.op) {
                .add => "ADD",
                .subtract => "SUB",
                .multiply => "MUL",
                .divide => "DIV",
                .modulo => "MOD",
                .neq => "NEQ",
                .eq => "EQL",
                .ge => "GRE",
                .le => "LSE",
                .greater => "GRT",
                .less => "LES",
                .bAnd => "AND",
                .bOr => "OR",
                .pop => "POP",
                else => @panic("ahhhh what the hell"),
            };
            switch (ins.op.argType) {
                .bothHandle => try out.print("( {s} HANDLE({d}) HANDLE({d}) ", .{ name, ins.a.item, ins.b.item }),
                .handleALiteralB => try out.print("( {s} HANDLE({d}) LIT(ASNUM({d:.4}), ASBOOL({s}), ASUINT({d})) ", .{ name, ins.a.item, @as(f64, @bitCast(ins.b.item)), if (ins.b.item != 0) "TRUE" else "FALSE", ins.b.item }),
                .bothLiteral => try out.print("( {s} LIT(ASNUM({d}), ASBOOL({s}), ASUINT({d})) LIT(ASNUM({d:.4}), ASBOOL({s}), ASUINT({d})) ", .{ name, @as(f64, @bitCast(ins.a.item)), if (ins.a.item != 0) "TRUE" else "FALSE", ins.a.item, @as(f64, @bitCast(ins.b.item)), if (ins.b.item != 0) "TRUE" else "FALSE", ins.b.item }),
                .literalAHandleB => try out.print("( {s} LIT(ASNUM({d:.4}), ASBOOL({s}), ASUINT({d})) HANDLE({d}) ", .{ name, @as(f64, @bitCast(ins.a.item)), if (ins.a.item != 0) "TRUE" else "FALSE", ins.a.item, ins.b.item }),
            }
        },
    }
    try out.print("DEST({d}) )", .{ins.dest});
    try out.writeByte('\n');
}
