const std = @import("std");
// circular imports are allowed!!!
const parsing = @import("parsing.zig");
const common = @import("common.zig");
const Allocator = std.mem.Allocator;

pub const CompilationError = error{
    FunctionsCannotBeNested,
    IncompatibleType,
    CannotMoveIntoLiteral,
    VariableNotDeclared,
    VariableAlreadyDeclared,
    MainFunctionNotDeclared,
    MainFunctionCannotHaveArgs,
    MainFunctionCannotReturnValue,
};

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

    // A: Item to move (literal or handle), B: Unused, Dest: Where to move to, Arg Type: Used for A
    move,
};

pub const Operation = struct {
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
    // this is for user classes, but i'll deal with that later
    // _,
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

// Operand, but with a type. Not used in bytecode, but rather for variable tracking and pushing operations.
pub const HandledOperand = struct {
    operand: RawOperand,
    type: Type,

    pub const NIL: HandledOperand = .{ .operand = .NULL_HANDLE, .type = .nil };
};

pub const RawOperand = packed struct {
    item: u64,
    pub const NULL_HANDLE: RawOperand = .{ .item = 0 };
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
const ScopeExtentStack = common.Stack(ScopeExtent, 32767);
const ScopeNamesStack = common.Stack([]u8, 32767);

const CurrentFunctionInfo = struct {
    name: []u8,
    argsUsed: usize,
    args: [128]Type,
    retType: Type,
};

pub const BytecodeGenerator = struct {
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
    bytecodeList: std.ArrayListUnmanaged(Instruction),
    stringBuffer: std.ArrayListUnmanaged(u8),
    // Tracks how high the stack is currently in RawOperands.
    stackHeight: u32,
    // Because the first "instruction" is the entry point, 0 is used here to represent none.
    entryPoint: enum(u32) { none = 0, _ },
    currentFunction: ?CurrentFunctionInfo,

    pub fn finalize(self: *BytecodeGenerator) !Program {
        if (self.entryPoint == .none) {
            return CompilationError.MainFunctionNotDeclared;
        }
        return .{ .entryPoint = @as(usize, @intFromEnum(self.entryPoint)) - 1, .instructions = self.bytecodeList.items };
    }

    pub fn init(allocator: Allocator) !BytecodeGenerator {
        return BytecodeGenerator{
            .currentFunction = null,
            .allocator = allocator,
            .scopeExtentStack = try .init(allocator),
            .scopeNamesStack = try .init(allocator),
            .bytecodeList = std.ArrayListUnmanaged(Instruction){},
            .stringBuffer = std.ArrayListUnmanaged(u8){},
            .variableRegistry = std.StringHashMapUnmanaged(HandledOperand).empty,
            // The first element on the stack is the null handle.
            .stackHeight = 1,
            .entryPoint = .none,
        };
    }
    pub fn deinit(self: *BytecodeGenerator) void {
        self.scopeExtentStack.deinit(self.allocator);
        self.scopeNamesStack.deinit(self.allocator);
        self.variableRegistry.deinit(self.allocator);
        self.bytecodeList.deinit(self.allocator);
        self.stringBuffer.deinit(self.allocator);
    }

    pub fn enterFunction(self: *BytecodeGenerator, name: []u8, argumentTypes: []Type, retType: Type) !void {
        if (self.currentFunction != null) {
            return CompilationError.FunctionsCannotBeNested;
        }
        if (std.mem.eql(u8, name, "main")) {
            if (argumentTypes.len != 0) {
                return CompilationError.MainFunctionCannotHaveArgs;
            }
            if (retType != .nil) {
                return CompilationError.MainFunctionCannotReturnValue;
            }
            std.debug.print("ENTRY POINT main\n", .{});
            self.entryPoint = @enumFromInt(self.bytecodeList.items.len + 1);
            return;
        }
        std.debug.print("function \"{s}\" ( ", .{name});
        for (0..argumentTypes.len) |i| {
            const ty = switch (argumentTypes[i]) {
                .nil => "void",
                .number, .numberLit => "num",
                .bool, .boolLit => "bool",
                .string => "string",
            };

            std.debug.print("(type {s}) ", .{ty});
        }
        const ty = switch (retType) {
            .nil => "void",
            .number, .numberLit => "num",
            .bool, .boolLit => "bool",
            .string => "string",
        };
        std.debug.print(") RETURNS {s}\n", .{ty});
        var func: CurrentFunctionInfo = .{ .argsUsed = argumentTypes.len, .args = undefined, .name = name, .retType = retType };
        std.mem.copyForwards(Type, &func.args, argumentTypes);
        self.currentFunction = func;
    }

    pub fn exitFunction(self: *BytecodeGenerator) void {
        self.currentFunction = null;
    }

    pub fn enterScope(self: *BytecodeGenerator) void {
        self.scopeExtentStack.push(.{});
        std.debug.print("entered scope\n", .{});
    }

    pub fn exitScope(self: *BytecodeGenerator) !void {
        const num: ScopeExtent = self.scopeExtentStack.pop() orelse .{};
        for (0..num.numVars) |_| {
            const name = self.scopeNamesStack.pop() orelse break;
            _ = self.variableRegistry.remove(name);
        }
        for (0..num.numItems) |_| {
            try self.popFromStack();
        }
        std.debug.print("exited scope\n", .{});
    }

    pub fn registerVariable(self: *BytecodeGenerator, name: []u8, typeInfo: NewVariableTypeInfo) !HandledOperand {
        const res = try self.variableRegistry.getOrPut(self.allocator, name);
        if (!res.found_existing) {
            self.scopeNamesStack.push(name);
            const handle = try self.pushOperand(name, typeInfo);
            const scopeVarCount = self.scopeExtentStack.top();
            if (scopeVarCount != null) {
                (scopeVarCount orelse unreachable).numVars += 1;
            }
            res.value_ptr.* = handle;
            return handle;
        }
        return CompilationError.VariableAlreadyDeclared;
    }

    pub fn getVariable(self: *BytecodeGenerator, name: []u8) !HandledOperand {
        return self.variableRegistry.get(name) orelse return CompilationError.VariableNotDeclared;
    }

    pub fn updateVariable(self: *BytecodeGenerator, name: []u8, new: HandledOperand) !HandledOperand {
        const handle = self.variableRegistry.getPtr(name) orelse unreachable;
        const oldType = handle.type;
        handle.type = switch (new.type) {
            .boolLit => .bool,
            .numberLit => .number,
            else => |s| s,
        };
        errdefer handle.type = oldType;
        return try self.moveOperand(new, handle.*);
    }

    pub fn moveOperand(self: *BytecodeGenerator, item: HandledOperand, dest: HandledOperand) !HandledOperand {
        switch (dest.type) {
            .boolLit, .numberLit => return CompilationError.CannotMoveIntoLiteral,
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
        try self.bytecodeList.append(self.allocator, ins);
        return .{ .type = retType, .operand = dest.operand };
    }

    pub fn popFromStack(self: *BytecodeGenerator) !void {
        self.stackHeight -= 1;
        try self.bytecodeList.append(self.allocator, .{ .op = .{ .op = .pop, .argType = .bothHandle }, .a = .{ .item = 0 }, .b = .{ .item = 0 }, .dest = 0 });
    }

    // name is only used for debugging currently
    pub fn pushOperand(self: *BytecodeGenerator, debugName: []u8, info: NewVariableTypeInfo) !HandledOperand {
        // This can store any (built-in) type.
        const variableSize = 1;
        const InitializeInformation = struct { value: HandledOperand, type: Type };
        const typeInfo: InitializeInformation = switch (info) {
            .provided => |t| .{ .value = t.initial orelse zero: {
                switch (t.type) {
                    .string => break :zero try self.newStringLit(""),
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
            return CompilationError.IncompatibleType;
        }
        return h: switch (typeInfo.type) {
            .string => {
                const strHandle = typeInfo.value.operand.item;

                std.debug.print("( REGISTER \"{s}\" STRING_HANDLE({d}) )\n", .{ debugName, strHandle });

                break :h .{ .operand = .{ .item = strHandle }, .type = .string };
            },
            .nil => HandledOperand.NIL,
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
                try self.bytecodeList.append(self.allocator, variable);

                break :h .{ .operand = .{ .item = @as(u64, start) }, .type = decayedType };
            },
        };
    }

    pub fn pushBinaryOperation(self: *BytecodeGenerator, op: parsing.BinaryExprType, a: HandledOperand, b: HandledOperand) !HandledOperand {
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
                .numberLit => .number,
                .boolLit => .bool,
                else => |t| t,
            };
            const bTypeDecayed: Type = switch (a.type) {
                .numberLit => .number,
                .boolLit => .bool,
                else => |t| t,
            };

            if (aTypeDecayed != info.argType or bTypeDecayed != info.argType) {
                return CompilationError.IncompatibleType;
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
            const dest = try self.pushOperand(@constCast("TEMP TEMP TEMP TEMP"), newVar);
            break :r .{ .op = .{ .op = info.op, .argType = @as(ArgTypes, @enumFromInt(argFlag)) }, .dest = dest };
        };
        const item = Instruction{ .op = res.op, .a = a.operand, .b = b.operand, .dest = @truncate(res.dest.operand.item) };
        try self.bytecodeList.append(self.allocator, item);
        return res.dest;
    }

    pub fn pushUnaryOperation(self: *BytecodeGenerator, op: parsing.UnaryExprType, a: HandledOperand) !HandledOperand {
        const res: Operation = switch (op) {
            .negate => switch (a.type) {
                .number => .{ .op = .negateNumber, .argType = .bothHandle },
                .numberLit => .{ .op = .negateNumber, .argType = .bothLiteral },
                else => return CompilationError.IncompatibleType,
            },
            .negateBool => switch (a.type) {
                .bool => .{ .op = .negateBool, .argType = .bothHandle },
                .boolLit => .{ .op = .negateBool, .argType = .bothLiteral },
                else => return CompilationError.IncompatibleType,
            },
        };
        std.debug.print("pushed unary\n", .{});
        const dest = try self.pushOperand(@constCast("TEMP TEMP TEMP TEMP"), .{ .provided = .{ .type = a.type, .initial = a } });
        const item = Instruction{ .op = res, .a = a.operand, .b = .NULL_HANDLE, .dest = @truncate(dest.operand.item) };
        try self.bytecodeList.append(self.allocator, item);
        return dest;
    }
    pub fn newStringLit(self: *BytecodeGenerator, string: []u8) !HandledOperand {
        // allocate shit ig
        const strStart = self.stringBuffer.items.len;
        try self.stringBuffer.appendSlice(self.allocator, string);

        const start = self.stackHeight;
        self.stackHeight += 2 * @sizeOf(u64);
        const ptr = Instruction{ .op = .{ .argType = .bothHandle, .op = .pushItem }, .a = .{ .item = @bitCast(strStart) }, .b = .{ .item = 8 }, .dest = 0 };
        try self.bytecodeList.append(self.allocator, ptr);
        const len = Instruction{ .op = .{ .argType = .bothHandle, .op = .pushItem }, .a = .{ .item = @bitCast(string.len) }, .b = .{ .item = 8 }, .dest = 0 };
        try self.bytecodeList.append(self.allocator, len);

        const top = self.scopeExtentStack.top();
        if (top != null) {
            (top orelse unreachable).numItems += 2;
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

pub fn printInstruction(ins: Instruction, out: std.io.AnyWriter) !void {
    switch (ins.op.op) {
        .move => switch (ins.op.argType) {
            .bothHandle, .handleALiteralB => try out.print("( MOV HANDLE({d}) ", .{ins.a.item}),
            .bothLiteral, .literalAHandleB => try out.print("( MOV LIT(ASNUM({d}), ASBOOL({s}), ASUINT({d})) ", .{ @as(f64, @bitCast(ins.a.item)), if (ins.a.item != 0) "TRUE" else "FALSE", ins.a.item }),
        },
        .noop => _ = try out.write("( NOP "),
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
