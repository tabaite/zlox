const std = @import("std");
// circular imports are allowed!!!
const parsing = @import("parsing.zig");
const Allocator = std.mem.Allocator;

pub const CompilationError = error{
    IncompatibleType,
    CannotMoveIntoLiteral,
    VariableNotDeclared,
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
    handleAliteralB = 1,
    literalAHandleB = 2,
    bothLiteral = 3,
};

pub const OpCode = enum(u30) {
    // Just for now!
    noop,
    // Used for pushing the index/length of string literals.
    // A: Bytes to be pushed, B: Number of bytes to push (MAX 8), Dest: Unused, Arg Type: Unused
    pushBytes,
    // A: Number to be negated, B: Unused, Dest: Where to store the result, Arg Type: Used for A
    negateNumber,
    // A: Bool to be negated, B: Unused, Dest: Where to store the result, Arg Type: Used for A
    negateBool,
    // A, B: Numbers to add, Dest: Where to store the result, Arg Type: Used for A and B
    add,
    // A, B: Numbers to subtract, Dest: Where to store the result, Arg Type: Used for A and B
    subtract,
    // A, B: Numbers to multiply, Dest: Where to store the result, Arg Type: Used for A and B
    multiply,
    // A, B: Numbers to divide, Dest: Where to store the result, Arg Type: Used for A and B
    divide,
    // A, B: Numbers to compare, Dest: Where to store the result, Arg Type: Used for A and B
    eq,
    // A, B: Numbers to compare, Dest: Where to store the result, Arg Type: Used for A and B
    neq,
    // A, B: Bools to or, Dest: Where to store the result, Arg Type: Used for A and B
    bOr,
    // A, B: Bools to and, Dest: Where to store the result, Arg Type: Used for A and B
    bAnd,
    // A: Item to move (literal or handle), B: Unused, Dest: Where to move to, Arg Type: Used for A
    move,
};

pub const Operation = struct {
    argType: ArgTypes,
    op: OpCode,
};

pub const Handle = u32;
pub const Type = enum(u64) {
    nil,
    number,
    numberLit,
    string,
    bool,
    boolLit,
    // this is for user classes, but i'll deal with that later
    // _,
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

pub const BytecodeGenerator = struct {
    allocator: Allocator,
    variableRegistry: std.StringHashMapUnmanaged(HandledOperand),
    bytecodeList: std.ArrayListUnmanaged(Instruction),
    stringBuffer: std.ArrayListUnmanaged(u8),
    // Tracks how high the stack is currently in bytes.
    stackHeight: u32,

    pub fn init(allocator: Allocator) !BytecodeGenerator {
        return BytecodeGenerator{
            .allocator = allocator,
            .bytecodeList = std.ArrayListUnmanaged(Instruction){},
            .stringBuffer = std.ArrayListUnmanaged(u8){},
            .variableRegistry = std.StringHashMapUnmanaged(HandledOperand).empty,
            .stackHeight = 0,
        };
    }
    pub fn deinit(self: *BytecodeGenerator) void {
        self.variableRegistry.deinit(self.allocator);
        self.bytecodeList.deinit(self.allocator);
        self.stringBuffer.deinit(self.allocator);
    }

    pub fn registerVariable(self: *BytecodeGenerator, name: []u8, initialValue: ?HandledOperand) !HandledOperand {
        const handle = try self.pushOperand(name, initialValue);
        try self.variableRegistry.put(self.allocator, name, handle);
        return handle;
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

    // name is only used for debugging currently
    pub fn pushOperand(self: *BytecodeGenerator, debugName: []u8, initialValue: ?HandledOperand) !HandledOperand {
        // This can store any (built-in) type.
        const variableSize = 8;
        return h: switch ((initialValue orelse HandledOperand.NIL).type) {
            .string => {
                const strHandle = (initialValue orelse unreachable).operand.item;

                std.debug.print("( REGISTER \"{s}\" STRING_HANDLE({d}) )\n", .{ debugName, strHandle });

                break :h .{ .operand = .{ .item = strHandle }, .type = .string };
            },
            .nil => HandledOperand.NIL,
            else => |ty| {
                const n = if (ty == .numberLit or ty == .boolLit) (initialValue orelse unreachable).operand.item else 0;

                const start = self.stackHeight;

                // Dest is unused, but we set it to the stack height just for convenience purposes
                const variable = Instruction{ .op = .{ .argType = .bothHandle, .op = .pushBytes }, .a = .{ .item = n }, .b = .{ .item = variableSize }, .dest = self.stackHeight };
                self.stackHeight += variableSize;
                try self.bytecodeList.append(self.allocator, variable);

                const t: Type = switch (ty) {
                    .numberLit => .number,
                    .boolLit => .bool,
                    else => |t| t,
                };
                break :h .{ .operand = .{ .item = @as(u64, start) }, .type = t };
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
                .divide => .{ .op = .divide, .argType = .number, .retType = .number },
                .notEquality => .{ .op = .neq, .argType = .number, .retType = .bool },
                .equality => .{ .op = .eq, .argType = .number, .retType = .bool },
                .bOr => .{ .op = .bOr, .argType = .bool, .retType = .bool },
                .bAnd => .{ .op = .bAnd, .argType = .bool, .retType = .bool },
                else => break :r .{ .op = .{ .op = .noop, .argType = .bothLiteral }, .dest = .NIL },
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
                .numberLit, .boolLit => @intFromEnum(ArgTypes.handleAliteralB),
                else => @intFromEnum(ArgTypes.bothHandle),
            };

            const dest = try self.pushOperand(@constCast("TEMP TEMP TEMP TEMP"), .{ .type = info.retType, .operand = .{ .item = 0 } });
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
        const dest = try self.pushOperand(@constCast("TEMP TEMP TEMP TEMP"), a);
        const item = Instruction{ .op = res, .a = a.operand, .b = .NULL_HANDLE, .dest = @truncate(dest.operand.item) };
        try self.bytecodeList.append(self.allocator, item);
        return dest;
    }
    pub fn newLiteral(self: *BytecodeGenerator, lit: parsing.Literal) !HandledOperand {
        switch (lit) {
            .string => |s| {
                std.debug.print("pushed \"{s}\" BUT not really since strings are hard\n", .{s});

                // allocate shit ig
                const strStart = self.stringBuffer.items.len;
                try self.stringBuffer.appendSlice(self.allocator, s);

                const start = self.stackHeight;
                self.stackHeight += 2 * @sizeOf(u64);
                const ptr = Instruction{ .op = .{ .argType = .bothHandle, .op = .pushBytes }, .a = .{ .item = @bitCast(strStart) }, .b = .{ .item = 8 }, .dest = 0 };
                try self.bytecodeList.append(self.allocator, ptr);
                const len = Instruction{ .op = .{ .argType = .bothHandle, .op = .pushBytes }, .a = .{ .item = @bitCast(s.len) }, .b = .{ .item = 8 }, .dest = 0 };
                try self.bytecodeList.append(self.allocator, len);

                return .{ .operand = .{ .item = @as(u64, start) }, .type = .string };
            },
            .number => |n| {
                std.debug.print("pushed number {d}\n", .{n});
                return HandledOperand{ .operand = .{ .item = @bitCast(n) }, .type = .numberLit };
            },
            .bool => |b| {
                std.debug.print("pushed {s}\n", .{if (b) "true" else "false"});
                return HandledOperand{ .operand = .{ .item = @as(u64, @intFromBool(b)) }, .type = .boolLit };
            },
            .nil => return .{ .operand = RawOperand.NULL_HANDLE, .type = .nil },
        }
    }
};

pub fn printInstruction(ins: Instruction, out: std.io.AnyWriter) !void {
    switch (ins.op.op) {
        .move => switch (ins.op.argType) {
            .bothHandle, .handleAliteralB => try out.print("( MOV HANDLE({d}) ", .{ins.a.item}),
            .bothLiteral, .literalAHandleB => try out.print("( MOV LIT(ASNUM({d}), ASBOOL({s}), ASUINT({d})) ", .{ @as(f64, @bitCast(ins.a.item)), if (ins.a.item != 0) "TRUE" else "FALSE", ins.a.item }),
        },
        .noop => _ = try out.write("( NOP "),
        .negateBool => switch (ins.op.argType) {
            .bothHandle, .handleAliteralB => try out.print("( NOT HANDLE({d}) ", .{ins.a.item}),
            .bothLiteral, .literalAHandleB => try out.print("( NOT LIT({s}) ", .{if (ins.a.item != 0) "TRUE" else "FALSE"}),
        },
        .negateNumber => switch (ins.op.argType) {
            .bothHandle, .handleAliteralB => try out.print("( NEG HANDLE({d}) ", .{ins.a.item}),
            .bothLiteral, .literalAHandleB => try out.print("( NEG LIT({d}) ", .{@as(f64, @bitCast(ins.a.item))}),
        },
        // types are erased so yeah
        .pushBytes => try out.print("( PSH LIT(ASNUM({d:.4}), ASBOOL({s}), ASUINT({d})) SIZE({d}) ", .{ @as(f64, @bitCast(ins.a.item)), if (ins.a.item != 0) "TRUE" else "FALSE", ins.a.item, ins.b.item }),
        else => {
            const name = switch (ins.op.op) {
                .add => "ADD",
                .subtract => "SUB",
                .multiply => "MUL",
                .divide => "DIV",
                .neq => "NEQ",
                .eq => "EQL",
                .bAnd => "AND",
                .bOr => "OR",
                else => @panic("ahhhh what the hell"),
            };
            switch (ins.op.argType) {
                .bothHandle => try out.print("( {s} HANDLE({d}) HANDLE({d}) ", .{ name, ins.a.item, ins.b.item }),
                .handleAliteralB => try out.print("( {s} HANDLE({d}) LIT(ASNUM({d:.4}), ASBOOL({s}), ASUINT({d})) ", .{ name, ins.a.item, @as(f64, @bitCast(ins.b.item)), if (ins.b.item != 0) "TRUE" else "FALSE", ins.b.item }),
                .bothLiteral => try out.print("( {s} LIT(ASNUM({d}), ASBOOL({s}), ASUINT({d})) LIT(ASNUM({d:.4}), ASBOOL({s}), ASUINT({d})) ", .{ name, @as(f64, @bitCast(ins.a.item)), if (ins.a.item != 0) "TRUE" else "FALSE", ins.a.item, @as(f64, @bitCast(ins.b.item)), if (ins.b.item != 0) "TRUE" else "FALSE", ins.b.item }),
                .literalAHandleB => try out.print("( {s} LIT(ASNUM({d:.4}), ASBOOL({s}), ASUINT({d})) HANDLE({d}) ", .{ name, @as(f64, @bitCast(ins.a.item)), if (ins.a.item != 0) "TRUE" else "FALSE", ins.a.item, ins.b.item }),
            }
        },
    }
    try out.print("DEST({d}) )", .{ins.dest});
    try out.writeByte('\n');
}
