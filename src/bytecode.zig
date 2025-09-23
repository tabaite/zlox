const std = @import("std");
// circular imports are allowed!!!
const parsing = @import("parsing.zig");
const Allocator = std.mem.Allocator;

pub const CompilationError = error{
    IncompatibleType,
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
    bytecodeList: std.ArrayListUnmanaged(Instruction),
    stringBuffer: std.ArrayListUnmanaged(u8),
    // Tracks how high the stack is currently in bytes.
    stackHeight: u32,

    pub fn init(allocator: Allocator) !BytecodeGenerator {
        return BytecodeGenerator{
            .allocator = allocator,
            .bytecodeList = std.ArrayListUnmanaged(Instruction){},
            .stringBuffer = std.ArrayListUnmanaged(u8){},
            .stackHeight = 0,
        };
    }

    pub fn registerVariable(self: *BytecodeGenerator, name: []u8, initialValue: ?HandledOperand) !HandledOperand {
        return self.pushOperand(name, initialValue);
    }

    // name is only used for debugging currently
    pub fn pushOperand(self: *BytecodeGenerator, name: []u8, initialValue: ?HandledOperand) !HandledOperand {
        return h: switch ((initialValue orelse HandledOperand.NIL).type) {
            .numberLit => {
                const n = (initialValue orelse unreachable).operand.item;

                std.debug.print("( REGISTER \"{s}\" NUMBER({d}) )\n", .{ name, n });
                const start = self.stackHeight;

                const floatSz = @sizeOf(f64);

                // Dest is unused, but we set it to the stack height just for convenience purposes
                const variable = Instruction{ .op = .{ .argType = .bothHandle, .op = .pushBytes }, .a = .{ .item = n }, .b = .{ .item = floatSz }, .dest = self.stackHeight };
                self.stackHeight += floatSz;
                try self.bytecodeList.append(self.allocator, variable);

                break :h .{ .operand = .{ .item = @as(u64, start) }, .type = .number };
            },
            .number, .bool => return initialValue orelse unreachable,
            .boolLit => {
                const bol: u64 = (initialValue orelse unreachable).operand.item;

                std.debug.print("( REGISTER \"{s}\" BOOLEAN({s}) )\n", .{ name, if (bol != 0) "TRUE" else "FALSE" });
                const start = self.stackHeight;

                const boolSz = @sizeOf(bool);

                // Dest is unused, but we set it to the stack height just for convenience purposes
                const variable = Instruction{ .op = .{ .argType = .bothHandle, .op = .pushBytes }, .a = .{ .item = bol }, .b = .{ .item = boolSz }, .dest = self.stackHeight };
                self.stackHeight += boolSz;
                try self.bytecodeList.append(self.allocator, variable);

                break :h .{ .operand = .{ .item = @as(u64, start) }, .type = .bool };
            },
            .string => {
                const strHandle = (initialValue orelse unreachable).operand.item;

                std.debug.print("( REGISTER \"{s}\" STRING_HANDLE({d}) )\n", .{ name, strHandle });

                break :h .{ .operand = .{ .item = strHandle }, .type = .string };
            },
            .nil => HandledOperand.NIL,
        };
    }

    pub fn pushBinaryOperation(self: *BytecodeGenerator, op: parsing.BinaryExprType, a: HandledOperand, b: HandledOperand) !HandledOperand {
        const dest = try self.pushOperand(@constCast("TEMP TEMP TEMP TEMP"), a);
        const res: Operation = r: switch (op) {
            .add => {
                var argFlag = @intFromEnum(ArgTypes.bothHandle);
                argFlag |= switch (a.type) {
                    .number => @intFromEnum(ArgTypes.bothHandle),
                    .numberLit => @intFromEnum(ArgTypes.literalAHandleB),
                    else => return CompilationError.IncompatibleType,
                };
                argFlag |= switch (b.type) {
                    .number => @intFromEnum(ArgTypes.bothHandle),
                    .numberLit => @intFromEnum(ArgTypes.handleAliteralB),
                    else => return CompilationError.IncompatibleType,
                };
                break :r .{ .op = .add, .argType = @as(ArgTypes, @enumFromInt(argFlag)) };
            },
            .subtract => {
                var argFlag = @intFromEnum(ArgTypes.bothHandle);
                argFlag |= switch (a.type) {
                    .number => @intFromEnum(ArgTypes.bothHandle),
                    .numberLit => @intFromEnum(ArgTypes.literalAHandleB),
                    else => return CompilationError.IncompatibleType,
                };
                argFlag |= switch (b.type) {
                    .number => @intFromEnum(ArgTypes.bothHandle),
                    .numberLit => @intFromEnum(ArgTypes.handleAliteralB),
                    else => return CompilationError.IncompatibleType,
                };
                break :r .{ .op = .subtract, .argType = @as(ArgTypes, @enumFromInt(argFlag)) };
            },
            else => {
                break :r .{ .op = .noop, .argType = .bothLiteral };
            },
        };
        const item = Instruction{ .op = res, .a = a.operand, .b = b.operand, .dest = @truncate(dest.operand.item) };
        try self.bytecodeList.append(self.allocator, item);
        return dest;
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
        .noop => _ = try out.write("( NOP )"),
        .negateBool => switch (ins.op.argType) {
            .bothHandle, .handleAliteralB => try out.print("( NOT HANDLE({d}) ", .{ins.a.item}),
            .bothLiteral, .literalAHandleB => try out.print("( NOT LIT({s}) ", .{if (ins.a.item != 0) "TRUE" else "FALSE"}),
        },
        .negateNumber => switch (ins.op.argType) {
            .bothHandle, .handleAliteralB => try out.print("( NEG HANDLE({d}) ", .{ins.a.item}),
            .bothLiteral, .literalAHandleB => try out.print("( NEG LIT({d}) ", .{@as(f64, @bitCast(ins.a.item))}),
        },
        .add => switch (ins.op.argType) {
            .bothHandle => try out.print("( ADD HANDLE({d}) HANDLE({d}) ", .{ ins.a.item, ins.b.item }),
            .handleAliteralB => try out.print("( ADD HANDLE({d}) LIT({d}) ", .{ ins.a.item, @as(f64, @bitCast(ins.b.item)) }),
            .bothLiteral => try out.print("( ADD LIT({d}) LIT({d}) ", .{ @as(f64, @bitCast(ins.a.item)), @as(f64, @bitCast(ins.b.item)) }),
            .literalAHandleB => try out.print("( ADD LIT({d}) HANDLE({d}) ", .{ @as(f64, @bitCast(ins.a.item)), ins.b.item }),
        },
        .subtract => switch (ins.op.argType) {
            .bothHandle => try out.print("( SUB HANDLE({d}) HANDLE({d}) ", .{ ins.a.item, ins.b.item }),
            .handleAliteralB => try out.print("( SUB HANDLE({d}) LIT({d}) ", .{ ins.a.item, @as(f64, @bitCast(ins.b.item)) }),
            .bothLiteral => try out.print("( SUB LIT({d}) LIT({d}) ", .{ @as(f64, @bitCast(ins.a.item)), @as(f64, @bitCast(ins.b.item)) }),
            .literalAHandleB => try out.print("( SUB LIT({d}) HANDLE({d}) ", .{ @as(f64, @bitCast(ins.a.item)), ins.b.item }),
        },
        // types are erased so yeah
        .pushBytes => try out.print("( PSH LIT(ASNUM({d}), ASBOOL({s}), ASUINT({d})) SIZE({d}) ", .{ @as(f64, @bitCast(ins.a.item)), if (ins.a.item != 0) "TRUE" else "FALSE", ins.a.item, ins.b.item }),
    }
    try out.print("DEST({d}) )", .{ins.dest});
    try out.writeByte('\n');
}
