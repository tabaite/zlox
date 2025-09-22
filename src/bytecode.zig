const std = @import("std");
// circular imports are allowed!!!
const parsing = @import("parsing.zig");
const Allocator = std.mem.Allocator;

// How literals work:
// Each operation has a component that displays the type of the arguments (literal OR handle).
// We store this information in the HandledOperand type.
// For strings, we push the index of the string within the ROM and the length onto the stack, then return a handle to that item.
// The first qword is the index into rom, and the second is the length.

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
    print,
    // A: Boolean to be pushed (last bit), B: Unused, Dest: Unused, Arg Type: Unused
    pushBoolLiteral,
    // A: Number to be pushed, B: Unused, Dest: Unused, Arg Type: Unused
    pushNumberLiteral,
    // Used for pushing the index/length of string literals.
    // A: Unsigned int to be pushed, B: Unused, Dest: Unused, Arg Type: Unused
    pushUintLiteral,
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
    _,
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

    pub fn pushBinaryOperation(self: *BytecodeGenerator, _: parsing.BinaryExprType, a: HandledOperand, b: HandledOperand) !void {
        std.debug.print("pushed binary\n", .{});
        const item = Instruction{ .op = .{ .argType = .bothLiteral, .op = .print }, .a = a.operand, .b = b.operand, .dest = 0 };
        try self.bytecodeList.append(self.allocator, item);
    }

    pub fn pushUnaryOperation(self: *BytecodeGenerator, _: parsing.UnaryExprType, a: HandledOperand) !void {
        std.debug.print("pushed unary\n", .{});
        const item = Instruction{ .op = .{ .argType = .bothLiteral, .op = .print }, .a = a.operand, .b = .NULL_HANDLE, .dest = 0 };
        try self.bytecodeList.append(self.allocator, item);
    }

    pub fn pushLiteral(self: *BytecodeGenerator, lit: parsing.Literal) !HandledOperand {
        switch (lit) {
            .string => |s| {
                std.debug.print("pushed \"{s}\" BUT not really since strings are hard\n", .{s});

                // allocate shit ig
                const strStart = self.stringBuffer.items.len;
                try self.stringBuffer.appendSlice(self.allocator, s);

                const start = self.stackHeight;
                self.stackHeight += 2 * @sizeOf(u64);
                const ptr = Instruction{ .op = .{ .argType = .bothHandle, .op = .pushUintLiteral }, .a = .{ .item = @bitCast(strStart) }, .b = .NULL_HANDLE, .dest = 0 };
                try self.bytecodeList.append(self.allocator, ptr);
                const len = Instruction{ .op = .{ .argType = .bothHandle, .op = .pushUintLiteral }, .a = .{ .item = @bitCast(s.len) }, .b = .NULL_HANDLE, .dest = 0 };
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
