const std = @import("std");

// i love circular imports!
const parsing = @import("parsing.zig");
const bytecode = @import("bytecode.zig");
const scanning = @import("scanning.zig");

const Allocator = std.mem.Allocator;

const Type = bytecode.Type;
const Token = scanning.Token;
const TokenType = scanning.TokenType;

pub const Error = union(enum) {
    // SCANNING ERRORS
    /// When we can't recognize.
    /// Example:
    /// fun 屿()
    /// ----^ we don't do "unicode" around here bub
    illegalToken: struct {
        // has to be a slice since illegal
        // tokens may be unicode we can't recognize
        // i promise we will eventually have utf-8 support
        token: []u8,
    },
    /// Example:
    /// var a = " EOF
    /// ----------^ close your string
    unterminatedString,

    // PARSING ERRORS
    /// Parsing errors where one token was expected,
    /// but something else was found.
    /// Example:
    /// fun * ()
    /// ----^ expected IDENTIFIER, found STAR
    expectedToken: struct {
        // We can expect something and find null.
        found: ?Token,
        expected: TokenType,
    },

    /// Parsing errors where we expect a type but find
    /// whatever else.
    /// Example:
    /// fun ewrerwr() / {}
    /// --------------^ expected type, found SLASH
    expectedTypeToken: struct {
        // We can expect something and find null.
        found: ?Token,
    },

    /// A specialized version of expectedToken
    /// for type annotations (which are multiple tokens)
    /// The "found" attribute is not really relevant since
    /// type annotations are only expected at argument/variable declarations.
    /// var h;
    /// -----^ expected type annotation
    /// fun foo(a, b: bool) void {};
    /// --------^ expected type annotation
    expectedTypeAnnotation,
    /// A specialized version of expectedToken for
    /// when we want an expression but don't get one.
    /// var x = ;
    /// -------^ expected expression
    expectedExpression,

    // COMPILE ERRORS
    argLimitExceeded,

    argumentTypeCannotBeVoid,
    argumentTypeIncorrect: struct {
        found: Type,
        expected: Type,
    },

    // maybe we don't need the distinction but whatever
    // For when we find an incompatible type on an operation.
    incompatibleTypeUnary: struct {
        operation: parsing.UnaryExprType,
        foundType: Type,
    },
    incompatibleTypeBinary: struct {
        operation: parsing.BinaryExprType,
        lhsType: Type,
        rhsType: Type,
    },
    incompatibleTypeInitialValue: struct {
        expectedType: Type,
        foundType: Type,
    },

    mainFunctionNotDeclared,
    mainFunctionCannotHaveReturnType: struct {
        foundType: Type,
    },
    mainFunctionCannotHaveArguments: struct { numArgsFound: u16 },

    incompatibleTypeReturn: struct {
        expectedType: Type,
        foundType: Type,
    },

    incompatibleTypeArgument: struct {
        expectedType: Type,
        foundType: Type,
    },
    incorrectNumberOfArguments: struct {
        // Currently MAX_ARGS is 128.
        // It's very unlikely it will exceed 65536.
        numFound: u16,
        numExpected: u16,
    },

    /// When a variable is declared without a type annotation
    /// AND the type cannot be inferred from an initial value.
    variableMustHaveTypeWhenDefined,

    // Referencing something that does
    // not exist.
    // Example:
    // var rad = 5;
    // var area = pi * rad * rad;
    // -----------^ pi not defined
    variableNotDefined: struct { name: []u8 },
    functionNotDefined: struct { name: []u8 },
    // Defining a new var/function with
    // the same name as an existing function.
    // Example:
    // var rad = 5;
    // var rad = 15;
    // -----------^ rad already defined
    variableAlreadyDefined: struct { name: []u8 },
    functionAlreadyDefined: struct { name: []u8 },
};

pub const ErrorTrace = struct {
    err: Error,
    lineNum: u32,
    line: []u8,
};

pub const ErrorLog = struct {
    const BACKINGSIZE = 32767;
    // li'l bit spaghetti
    context: *scanning.TokenIterator,
    backing: []ErrorTrace,
    used: usize,

    pub fn init(allocator: Allocator, iter: *scanning.TokenIterator) !ErrorLog {
        return .{
            .context = iter,
            .backing = try allocator.alloc(ErrorTrace, BACKINGSIZE),
            .used = 0,
        };
    }

    pub fn push(self: *ErrorLog, err: Error) void {
        var start: usize = 0;
        var end = self.context.source.len;

        const pos = self.context.position;
        for (0..pos) |i| {
            const idx = pos - i - 1;
            if (self.context.source[idx] == '\n') {
                start = idx;
                break;
            }
        }
        for (pos..self.context.source.len) |i| {
            if (self.context.source[i] == '\n') {
                end = i;
                break;
            }
        }
        const trace: ErrorTrace = .{ .err = err, .lineNum = self.context.lineNumber, .line = self.context.source[start..end] };
        self.backing[self.used] = trace;
        self.used += 1;
    }

    pub fn recover(self: ErrorLog) ?[]ErrorTrace {
        if (self.used == 0) {
            return null;
        } else {
            return self.backing[0..self.used];
        }
    }
};

pub fn Stack(T: type, BackingSize: comptime_int) type {
    return struct {
        const Self = @This();
        backing: []T,
        used: usize,
        pub fn init(allocator: Allocator) !Self {
            return .{ .used = 0, .backing = try allocator.alloc(T, BackingSize) };
        }
        pub fn push(self: *Self, item: T) void {
            self.backing[self.used] = item;
            self.used += 1;
        }
        pub fn top(self: *Self) ?*T {
            if (self.used == 0) {
                return null;
            }
            return &self.backing[self.used - 1];
        }
        pub fn height(self: Self) usize {
            return self.used;
        }
        pub fn pop(self: *Self) ?T {
            if (self.used == 0) {
                return null;
            }
            const item = self.backing[self.used - 1];
            self.used -= 1;
            return item;
        }
        pub fn deinit(self: *Self, allocator: Allocator) void {
            allocator.free(self.backing);
        }
    };
}
