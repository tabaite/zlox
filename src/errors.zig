const std = @import("std");

// i love circular imports!
const parsing = @import("parsing.zig");
const bytecode = @import("bytecode.zig");
const scanning = @import("scanning.zig");

const Allocator = std.mem.Allocator;

const Type = bytecode.Type;
const Token = scanning.Token;
const TokenType = scanning.TokenType;
const TokenContext = scanning.TokenContext;

pub const ErrorLocation = union(enum) {
    eof,
    token: struct {
        t: Token,
    },
    sourceRange: struct {
        start: u32,
        /// We set this to max(u32) if the end provided is null.
        endPossiblyOOB: u32,
    },
};

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
        expected: TokenType,
    },

    /// Parsing errors where we expect a type but find
    /// whatever else.
    /// Example:
    /// fun ewrerwr() / {}
    /// --------------^ expected type, found SLASH
    expectedTypeToken,

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
    // null means EOF
    where: ErrorLocation,
    lineNumber: u32,

    pub fn printSelf(self: ErrorTrace, out: *std.Io.Writer) !void {
        const location: []u8 = switch (self.where) {
            .eof => @constCast("end of file"),
            .token => |t| @constCast(t.t.tokenType.typeAsString()),
            // errors using sourceRange do not print location generally
            .sourceRange => @constCast(""),
        };
        switch (self.err) {
            .illegalToken => |t| try out.print("illegal token: \"{s}\" is not recognized as a valid token", .{t.token}),
            .unterminatedString => _ = try out.write("unterminated string"),
            .expectedToken => |e| {
                try out.print("expected {s}, found {s}", .{ e.expected.typeAsString(), location });
            },
            .expectedTypeToken => {
                try out.print("expected a type here, found {s}", .{location});
            },
            .expectedTypeAnnotation => _ = try out.write("expected a type annotation"),
            .expectedExpression => _ = try out.write("expected a valid expression"),
            .argLimitExceeded => _ = try out.write("argument limit for functions exceeded"),
            .argumentTypeCannotBeVoid => _ = try out.write("arguments must not be type 'void'"),

            .incompatibleTypeUnary => _ = try out.write("incompatible type (unary)"),
            .incompatibleTypeBinary => |b| try out.print("cannot perform operation {s} on operands of type {s} and {s}", .{ b.operation.asVerb(), b.lhsType.asString(), b.rhsType.asString() }),
            .incompatibleTypeInitialValue => _ = try out.write("incompatible type (initial value)"),
            .incompatibleTypeReturn => _ = try out.write("incompatible type (return value)"),
            .argumentTypeIncorrect => _ = try out.write("incompatible type (provided argument)"),

            .mainFunctionNotDeclared => _ = try out.write("must declare a main function"),
            .mainFunctionCannotHaveReturnType => _ = try out.write("main function must not return a value"),
            .mainFunctionCannotHaveArguments => _ = try out.write("main function must not have arguments"),

            .incorrectNumberOfArguments => _ = try out.write("incorrect number of arguments"),

            .variableMustHaveTypeWhenDefined => _ = try out.write("variables must have either a provided or inferred type when declared"),

            .variableNotDefined => _ = try out.write("trying to access undeclared variable"),
            .functionNotDefined => |n| try out.print("trying to call undeclared function \"{s}\"", .{n.name}),

            .variableAlreadyDefined => _ = try out.write("trying to redefine a variable (shadowing soon i promise)"),
            .functionAlreadyDefined => _ = try out.write("trying to redefine a function (overloading soon i promise)"),
        }
        try out.flush();
    }
};

pub const ErrorLog = struct {
    const BACKINGSIZE = 32767;
    backing: []ErrorTrace,
    used: usize,

    pub fn init(allocator: Allocator) !ErrorLog {
        return .{
            .backing = try allocator.alloc(ErrorTrace, BACKINGSIZE),
            .used = 0,
        };
    }
    pub fn deinit(self: *ErrorLog, allocator: Allocator) void {
        allocator.free(self.backing);
    }

    pub fn push(self: *ErrorLog, err: Error, context: TokenContext) void {
        const trace: ErrorTrace = .{ .err = err, .where = if (context.token.tokenType != .eof) .{ .token = .{ .t = context.token } } else .eof, .lineNumber = context.lineNumber };
        self.backing[self.used] = trace;
        self.used += 1;
    }

    pub fn pushTokenRange(self: *ErrorLog, err: Error, start: TokenContext, endInclusive: TokenContext) void {
        const u32Max = std.math.maxInt(u32);
        const rangeStart = if (start.token) |t| t.sourceStart else u32Max;
        const rangeEnd = if (endInclusive.token) |t| t.sourceEndExclusive else u32Max;
        const trace: ErrorTrace = .{ .err = err, .where = .{ .sourceRange = .{ .start = rangeStart, .endPossiblyOOB = rangeEnd } }, .lineNumber = start.lineNumber };
        self.backing[self.used] = trace;
        self.used += 1;
    }
    pub fn pushTokenRangeEndExlusive(self: *ErrorLog, err: Error, start: TokenContext, endExclusive: TokenContext) void {
        const u32Max = std.math.maxInt(u32);
        const rangeStart = if (start.token) |t| t.sourceStart else u32Max;
        const rangeEnd = if (endExclusive.token) |t| t.sourceStart else u32Max;
        const trace: ErrorTrace = .{ .err = err, .where = .{ .sourceRange = .{ .start = rangeStart, .endPossiblyOOB = rangeEnd } }, .lineNumber = start.lineNumber };
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
