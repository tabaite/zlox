const errors = @import("errors.zig");
const ErrorLog = errors.ErrorLog;

const std = @import("std");
const testing = std.testing;

pub const TokenType = enum {
    // workaround since we don't have Option<T>
    invalidChar,

    leftParen,
    rightParen,
    leftBrace,
    rightBrace,
    comma,
    dot,
    minus,
    plus,
    semicolon,
    colon,
    slash,
    percent,
    star,

    bang,
    bangEqual,
    equal,
    equalEqual,
    greater,
    greaterEqual,
    rightShift,
    less,
    lessEqual,
    leftShift,

    identifier,
    string,
    number,

    kwAnd,
    kwClass,
    kwElse,
    kwFalse,
    kwFun,
    kwFor,
    kwIf,
    kwNil,
    kwOr,
    kwPrint,
    kwReturn,
    kwSuper,
    kwThis,
    kwTrue,
    kwVar,
    kwWhile,

    tyNum,
    tyBool,
    tyString,
    tyVoid,

    pub fn typeAsString(self: TokenType) []const u8 {
        return switch (self) {
            .invalidChar => "INVALID",
            .leftParen => "left parenthesis",
            .rightParen => "right parenthesis",
            .leftBrace => "left brace",
            .rightBrace => "right brace",
            .comma => "comma",
            .dot => "dot",
            .minus => "dash",
            .plus => "plus",
            .semicolon => "semicolon",
            .colon => "colon",
            .slash => "slash",
            .star => "star",
            .percent => "percent",
            .bang => "bang",
            .bangEqual => "bang-equal",
            .equal => "equal",
            .equalEqual => "equal-equal",
            .greater => "greater",
            .greaterEqual => "greater-equal",
            .rightShift => "right-shift",
            .less => "less",
            .lessEqual => "less-equal",
            .leftShift => "left-shift",
            .identifier => "identifier",
            .string => "string literal",
            .number => "number literal",

            .kwAnd => "keyword \"and\"",
            .kwClass => "keyword \"class\"",
            .kwElse => "keyword \"else\"",
            .kwFalse => "keyword \"false\"",
            .kwFun => "keyword \"fun\"",
            .kwFor => "keyword \"for\"",
            .kwIf => "keyword \"if\"",
            .kwNil => "keyword \"nil\"",
            .kwOr => "keyword \"or\"",
            .kwPrint => "keyword \"print\"",
            .kwReturn => "keyword \"return\"",
            .kwSuper => "keyword \"super\"",
            .kwThis => "keyword \"this\"",
            .kwTrue => "keyword \"true\"",
            .kwVar => "keyword \"var\"",
            .kwWhile => "keyword \"while\"",

            .tyNum => "keyword \"number\"",
            .tyBool => "keyword \"bool\"",
            .tyString => "keyword \"string\"",
            .tyVoid => "keyword \"void\"",
        };
    }
};

pub const keywordMap = std.StaticStringMap(TokenType).initComptime(.{
    .{ "number", .tyNum },
    .{ "bool", .tyBool },
    .{ "string", .tyString },
    .{ "void", .tyVoid },
    .{ "and", .kwAnd },
    .{ "class", .kwClass },
    .{ "else", .kwElse },
    .{ "false", .kwFalse },
    .{ "fun", .kwFun },
    .{ "for", .kwFor },
    .{ "if", .kwIf },
    .{ "nil", .kwNil },
    .{ "or", .kwOr },
    .{ "print", .kwPrint },
    .{ "return", .kwReturn },
    .{ "super", .kwSuper },
    .{ "this", .kwThis },
    .{ "true", .kwTrue },
    .{ "var", .kwVar },
    .{ "while", .kwWhile },
});

pub const Token = struct {
    tokenType: TokenType,

    // Subslice of the source buffer.
    /// Start (inclusive).
    sourceStart: u32,
    /// End (exclusive).
    sourceEndExclusive: u32,
};

pub const TokenIterator = struct {
    source: []u8,
    position: usize = 0,
    lineNumber: u32 = 1,

    pub fn exchangeTokenForSource(self: *TokenIterator, token: Token) []u8 {
        return self.source[token.sourceStart..token.sourceEndExclusive];
    }

    pub fn init(source: []u8) TokenIterator {
        return .{ .source = source };
    }

    pub fn next(self: *TokenIterator, log: *ErrorLog) ?Token {
        var i = self.position;

        while (i < self.source.len) {
            const current = self.source[i];
            if (isAlpha(current)) {
                var end = i;
                while (end < self.source.len) {
                    const icurrent = self.source[end];
                    if (!isAlphaNumeric(icurrent)) {
                        break;
                    }

                    end += 1;
                }
                self.position = end;
                const kwLookup = keywordMap.get(self.source[i..end]);
                const idenType = kwLookup orelse .identifier;
                return .{ .tokenType = idenType, .sourceStart = @truncate(i), .sourceEndExclusive = @truncate(end) };
            }

            if (isNumeric(current)) {
                var seenDecimal = false;
                var end = i;
                while (end < self.source.len) {
                    const icurrent = self.source[end];
                    if (icurrent == '.') {
                        if (seenDecimal) {
                            break;
                        }
                        // the next one must be a number for the decimal to be valid
                        if ((end + 1 >= self.source.len) or !isNumeric(self.source[end + 1])) {
                            break;
                        }
                        seenDecimal = true;
                    } else if (!isNumeric(icurrent)) {
                        break;
                    }

                    end += 1;
                }
                self.position = end;
                return .{ .tokenType = .number, .sourceStart = @truncate(i), .sourceEndExclusive = @truncate(self.source.len) };
            }

            const cnext = if (i >= self.source.len - 1) 'a' else self.source[i + 1];
            switch (current) {
                // windows bs (crlf), whitespace
                '\r', '\t', ' ' => {},
                '\n' => self.lineNumber += 1,

                // slash or comments
                '/' => comment: {
                    if (cnext == '/') {
                        for (i..self.source.len) |j| {
                            const sscurrent = self.source[j];
                            if (sscurrent == '\n') {
                                self.lineNumber += 1;
                                i = j;
                                break :comment;
                            }
                        }
                        i = self.source.len;
                    } else {
                        self.position = i + 1;
                        return .{ .tokenType = .slash, .sourceStart = @truncate(i), .sourceEndExclusive = @truncate(i + 1) };
                    }
                },

                // string literals
                '"' => {
                    const start = if (i + 1 > self.source.len) self.source.len else i + 1;
                    for (start..self.source.len) |j| {
                        const sscurrent = self.source[j];
                        if (sscurrent == '\n') {
                            self.lineNumber += 1;
                        }
                        if (sscurrent == '"') {
                            self.position = j + 1;
                            return .{ .tokenType = .string, .sourceStart = @truncate(i), .sourceEndExclusive = @truncate(j) };
                        }
                    }
                    // error but we'll get there
                    log.push(.unterminatedString);
                    return .{ .tokenType = .string, .sourceStart = @truncate(i), .sourceEndExclusive = @truncate(self.source.len) };
                },

                // one character tokens
                '(' => {
                    self.position = i + 1;
                    return .{ .tokenType = .leftParen, .sourceStart = @truncate(i), .sourceEndExclusive = @truncate(i + 1) };
                },
                ')' => {
                    self.position = i + 1;
                    return .{ .tokenType = .rightParen, .sourceStart = @truncate(i), .sourceEndExclusive = @truncate(i + 1) };
                },
                '{' => {
                    self.position = i + 1;
                    return .{ .tokenType = .leftBrace, .sourceStart = @truncate(i), .sourceEndExclusive = @truncate(i + 1) };
                },
                '}' => {
                    self.position = i + 1;
                    return .{ .tokenType = .rightBrace, .sourceStart = @truncate(i), .sourceEndExclusive = @truncate(i + 1) };
                },
                ',' => {
                    self.position = i + 1;
                    return .{ .tokenType = .comma, .sourceStart = @truncate(i), .sourceEndExclusive = @truncate(i + 1) };
                },
                '.' => {
                    self.position = i + 1;
                    return .{ .tokenType = .dot, .sourceStart = @truncate(i), .sourceEndExclusive = @truncate(i + 1) };
                },
                '-' => {
                    self.position = i + 1;
                    return .{ .tokenType = .minus, .sourceStart = @truncate(i), .sourceEndExclusive = @truncate(i + 1) };
                },
                '+' => {
                    self.position = i + 1;
                    return .{ .tokenType = .plus, .sourceStart = @truncate(i), .sourceEndExclusive = @truncate(i + 1) };
                },
                ';' => {
                    self.position = i + 1;
                    return .{ .tokenType = .semicolon, .sourceStart = @truncate(i), .sourceEndExclusive = @truncate(i + 1) };
                },
                '*' => {
                    self.position = i + 1;
                    return .{ .tokenType = .star, .sourceStart = @truncate(i), .sourceEndExclusive = @truncate(i + 1) };
                },
                '%' => {
                    self.position = i + 1;
                    return .{ .tokenType = .percent, .sourceStart = @truncate(i), .sourceEndExclusive = @truncate(i + 1) };
                },
                ':' => {
                    self.position = i + 1;
                    return .{ .tokenType = .colon, .sourceStart = @truncate(i), .sourceEndExclusive = @truncate(i + 1) };
                },

                // one/two character tokens
                '<' => {
                    const offset: usize = if (cnext != '=') 1 else 2;
                    self.position = i + offset;
                    return .{ .tokenType = if (cnext == '=') .lessEqual else .less, .sourceStart = @truncate(i), .sourceEndExclusive = @truncate(i + offset) };
                },
                '>' => {
                    const offset: usize = if (cnext != '=') 1 else 2;
                    self.position = i + offset;
                    return .{ .tokenType = if (cnext == '=') .greaterEqual else .greater, .sourceStart = @truncate(i), .sourceEndExclusive = @truncate(i + offset) };
                },
                '!' => {
                    const offset: usize = if (cnext != '=') 1 else 2;
                    self.position = i + offset;
                    return .{ .tokenType = if (cnext != '=') .bang else .bangEqual, .sourceStart = @truncate(i), .sourceEndExclusive = @truncate(i + offset) };
                },
                '=' => {
                    const offset: usize = if (cnext != '=') 1 else 2;
                    self.position = i + offset;
                    return .{ .tokenType = if (cnext != '=') .equal else .equalEqual, .sourceStart = @truncate(i), .sourceEndExclusive = @truncate(i + offset) };
                },
                else => {
                    self.position = i + 1;
                    log.push(.{ .illegalToken = .{ .token = self.source[i .. i + 1] } });
                    return .{ .tokenType = .invalidChar, .sourceStart = @truncate(i), .sourceEndExclusive = @truncate(i + 1) };
                },
            }

            i += 1;
        }

        self.position = self.source.len;
        return null;
    }
};

fn isAlpha(char: u8) bool {
    return (char >= 'a' and char <= 'z') or
        (char >= 'A' and char <= 'Z') or
        char == '_';
}

fn isNumeric(char: u8) bool {
    return (char >= '0' and char <= '9');
}

fn isAlphaNumeric(char: u8) bool {
    return isAlpha(char) or isNumeric(char);
}

pub fn printToken(iter: *TokenIterator, token: Token, out: std.io.AnyWriter) !void {
    _ = switch (token.tokenType) {
        .bang => try out.write("BANG ! null\n"),
        .bangEqual => try out.write("BANG_EQUAL != null\n"),
        .less => try out.write("LESS < null\n"),
        .lessEqual => try out.write("LESS_EQUAL <= null\n"),
        .greater => try out.write("GREATER > null\n"),
        .greaterEqual => try out.write("GREATER >= null\n"),
        .equal => try out.write("EQUAL = null\n"),
        .equalEqual => try out.write("EQUAL_EQUAL == null\n"),
        .leftParen => try out.write("LEFT_PAREN ( null\n"),
        .rightParen => try out.write("RIGHT_PAREN ) null\n"),
        .leftBrace => try out.write("LEFT_BRACE { null\n"),
        .rightBrace => try out.write("RIGHT_BRACE } null\n"),
        .comma => try out.write("COMMA , null\n"),
        .dot => try out.write("DOT . null\n"),
        .minus => try out.write("MINUS - null\n"),
        .plus => try out.write("PLUS + null\n"),
        .semicolon => try out.write("SEMICOLON ; null\n"),
        .colon => try out.write("COLON : null\n"),
        .star => try out.write("STAR * null\n"),
        .slash => try out.write("SLASH / null\n"),
        .percent => try out.write("PERCENT % null\n"),

        .kwAnd => try out.write("AND and null\n"),
        .kwClass => try out.write("CLASS class null\n"),
        .kwElse => try out.write("ELSE else null\n"),
        .kwFalse => try out.write("FALSE false null\n"),
        .kwFun => try out.write("FUN fun null\n"),
        .kwFor => try out.write("FOR for null\n"),
        .kwIf => try out.write("IF if null\n"),
        .kwNil => try out.write("NIL nil null\n"),
        .kwOr => try out.write("OR or null\n"),
        .kwPrint => try out.write("PRINT print null\n"),
        .kwReturn => try out.write("RETURN return null\n"),
        .kwSuper => try out.write("SUPER super null\n"),
        .kwThis => try out.write("THIS this null\n"),
        .kwTrue => try out.write("TRUE true null\n"),
        .kwVar => try out.write("VAR var null\n"),
        .kwWhile => try out.write("WHILE while null\n"),

        .tyNum => try out.write("TYPE number null\n"),
        .tyBool => try out.write("TYPE bool null\n"),
        .tyString => try out.write("TYPE string null\n"),
        .tyVoid => try out.write("TYPE void null\n"),

        .number => {
            const str = iter.exchangeTokenForSource(token);
            try out.print("NUMBER {s} <NUMBER>\n", .{str});
        },
        .string => {
            const str = iter.exchangeTokenForSource(token);
            try out.print("STRING \"{s}\" {s}\n", .{ str, str });
        },
        .identifier => {
            const str = iter.exchangeTokenForSource(token);
            try out.print("IDENTIFIER {s} null\n", .{str});
        },
        else => unreachable,
    };
}
