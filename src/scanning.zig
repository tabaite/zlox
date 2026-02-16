const errors = @import("errors.zig");
const ErrorLog = errors.ErrorLog;

const std = @import("std");
const testing = std.testing;

pub const TokenType = enum {
    // workarounds to interface properly with error log
    unterminatedString,
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

pub const TokenContext = struct {
    token: ?Token,
    lineNumber: u32,
};
inline fn tok(ty: TokenType, start: u32, end: u32, lineNumber: u32) TokenContext {
    return .{ .token = .{ .tokenType = ty, .sourceStart = start, .sourceEndExclusive = end }, .lineNumber = lineNumber };
}
inline fn nulltok(lineNumber: u32) TokenContext {
    return .{ .token = null, .lineNumber = lineNumber };
}

pub const TokenIterator = struct {
    source: []u8,
    position: usize = 0,
    lineNumber: u32 = 1,

    pub fn exchangeTokenForSource(self: *TokenIterator, token: Token) []u8 {
        return self.source[token.sourceStart..token.sourceEndExclusive];
    }

    pub fn exchangeTokenForLine(self: *TokenIterator, token: Token) []u8 {
        const lineStart = s: {
            // the token's start can never be a new line, so it's fine
            for (1..token.sourceStart + 1) |ii| {
                const idx: usize = @as(usize, @intCast(token.sourceStart)) - ii;
                if (self.source[idx] == '\n') {
                    break :s idx + 1;
                }
            }
            break :s 0;
        };
        const lineEnd: u32 = s: {
            // the token's start can never be a new line, so it's fine
            for (token.sourceEnd..self.source.len) |idx| {
                if (self.source[idx] == '\n') {
                    break :s idx;
                }
            }
            break :s self.source.len;
        };
        return self.source[lineStart..lineEnd];
    }

    pub fn getLineWithEOF(self: *TokenIterator) []u8 {
        const lineStart = s: {
            // the token's start can never be a new line, so it's fine
            for (1..self.source.len + 1) |ii| {
                const idx: usize = self.source.len - ii;
                if (self.source[idx] == '\n') {
                    break :s idx + 1;
                }
            }
            break :s 0;
        };
        return self.source[lineStart..];
    }

    pub fn init(source: []u8) TokenIterator {
        return .{ .source = source };
    }

    pub fn next(self: *TokenIterator, log: *ErrorLog) ?Token {
        const token = self.peek(log);
        if (token) |t| {
            self.position = t.sourceEndExclusive;
            return t;
        } else {
            return null;
        }
    }

    pub fn peek(self: *TokenIterator, log: *ErrorLog) ?Token {
        const result = self.scan();
        if (result.token) |token| {
            switch (token.tokenType) {
                .invalidChar => log.push(.{ .illegalToken = .{ .token = self.exchangeTokenForSource(token) } }, result),
                .unterminatedString => log.push(.unterminatedString, result),
                else => {},
            }
            return token;
        } else {
            return null;
        }
    }

    pub fn getCurrentTokenContext(self: *TokenIterator) TokenContext {
        return self.scan();
    }

    fn scan(self: *TokenIterator) TokenContext {
        var lineNumber = self.lineNumber;
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
                const kwLookup = keywordMap.get(self.source[i..end]);
                const idenType = kwLookup orelse .identifier;
                return tok(idenType, @truncate(i), @truncate(end), lineNumber);
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
                return tok(.number, @truncate(i), @truncate(self.source.len), lineNumber);
            }

            const cnext = if (i >= self.source.len - 1) 'a' else self.source[i + 1];
            switch (current) {
                // windows bs (crlf), whitespace
                '\r', '\t', ' ' => {},
                '\n' => lineNumber += 1,

                // slash or comments
                '/' => comment: {
                    if (cnext == '/') {
                        for (i..self.source.len) |j| {
                            const sscurrent = self.source[j];
                            if (sscurrent == '\n') {
                                lineNumber += 1;
                                i = j;
                                break :comment;
                            }
                        }
                        i = self.source.len;
                    } else {
                        return tok(.slash, @truncate(i), @truncate(i + 1), lineNumber);
                    }
                },

                // string literals
                '"' => {
                    const start = if (i + 1 > self.source.len) self.source.len else i + 1;
                    for (start..self.source.len) |j| {
                        const sscurrent = self.source[j];
                        if (sscurrent == '\n') {
                            lineNumber += 1;
                        }
                        if (sscurrent == '"') {
                            return tok(.string, @truncate(i), @truncate(j), lineNumber);
                        }
                    }
                    // error but we'll get there
                    const res = tok(.unterminatedString, @truncate(i), @truncate(self.source.len), lineNumber);
                    return res;
                },

                // one character tokens
                '(' => {
                    return tok(.leftParen, @truncate(i), @truncate(i + 1), lineNumber);
                },
                ')' => {
                    return tok(.rightParen, @truncate(i), @truncate(i + 1), lineNumber);
                },
                '{' => {
                    return tok(.leftBrace, @truncate(i), @truncate(i + 1), lineNumber);
                },
                '}' => {
                    return tok(.rightBrace, @truncate(i), @truncate(i + 1), lineNumber);
                },
                ',' => {
                    return tok(.comma, @truncate(i), @truncate(i + 1), lineNumber);
                },
                '.' => {
                    return tok(.dot, @truncate(i), @truncate(i + 1), lineNumber);
                },
                '-' => {
                    return tok(.minus, @truncate(i), @truncate(i + 1), lineNumber);
                },
                '+' => {
                    return tok(.plus, @truncate(i), @truncate(i + 1), lineNumber);
                },
                ';' => {
                    return tok(.semicolon, @truncate(i), @truncate(i + 1), lineNumber);
                },
                '*' => {
                    return tok(.star, @truncate(i), @truncate(i + 1), lineNumber);
                },
                '%' => {
                    return tok(.percent, @truncate(i), @truncate(i + 1), lineNumber);
                },
                ':' => {
                    return tok(.colon, @truncate(i), @truncate(i + 1), lineNumber);
                },

                // one/two character tokens
                '<' => {
                    const offset: usize = if (cnext != '=') 1 else 2;
                    return tok(if (cnext == '=') .lessEqual else .less, @truncate(i), @truncate(i + offset), lineNumber);
                },
                '>' => {
                    const offset: usize = if (cnext != '=') 1 else 2;
                    return tok(if (cnext == '=') .greaterEqual else .greater, @truncate(i), @truncate(i + offset), lineNumber);
                },
                '!' => {
                    const offset: usize = if (cnext != '=') 1 else 2;
                    return tok(if (cnext != '=') .bang else .bangEqual, @truncate(i), @truncate(i + offset), lineNumber);
                },
                '=' => {
                    const offset: usize = if (cnext != '=') 1 else 2;
                    return tok(if (cnext != '=') .equal else .equalEqual, @truncate(i), @truncate(i + offset), lineNumber);
                },
                else => {
                    const res = tok(.invalidChar, @truncate(i), @truncate(i + 1), lineNumber);
                    return res;
                },
            }

            i += 1;
        }

        return nulltok(lineNumber);
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
