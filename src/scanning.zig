const std = @import("std");
const testing = std.testing;

pub const TokenType = enum {
    // Invalid tokens can be handled however we want.
    // However, it is not really in the best interest
    // of the user to stop after only 1 invalid has been found.

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

    // Subslice of the source buffer. no, the nullable slice does NOT affect the size
    source: ?[]u8,
};

pub const TokenIterator = struct {
    source: []u8,
    position: usize = 0,
    lineNumber: usize = 1,
    pub fn init(source: []u8) TokenIterator {
        return .{ .source = source };
    }

    // I hate the usage of *unexpected_char, but whatever.
    // TODO: Remove unexpected_char. Information about the scanning errors (unexpected, unterminated string)
    // should be manually recovered, if desirable.
    pub fn next(self: *TokenIterator) ?Token {
        var i = self.position;

        while (i < self.source.len) {
            const current = self.source[i];
            if (isAlpha(current)) {
                for (i..self.source.len) |j| {
                    const icurrent = self.source[j];
                    if (!isAlphaNumeric(icurrent)) {
                        self.position = j;
                        const kwLookup = keywordMap.get(self.source[i..j]);
                        const idenType = kwLookup orelse .identifier;
                        const source = if (kwLookup == null) self.source[i..j] else null;
                        return .{ .tokenType = idenType, .source = source };
                    }
                }
                const kwLookup = keywordMap.get(self.source[i..self.source.len]);
                const idenType = kwLookup orelse .identifier;
                const source = if (kwLookup == null) self.source[i..self.source.len] else null;
                return .{ .tokenType = idenType, .source = source };
            }

            if (isNumeric(current)) {
                var seenDecimal = false;
                for (i..self.source.len) |j| {
                    const icurrent = self.source[j];
                    if (icurrent == '.') {
                        if (seenDecimal) {
                            return .{ .tokenType = .number, .source = self.source[i..j] };
                        }
                        if ((j + 1 >= self.source.len) or !isNumeric(self.source[j + 1])) {
                            self.position = j;
                            return .{ .tokenType = .number, .source = self.source[i..j] };
                        }
                        seenDecimal = true;
                    } else if (!isNumeric(icurrent)) {
                        self.position = j;
                        return .{ .tokenType = .number, .source = self.source[i..j] };
                    }
                }
                self.position = self.source.len;
                return .{ .tokenType = .number, .source = self.source[i..self.source.len] };
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
                        return .{ .tokenType = .slash, .source = null };
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
                            return .{ .tokenType = .string, .source = self.source[start..j] };
                        }
                    }
                    return .{ .tokenType = .unterminatedString, .source = self.source[start..self.source.len] };
                },

                // one character tokens
                '(' => {
                    self.position = i + 1;
                    return .{ .tokenType = .leftParen, .source = null };
                },
                ')' => {
                    self.position = i + 1;
                    return .{ .tokenType = .rightParen, .source = null };
                },
                '{' => {
                    self.position = i + 1;
                    return .{ .tokenType = .leftBrace, .source = null };
                },
                '}' => {
                    self.position = i + 1;
                    return .{ .tokenType = .rightBrace, .source = null };
                },
                ',' => {
                    self.position = i + 1;
                    return .{ .tokenType = .comma, .source = null };
                },
                '.' => {
                    self.position = i + 1;
                    return .{ .tokenType = .dot, .source = null };
                },
                '-' => {
                    self.position = i + 1;
                    return .{ .tokenType = .minus, .source = null };
                },
                '+' => {
                    self.position = i + 1;
                    return .{ .tokenType = .plus, .source = null };
                },
                ';' => {
                    self.position = i + 1;
                    return .{ .tokenType = .semicolon, .source = null };
                },
                '*' => {
                    self.position = i + 1;
                    return .{ .tokenType = .star, .source = null };
                },
                '%' => {
                    self.position = i + 1;
                    return .{ .tokenType = .percent, .source = null };
                },
                ':' => {
                    self.position = i + 1;
                    return .{ .tokenType = .colon, .source = null };
                },

                // one/two character tokens
                '<' => {
                    self.position = if (cnext != '=' and cnext != '<') i + 1 else i + 2;
                    return .{ .tokenType = if (cnext == '=') .lessEqual else if (cnext == '<') .leftShift else .less, .source = null };
                },
                '>' => {
                    self.position = if (cnext != '=' and cnext != '>') i + 1 else i + 2;
                    return .{ .tokenType = if (cnext == '=') .greaterEqual else if (cnext == '>') .rightShift else .greater, .source = null };
                },
                '!' => {
                    self.position = if (cnext != '=') i + 1 else i + 2;
                    return .{ .tokenType = if (cnext != '=') .bang else .bangEqual, .source = null };
                },
                '=' => {
                    self.position = if (cnext != '=') i + 1 else i + 2;
                    return .{ .tokenType = if (cnext != '=') .equal else .equalEqual, .source = null };
                },
                else => {
                    self.position = i + 1;
                    return .{ .tokenType = .invalidChar, .source = self.source[i .. i + 1] };
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

pub fn printToken(token: Token, out: std.io.AnyWriter) !void {
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
            const str = token.source orelse "";
            try out.print("NUMBER {s} <NUMBER>\n", .{str});
        },
        .string => {
            const str = token.source orelse "";
            try out.print("STRING \"{s}\" {s}\n", .{ str, str });
        },
        .identifier => {
            const str = token.source orelse "";
            try out.print("IDENTIFIER {s} null\n", .{str});
        },
        else => unreachable,
    };
}
