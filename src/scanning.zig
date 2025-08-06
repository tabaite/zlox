const std = @import("std");
const testing = std.testing;

pub const SyntaxError = error{ UnterminatedString, UnexpectedCharacter };

pub const TokenType = enum {
    // Invalid tokens can be handled however we want.
    // However, it is not really in the best interest
    // of the user to stop after only 1 invalid has been found.
    end,
    invalid,

    leftParen,
    rightParen,
    leftBrace,
    rightBrace,
    comma,
    dot,
    minus,
    plus,
    semicolon,
    slash,
    star,

    bang,
    bangEqual,
    equal,
    equalEqual,
    greater,
    greaterEqual,
    less,
    lessEqual,

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
};

pub const keywordMap = std.StaticStringMap(TokenType).initComptime(.{
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
    pub fn next(self: *TokenIterator, unexpected_char: *u8) SyntaxError!?Token {
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
                '/' => if (cnext == '/') {
                    for (i..self.source.len) |j| {
                        const sscurrent = self.source[j];
                        if (sscurrent == '\n') {
                            i = j;
                            break;
                        }
                    }
                } else {
                    self.position = i + 1;
                    return .{ .tokenType = .slash, .source = null };
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
                    return error.UnterminatedString;
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

                // one/two character tokens
                '<' => {
                    self.position = if (cnext != '=') i + 1 else i + 2;
                    return .{ .tokenType = if (cnext != '=') .less else .lessEqual, .source = null };
                },
                '>' => {
                    self.position = if (cnext != '=') i + 1 else i + 2;
                    return .{ .tokenType = if (cnext != '=') .greater else .greaterEqual, .source = null };
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
                    unexpected_char.* = current;
                    return error.UnexpectedCharacter;
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

pub export fn add(a: i32, b: i32) i32 {
    return a + b;
}
