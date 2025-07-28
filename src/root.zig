//! By convention, root.zig is the root source file when making a library. If
//! you are making an executable, the convention is to delete this file and
//! start with main.zig instead.
const std = @import("std");
const testing = std.testing;

pub const SyntaxError = error{ UnterminatedString, UnexpectedCharacter };

pub const TokenType = enum {
    // Invalid tokens can be handled however we want.
    // However, it is not really in the best interest
    // of the user to stop after only 1 invalid has been found.
    end,
    invalid,

    left_paren,
    right_paren,
    left_brace,
    right_brace,
    comma,
    dot,
    minus,
    plus,
    semicolon,
    slash,
    star,

    bang,
    bang_equal,
    equal,
    equal_equal,
    greater,
    greater_equal,
    less,
    less_equal,

    identifier,
    string,
    number,

    kw_and,
    kw_class,
    kw_else,
    kw_false,
    kw_fun,
    kw_for,
    kw_if,
    kw_nil,
    kw_or,
    kw_print,
    kw_return,
    kw_super,
    kw_this,
    kw_true,
    kw_var,
    kw_while,
};

pub const keywordMap = std.StaticStringMap(TokenType).initComptime(.{
    .{ "and", .kw_and },
    .{ "class", .kw_class },
    .{ "else", .kw_else },
    .{ "false", .kw_false },
    .{ "fun", .kw_fun },
    .{ "for", .kw_for },
    .{ "if", .kw_if },
    .{ "nil", .kw_nil },
    .{ "or", .kw_or },
    .{ "print", .kw_print },
    .{ "return", .kw_return },
    .{ "super", .kw_super },
    .{ "this", .kw_this },
    .{ "true", .kw_true },
    .{ "var", .kw_var },
    .{ "while", .kw_while },
});

pub const Token = struct {
    token_type: TokenType,

    // Subslice of the source buffer. no, the nullable slice does NOT affect the size
    source: ?[]u8,
};

pub const TokenIterator = struct {
    source: []u8,
    position: usize = 0,
    line_number: usize = 1,
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
                        return .{ .token_type = idenType, .source = source };
                    }
                }
                const kwLookup = keywordMap.get(self.source[i..self.source.len]);
                const idenType = kwLookup orelse .identifier;
                const source = if (kwLookup == null) self.source[i..self.source.len] else null;
                return .{ .token_type = idenType, .source = source };
            }

            if (isNumeric(current)) {
                var seenDecimal = false;
                for (i..self.source.len) |j| {
                    const icurrent = self.source[j];
                    if (icurrent == '.') {
                        if (seenDecimal) {
                            return .{ .token_type = .identifier, .source = self.source[i..j] };
                        }
                        if ((j + 1 >= self.source.len) or !isNumeric(self.source[j + 1])) {
                            self.position = j;
                            return .{ .token_type = .identifier, .source = self.source[i..j] };
                        }
                        seenDecimal = true;
                    } else if (!isNumeric(icurrent)) {
                        self.position = j;
                        return .{ .token_type = .identifier, .source = self.source[i..j] };
                    }
                }
                return .{ .token_type = .identifier, .source = self.source[i..self.source.len] };
            }

            const cnext = if (i >= self.source.len - 1) 'a' else self.source[i + 1];
            switch (current) {
                // windows bs (crlf), whitespace
                '\r', '\t', ' ' => {},
                '\n' => self.line_number += 1,

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
                    return .{ .token_type = .slash, .source = null };
                },

                // string literals
                '"' => {
                    const start = if (i + 1 > self.source.len) self.source.len else i + 1;
                    for (start..self.source.len) |j| {
                        const sscurrent = self.source[j];
                        if (sscurrent == '\n') {
                            self.line_number += 1;
                        }
                        if (sscurrent == '"') {
                            self.position = j + 1;
                            return .{ .token_type = .string, .source = self.source[start..j] };
                        }
                    }
                    return error.UnterminatedString;
                },

                // one character tokens
                '(' => {
                    self.position = i + 1;
                    return .{ .token_type = .left_paren, .source = null };
                },
                ')' => {
                    self.position = i + 1;
                    return .{ .token_type = .right_paren, .source = null };
                },
                '{' => {
                    self.position = i + 1;
                    return .{ .token_type = .left_brace, .source = null };
                },
                '}' => {
                    self.position = i + 1;
                    return .{ .token_type = .right_brace, .source = null };
                },
                ',' => {
                    self.position = i + 1;
                    return .{ .token_type = .comma, .source = null };
                },
                '.' => {
                    self.position = i + 1;
                    return .{ .token_type = .dot, .source = null };
                },
                '-' => {
                    self.position = i + 1;
                    return .{ .token_type = .minus, .source = null };
                },
                '+' => {
                    self.position = i + 1;
                    return .{ .token_type = .plus, .source = null };
                },
                ';' => {
                    self.position = i + 1;
                    return .{ .token_type = .semicolon, .source = null };
                },
                '*' => {
                    self.position = i + 1;
                    return .{ .token_type = .star, .source = null };
                },

                // one/two character tokens
                '<' => {
                    self.position = if (cnext != '=') i + 1 else i + 2;
                    return .{ .token_type = if (cnext != '=') .less else .less_equal, .source = null };
                },
                '>' => {
                    self.position = if (cnext != '=') i + 1 else i + 2;
                    return .{ .token_type = if (cnext != '=') .greater else .greater_equal, .source = null };
                },
                '!' => {
                    self.position = if (cnext != '=') i + 1 else i + 2;
                    return .{ .token_type = if (cnext != '=') .bang else .bang_equal, .source = null };
                },
                '=' => {
                    self.position = if (cnext != '=') i + 1 else i + 2;
                    return .{ .token_type = if (cnext != '=') .equal else .equal_equal, .source = null };
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

test "basic add functionality" {
    try testing.expect(add(3, 7) == 10);
}
