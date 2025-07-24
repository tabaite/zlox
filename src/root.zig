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

pub const Token = struct {
    token_type: TokenType,

    // Subslice of the source buffer.
    source: []u8,
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
        for (self.position..self.source.len) |i| {
            const current = self.source[i];
            if (isAlpha(current)) {
                return null;
            }
            if (isNumeric(current)) {
                return null;
            }

            const cnext = if (i >= self.source.len - 1) 'a' else self.source[i + 1];
            switch (current) {
                // windows bs (crlf), whitespace
                '\r', '\t', ' ' => {},
                '\n' => self.line_number += 1,

                // one character tokens
                '(' => {
                    self.position = i + 1;
                    return .{ .token_type = .left_paren, .source = self.source[i .. i + 1] };
                },
                ')' => {
                    self.position = i + 1;
                    return .{ .token_type = .right_paren, .source = self.source[i .. i + 1] };
                },
                '{' => {
                    self.position = i + 1;
                    return .{ .token_type = .left_brace, .source = self.source[i .. i + 1] };
                },
                '}' => {
                    self.position = i + 1;
                    return .{ .token_type = .right_brace, .source = self.source[i .. i + 1] };
                },
                ',' => {
                    self.position = i + 1;
                    return .{ .token_type = .comma, .source = self.source[i .. i + 1] };
                },
                '.' => {
                    self.position = i + 1;
                    return .{ .token_type = .dot, .source = self.source[i .. i + 1] };
                },
                '-' => {
                    self.position = i + 1;
                    return .{ .token_type = .minus, .source = self.source[i .. i + 1] };
                },
                '+' => {
                    self.position = i + 1;
                    return .{ .token_type = .plus, .source = self.source[i .. i + 1] };
                },
                ';' => {
                    self.position = i + 1;
                    return .{ .token_type = .semicolon, .source = self.source[i .. i + 1] };
                },
                '*' => {
                    self.position = i + 1;
                    return .{ .token_type = .star, .source = self.source[i .. i + 1] };
                },

                // one/two character tokens
                '<' => {
                    self.position = if (cnext != '=') i + 1 else i + 2;
                    return .{ .token_type = if (cnext != '=') .less else .less_equal, .source = self.source[i .. i + 1] };
                },
                '>' => {
                    self.position = if (cnext != '=') i + 1 else i + 2;
                    return .{ .token_type = if (cnext != '=') .greater else .greater_equal, .source = self.source[i .. i + 1] };
                },
                '!' => {
                    self.position = if (cnext != '=') i + 1 else i + 2;
                    return .{ .token_type = if (cnext != '=') .bang else .bang_equal, .source = self.source[i .. i + 1] };
                },
                '=' => {
                    self.position = if (cnext != '=') i + 1 else i + 2;
                    return .{ .token_type = if (cnext != '=') .equal else .equal_equal, .source = self.source[i .. i + 1] };
                },
                else => {
                    self.position = i + 1;
                    unexpected_char.* = current;
                    return error.UnexpectedCharacter;
                },
            }
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
