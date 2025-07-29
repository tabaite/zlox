pub const Expression = union(enum) {
    literal: union(enum) {
        number: f32,
        string: []u8,
        true,
        false,
        nil,
    },
    unary: struct {
        operation: enum { Negate, NegateBool },
        expr: *Expression,
    },
    binary: struct {
        operation: enum {
            Equality,
            NotEquality,
            Greater,
            GreaterEqual,
            Less,
            LessEqual,
            Add,
            Subtract,
            Multiply,
            Divide,
        },
        left: *Expression,
        right: *Expression,
    },
    grouping: struct { expr: *Expression },
};
