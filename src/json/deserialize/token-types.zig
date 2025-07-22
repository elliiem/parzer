pub const Default = enum {
    number,
    string,
    bool,
    field,
    null,
    object_begin,
    object_end,
    array_begin,
    array_end,
    comma,
};

pub const Token = union(Default) {
    number: []const u8,
    string: []const u8,
    bool: bool,
    field: []const u8,
    null: void,
    object_begin: void,
    object_end: void,
    array_begin: void,
    array_end: void,
    comma: void,
};

// TODO: Maybe remove
pub const Optional = enum {
    number,
    string,
    bool,
    object_begin,
    object_end,
    array_begin,
    array_end,
    colon,
};

pub const OptionalToken = union(Optional) {
    number: ?[]const u8,
    string: ?[]const u8,
    bool: ?bool,
    field: []const u8,
    object_begin: void,
    object_end: void,
    array_begin: void,
    array_end: void,
    colon: void,
};

pub const Primitive = enum {
    number,
    string,
    true,
    false,
    null,
    object_begin,
    object_end,
    array_begin,
    array_end,
    comma,
};

pub const Value = enum {
    number,
    string,
    field,
    bool,
};

pub fn tokenValueType(
    comptime token: Value,
) type {
    return switch (token) {
        .number,
        .string,
        .field,
        => []const u8,
        .bool => bool,
    };
}

pub const OptionalValue = enum {
    number,
    string,
    bool,
};

pub fn tokenValueTypeNullable(
    comptime token: OptionalValue,
) type {
    return switch (token) {
        .number => ?[]const u8,
        .string => ?[]const u8,
        .bool => ?bool,
        .field => []const u8,
        else => void,
    };
}
