const primitive = @import("primitives.zig");

pub const @"null" = primitive.null;
pub const @"bool" = primitive.bool;
pub const string = primitive.string;

pub const number = struct {
    pub const int = primitive.int;
    pub const float = primitive.float;
};

pub const list = @import("list.zig");
pub const @"struct" = @import("struct.zig");
pub const @"enum" = @import("enum.zig");
pub const @"union" = @import("union.zig");
