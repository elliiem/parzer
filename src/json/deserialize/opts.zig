const std = @import("std");
const builtin = @import("builtin");

pub const DeserializeOpts = @This();

allow_trailing_comma: bool = true,
whitespace: bool = true,
precice_errors: bool = builtin.mode == .Debug,
union_opts: UnionOpts,

pub const UnionOpts = union(enum) {
    externally_tagged: void,
    internally_tagged: InternallyTagged,
    adjacently_tagged: void,
    untagged: void,

    pub const InternallyTagged = struct {
        flexible_tag_location: bool = true,
        tag_field: []const u8 = "type",
    };
};

pub const UnionRepresentation = enum {
    externally_tagged,
    internally_tagged,
    adjacently_tagged,
    untagged,
};

pub const UnionDeserializeOpts = struct {
    representation: UnionRepresentation = .externally_tagged,
    flexible_tag_location: bool = true,
};
