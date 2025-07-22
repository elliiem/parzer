const std = @import("std");
const builtin = @import("builtin");

const meta = @import("meta.zig");

pub const DeserializeOpts = @This();

allow_trailing_comma: bool = true,
whitespace: bool = true,
precice_errors: bool = builtin.mode == .Debug,
union_opts: UnionDeserializeOpts,

pub const UnionRepresentation = enum {
    externally_tagged,
    internally_tagged,
    adjacently_tagged,
    untagged,
};

pub const UnionDeserializeOpts = struct {
    representation: UnionRepresentation = .externally_tagged,
    assume_internal_tag_is_first: bool = true,
};

fn optionalize(comptime T: type) type {
    comptime {
        meta.expectStruct(T);

        const info = @typeInfo(T).@"struct";

        const fields = info.fields;

        var new_fields: [fields.len]std.builtin.Type.StructField = undefined;

        for (fields, 0..) |field, i| {
            var new_type = field.type;

            if (@typeInfo(new_type) == .@"struct") {
                new_type = ?optionalize(new_type);
            } else {
                new_type = ?new_type;
            }

            new_fields[i] = .{
                .name = field.name,
                .type = ?field.type,
                .is_comptime = field.is_comptime,
                .alignment = field.alignment,
            };
        }

        return @Type(.{
            .@"struct" = .{
                .fields = &new_fields,
                .backing_integer = info.backing_integer,
                .decls = info.decls,
                .is_tuple = info.is_tuple,
                .layout = info.layout,
            },
        });
    }
}
