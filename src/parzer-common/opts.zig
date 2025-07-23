/// Generic helpers to assist with deserialization/deserialization options
const std = @import("std");

const meta = @import("meta.zig");

fn optionalizeStructInner(
    comptime T: type,
) type {
    comptime {
        const StructField = std.builtin.Type.StructField;

        meta.expectStruct(T);

        const info = @typeInfo(T).@"struct";

        var optionalized: [info.fields.len]StructField = undefined;

        for (info.fields, 0..) |field, i| {
            const field_type = optionalize(field.type);

            const default_value: field_type = null;

            optionalized[i] = StructField{
                .name = field.name,
                .type = field_type,
                .alignment = field.alignment,
                .default_value_ptr = &default_value,
                .is_comptime = field.is_comptime,
            };
        }

        return @Type(.{
            .@"struct" = .{
                .fields = optionalized[0..],
                .decls = &[0]std.builtin.Type.Declaration{},
                .layout = info.layout,
                .backing_integer = info.backing_integer,
                .is_tuple = info.is_tuple,
            },
        });
    }
}

fn optionalizeUnionInner(
    comptime T: type,
) type {
    comptime {
        const UnionField = std.builtin.Type.UnionField;

        meta.expectUnion(T);

        const info = @typeInfo(T).@"union";

        var optionalized: [info.fields.len]UnionField = undefined;

        for (info.fields, 0..) |field, i| {
            if (std.meta.eql(field.type, void)) {
                optionalized[i] = field;
            } else {
                optionalized[i] = UnionField{
                    .name = field.name,
                    .type = optionalizeInner(field.type),
                    .alignment = field.alignment,
                };
            }
        }

        return @Type(.{
            .@"union" = .{
                .fields = optionalized[0..],
                .decls = &[0]std.builtin.Type.Declaration{},
                .layout = info.layout,
                .tag_type = info.tag_type,
            },
        });
    }
}

fn optionalizeInner(
    comptime T: type,
) type {
    switch (@typeInfo(T)) {
        .bool,
        .int,
        .comptime_int,
        .float,
        .comptime_float,
        .pointer,
        .@"enum",
        => {
            return T;
        },
        .optional => {
            return optionalizeInner(std.meta.Child(T));
        },
        .@"struct" => {
            return optionalizeStructInner(T);
        },
        .@"union" => {
            return optionalizeUnionInner(T);
        },
        else => {
            @compileError("Unimplemented type!");
        },
    }
}

pub fn optionalize(
    comptime T: type,
) type {
    return ?optionalizeInner(T);
}

fn overrideInner(
    comptime T: type,
    dest: *T,
    opts_src: *const T,
    override_src: *const optionalize(T),
) void {
    switch (@typeInfo(T)) {
        .bool,
        .int,
        .comptime_int,
        .float,
        .comptime_float,
        .pointer,
        .@"enum",
        => {
            dest.* = override_src.* orelse opts_src.*;
        },
        .@"struct" => |info| {
            inline for (info.fields) |field| {
                overrideInner(
                    field.type,
                    &@field(dest.*, field.name),
                    &@field(opts_src.*, field.name),
                    &@field(override_src.*.?, field.name),
                );
            }
        },
        .@"union" => {
            if (override_src.*) |_| {
                switch (override_src.*.?) {
                    inline else => |field_value, tag| {
                        const field_name = @tagName(tag);

                        dest.* = @unionInit(T, field_name, undefined);

                        if (@TypeOf(field_value) == void) {
                            return;
                        }

                        const Default = @TypeOf(@field(dest.*, field_name));
                        const Override = @TypeOf(field_value);

                        overrideInner(
                            Default,
                            &@field(dest.*, field_name),
                            &@field(opts_src.*, field_name),
                            &@as(Override, @field(override_src.*.?, field_name)),
                        );
                    },
                }
            } else {
                dest.* = opts_src.*;
            }
        },
        else => {
            @compileError("Unimplemented type!");
        },
    }
}

pub fn override(
    comptime T: type,
    default: T,
    override_value: optionalize(T),
) T {
    var overridden = meta.createUndefined(T);

    overrideInner(T, &overridden, &default, &override_value);

    return overridden;
}
