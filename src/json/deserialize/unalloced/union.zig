const std = @import("std");

const common = @import("parzer-common");
const src = @import("../token-source.zig");

const Tokenizer = @import("../tokenizer.zig").Tokenizer;
const DeserializeError = @import("../errors.zig").DeserializeError;
const DeserializeOpts = @import("../opts.zig").DeserializeOpts;

const obj = @import("object.zig");
const @"union" = @This();

const deserializeChild = @import("deserialize.zig").deserializeChild;

pub fn unionTags(
    comptime T: type,
) type {
    comptime {
        common.meta.expectUnion(T);

        const union_fields = @typeInfo(T).@"union".fields;

        var tags: [union_fields.len]std.builtin.Type.EnumField = undefined;

        for (union_fields, 0..) |field, i| {
            tags[i] = .{
                .name = field.name,
                .value = i,
            };
        }

        return @Type(.{
            .@"enum" = .{
                .tag_type = std.math.IntFittingRange(0, union_fields.len),
                .fields = tags[0..],
                .decls = &[_]std.builtin.Type.Declaration{},
                .is_exhaustive = true,
            },
        });
    }
}

pub fn unionValueTags(
    comptime T: type,
) type {
    comptime {
        common.meta.expectUnion(T);

        const union_fields = @typeInfo(T).@"union".fields;

        var tags: [union_fields.len]std.builtin.Type.EnumField = undefined;
        var tag_i = 0;

        for (union_fields) |field| {
            if (field.type != void) {
                tags[tag_i] = .{
                    .name = field.name,
                    .value = tag_i,
                };

                tag_i += 1;
            }
        }

        return @Type(.{
            .@"enum" = .{
                .tag_type = std.math.IntFittingRange(0, tag_i),
                .fields = tags[0..tag_i],
                .decls = &[_]std.builtin.Type.Declaration{},
                .is_exhaustive = true,
            },
        });
    }
}

pub fn unionVoidTags(
    comptime T: type,
) type {
    comptime {
        common.meta.expectUnion(T);

        const union_fields = @typeInfo(T).@"union".fields;

        var tags: [union_fields.len]std.builtin.Type.EnumField = undefined;
        var tag_i = 0;

        for (union_fields) |field| {
            if (field.type == void) {
                tags[tag_i] = .{
                    .name = field.name,
                    .value = tag_i,
                };

                tag_i += 1;
            }
        }

        return @Type(.{
            .@"enum" = .{
                .tag_type = std.math.IntFittingRange(0, tag_i),
                .fields = tags[0..tag_i],
                .decls = &[0]std.builtin.Type.Declaration{},
                .is_exhaustive = true,
            },
        });
    }
}

pub const external = struct {
    //
};

pub const internal = struct {
    // TODO: Make tag field name overridable

    fn searchTag(
        comptime T: type,
        source: *Tokenizer,
        comptime opts: DeserializeOpts,
    ) DeserializeError!unionTags(T) {
        common.meta.expectUnion(T);

        const TagType = unionTags(T);

        const start: usize = source.i;
        defer source.i = start;

        {
            const field_name = try obj.@"struct".getFieldName(source, opts) orelse return DeserializeError.ExpectedTag;

            if (std.mem.eql(u8, field_name, "type")) {
                var tag: TagType = undefined;
                try obj.@"enum".deserialize(TagType, &tag, source, opts);

                return tag;
            } else {
                try src.skipNextObject(source, opts);

                try obj.@"struct".consumeSeperator(source, opts) orelse return DeserializeError.ExpectedTag;
            }
        }

        while (try obj.@"struct".getFieldName(source, opts)) |field_name| {
            if (std.mem.eql(u8, field_name, "type")) {
                var tag: TagType = undefined;
                try obj.@"enum".deserialize(TagType, &tag, source, opts);

                return tag;
            } else {
                try src.skipNextObject(source, opts);

                try obj.@"struct".consumeSeperator(source, opts) orelse return DeserializeError.ExpectedTag;
            }
        }

        return DeserializeError.ExpectedTag;
    }

    /// deserializes the field value, if the field is the tag consumes the value and returns true
    /// otherwise deserializes the field normally and returns false
    fn deserializeFieldValueMaybeTag(
        comptime T: type,
        dest: *T,
        source: *Tokenizer,
        seen: []bool,
        name: []const u8,
        comptime opts: DeserializeOpts,
    ) DeserializeError!bool {
        if (std.mem.eql(u8, name, "type")) {
            try source.skipNextTokenExpect(.string);

            return true;
        } else {
            try obj.@"struct".deserializeFieldValue(
                T,
                dest,
                source,
                seen,
                name,
                opts,
            );

            return false;
        }
    }

    /// Deserializes the body of a internally tagged struct.
    /// Because the tag field is not part of the struct we want to deserialize we have to make sure we skip it,
    /// which the normal struct deserialization code does not do
    fn deserializeInnerFlex(
        comptime T: type,
        dest: *T,
        source: *Tokenizer,
        comptime opts: DeserializeOpts,
    ) DeserializeError!void {
        common.meta.expectUnion(T);

        // get active union field
        const tag = try searchTag(T, source, opts);

        const tag_name = switch (tag) {
            inline else => |field_tag_inner| @tagName(field_tag_inner),
        };

        // NOTE: For clarity this is called child and not field,
        // as calling it the latter could create confusion between the struct
        // fields and the union fields.
        const Child = @FieldType(T, tag_name);
        common.meta.expectStruct(Child);

        // activate union field
        dest.* = @unionInit(T, tag_name, undefined);

        const child_dest = &@field(dest.*, tag_name);

        // setup seen fields
        const info = @typeInfo(Child).@"struct";
        var seen = [1]bool{false} ** info.fields.len;

        { // deserialize first field
            const field_name = try obj.@"struct".getFieldName(source, opts) orelse {
                return obj.@"struct".visitFields(Child, child_dest, &seen);
            };

            const is_tag = try deserializeFieldValueMaybeTag(
                Child,
                child_dest,
                source,
                &seen,
                field_name,
                opts,
            );

            try obj.@"struct".consumeSeperator(source, opts) orelse {
                return obj.@"struct".visitFields(T, dest, seen);
            };

            // Once we have consumed the tag field we can resume parsing the struct as we would do normally
            if (is_tag) {
                return obj.@"struct".deserializeStructTrail(
                    Child,
                    child_dest,
                    source,
                    &seen,
                    opts,
                );
            }
        }

        // deserialize remaining fields
        while (try obj.@"struct".getFieldName(source, opts)) |field_name| {
            const is_tag = try deserializeFieldValueMaybeTag(
                Child,
                child_dest,
                source,
                &seen,
                field_name,
                opts,
            );

            try obj.@"struct".consumeSeperator(source, opts) orelse {
                return obj.@"struct".visitFields(T, dest, seen);
            };

            // Once we have consumed the tag field we can resume parsing the struct as we would do normally
            if (is_tag) {
                return obj.@"struct".deserializeTrailingFields(
                    Child,
                    child_dest,
                    source,
                    &seen,
                    opts,
                );
            }
        } else {
            if (!opts.allow_trailing_comma) {
                return DeserializeError.TrailingComma;
            }

            return obj.@"struct".visitFields(T, dest, seen);
        }
    }

    fn deserializeInner(
        comptime T: type,
        dest: *T,
        source: *Tokenizer,
        comptime opts: DeserializeOpts,
    ) DeserializeError!void {
        common.meta.expectUnion(T);

        const ValueTagsEnum = unionValueTags(T);

        { // skip field name
            if (opts.precice_errors) {
                const field_name = try obj.@"struct".getFieldName(source, opts) orelse {
                    return DeserializeError.ExpectedTag;
                };

                if (!std.mem.eql(u8, field_name, "type")) {
                    return DeserializeError.ExpectedTag;
                }
            } else {
                try src.skipNextObjectExpect(source, .string, opts);
            }
        }

        const tag: ValueTagsEnum = switch (try src.peekNextTokenTypeDiscard(source, opts)) {
            .string => std.meta.stringToEnum(
                ValueTagsEnum,
                try source.takeStringInner(),
            ) orelse {
                return DeserializeError.UnknownTag;
            },
            else => {
                return DeserializeError.ExpectedTag;
            },
        };

        // deserialize value
        switch (tag) {
            inline else => |tag_inner| {
                const tag_name = @tagName(tag_inner);

                // activate union field
                dest.* = @unionInit(T, tag_name, undefined);

                // setup seen fields
                const info = @typeInfo(@FieldType(T, tag_name)).@"struct";
                var seen = [1]bool{false} ** info.fields.len;

                return obj.@"struct".deserializeTrailingFields(
                    @FieldType(T, tag_name),
                    &@field(dest.*, tag_name),
                    source,
                    &seen,
                    opts,
                );
            },
        }
    }
};

pub const adjacent = struct {
    //
};

pub const untagged = struct {
    //
};

fn deserializeValueInner(
    comptime T: type,
    dest: *T,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.meta.expectUnion(T);

    const ValueTagsEnum = unionValueTags(T);

    if (@typeInfo(ValueTagsEnum).@"enum".fields.len == 0) {
        return DeserializeError.UnknownTag;
    }

    switch (opts.union_opts) {
        .externally_tagged => {
            const tag: ValueTagsEnum = switch (try src.peekNextTokenTypeDiscard(source, opts)) {
                .string => std.meta.stringToEnum(
                    ValueTagsEnum,
                    try source.takeFieldInner(),
                ) orelse {
                    return DeserializeError.UnknownTag;
                },
                else => {
                    return DeserializeError.ExpectedTag;
                },
            };

            // deserialize value
            switch (tag) {
                inline else => |t| {
                    const tag_name = @tagName(t);

                    // activate union field
                    dest.* = @unionInit(T, tag_name, undefined);

                    return deserializeChild(
                        @FieldType(T, tag_name),
                        &@field(dest.*, tag_name),
                        source,
                        opts,
                    );
                },
            }

            return src.nextTokenExpect(source, .object_end, opts);
        },
        .internally_tagged => |union_opts| {
            if (union_opts.flexible_tag_location) {
                return @"union".internal.deserializeInner(T, dest, source, opts);
            } else {
                return @"union".internal.deserializeInnerFlex(T, dest, source, opts);
            }
        },
        .adjacently_tagged => {},
        .untagged => {},
    }
}

fn deserializeEnumValueInner(
    comptime T: type,
    dest: *T,
    source: *Tokenizer,
) DeserializeError!void {
    common.meta.expectUnion(T);

    const VoidTagsEnum = unionVoidTags(T);

    if (@typeInfo(VoidTagsEnum).@"enum".fields.len == 0) {
        return DeserializeError.UnknownTag;
    }

    var tag: VoidTagsEnum = undefined;
    try obj.@"enum".deserializeInner(VoidTagsEnum, &tag, source);

    switch (tag) {
        inline else => |t| {
            dest.* = @unionInit(T, @tagName(t), undefined);
        },
    }
}

pub fn deserialize(
    comptime T: type,
    dest: *T,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    switch (try src.peekNextTokenTypeDiscard(source, opts)) {
        .object_begin => {
            try deserializeValueInner(T, dest, source, opts);
        },
        .string => {
            try deserializeEnumValueInner(T, dest, source);
        },
        else => {
            return DeserializeError.ExpectedObject;
        },
    }
}

pub fn deserializeNullable(
    comptime T: type,
    dest: *?T,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    switch (try src.peekNextTokenTypeDiscard(source, opts)) {
        .object_begin => {
            try deserializeValueInner(T, &dest.*.?, source, opts);

            // close the union body
            return src.nextTokenExpect(source, .object_end, opts);
        },
        .string => {
            return deserializeEnumValueInner(T, &dest.*.?, source);
        },
        .null => {
            return obj.null.deserializeAssume(T, dest, source);
        },
        else => {
            return DeserializeError.ExpectedObject;
        },
    }
}
