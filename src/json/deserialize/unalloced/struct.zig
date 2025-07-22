const std = @import("std");

const meta = @import("../meta.zig");
const types = @import("../token-types.zig");
const src = @import("../token-source.zig");

const Tokenizer = @import("../tokenizer.zig").Tokenizer;

const DeserializeOpts = @import("../opts.zig").DeserializeOpts;
const DeserializeError = @import("../errors.zig").DeserializeError;

const @"null" = @import("primitives.zig").null;
const @"struct" = @This();

const deserializeChild = @import("deserialize.zig").deserializeChild;

pub fn getFieldName(
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!?[]const u8 {
    switch (try src.peekNextTokenTypeDiscard(source, opts)) {
        .string => {
            return try source.takeTokenAssume(.field);
        },
        .object_end => {
            return null;
        },
        .comma => {
            return DeserializeError.MissingField;
        },
        else => {
            return DeserializeError.UnexpectedToken;
        },
    }
}

pub fn consumeSeperator(
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!?void {
    switch (try src.peekNextTokenTypeDiscard(source, opts)) {
        .comma => {
            return;
        },
        .object_end => {
            return null;
        },
        .string => {
            return DeserializeError.MissingComma;
        },
        else => {
            return DeserializeError.UnexpectedToken;
        },
    }
}

pub fn deserializeFieldValue(
    comptime T: type,
    dest: *T,
    source: *Tokenizer,
    seen: []bool,
    name: []const u8,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    meta.expectStruct(T);

    const info = @typeInfo(T).@"struct";

    inline for (info.fields, 0..) |field, i| {
        if (std.mem.eql(u8, name, field.name)) {
            if (seen[i]) {
                return DeserializeError.DuplicateField;
            }

            seen[i] = true;

            return deserializeChild(
                field.type,
                &@field(dest.*, field.name),
                source,
                opts,
            );
        }
    } else {
        return DeserializeError.UnknownField;
    }
}

pub fn visitFields(
    comptime T: type,
    dest: *T,
    seen: []bool,
) DeserializeError!void {
    meta.expectStruct(T);

    const fields = @typeInfo(T).@"struct".fields;

    inline for (fields, 0..) |field, i| {
        switch (@typeInfo(field.type)) {
            .optional => {
                @field(dest.*, field.name) = null;
            },
            else => {
                if (!seen[i]) {
                    return DeserializeError.MissingField;
                }
            },
        }
    }
}

pub fn deserializeTrailingFields(
    comptime T: type,
    dest: *T,
    source: *Tokenizer,
    seen: []bool,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    meta.expectStruct(T);

    while (try getFieldName(source, opts)) |field_name| {
        try deserializeFieldValue(
            T,
            dest,
            source,
            seen,
            field_name,
            opts,
        );

        try consumeSeperator(source, opts) orelse {
            return visitFields(T, dest, seen);
        };
    } else {
        if (!opts.allow_trailing_comma) {
            return DeserializeError.TrailingComma;
        }

        return visitFields(T, dest, seen);
    }
}

pub fn deserializeInner(
    comptime T: type,
    dest: *T,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    meta.expectStruct(T);

    const info = @typeInfo(T).@"struct";

    var seen = [1]bool{false} ** info.fields.len;

    { // deserialize first field
        const field_name = getFieldName(source, opts) orelse {
            return visitFields(T, dest, &seen);
        };

        try deserializeFieldValue(
            T,
            dest,
            source,
            &seen,
            field_name,
            opts,
        );

        try consumeSeperator(source, opts) orelse {
            return visitFields(T, dest, seen);
        };
    }

    return deserializeTrailingFields(T, dest, source, &seen, opts);
}

pub fn deserializeRecheck(
    comptime T: type,
    dest: *T,
    source: *Tokenizer,
    comptime token_type: types.Primitive,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    meta.expectStruct(T);

    switch (token_type) {
        .object_begin => {
            return @"struct".deserializeInner(T, dest, source, opts);
        },
        else => {
            return DeserializeError.ExpectedObject;
        },
    }
}

pub fn deserialize(
    comptime T: type,
    dest: *T,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    meta.expectStruct(T);

    if (opts.precice_errors) {
        switch (try src.peekNextTokenTypeDiscard(source, opts)) {
            .object_begin => {},
            else => {
                return DeserializeError.ExpectedObject;
            },
        }
    } else {
        try src.nextTokenExpect(source, .object_begin, opts);
    }

    return @"struct".deserializeInner(T, dest, source, opts);
}

pub fn deserializeNullableRecheck(
    comptime T: type,
    dest: *?T,
    source: *Tokenizer,
    comptime token_type: types.Primitive,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    meta.expectStruct(T);

    switch (token_type) {
        .object_begin => {
            return @"struct".deserializeInner(T, &dest.*.?, source, opts);
        },
        .null => {
            return @"null".deserializeAssume(T, dest, source);
        },
        else => {
            return DeserializeError.ExpectedOptionalObject;
        },
    }
}

pub fn deserializeNullable(
    comptime T: type,
    dest: *?T,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    meta.expectStruct(T);

    switch (try src.peekNextTokenTypeDiscard(source, opts)) {
        .object_begin => {
            return @"struct".deserializeInner(T, &dest.*.?, source, opts);
        },
        .null => {
            return @"null".deserializeAssume(T, dest, source);
        },
        else => {
            return DeserializeError.ExpectedOptionalObject;
        },
    }
}
