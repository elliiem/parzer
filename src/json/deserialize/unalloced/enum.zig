const std = @import("std");

const common = @import("parzer-common");

const types = @import("../token-types.zig");
const src = @import("../token-source.zig");

const Tokenizer = @import("../tokenizer.zig");

const DeserializeError = @import("../errors.zig").DeserializeError;
const DeserializeOpts = @import("../opts.zig").DeserializeOpts;

const obj = @import("object.zig");

const @"enum" = @This();

pub inline fn deserializeInner(
    comptime T: type,
    dest: *T,
    source: *Tokenizer,
) DeserializeError!void {
    common.meta.expectEnum(T);

    const tag = try source.takeStringInner();

    dest.* = std.meta.stringToEnum(T, tag) orelse return DeserializeError.UnknownTag;
}

pub fn deserialize(
    comptime T: type,
    dest: *T,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.meta.expectEnum(T);

    switch (try src.peekNextTokenTypeDiscard(source, opts)) {
        .string => {
            return @"enum".deserializeInner(T, dest, source);
        },
        else => {
            return DeserializeError.ExpectedEnum;
        },
    }
}

pub fn deserializeRecheck(
    comptime T: type,
    dest: *T,
    source: *Tokenizer,
    comptime id_token_type: types.Primitive,
) DeserializeError!void {
    common.meta.expectEnum(T);

    switch (id_token_type) {
        .string => {
            return @"enum".deserializeInner(T, dest, source);
        },
        else => {
            return DeserializeError.ExpectedEnum;
        },
    }
}

pub fn deserializeNullable(
    comptime T: type,
    dest: *?T,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.meta.expectEnum(T);

    switch (try src.peekNextTokenTypeDiscard(source, opts)) {
        .string => {
            return @"enum".deserializeInner(T, &dest.*.?, source);
        },
        .null => {
            return obj.null.deserializeAssume(T, dest, source);
        },
        else => {
            return DeserializeError.ExpectedOptionaEnum;
        },
    }
}

pub fn deserializeNullableRecheck(
    comptime T: type,
    dest: *?T,
    source: *Tokenizer,
    comptime id_token_type: types.Primitive,
) DeserializeError!void {
    common.meta.expectEnum(T);

    switch (id_token_type) {
        .string => {
            return @"enum".deserializeInner(T, &dest.*.?, source);
        },
        .null => {
            return obj.null.deserializeAssume(T, dest, source);
        },
        else => {
            return DeserializeError.ExpectedOptionalEnum;
        },
    }
}
