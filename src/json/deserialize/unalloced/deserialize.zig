const std = @import("std");

const meta = @import("../meta.zig");
const types = @import("../token-types.zig");
const src = @import("../token-source.zig");

const Tokenizer = @import("../tokenizer.zig").Tokenizer;

const DeserializeOpts = @import("../opts.zig").DeserializeOpts;
const DeserializeError = @import("../errors.zig").DeserializeError;

const obj = @import("object.zig");

pub fn deserializePointerRecheck(
    comptime T: type,
    dest: *T,
    source: *Tokenizer,
    comptime id_token_type: types.Primitve,
) DeserializeError!void {
    meta.expectPointer(T);

    const info = @typeInfo(T).pointer;

    switch (info.size) {
        .slice => {
            if (T == []const u8) {
                return obj.string.deserializeRecheck(dest, source, id_token_type);
            }

            @compileError("Slices (exept strings ([]const u8)) are not allowed while parsing unallocated! Consider using deserializeAlloc().");
        },
        else => {
            @compileError("Pointers are not allowed while parsing unallocated! Consider using deserializeAlloc().");
        },
    }
}

pub fn deserializePointer(
    comptime T: type,
    dest: *T,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    meta.expectPointer(T);

    const info = @typeInfo(T).pointer;

    switch (info.size) {
        .slice => {
            if (T == []const u8) {
                return obj.string.deserialize(dest, source, opts);
            }

            @compileError("Slices (exept strings ([]const u8)) are not allowed while parsing unallocated! Consider using deserializeAlloc().");
        },
        else => {
            @compileError("Pointers are not allowed while parsing unallocated! Consider using deserializeAlloc().");
        },
    }
}

pub fn deserializePointerNullableRecheck(
    comptime T: type,
    dest: *?T,
    source: *Tokenizer,
    comptime id_token_type: types.Primitve,
) DeserializeError!void {
    meta.expectPointer(T);

    const info = @typeInfo(T).pointer;

    switch (info.size) {
        .slice => {
            if (T == []const u8) {
                return obj.string.deserializeNullableRecheck(dest, source, id_token_type);
            }

            @compileError("Slices (exept strings ([]const u8)) are not allowed while parsing unallocated! Consider using deserializeAlloc().");
        },
        else => {
            @compileError("Pointers are not allowed while parsing unallocated! Consider using deserializeAlloc().");
        },
    }
}

pub fn deserializePointerNullable(
    comptime T: type,
    dest: *?T,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    meta.expectPointer(T);

    const info = @typeInfo(T).pointer;

    switch (info.size) {
        .slice => {
            if (T == []const u8) {
                return obj.string.deserializeNullable(dest, source, opts);
            }

            @compileError("Slices (exept strings ([]const u8)) are not allowed while parsing unallocated! Consider using deserializeAlloc().");
        },
        else => {
            @compileError("Pointers are not allowed while parsing unallocated! Consider using deserializeAlloc().");
        },
    }
}

fn deserializeOptional(
    comptime T: type,
    dest: *T,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    meta.expectOptional(T);

    const Child = std.meta.Child(T);

    switch (@typeInfo(Child)) {
        .bool => {
            try obj.bool.deserializeNullable(dest, source, opts);
        },
        .int, .comptime_int => {
            try obj.number.int.deserializeNullable(Child, dest, source, opts);
        },
        .float, .comptime_float => {
            try obj.number.float.deserializeNullable(Child, dest, source, opts);
        },
        .array => {
            try obj.list.array.deserializeNullable(Child, dest, source, opts);
        },
        .@"struct" => {
            try obj.@"struct".deserializeNullable(Child, dest, source, opts);
        },
        .@"enum" => {
            try obj.@"enum".deserializeNullable(Child, dest, source, opts);
        },
        .@"union" => {
            try obj.@"union".deserializeNullable(Child, dest, source, opts);
        },
        .pointer => {
            try deserializePointerNullable(Child, dest, source, opts);
        },
        .optional => {
            try deserializeOptional(std.meta.Child(T), &dest.*.?, source, opts);
        },
        else => {
            @compileError("Unimplemented type!");
        },
    }
}

fn deserializeOptionalRecheck(
    comptime T: type,
    dest: *T,
    source: *Tokenizer,
    peek: u8,
    comptime token_type: types.Primitve,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    meta.expectOptional(T);

    const Child = std.meta.Child(T);

    switch (@typeInfo(Child)) {
        .bool => {
            try obj.bool.deserializeNullableRecheck(Child, dest, source, token_type);
        },
        .int, .comptime_int => {
            try obj.int.deserializeNullableRecheck(Child, dest, source, peek, token_type);
        },
        .float, .comptime_float => {
            try obj.float.deserializeNullableRecheck(Child, dest, source, peek, token_type);
        },
        .array => {
            try obj.list.array.deserializeNullableRecheck(Child, dest, source, token_type, opts);
        },
        .@"struct" => {
            try obj.@"struct".deserializeNullableRecheck(Child, dest, source, token_type, opts);
        },
        .@"enum" => {
            try obj.@"enum".deserializeNullableRecheck(Child, dest, source, token_type);
        },
        .pointer => {
            try deserializePointerNullableRecheck(Child, dest, source, token_type);
        },
        .optional => {
            try deserializeOptionalRecheck(Child, &dest.*.?, source, peek, token_type, opts);
        },
        else => {
            @compileError("Unimplemented type!");
        },
    }
}

pub fn deserializeChildRecheck(
    comptime T: type,
    dest: anytype,
    source: *Tokenizer,
    peeked: u8,
    comptime token_type: types.Primitve,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    meta.expectPointer(@TypeOf(dest));

    switch (@typeInfo(T)) {
        .bool => {
            try obj.bool.deserializeRecheck(dest, source, token_type);
        },
        .int, .comptime_int => {
            try obj.number.int.deserializeRecheck(T, dest, source, peeked, token_type);
        },
        .float, .comptime_float => {
            try obj.number.float.deserializeRecheck(T, dest, source, opts, token_type);
        },
        .array => {
            try obj.list.array.deserializeRecheck(T, dest, source, token_type, opts);
        },
        .@"struct" => {
            try obj.@"struct".deserializeRecheck(T, dest, source, token_type, opts);
        },
        .@"enum" => {
            try obj.@"enum".deserializeRecheck(T, dest, source, token_type);
        },
        .pointer => {
            try deserializePointerRecheck(T, dest, source, opts);
        },
        .optional => {
            try deserializeOptionalRecheck(T, dest, source, peeked, token_type, opts);
        },
        else => {
            @compileError("Unimplemented type!");
        },
    }
}

pub fn deserializeChild(
    comptime T: type,
    dest: *T,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    switch (@typeInfo(T)) {
        .bool => {
            try obj.bool.deserialize(T, dest, source, opts);
        },
        .int, .comptime_int => {
            try obj.number.int.deserialize(T, dest, source, opts);
        },
        .float, .comptime_float => {
            try obj.number.float.deserialize(T, dest, source, opts);
        },
        .array => {
            try obj.list.array.deserialize(T, dest, source, opts);
        },
        .@"struct" => {
            try obj.@"struct".deserialize(T, dest, source, opts);
        },
        .@"enum" => {
            try obj.@"enum".deserialize(T, dest, source, opts);
        },
        .@"union" => {
            try obj.@"union".deserialize(T, dest, source, opts);
        },
        .pointer => {
            try deserializePointer(T, dest, source, opts);
        },
        .optional => {
            try deserializeOptional(T, dest, source, opts);
        },
        else => {
            @compileError("Unimplemented type!");
        },
    }
}

pub fn deserialzeFromSource(
    comptime T: type,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!T {
    var dest = meta.createUndefined(T);

    try deserializeChild(T, &dest, source, opts);

    return dest;
}

pub fn deserialize(
    comptime T: type,
    json: []const u8,
    comptime opts: DeserializeOpts,
) DeserializeError!T {
    var source = Tokenizer{ .source = json };

    return deserialzeFromSource(T, &source, opts);
}
