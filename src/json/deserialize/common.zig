const std = @import("std");
const builtin = @import("builtin");

const Tokenizer = @import("tokenizer.zig");

// const keywords = @import("keywords.zig");
const meta = @import("meta.zig");

const TokenTypePrimitive = Tokenizer.TokenTypePrimitive;

// maybe remove
pub inline fn tokenFitsType(
    comptime T: type,
    token_type: TokenTypePrimitive,
) bool {
    switch (@typeInfo(T)) {
        .bool => {
            return token_type == .true or token_type == .false;
        },
        .int, .comptime_int, .float, .comptime_float => {
            return token_type == .number;
        },
        .pointer => |info| {
            switch (info.size) {
                .slice => {
                    if (comptime std.mem.eql(@typeName(std.meta.Child(T)), @typeName([]const u8))) {
                        return token_type == .string;
                    }

                    return token_type == .array_begin;
                },
                else => {
                    return tokenFitsType(std.meta.Child(T), token_type);
                },
            }
        },
        .array => {
            return token_type == .array_begin;
        },
        .@"struct" => {
            return token_type == .object_begin;
        },
        .optional => {
            return tokenFitsType(std.meta.Child(T), token_type) or token_type == .null;
        },
        .@"enum" => {
            return token_type == .string;
        },
        else => {
            @compileError("Unimplemented type!");
        },
    }
}
