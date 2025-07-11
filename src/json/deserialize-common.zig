const std = @import("std");
const builtin = @import("builtin");

const keywords = @import("keywords.zig");

const Tokenizer = @import("tokenizer.zig");

const Token = Tokenizer.Token;
const TokenType = Tokenizer.TokenType;
const TokenTypeNullable = Tokenizer.TokenTypeNullable;
const TokenTypePrimitive = Tokenizer.TokenTypePrimitive;

pub const DeserializeError = error{
    ExpectedBool,
    ExpectedOptionalBool,
    ExpectedNumber,
    ExpectedOptionalNumber,
    ExpectedString,
    ExpectedOptionalString,
    ExpectedField,
    ExpectedObject,
    ExpectedOptionalObject,
    ExpectedArray,
    ExpectedOptionalArray,
    ExpectedEnum,
    ExpectedOptionalEnum,
    ExpectedUnion,
    ExpectedOptionalUnion,
    ExpectedTag,
    UnknownTag,
    ArrayTooShort,
    ArrayTooLong,
    TrailingComma,
    MissingComma,
    MissingArrayItem,
    MissingField,
    UnknownField,
    DuplicateField,
    IllegalWhitespace,
} || Tokenizer.ParseError;

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

pub const DeserializeOpts = struct {
    allow_trailing_comma: bool = true,
    whitespace: bool = true,
    precice_errors: bool = builtin.mode == .Debug,
    union_opts: UnionDeserializeOpts,
};

fn optionalize(comptime T: type) type {
    comptime {
        expectStruct(T);

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

pub fn expectBoolean(
    comptime T: type,
) void {
    switch (@typeInfo(T)) {
        .bool => {},
        else => {
            @compileError("Expected T to be a boolean!");
        },
    }
}

pub fn expectInt(
    comptime T: type,
) void {
    switch (@typeInfo(T)) {
        .int, .comptime_int => {},
        else => {
            @compileError("Expected T to be a integer!");
        },
    }
}

pub fn expectFloat(
    comptime T: type,
) void {
    switch (@typeInfo(T)) {
        .float, .comptime_float => {},
        else => {
            @compileError("Expected T to be a float!");
        },
    }
}

pub fn expectPointer(
    comptime T: type,
) void {
    switch (@typeInfo(T)) {
        .pointer => {},
        else => {
            @compileError("Expected T to be a pointer!");
        },
    }
}

pub fn expectArray(
    comptime T: type,
) void {
    switch (@typeInfo(T)) {
        .array => {},
        else => {
            @compileError("Expected T to be a array!");
        },
    }
}

pub fn expectString(
    comptime T: type,
) void {
    if (!(T == []const u8)) {
        @compileError("Expected T to be a array!");
    }
}

pub fn expectOptional(
    comptime T: type,
) void {
    switch (@typeInfo(T)) {
        .optional => {},
        else => {
            @compileError("Expected T to be a optional!");
        },
    }
}

pub fn expectStruct(
    comptime T: type,
) void {
    switch (@typeInfo(T)) {
        .@"struct" => {},
        else => {
            @compileError("Expected T to be a struct!");
        },
    }
}

pub fn expectEnum(
    comptime T: type,
) void {
    switch (@typeInfo(T)) {
        .@"enum" => {},
        else => {
            @compileError("Expected T to be a enum!");
        },
    }
}

pub fn expectUnion(
    comptime T: type,
) void {
    switch (@typeInfo(T)) {
        .@"union" => {},
        else => {
            @compileError("Expected T to be a enum!");
        },
    }
}

pub fn unionTags(
    comptime T: type,
) type {
    comptime {
        expectUnion(T);

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
        expectUnion(T);

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
        expectUnion(T);

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

pub fn arrayLenght(comptime T: type) comptime_int {
    expectArray(T);

    return @typeInfo(T).array.len;
}

pub inline fn createUndefined(
    comptime T: type,
) T {
    comptime {
        switch (@typeInfo(T)) {
            .bool,
            .int,
            .comptime_int,
            .float,
            .comptime_float,
            .pointer,
            .@"enum",
            .@"union",
            => {
                return undefined;
            },
            .array => {
                const Item = std.meta.Child(T);

                return [1]Item{createUndefined(Item)} ** arrayLenght(T);
            },
            .@"struct" => |info| {
                var value: T = undefined;

                for (info.fields) |field| {
                    @field(value, field.name) = createUndefined(field.type);
                }

                return value;
            },
            .optional => {
                return createUndefined(std.meta.Child(T));
            },
            else => {
                @compileError("Unimplemented type!");
            },
        }
    }
}

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

pub fn skipStructInner(
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    while (true) {
        switch (try peekNextTokenTypeDiscard(source, opts)) {
            .object_end => {
                break;
            },
            .object_begin => {
                try skipStructInner(source, opts);
            },
            .array_begin => {
                try skipArrayInner(source, opts);
            },
            .true => {
                source.skipTrue();
            },
            .false => {
                source.skipFalse();
            },
            .number => {
                source.skipNumber();
            },
            .string => {
                source.skipString();
            },
            else => {
                continue;
            },
        }
    }
}

pub fn skipArrayInner(
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    while (true) {
        switch (try peekNextTokenTypeDiscard(source, opts)) {
            .array_end => {
                break;
            },
            .object_begin => {
                try skipStructInner(source, opts);
            },
            .array_begin => {
                try skipArrayInner(source, opts);
            },
            .true => {
                source.skipTrue();
            },
            .false => {
                source.skipFalse();
            },
            .number => {
                source.skipNumber();
            },
            .string => {
                source.skipString();
            },
            else => {
                continue;
            },
        }
    }
}

pub fn skipNext(
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    switch (try peekNextTokenTypeDiscard(source, opts)) {
        .object_begin => {
            try skipStructInner(source, opts);
        },
        .array_begin => {
            try skipArrayInner(source, opts);
        },
        .true => {
            source.skipTrue();
        },
        .false => {
            source.skipFalse();
        },
        .number => {
            source.skipNumber();
        },
        .string => {
            source.skipString();
        },
        else => {},
    }
}

pub inline fn peekNext(
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!?u8 {
    if (opts.whitespace) {
        return source.consumeWhitespace();
    } else {
        if (opts.precice_errors) {
            const peeked = source.takeChar() orelse return null;

            return switch (peeked) {
                else => peeked,
                ' ', '\n', '\r', '\t' => {
                    return DeserializeError.IllegalWhitespace;
                },
            };
        } else {
            return source.takeChar();
        }
    }
}

pub fn nextToken(
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!Token {
    if (opts.whitespace) {
        source.nextToken();
    } else {
        source.takeToken();
    }
}

pub fn nextTokenExpect(
    source: *Tokenizer,
    comptime expected: TokenType,
    comptime opts: DeserializeOpts,
) DeserializeError!Tokenizer.tokenValueType(expected) {
    if (opts.whitespace) {
        return source.nextTokenExpect(expected);
    } else {
        return source.takeTokenExpect(expected);
    }
}

pub fn nextTokenExpectNullable(
    source: *Tokenizer,
    comptime expected: TokenTypeNullable,
    comptime opts: DeserializeOpts,
) DeserializeError!Tokenizer.tokenValueTypeNullable(expected) {
    if (opts.whitespace) {
        return source.nextTokenExpectNullable(expected);
    } else {
        return source.takeTokenExpectNullable(expected);
    }
}

pub const Inferred = struct {
    token_type: TokenTypePrimitive,
    ch: u8,
};

pub inline fn peekNextTokenType(
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!Inferred {
    const ch = try peekNext(source, opts) orelse return DeserializeError.ExpectedToken;

    return .{
        .token_type = Tokenizer.inferrTokenType(ch) orelse return DeserializeError.InvalidToken,
        .ch = ch,
    };
}

pub inline fn peekNextTokenTypeDiscard(
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!TokenTypePrimitive {
    const peek = try peekNext(source, opts) orelse return DeserializeError.ExpectedToken;

    return Tokenizer.inferrTokenType(peek) orelse return DeserializeError.InvalidToken;
}
