const std = @import("std");
const builtin = @import("builtin");

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
    ExpectedArray,
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

pub const DeserializeOpts = struct {
    allow_trailing_comma: bool = true,
    whitespace: bool = true,
    precice_errors: bool = builtin.mode == .Debug,
};

pub inline fn tokenFitsType(
    comptime T: type,
    token_type: TokenTypePrimitive,
    comptime opts: DeserializeOpts,
) bool {
    _ = opts;

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
            @compileError("Unimplemented type!");
        },
        .@"union" => {
            @compileError("Unimplemented type!");
        },
        else => {
            @compileError("Invalid type!");
        },
    }
}

pub fn typeNameEql(
    comptime A: type,
    comptime B: type,
) bool {
    return comptime std.mem.eql(@typeName(A), @typeName(B));
}

pub fn expectedError(
    comptime T: type,
) DeserializeError {
    switch (@typeInfo(T)) {
        .bool => {
            return DeserializeError.ExpectedBool;
        },
        .int, .comptime_int, .float, .comptime_float => {
            return DeserializeError.ExpectedNumber;
        },
        .pointer => |info| {
            switch (info.size) {
                .slice => {
                    if (typeNameEql(std.meta.Child(T), []const u8)) {
                        return DeserializeError.ExpectedString;
                    }

                    return DeserializeError.ExpectedArray;
                },
                else => {
                    return expectedError(std.meta.Child(T));
                },
            }
        },
        .array => {
            return DeserializeError.ExpectedArray;
        },
        .@"struct" => {
            return DeserializeError.ExpectedObject;
        },
        else => {
            @compileError("Unimplemented type!");
        },
    }
}

pub fn expectBoolean(comptime T: type) void {
    switch (@typeInfo(T)) {
        .bool => {},
        else => {
            @compileError("Expected T to be an array!");
        },
    }
}

pub fn expectInt(comptime T: type) void {
    switch (@typeInfo(T)) {
        .int, .comptime_int => {},
        else => {
            @compileError("Expected T to be an array!");
        },
    }
}

pub fn expectFloat(comptime T: type) void {
    switch (@typeInfo(T)) {
        .float, .comptime_float => {},
        else => {
            @compileError("Expected T to be an array!");
        },
    }
}

pub fn expectPointer(comptime T: type) void {
    switch (@typeInfo(T)) {
        .pointer => {},
        else => {
            @compileError("Expected T to be an array!");
        },
    }
}

pub fn expectArray(comptime T: type) void {
    switch (@typeInfo(T)) {
        .array => {},
        else => {
            @compileError("Expected T to be an array!");
        },
    }
}

pub fn expectStruct(comptime T: type) void {
    switch (@typeInfo(T)) {
        .@"struct" => {},
        else => {
            @compileError("Expected T to be an array!");
        },
    }
}

pub fn arrayLenght(comptime T: type) comptime_int {
    expectArray(T);

    return @typeInfo(T).array.len;
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

pub inline fn peekNextTokenTypeDiscard(
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!TokenTypePrimitive {
    const peek = try peekNext(source, opts) orelse return DeserializeError.ExpectedToken;

    return Tokenizer.inferrTokenType(peek) orelse return DeserializeError.InvalidToken;
}

pub const Inferred = struct {
    token_type: TokenTypePrimitive,
    peeked: u8,
};

pub inline fn peekNextTokenType(
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!Inferred {
    const peeked = try peekNext(source, opts) orelse return DeserializeError.ExpectedToken;

    return .{
        .token_type = Tokenizer.inferrTokenType(peeked) orelse return DeserializeError.InvalidToken,
        .peeked = peeked,
    };
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
