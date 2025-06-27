const std = @import("std");
const builtin = @import("builtin");

const Tokenizer = @import("tokenizer.zig");

const TokenType = Tokenizer.TokenType;
const TokenTypePrimitive = Tokenizer.TokenTypePrimitive;

const common = @import("deserialize-common.zig");

const DeserializeError = common.DeserializeError;
const DeserializeOpts = common.DeserializeOpts;

inline fn deserializeBoolean(
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!bool {
    return common.nextTokenExpect(source, .bool, opts);
}

inline fn deserializeBooleanInferred(
    source: *Tokenizer,
    inferred: TokenTypePrimitive,
) DeserializeError!void {
    switch (inferred) {
        .true => {
            try source.consumeTrueAssume();
            return true;
        },
        .false => {
            try source.consumeFalseAssume();
            return false;
        },
        else => {
            return DeserializeError.ExpectedBool;
        },
    }
}

inline fn deserializeOptionalBoolean(
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!?bool {
    return common.nextTokenExpect(source, .bool, opts);
}

inline fn deserializeOptionalBooleanInferred(
    source: *Tokenizer,
    inferred: TokenTypePrimitive,
) DeserializeError!?void {
    switch (inferred) {
        .true => {
            try source.consumeTrueAssume();
            return true;
        },
        .false => {
            try source.consumeFalseAssume();
            return false;
        },
        .null => {
            return null;
        },
        else => {
            return DeserializeError.ExpectedBool;
        },
    }
}

inline fn deserializeInteger(
    comptime T: type,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!T {
    const number = try common.nextTokenExpect(source, .number, opts);

    return std.fmt.parseInt(T, number, 10) catch return Tokenizer.ParseError.InvalidNumber;
}

inline fn deserializeIntegerInferred(
    comptime T: type,
    source: *Tokenizer,
    peek: u8,
) DeserializeError!T {
    const number = try source.takeTokenExpectPeek(peek, .number);

    return std.fmt.parseInt(T, number, 10) catch return Tokenizer.ParseError.InvalidNumber;
}

inline fn deserializeOptionalInteger(
    comptime T: type,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!T {
    const number = try common.nextTokenExpectNullable(source, .number, opts) orelse return null;

    return std.fmt.parseInt(T, number, 10) catch return Tokenizer.ParseError.InvalidNumber;
}

inline fn deserializeOptionalIntegerInferred(
    comptime T: type,
    source: *Tokenizer,
    peek: u8,
) DeserializeError!T {
    const number = try source.takeTokenExpectNullablePeek(peek, .number);

    return std.fmt.parseInt(T, number, 10) catch return Tokenizer.ParseError.InvalidNumber;
}

inline fn deserializeFloat(
    comptime T: type,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!T {
    const number = try common.nextTokenExpect(source, .number, opts);

    return std.fmt.parseFloat(T, number) catch return Tokenizer.ParseError.InvalidNumber;
}

inline fn deserializeFloatInferred(
    comptime T: type,
    source: *Tokenizer,
    peek: u8,
) DeserializeError!T {
    const number = try source.takeTokenExpectPeek(peek, .number);

    return std.fmt.parseFloat(T, number) catch return Tokenizer.ParseError.InvalidNumber;
}

inline fn deserializeOptionalFloat(
    comptime T: type,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!T {
    const number = try common.nextTokenExpectNullable(source, .number, opts) orelse return null;

    return std.fmt.parseFloat(T, number) catch return Tokenizer.ParseError.InvalidNumber;
}

inline fn deserializeOptionalFloatInferred(
    comptime T: type,
    source: *Tokenizer,
    peek: u8,
) DeserializeError!?T {
    const number = try source.takeTokenExpectNullablePeek(peek, .number) orelse return null;

    return std.fmt.parseFloat(T, number) catch return Tokenizer.ParseError.InvalidNumber;
}

inline fn deserializeString(
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError![]const u8 {
    if (opts.whitespace) {
        return source.nextTokenExpect(.string);
    } else {
        return source.takeTokenExpect(.string);
    }
}

inline fn deserializeOptionalString(
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeOpts!?[]const u8 {
    if (opts.whitespace) {
        return source.nextTokenExpectNullable(.string);
    } else {
        return source.takeTokenExpectNullable(.string);
    }
}

inline fn deserializePointer(
    comptime T: type,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) !T {
    const info = @typeInfo(T).pointer;

    switch (info.size) {
        .slice => {
            if (comptime std.mem.eql(u8, @typeName(T), @typeName([]const u8))) {
                return deserializeString(source, opts);
            }

            @compileError("Slices (exept strings ([]const u8)) are not allowed while parsing unallocated! Consider using deserializeAlloc().");
        },
        else => {
            @compileError("Pointers are not allowed while parsing unallocated! Consider using deserializeAlloc().");
        },
    }
}

inline fn deserializeOptionalPointer(
    comptime T: type,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) !T {
    const info = @typeInfo(T).pointer;

    switch (info.size) {
        .slice => {
            if (comptime std.mem.eql(u8, @typeName(T), @typeName([]const u8))) {
                return deserializeOptionalString(source, opts);
            }

            @compileError("Slices (exept strings ([]const u8)) are not allowed while parsing unallocated! Consider using deserializeAlloc().");
        },
        else => {
            @compileError("Pointers are not allowed while parsing unallocated! Consider using deserializeAlloc().");
        },
    }
}

fn deserializeArrayItem(
    comptime T: type,
    item: *T,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    if (opts.precice_errors) {
        const peek = try common.peekNext(source, opts) orelse return DeserializeError.ExpectedToken;

        switch (Tokenizer.inferrTokenType(peek) orelse return DeserializeError.InvalidToken) {
            .comma => {
                return DeserializeError.MissingArrayItem;
            },
            .array_end => {
                return DeserializeError.ArrayTooShort;
            },
            inline else => |inferred_type| {
                if (!common.tokenFitsType(T, inferred_type)) {
                    return common.expectedError(T);
                }

                try deserializeFieldInferred(T, item, source, peek, inferred_type, opts);
            },
        }
    } else {
        try deserializeField(T, item, source, opts);
    }
}

fn deserializeEmptyArray(
    comptime T: type,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.expectArray(T);

    if (common.arrayLenght(T) > 0) {
        @compileError("Expected array with lenght 0!");
    }

    try source.nextTokenExpect(.array_begin);

    if (opts.precice_errors) {
        const peek = try common.peekNext(source, opts) orelse return DeserializeError.ExpectedToken;

        switch (Tokenizer.inferrTokenType(peek) orelse return DeserializeError.InvalidToken) {
            .array_end => {},
            inline else => |inferred_type| {
                if (common.tokenFitsType(std.meta.Child(T), inferred_type)) {
                    return DeserializeError.ArrayTooLong;
                }

                return common.expectedError(std.meta.Child(T));
            },
        }
    } else {
        try common.nextTokenExpect(source, .array_end, opts);
    }
}

pub fn deserializeArray(
    comptime T: type,
    array: *T,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.expectArray(T);

    if (common.arrayLenght(T) == 0) {
        return deserializeEmptyArray(T, source, opts);
    }

    try common.nextTokenExpect(source, .array_begin, opts);

    { // deserialize items
        for (0..common.arrayLenght(T) - 1) |i| {
            try deserializeArrayItem(std.meta.Child(T), &array[i], source, opts);

            { // deserialize comma
                if (opts.precice_errors) {
                    switch (try common.inferrNext(source, opts)) {
                        .comma => {},
                        .array_end => {
                            return DeserializeError.ArrayTooShort;
                        },
                        else => {
                            return DeserializeError.MissingComma;
                        },
                    }
                } else {
                    try common.nextTokenExpect(source, .comma, opts);
                }
            }
        }

        try deserializeArrayItem(std.meta.Child(T), &array[common.arrayLenght(T) - 1], source, opts);
    }

    { // consume end
        if (opts.allow_trailing_comma) {
            switch (try common.inferrNext(source, opts)) {
                .array_end => {},
                .comma => {
                    if (opts.precice_errors) {
                        switch (try common.inferrNext(source, opts)) {
                            .array_end => {},
                            else => {
                                return DeserializeError.ArrayTooLong;
                            },
                        }
                    } else {
                        try common.nextTokenExpect(source, .array_end, opts);
                    }
                },
                else => {
                    return DeserializeError.MissingComma;
                },
            }
        } else {
            switch (try common.inferrNext(source, opts)) {
                .array_end => {},
                .comma => {
                    if (opts.precice_errors) {
                        switch (try common.inferrNext(source, opts)) {
                            .array_end => {
                                return DeserializeError.TrailingComma;
                            },
                            else => {
                                return DeserializeError.ArrayTooLong;
                            },
                        }
                    } else {
                        return DeserializeError.TrailingComma;
                    }
                },
                else => {
                    return DeserializeError.MissingComma;
                },
            }
        }
    }
}

pub inline fn deserializeField(
    comptime T: type,
    field: *T,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    switch (@typeInfo(T)) {
        .bool => {
            field.* = try deserializeBoolean(source, opts);
        },
        .int => {
            field.* = try deserializeInteger(T, source, opts);
        },
        .float => {
            field.* = try deserializeFloat(T, source, opts);
        },
        .pointer => {
            field.* = try deserializePointer(T, source, opts);
        },
        .array => {
            return deserializeArray(T, field, source, opts);
        },
        else => {
            @compileError("Unimplemented type!");
        },
    }
}

pub inline fn deserializeFieldInferred(
    comptime T: type,
    field: *T,
    source: *Tokenizer,
    peek: u8,
    comptime token_type: TokenTypePrimitive,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    _ = opts;

    switch (@typeInfo(T)) {
        .bool => {
            field.* = try deserializeBooleanInferred(source, token_type);
        },
        .int, .comptime_int => {
            field.* = try deserializeIntegerInferred(T, source, peek);
        },
        .float, .comptime_float => {
            field.* = try deserializeFloatInferred(T, source, peek);
        },
        else => {
            @compileError("Unimplemented type!");
        },
    }
}

pub fn deserialzeFromSource(comptime T: type, source: *Tokenizer, comptime opts: DeserializeOpts) DeserializeError!T {
    switch (@typeInfo(T)) {
        .bool => {
            return deserializeBoolean(source, opts);
        },
        .int => {
            return deserializeInteger(T, source, opts);
        },
        .float => {
            return deserializeFloat(T, source, opts);
        },
        .pointer => {
            return deserializePointer(T, source, opts);
        },
        .array => {
            @compileError("Arrays are only allowed as fields when parsing unallocated! Consider using deserializeAlloc().");
        },
        else => {
            @compileError("Unimplemented type!");
        },
    }
}

pub fn deserialize(comptime T: type, json: []const u8, comptime opts: DeserializeOpts) !T {
    var source = Tokenizer{ .source = json };

    return deserialzeFromSource(T, &source, opts);
}
