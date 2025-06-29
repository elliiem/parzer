const std = @import("std");
const builtin = @import("builtin");

const Tokenizer = @import("tokenizer.zig");

const TokenType = Tokenizer.TokenType;
const TokenTypePrimitive = Tokenizer.TokenTypePrimitive;

const common = @import("deserialize-common.zig");

const DeserializeError = common.DeserializeError;
const DeserializeOpts = common.DeserializeOpts;

// --------------------------------------------------
// deserializeBoolean
//
inline fn deserializeBooleanInferred(
    source: *Tokenizer,
    comptime token_type: TokenTypePrimitive,
) DeserializeError!bool {
    switch (token_type) {
        .true => {
            try source.consumeTrueAssume();
            return true;
        },
        .false => {
            try source.consumeFalseAssume();
            return false;
        },
        else => {
            return DeserializeError.ExpectedOptionalBool;
        },
    }
}

inline fn deserializeBoolean(
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!bool {
    if (opts.precice_errors) {
        switch (try common.peekNextTokenTypeDiscard(source, opts)) {
            inline else => |token_type| {
                return deserializeBooleanInferred(source, token_type);
            },
        }
    } else {
        return common.nextTokenExpect(source, .bool, opts);
    }
}

inline fn deserializeOptionalBooleanInferred(
    source: *Tokenizer,
    comptime token_type: TokenTypePrimitive,
) DeserializeError!?bool {
    switch (token_type) {
        .true => {
            try source.consumeTrueAssume();
            return true;
        },
        .false => {
            try source.consumeFalseAssume();
            return false;
        },
        .null => {
            try source.consumeNullAssume();
            return null;
        },
        else => {
            return DeserializeError.ExpectedOptionalBool;
        },
    }
}

inline fn deserializeOptionalBoolean(
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!?bool {
    if (opts.precice_errors) {
        switch (try common.peekNextTokenTypeDiscard(source, opts)) {
            inline else => |token_type| {
                return deserializeOptionalBooleanInferred(source, token_type);
            },
        }
    } else {
        return common.nextTokenExpectNullable(source, .bool, opts);
    }
}

// deserializeBoolean
// --------------------------------------------------

// --------------------------------------------------
// deserializeInteger

inline fn deserializeIntegerInferred(
    comptime T: type,
    source: *Tokenizer,
    peeked: u8,
    comptime token_type: TokenTypePrimitive,
) DeserializeError!T {
    switch (token_type) {
        .number => {
            const number = try source.takeTokenExpectPeek(peeked, .number);

            return std.fmt.parseInt(T, number, 10) catch return Tokenizer.ParseError.InvalidNumber;
        },
        else => {
            return DeserializeError.ExpectedNumber;
        },
    }
}

inline fn deserializeInteger(
    comptime T: type,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!T {
    common.expectInt(T);

    if (opts.precice_errors) {
        const inferred = try common.peekNextTokenType(source, opts);

        switch (inferred) {
            .number => {
                deserializeIntegerInferred(T, source, inferred.peeked, .number);
            },
            else => {
                return DeserializeError.ExpectedNumber;
            },
        }
    } else {
        const number = try common.nextTokenExpect(source, .number, opts);

        return std.fmt.parseInt(T, number, 10) catch return Tokenizer.ParseError.InvalidNumber;
    }
}

inline fn deserializeOptionalIntegerInferred(
    comptime T: type,
    source: *Tokenizer,
    peeked: u8,
    comptime token_type: TokenTypePrimitive,
) DeserializeError!?T {
    switch (token_type) {
        .number => {
            return deserializeIntegerInferred(T, source, peeked, token_type, .number);
        },
        .null => {
            try source.consumeNullAssume();
            return null;
        },
        else => {
            return DeserializeError.ExpectedOptionalNumber;
        },
    }
}

inline fn deserializeOptionalInteger(
    comptime T: type,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!?T {
    common.expectInt(T);

    if (opts.precice_errors) {
        const inferred = try common.peekNextTokenType(source, opts);

        return deserializeOptionalIntegerInferred(T, source, inferred.peeked, inferred.token_type);
    } else {
        const number = try common.nextTokenExpectNullable(source, .number, opts);

        return std.fmt.parseInt(T, number, 10) catch return Tokenizer.ParseError.InvalidNumber;
    }
}

// deserializeInteger
// --------------------------------------------------

// --------------------------------------------------
// deserializeFloat

inline fn deserializeFloatInferred(
    comptime T: type,
    source: *Tokenizer,
    peeked: u8,
    comptime token_type: TokenTypePrimitive,
) DeserializeError!T {
    switch (token_type) {
        .number => {
            const number = try source.takeTokenExpectPeek(peeked, .number);

            return std.fmt.parseFloat(T, number) catch return Tokenizer.ParseError.InvalidNumber;
        },
        else => {
            return DeserializeError.ExpectedNumber;
        },
    }
}

inline fn deserializeFloat(
    comptime T: type,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!T {
    common.expectFloat(T);

    if (opts.precice_errors) {
        const inferred = try common.peekNextTokenType(source, opts);

        switch (inferred.token_type) {
            .number => {
                return deserializeFloatInferred(T, source, inferred.peeked, .number);
            },
            else => {
                return DeserializeError.ExpectedNumber;
            },
        }
    } else {
        const number = try common.nextTokenExpect(source, .number, opts);

        return std.fmt.parseFloat(T, number) catch return Tokenizer.ParseError.InvalidNumber;
    }
}

inline fn deserializeOptionalFloatInferred(
    comptime T: type,
    source: *Tokenizer,
    peeked: u8,
    comptime token_type: TokenTypePrimitive,
) DeserializeError!?T {
    switch (token_type) {
        .number => {
            const number = try source.takeTokenExpectPeek(peeked, .number);

            return std.fmt.parseFloat(T, number) catch return Tokenizer.ParseError.InvalidNumber;
        },
        .null => {
            try source.consumeNullAssume();
            return null;
        },
        else => {
            return DeserializeError.ExpectedOptionalNumber;
        },
    }
}

inline fn deserializeOptionalFloat(
    comptime T: type,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!T {
    common.expectFloat(T);

    if (opts.precice_errors) {
        const inferred = try common.peekNextTokenType(source, opts);

        return deserializeOptionalFloatInferred(T, source, inferred.peeked, inferred.token_type);
    } else {
        const number = try common.nextTokenExpectNullable(source, .number, opts);

        return std.fmt.parseFloat(T, number) catch return Tokenizer.ParseError.InvalidNumber;
    }
}

// deserializeFloat
// --------------------------------------------------

// --------------------------------------------------
// deserializeString

inline fn deserializeStringPeeked(
    source: *Tokenizer,
    comptime token_type: TokenTypePrimitive,
) DeserializeError![]const u8 {
    switch (token_type) {
        .string => {
            return source.takeStringAssume();
        },
        else => {
            return DeserializeError.ExpectedString;
        },
    }
}

inline fn deserializeString(
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError![]const u8 {
    if (opts.precice_errors) {
        return deserializeStringPeeked(source, common.peekNextTokenTypeDiscard(source, opts));
    } else {
        return common.nextTokenExpect(source, .string, opts);
    }
}

inline fn deserializeOptionalStringPeeked(
    source: *Tokenizer,
    comptime token_type: TokenTypePrimitive,
) DeserializeError![]const u8 {
    switch (token_type) {
        .string => {
            return source.takeStringAssume();
        },
        .null => {
            try source.consumeNullAssume();
            return null;
        },
        else => {
            return DeserializeError.ExpectedOptionalString;
        },
    }
}

inline fn deserializeOptionalString(
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeOpts!?[]const u8 {
    if (opts.precice_errors) {
        return deserializeOptionalStringPeeked(source, common.peekNextTokenTypeDiscard(source, opts));
    } else {
        return common.nextTokenExpectNullable(source, .string, opts);
    }
}

// deserializeString
// --------------------------------------------------

// --------------------------------------------------
// deserializePointer

inline fn deserializePointerInferred(
    comptime T: type,
    source: *Tokenizer,
) DeserializeError!T {
    common.expectPointer(T);

    const info = @typeInfo(T).pointer;

    switch (info.size) {
        .slice => {
            if (comptime std.mem.eql(u8, @typeName(T), @typeName([]const u8))) {
                return deserializeStringPeeked(source);
            }

            @compileError("Slices (exept strings ([]const u8)) are not allowed while parsing unallocated! Consider using deserializeAlloc().");
        },
        else => {
            @compileError("Pointers are not allowed while parsing unallocated! Consider using deserializeAlloc().");
        },
    }
}

inline fn deserializePointer(
    comptime T: type,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!T {
    common.expectPointer(T);

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

inline fn deserializeOptionalPointerInferred(
    comptime T: type,
    source: *Tokenizer,
) DeserializeError!T {
    common.expectPointer(T);

    const info = @typeInfo(T).pointer;

    switch (info.size) {
        .slice => {
            if (comptime std.mem.eql(u8, @typeName(T), @typeName([]const u8))) {
                return deserializeOptionalStringPeeked(source);
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
) !?T {
    common.expectPointer(T);

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

// deserializePointer
// --------------------------------------------------

// --------------------------------------------------
// deserializeArray

inline fn deserializeArrayItem(
    comptime T: type,
    item: *T,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    if (opts.precice_errors) {
        const peeked = try common.peekNext(source, opts) orelse return DeserializeError.ExpectedToken;

        switch (Tokenizer.inferrTokenType(peeked) orelse return DeserializeError.InvalidToken) {
            .comma => {
                return DeserializeError.MissingArrayItem;
            },
            .array_end => {
                return DeserializeError.ArrayTooShort;
            },
            inline else => |token_type| {
                try deserializeInnerInferred(T, item, source, peeked, token_type, opts);
            },
        }
    } else {
        try deserializeInner(T, item, source, opts);
    }
}

inline fn consumeArrayTerminator(
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    switch (try common.peekNextTokenTypeDiscard(source, opts)) {
        .array_end => {},
        .comma => {
            if (opts.precice_errors) {
                switch (try common.peekNextTokenTypeDiscard(source, opts)) {
                    .array_end => {
                        if (!opts.allow_trailing_comma) {
                            return DeserializeError.TrailingComma;
                        }
                    },
                    else => {
                        return DeserializeError.ArrayTooLong;
                    },
                }
            } else {
                if (opts.allow_trailing_comma) {
                    try common.nextTokenExpect(source, .array_end, opts);
                } else {
                    return DeserializeError.TrailingComma;
                }
            }
        },
        else => {
            return DeserializeError.MissingComma;
        },
    }
}

inline fn consumeArraySeperator(
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    if (opts.precice_errors) {
        switch (try common.peekNextTokenTypeDiscard(source, opts)) {
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

inline fn deserializeEmptyArrayPeeked(
    comptime T: type,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    { // inspect passed type
        common.expectArray(T);

        if (common.arrayLenght(T) > 0) {
            @compileError("Expected array with lenght 0!");
        }
    }

    if (opts.precice_errors) {
        switch (try common.peekNextTokenTypeDiscard(source, opts)) {
            .array_end => {},
            inline else => |token_type| {
                if (common.tokenFitsType(std.meta.Child(T), token_type, opts)) {
                    return DeserializeError.ArrayTooLong;
                }

                return common.expectedError(std.meta.Child(T));
            },
        }
    } else {
        try common.nextTokenExpect(source, .array_end, opts);
    }
}

inline fn deserializeEmptyArray(
    comptime T: type,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    { // inspect passed type
        common.expectArray(T);

        if (common.arrayLenght(T) > 0) {
            @compileError("Expected array with lenght 0!");
        }
    }

    if (opts.precice_errors) {
        switch (try common.peekNextTokenTypeDiscard(source, opts)) {
            .array_begin => {},
            else => {
                return DeserializeError.ExpectedArray;
            },
        }
    } else {
        try common.nextTokenExpect(source, .array_begin, opts);
    }

    return deserializeEmptyArrayPeeked(T, source, opts);
}

fn deserializeArrayInferred(
    comptime T: type,
    array: *T,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    { // inspect passed type
        common.expectArray(T);

        if (common.arrayLenght(T) == 0) {
            return deserializeEmptyArrayPeeked(T, source, opts);
        }
    }

    {
        for (0..common.arrayLenght(T) - 1) |i| {
            try deserializeArrayItem(std.meta.Child(T), &array[i], source, opts);

            try consumeArraySeperator(source, opts);
        }

        try deserializeArrayItem(std.meta.Child(T), &array[common.arrayLenght(T) - 1], source, opts);
    }

    try consumeArrayTerminator(source, opts);
}

inline fn deserializeArray(
    comptime T: type,
    array: *T,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.expectArray(T);

    if (common.arrayLenght(T) == 0) {
        return deserializeEmptyArray(T, source, opts);
    }

    if (opts.precice_errors) {
        switch (try common.peekNextTokenTypeDiscard(source, opts)) {
            .array_begin => {},
            else => {
                return DeserializeError.ExpectedArray;
            },
        }
    } else {
        try common.nextTokenExpect(source, .array_begin, opts);
    }

    return deserializeArrayInferred(T, array, source, opts);
}

// deserializeArray
// --------------------------------------------------

// --------------------------------------------------
// deserializeStruct

inline fn firstStructFieldName(
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!?[]const u8 {
    switch (try common.peekNextTokenTypeDiscard(source, opts)) {
        .string => {
            return try source.takeFieldAssume();
        },
        .object_end => {
            return null;
        },
        else => {
            return DeserializeError.ExpectedField;
        },
    }
}

inline fn takeStructFieldNameTrailing(
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!?[]const u8 {
    switch (try common.peekNextTokenTypeDiscard(source, opts)) {
        .string => {
            return try source.takeFieldAssume();
        },
        .object_end => {
            if (!opts.allow_trailing_comma) {
                return DeserializeError.TrailingComma;
            }

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

inline fn nextTrailingStructFieldName(
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!?[]const u8 {
    switch (try common.peekNextTokenTypeDiscard(source, opts)) {
        .comma => {
            return takeStructFieldNameTrailing(source, opts);
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

inline fn deserializeStructFieldValue(
    comptime T: type,
    dest: *T,
    seen: []bool,
    source: *Tokenizer,
    name: []const u8,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.expectStruct(T);

    const info = @typeInfo(T).@"struct";

    inline for (info.fields, 0..) |field, i| {
        if (std.mem.eql(u8, name, field.name)) {
            if (seen[i]) {
                return DeserializeError.DuplicateField;
            }

            seen[i] = true;

            return deserializeInner(
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

fn checkSeenFields(
    comptime T: type,
    seen: []bool,
) DeserializeError!void {
    common.expectStruct(T);

    const fields = @typeInfo(T).@"struct".fields;

    inline for (fields, 0..) |field, i| {
        switch (@typeInfo(field.type)) {
            .optional => {},
            else => {
                if (!seen[i]) {
                    return DeserializeError.MissingField;
                }
            },
        }
    }
}

fn deserialzeStructInferred(
    comptime T: type,
    dest: *T,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.expectStruct(T);

    const info = @typeInfo(T).@"struct";

    var seen = [1]bool{false} ** info.fields.len;

    { // Parse first field
        const field_name = try firstStructFieldName(source, opts) orelse return checkSeenFields(T, &seen);

        try deserializeStructFieldValue(
            T,
            dest,
            &seen,
            source,
            field_name,
            opts,
        );
    }

    {
        while (try nextTrailingStructFieldName(source, opts)) |field_name| {
            try deserializeStructFieldValue(
                T,
                dest,
                &seen,
                source,
                field_name,
                opts,
            );
        }
    }

    return checkSeenFields(T, &seen);
}

inline fn deserializeStruct(
    comptime T: type,
    dest: *T,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.expectStruct(T);

    if (opts.precice_errors) {
        switch (try common.peekNextTokenTypeDiscard(source, opts)) {
            .object_begin => {},
            else => {
                return DeserializeError.ExpectedObject;
            },
        }
    } else {
        try common.nextTokenExpect(source, .object_begin, opts);
    }

    return deserialzeStructInferred(T, dest, source, opts);
}

// deserializeStruct
// --------------------------------------------------

fn deserializeOptional(
    comptime T: type,
    dest: anytype,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.expectPointer(@TypeOf(dest));

    switch (@typeInfo(T)) {
        .bool => {
            dest.* = try deserializeOptionalBoolean(source, opts);
        },
        .int, .comptime_int => {
            dest.* = try deserializeOptionalInteger(T, source, opts);
        },
        .float, .comptime_float => {
            dest.* = try deserializeOptionalFloat(T, source, opts);
        },
        .pointer => {
            dest.* = try deserializeOptionalPointer(T, source, opts);
        },
        .optional => {
            try deserializeOptional(std.meta.Child(T), dest, source, opts);
        },
        else => {
            @compileError("Unimplemented type!");
        },
    }
}

fn deserializeOptionalInferred(
    comptime T: type,
    dest: anytype,
    source: *Tokenizer,
    peeked: u8,
    comptime token_type: TokenTypePrimitive,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.expectPointer(@TypeOf(dest));

    switch (@typeInfo(std.meta.Child(T))) {
        .bool => {
            dest.* = try deserializeBooleanInferred(source, token_type);
        },
        .int, .comptime_int => {
            dest.* = try deserializeOptionalIntegerInferred(T, source, peeked, token_type);
        },
        .float, .comptime_float => {
            dest.* = try deserializeOptionalFloatInferred(T, source, peeked, token_type);
        },
        .pointer => {
            dest.* = try deserializeOptionalPointerInferred(source, token_type);
        },
        .optional => {
            try deserializeOptionalInferred(T, dest, source, peeked, token_type, opts);
        },
        else => {
            @compileError("Unimplemented type!");
        },
    }
}

pub fn deserializeInnerInferred(
    comptime T: type,
    field: *T,
    source: *Tokenizer,
    peeked: u8,
    comptime token_type: TokenTypePrimitive,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.expectPointer(@TypeOf(field));

    switch (@typeInfo(T)) {
        .bool => {
            field.* = try deserializeOptionalBooleanInferred(source, token_type);
        },
        .int, .comptime_int => {
            field.* = try deserializeIntegerInferred(T, source, peeked, token_type);
        },
        .float, .comptime_float => {
            field.* = try deserializeFloatInferred(T, source, peeked, token_type);
        },
        .pointer => {
            field.* = try deserializePointerInferred(T, source);
        },
        .array => {
            try deserializeArrayInferred(T, field, source, opts);
        },
        .@"struct" => {
            try deserialzeStructInferred(T, field, source, opts);
        },
        .optional => {
            try deserializeOptionalInferred(T, field, source, peeked, token_type, opts);
        },
        else => {
            @compileError("Unimplemented type!");
        },
    }
}

pub fn deserializeInner(
    comptime T: type,
    field: *T,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.expectPointer(@TypeOf(field));

    switch (@typeInfo(T)) {
        .bool => {
            field.* = try deserializeBoolean(source, opts);
        },
        .int, .comptime_int => {
            field.* = try deserializeInteger(T, source, opts);
        },
        .float, .comptime_float => {
            field.* = try deserializeFloat(T, source, opts);
        },
        .pointer => {
            field.* = try deserializePointer(T, source, opts);
        },
        .array => {
            return deserializeArray(T, field, source, opts);
        },
        .@"struct" => {
            try deserializeStruct(T, field, source, opts);
        },
        .optional => {
            try deserializeOptional(std.meta.Child(T), field, source, opts);
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
        .@"struct" => {
            var dest: T = undefined;

            try deserializeStruct(T, &dest, source, opts);

            return dest;
        },
        else => {
            @compileError("Unimplemented type!");
        },
    }
}

pub fn deserialize(
    comptime T: type,
    json: []const u8,
    comptime opts: DeserializeOpts,
) !T {
    var source = Tokenizer{ .source = json };

    return deserialzeFromSource(T, &source, opts);
}
