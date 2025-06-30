const std = @import("std");
const builtin = @import("builtin");

const Tokenizer = @import("tokenizer.zig");

const TokenType = Tokenizer.TokenType;
const TokenTypePrimitive = Tokenizer.TokenTypePrimitive;

const common = @import("deserialize-common.zig");

const DeserializeError = common.DeserializeError;
const DeserializeOpts = common.DeserializeOpts;

inline fn deserializeNullAssume(
    dest: anytype,
    source: *Tokenizer,
) DeserializeError!void {
    defer dest.* = null;
    return source.consumeNullAssume();
}

// --------------------------------------------------
// deserializeBoolean

inline fn deserializeTrueAssume(
    dest: anytype,
    source: *Tokenizer,
) DeserializeError!void {
    common.checkTypesBoolean(@TypeOf(dest));

    defer dest.* = true;
    return source.consumeTrueAssume();
}

inline fn deserializeFalseAssume(
    dest: anytype,
    source: *Tokenizer,
) DeserializeError!void {
    common.checkTypesBoolean(@TypeOf(dest));

    defer dest.* = false;
    return source.consumeFalseAssume();
}

fn deserializeBooleanInferred(
    dest: anytype,
    source: *Tokenizer,
    comptime token_type: TokenTypePrimitive,
) DeserializeError!void {
    common.checkTypesBoolean(@TypeOf(dest));

    switch (token_type) {
        .true => {
            return deserializeTrueAssume(dest, source);
        },
        .false => {
            return deserializeFalseAssume(dest, source);
        },
        else => {
            return DeserializeError.ExpectedBool;
        },
    }
}

fn deserializeBoolean(
    dest: anytype,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.checkTypesBoolean(@TypeOf(dest));

    if (opts.precice_errors) {
        switch (try common.peekNextTokenTypeDiscard(source, opts)) {
            .true => {
                return deserializeTrueAssume(dest, source);
            },
            .false => {
                return deserializeFalseAssume(dest, source);
            },
            else => {
                return DeserializeError.ExpectedBool;
            },
        }
    } else {
        return common.nextTokenExpect(source, .bool, opts);
    }
}

fn deserializeOptionalBooleanInferred(
    dest: anytype,
    source: *Tokenizer,
    comptime token_type: TokenTypePrimitive,
) DeserializeError!void {
    common.checkTypesBoolean(@TypeOf(dest));

    switch (token_type) {
        .true => {
            return deserializeTrueAssume(dest, source);
        },
        .false => {
            return deserializeFalseAssume(dest, source);
        },
        .null => {
            return deserializeNullAssume(dest, source);
        },
        else => {
            return DeserializeError.ExpectedOptionalBool;
        },
    }
}

fn deserializeOptionalBoolean(
    dest: anytype,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.checkTypesBoolean(@TypeOf(dest));

    if (opts.precice_errors) {
        switch (try common.peekNextTokenTypeDiscard(source, opts)) {
            .true => {
                return deserializeTrueAssume(dest, source);
            },
            .false => {
                return deserializeFalseAssume(dest, source);
            },
            .null => {
                return deserializeNullAssume(dest, source);
            },
            else => {
                return DeserializeError.ExpectedOptionalBool;
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

inline fn deserializeIntegerAssume(
    comptime Int: type,
    dest: anytype,
    source: *Tokenizer,
    peek: u8,
) DeserializeError!void {
    common.checkTypesInteger(Int, @TypeOf(dest));

    const number = try source.takeTokenExpectPeek(peek, .number);

    dest.* = std.fmt.parseInt(Int, number, 10) catch return Tokenizer.ParseError.InvalidNumber;
}

fn deserializeIntegerInferred(
    comptime Int: type,
    dest: anytype,
    source: *Tokenizer,
    peeked: u8,
    comptime token_type: TokenTypePrimitive,
) DeserializeError!void {
    common.checkTypesInteger(Int, @TypeOf(dest));

    switch (token_type) {
        .number => {
            return deserializeIntegerAssume(Int, dest, source, peeked);
        },
        else => {
            return DeserializeError.ExpectedNumber;
        },
    }
}

fn deserializeInteger(
    comptime Int: type,
    dest: anytype,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.checkTypesInteger(Int, @TypeOf(dest));

    if (opts.precice_errors) {
        const peek = try common.peekNextTokenType(source, opts);

        switch (peek) {
            .number => {
                return deserializeIntegerAssume(Int, dest, source, peek.ch);
            },
            else => {
                return DeserializeError.ExpectedNumber;
            },
        }
    } else {
        return deserializeIntegerAssume(
            Int,
            dest,
            source,
            common.peekNext(source, opts) orelse return DeserializeError.ExpectedToken,
        );
    }
}

fn deserializeOptionalIntegerInferred(
    comptime Int: type,
    dest: anytype,
    source: *Tokenizer,
    peek: u8,
    comptime token_type: TokenTypePrimitive,
) DeserializeError!void {
    common.checkTypesInteger(Int, @TypeOf(dest));

    switch (token_type) {
        .number => {
            return deserializeIntegerAssume(Int, dest, source, peek);
        },
        .null => {
            return deserializeNullAssume(dest, source);
        },
        else => {
            return DeserializeError.ExpectedOptionalNumber;
        },
    }
}

fn deserializeOptionalInteger(
    comptime Int: type,
    dest: anytype,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.checkTypesInteger(Int, @TypeOf(dest));

    if (opts.precice_errors) {
        const peek = try common.peekNextTokenType(source, opts);

        switch (peek.token_type) {
            .number => {
                return deserializeIntegerAssume(Int, dest, source, peek.ch);
            },
            .null => {
                return deserializeNullAssume(dest, source);
            },
            else => {
                return DeserializeError.ExpectedOptionalNumber;
            },
        }
    } else {
        return deserializeIntegerAssume(
            Int,
            dest,
            source,
            common.peekNext(source, opts) orelse return DeserializeError.ExpectedToken,
        );
    }
}

// deserializeInteger
// --------------------------------------------------

// --------------------------------------------------
// deserializeFloat

inline fn deserializeFloatAssume(
    comptime T: type,
    dest: anytype,
    source: *Tokenizer,
    peek: u8,
) DeserializeError!void {
    common.checkTypesFloat(T, @TypeOf(dest));

    const number = try source.takeTokenExpectPeek(peek, .number);

    return std.fmt.parseFloat(T, number) catch return Tokenizer.ParseError.InvalidNumber;
}

inline fn deserializeFloatInferred(
    comptime T: type,
    dest: anytype,
    source: *Tokenizer,
    peek: u8,
    comptime token_type: TokenTypePrimitive,
) DeserializeError!void {
    common.checkTypesFloat(T, @TypeOf(dest));

    switch (token_type) {
        .number => {
            return deserializeFloatAssume(T, dest, source, peek);
        },
        else => {
            return DeserializeError.ExpectedNumber;
        },
    }
}

inline fn deserializeFloat(
    comptime T: type,
    dest: anytype,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.checkTypesFloat(T, @TypeOf(dest));

    if (opts.precice_errors) {
        const peek = try common.peekNextTokenType(source, opts);

        switch (peek.token_type) {
            .number => {
                return deserializeFloatAssume(T, dest, source, peek.ch);
            },
            else => {
                return DeserializeError.ExpectedNumber;
            },
        }
    } else {
        return deserializeFloatAssume(
            T,
            dest,
            source,
            common.peekNext(source, opts) orelse return DeserializeError.ExpectedToken,
        );
    }
}

inline fn deserializeOptionalFloatInferred(
    comptime T: type,
    dest: anytype,
    source: *Tokenizer,
    peek: u8,
    comptime token_type: TokenTypePrimitive,
) DeserializeError!void {
    common.checkTypesFloat(T, @TypeOf(dest));

    switch (token_type) {
        .number => {
            return deserializeFloatAssume(T, dest, source, peek.ch);
        },
        .null => {
            return deserializeNullAssume(dest, source);
        },
        else => {
            return DeserializeError.ExpectedOptionalNumber;
        },
    }
}

inline fn deserializeOptionalFloat(
    comptime T: type,
    dest: anytype,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.checkTypesFloat(T, @TypeOf(dest));

    if (opts.precice_errors) {
        const peek = try common.peekNextTokenType(source, opts);

        switch (peek.token_type) {
            .number => {
                return deserializeFloatAssume(T, dest, source, peek.ch);
            },
            .null => {
                return deserializeNullAssume(dest, source);
            },
            else => {
                return DeserializeError.ExpectedOptionalNumber;
            },
        }
    } else {
        return deserializeFloatAssume(
            T,
            dest,
            source,
            common.peekNext(source, opts) orelse return DeserializeError.ExpectedToken,
        );
    }
}

// deserializeFloat
// --------------------------------------------------

// --------------------------------------------------
// deserializeString

inline fn deserializeStringAssume(
    dest: anytype,
    source: *Tokenizer,
) DeserializeError!void {
    common.checkTypesString(@TypeOf(dest));

    dest.* = try source.takeStringAssume();
}

inline fn deserializeStringInferred(
    dest: anytype,
    source: *Tokenizer,
    comptime token_type: TokenTypePrimitive,
) DeserializeError!void {
    common.checkTypesString(@TypeOf(dest));

    switch (token_type) {
        .string => {
            return deserializeStringAssume(dest, source);
        },
        else => {
            return DeserializeError.ExpectedString;
        },
    }
}

inline fn deserializeString(
    dest: anytype,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.checkTypesString(@TypeOf(dest));

    if (opts.precice_errors) {
        return deserializeStringInferred(
            dest,
            source,
            common.peekNextTokenTypeDiscard(source, opts),
        );
    } else {
        return common.nextTokenExpect(source, .string, opts);
    }
}

inline fn deserializeOptionalStringInferred(
    dest: anytype,
    source: *Tokenizer,
    comptime token_type: TokenTypePrimitive,
) DeserializeError!void {
    common.checkTypesString(@TypeOf(dest));

    switch (token_type) {
        .string => {
            return deserializeStringAssume(dest, source);
        },
        .null => {
            return deserializeNullAssume(dest, source);
        },
        else => {
            return DeserializeError.ExpectedOptionalString;
        },
    }
}

inline fn deserializeOptionalString(
    dest: anytype,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.checkTypesString(@TypeOf(dest));

    if (opts.precice_errors) {
        switch (try common.peekNextTokenTypeDiscard(source, opts)) {
            .string => {
                return deserializeStringAssume(dest, source);
            },
            .null => {
                return deserializeNullAssume(dest, source);
            },
            else => {
                return DeserializeError.ExpectedOptionalString;
            },
        }
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
    dest: anytype,
    source: *Tokenizer,
    comptime id_token_type: TokenTypePrimitive,
) DeserializeError!void {
    common.checkTypesPointer(T, @TypeOf(dest));

    const info = @typeInfo(T).pointer;

    switch (info.size) {
        .slice => {
            if (T == []const u8) {
                return deserializeStringInferred(dest, source, id_token_type);
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
    dest: anytype,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.checkTypesPointer(T, @TypeOf(dest));

    const info = @typeInfo(T).pointer;

    switch (info.size) {
        .slice => {
            if (T == []const u8) {
                return deserializeString(dest, source, opts);
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
    dest: anytype,
    source: *Tokenizer,
    comptime id_token_type: TokenTypePrimitive,
) DeserializeError!void {
    common.checkTypesPointer(T, @TypeOf(dest));

    const info = @typeInfo(T).pointer;

    switch (info.size) {
        .slice => {
            if (T == []const u8) {
                return deserializeOptionalStringInferred(dest, source, id_token_type);
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
    dest: anytype,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.checkTypesPointer(T, @TypeOf(dest));

    const info = @typeInfo(T).pointer;

    switch (info.size) {
        .slice => {
            if (T == []const u8) {
                return deserializeOptionalString(dest, source, opts);
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
    comptime Array: type,
    dest: anytype,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    {
        common.expectPointer(@TypeOf(dest));
        common.expectArray(Array);
    }

    if (opts.precice_errors) {
        const peeked = try common.peekNext(source, opts) orelse return DeserializeError.ExpectedToken;

        switch (Tokenizer.inferrTokenType(peeked) orelse return DeserializeError.InvalidToken) {
            .comma => {
                if (common.arrayLenght(Array) == 1) {
                    return DeserializeError.UnexpectedToken;
                }

                return DeserializeError.MissingArrayItem;
            },
            .array_end => {
                return DeserializeError.ArrayTooShort;
            },
            inline else => |token_type| {
                try deserializeInnerInferred(std.meta.Child(Array), dest, source, peeked, token_type, opts);
            },
        }
    } else {
        try deserializeInner(std.meta.Child(Array), dest, source, opts);
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

inline fn consumeEmptyArrayAssume(
    comptime Array: type,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    {
        common.expectArray(Array);

        if (common.arrayLenght(Array) > 0) {
            @compileError("Expected array with lenght 0!");
        }
    }

    if (opts.precice_errors) {
        switch (try common.peekNextTokenTypeDiscard(source, opts)) {
            .array_end => {},
            .comma => {
                return DeserializeError.UnexpectedToken;
            },
            inline else => |token_type| {
                if (common.tokenFitsType(std.meta.Child(Array), token_type, opts)) {
                    return DeserializeError.ArrayTooLong;
                }

                return common.expectedError(std.meta.Child(Array));
            },
        }
    } else {
        try common.nextTokenExpect(source, .array_end, opts);
    }
}

inline fn consumeEmptyArrayInferred(
    comptime Array: type,
    source: *Tokenizer,
    comptime id_token_type: TokenTypePrimitive,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    {
        common.expectArray(Array);

        if (common.arrayLenght(Array) > 0) {
            @compileError("Expected array with lenght 0!");
        }
    }

    switch (id_token_type) {
        .array_end => {},
        .comma => {
            return DeserializeError.UnexpectedToken;
        },
        inline else => |token_type| {
            if (common.tokenFitsType(std.meta.Child(Array), token_type, opts)) {
                return DeserializeError.ArrayTooLong;
            }

            return common.expectedError(std.meta.Child(Array));
        },
    }

    return consumeEmptyArrayAssume(Array, source, id_token_type, opts);
}

inline fn consumeEmptyArray(
    comptime Array: type,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    {
        common.expectArray(Array);

        if (common.arrayLenght(Array) > 0) {
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

    return consumeEmptyArrayAssume(Array, source, opts);
}

inline fn deserializeOptionalEmptyArrayInferred(
    comptime Array: type,
    dest: anytype,
    source: *Tokenizer,
    comptime id_token_type: TokenTypePrimitive,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    {
        common.expectPointer(@TypeOf(dest));
        common.expectArray(Array);

        if (common.arrayLenght(Array) > 0) {
            @compileError("Expected array with lenght 0!");
        }
    }

    switch (id_token_type) {
        .array_begin => {},
        .null => {
            dest.* = null;
            return source.consumeNullAssume();
        },
        else => {
            return DeserializeError.ExpectedOptionalArray;
        },
    }

    return consumeEmptyArrayAssume(Array, source, opts);
}

inline fn deserializeOptionalEmptyArray(
    comptime Array: type,
    dest: anytype,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    {
        common.expectPointer(@TypeOf(dest));
        common.expectArray(Array);

        if (common.arrayLenght(Array) > 0) {
            @compileError("Expected array with lenght 0!");
        }
    }

    switch (try common.peekNextTokenTypeDiscard(source, opts)) {
        .array_begin => {},
        .null => {
            dest.* = null;
            return source.consumeNullAssume();
        },
        else => {
            return DeserializeError.ExpectedOptionalArray;
        },
    }

    return consumeEmptyArrayAssume(Array, source, opts);
}

fn deserializeArrayAssume(
    comptime Array: type,
    dest: anytype,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    {
        common.expectPointer(@TypeOf(dest));
        common.expectArray(Array);

        if (common.arrayLenght(Array) == 0) {
            return consumeEmptyArrayAssume(Array, source, opts);
        }
    }

    {
        for (0..common.arrayLenght(Array) - 1) |i| {
            try deserializeArrayItem(Array, &dest.*[i], source, opts);

            try consumeArraySeperator(source, opts);
        }

        try deserializeArrayItem(Array, &dest.*[common.arrayLenght(Array) - 1], source, opts);
    }

    try consumeArrayTerminator(source, opts);
}

inline fn deserializeArrayInferred(
    comptime Array: type,
    dest: anytype,
    source: *Tokenizer,
    comptime id_token_type: TokenTypePrimitive,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    {
        common.expectPointer(@TypeOf(dest));
        common.expectArray(Array);

        if (common.arrayLenght(Array) == 0) {
            return consumeEmptyArray(Array, source, opts);
        }
    }

    switch (id_token_type) {
        .array_begin => {
            return deserializeArrayAssume(Array, dest, source, opts);
        },
        else => {
            return DeserializeError.ExpectedArray;
        },
    }
}

inline fn deserializeArray(
    comptime Array: type,
    dest: anytype,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    {
        common.expectPointer(@TypeOf(dest));
        common.expectArray(Array);

        if (common.arrayLenght(Array) == 0) {
            return consumeEmptyArray(Array, source, opts);
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

    return deserializeArrayAssume(Array, dest, source, opts);
}

fn deserializeOptionalArrayAssume(
    comptime Array: type,
    dest: anytype,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    {
        common.expectPointer(@TypeOf(dest));
        common.expectArray(Array);

        if (common.arrayLenght(Array) == 0) {
            return consumeEmptyArrayAssume(Array, source, opts);
        }
    }

    {
        for (0..common.arrayLenght(Array) - 1) |i| {
            try deserializeArrayItem(Array, &dest.*.?[i], source, opts);

            try consumeArraySeperator(source, opts);
        }

        try deserializeArrayItem(Array, &dest.*.?[common.arrayLenght(Array) - 1], source, opts);
    }

    try consumeArrayTerminator(source, opts);
}

inline fn deserializeOptionalArrayInferred(
    comptime Array: type,
    dest: anytype,
    source: *Tokenizer,
    comptime id_token_type: TokenTypePrimitive,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    {
        common.expectPointer(@TypeOf(dest));
        common.expectArray(Array);

        if (common.arrayLenght(Array) == 0) {
            return deserializeOptionalEmptyArrayInferred(Array, dest, source, id_token_type, opts);
        }
    }

    switch (id_token_type) {
        .array_begin => {},
        .null => {
            dest.* = null;
            return source.consumeNullAssume();
        },
        else => {
            return DeserializeError.ExpectedArray;
        },
    }

    return deserializeOptionalArrayAssume(Array, dest, source, opts);
}

inline fn deserializeOptionalArray(
    comptime Array: type,
    dest: anytype,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    {
        common.expectPointer(@TypeOf(dest));
        common.expectArray(Array);

        if (common.arrayLenght(Array) == 0) {
            return deserializeOptionalEmptyArray(Array, dest, source, opts);
        }
    }

    switch (try common.peekNextTokenTypeDiscard(source, opts)) {
        .array_begin => {},
        .null => {
            dest.* = null;
            return source.consumeNullAssume();
        },
        else => {
            return DeserializeError.ExpectedArray;
        },
    }

    return deserializeOptionalArrayAssume(Array, dest, source, opts);
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
            try deserializeOptionalBoolean(dest, source, opts);
        },
        .int, .comptime_int => {
            try deserializeOptionalInteger(T, dest, source, opts);
        },
        .float, .comptime_float => {
            try deserializeOptionalFloat(T, dest, source, opts);
        },
        .pointer => {
            try deserializeOptionalPointer(T, dest, source, opts);
        },
        .array => {
            // FIXME: Bandaid solution to make parsing arrays in comptime work
            if (@inComptime()) {
                dest.* = [1]std.meta.Child(T){undefined} ** common.arrayLenght(T);
            }

            try deserializeOptionalArray(T, dest, source, opts);
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
    peek: u8,
    comptime token_type: TokenTypePrimitive,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.expectPointer(@TypeOf(dest));

    switch (@typeInfo(std.meta.Child(T))) {
        .bool => {
            try deserializeOptionalBooleanInferred(dest, source, token_type);
        },
        .int, .comptime_int => {
            try deserializeOptionalIntegerInferred(std.meta.Child(T), dest, source, peek, token_type);
        },
        .float, .comptime_float => {
            try deserializeOptionalFloatInferred(std.meta.Child(T), dest, source, peek, token_type);
        },
        .pointer => {
            try deserializeOptionalPointerInferred(std.meta.Child(T), dest, source, token_type);
        },
        .array => {
            // FIXME: Bandaid solution to make parsing arrays in comptime work
            if (@inComptime()) {
                dest.* = [1]std.meta.Child(T){undefined} ** common.arrayLenght(T);
            }

            try deserializeOptionalArrayInferred(std.meta.Child(T), dest, source, token_type, opts);
        },
        .optional => {
            try deserializeOptionalInferred(std.meta.Child(T), dest, source, peek, token_type, opts);
        },
        else => {
            @compileError("Unimplemented type!");
        },
    }
}

pub fn deserializeInnerInferred(
    comptime T: type,
    dest: anytype,
    source: *Tokenizer,
    peeked: u8,
    comptime token_type: TokenTypePrimitive,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.expectPointer(@TypeOf(dest));

    switch (@typeInfo(T)) {
        .bool => {
            try deserializeBooleanInferred(source, dest, token_type);
        },
        .int, .comptime_int => {
            try deserializeIntegerInferred(T, dest, source, peeked, token_type);
        },
        .float, .comptime_float => {
            try deserializeFloat(T, dest, source, opts);
        },
        .pointer => {
            try deserializePointerInferred(T, dest, source, opts);
        },
        .array => {
            try deserializeArrayInferred(T, dest, source, token_type, opts);
        },
        .@"struct" => {
            try deserialzeStructInferred(T, dest, source, opts);
        },
        .optional => {
            try deserializeOptionalInferred(T, dest, source, peeked, token_type, opts);
        },
        else => {
            @compileError("Unimplemented type!");
        },
    }
}

pub fn deserializeInner(
    comptime T: type,
    dest: anytype,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.expectPointer(@TypeOf(dest));

    switch (@typeInfo(T)) {
        .bool => {
            try deserializeBoolean(dest, source, opts);
        },
        .int, .comptime_int => {
            try deserializeInteger(T, dest, source, opts);
        },
        .float, .comptime_float => {
            try deserializeFloat(T, dest, source, opts);
        },
        .pointer => {
            try deserializePointer(T, dest, source, opts);
        },
        .array => {
            try deserializeArray(T, dest, source, opts);
        },
        .@"struct" => {
            try deserializeStruct(T, dest, source, opts);
        },
        .optional => {
            try deserializeOptional(std.meta.Child(T), dest, source, opts);
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
    var dest: T = undefined;

    switch (@typeInfo(T)) {
        .bool => {
            try deserializeBoolean(&dest, source, opts);
        },
        .int => {
            try deserializeInteger(T, &dest, source, opts);
        },
        .float => {
            try deserializeFloat(T, &dest, source, opts);
        },
        .pointer => {
            try deserializePointer(T, &dest, source, opts);
        },
        .array => {
            @compileError("Arrays are only allowed as fields when parsing unallocated! Consider using deserializeAlloc().");
        },
        .@"struct" => {
            try deserializeStruct(T, &dest, source, opts);
        },
        .optional => {
            try deserializeOptional(T, &dest, source, opts);
        },
        else => {
            @compileError("Unimplemented type!");
        },
    }

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
