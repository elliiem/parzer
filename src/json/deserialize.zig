const std = @import("std");
const builtin = @import("builtin");

const Tokenizer = @import("tokenizer.zig");

const TokenType = Tokenizer.TokenType;
const TokenTypePrimitive = Tokenizer.TokenTypePrimitive;

const common = @import("deserialize-common.zig");

const DeserializeError = common.DeserializeError;
const DeserializeOpts = common.DeserializeOpts;

fn deserializeNullAssume(
    comptime T: type,
    dest: *?T,
    source: *Tokenizer,
) DeserializeError!void {
    defer dest.* = null;
    return source.consumeNullAssume();
}

// --------------------------------------------------
// deserializeBoolean

fn deserializeTrueAssume(
    dest: *bool,
    source: *Tokenizer,
) DeserializeError!void {
    defer dest.* = true;
    return source.consumeTrueAssume();
}

fn deserializeFalseAssume(
    dest: *bool,
    source: *Tokenizer,
) DeserializeError!void {
    defer dest.* = false;
    return source.consumeFalseAssume();
}

fn deserializeBooleanInferred(
    dest: *bool,
    source: *Tokenizer,
    comptime token_type: TokenTypePrimitive,
) DeserializeError!void {
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
    dest: *bool,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
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
        dest.* = try common.nextTokenExpect(source, .bool, opts);
    }
}

fn deserializeOptionalBooleanInferred(
    dest: *?bool,
    source: *Tokenizer,
    comptime token_type: TokenTypePrimitive,
) DeserializeError!void {
    switch (token_type) {
        .true => {
            return deserializeTrueAssume(&dest.*.?, source);
        },
        .false => {
            return deserializeFalseAssume(&dest.*.?, source);
        },
        .null => {
            return deserializeNullAssume(bool, dest, source);
        },
        else => {
            return DeserializeError.ExpectedOptionalBool;
        },
    }
}

fn deserializeOptionalBoolean(
    dest: *?bool,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    switch (try common.peekNextTokenTypeDiscard(source, opts)) {
        .true => {
            return deserializeTrueAssume(&dest.*.?, source);
        },
        .false => {
            return deserializeFalseAssume(&dest.*.?, source);
        },
        .null => {
            return deserializeNullAssume(bool, dest, source);
        },
        else => {
            return DeserializeError.ExpectedOptionalBool;
        },
    }
}

// deserializeBoolean
// --------------------------------------------------

// --------------------------------------------------
// deserializeInteger

fn deserializeIntegerAssume(
    comptime T: type,
    dest: *T,
    source: *Tokenizer,
    peek: u8,
) DeserializeError!void {
    common.expectInt(T);

    const number = try source.takeTokenExpectPeek(peek, .number);

    dest.* = std.fmt.parseInt(T, number, 10) catch return Tokenizer.ParseError.InvalidNumber;
}

fn deserializeIntegerInferred(
    comptime T: type,
    dest: *T,
    source: *Tokenizer,
    peeked: u8,
    comptime token_type: TokenTypePrimitive,
) DeserializeError!void {
    common.expectInt(T);

    switch (token_type) {
        .number => {
            return deserializeIntegerAssume(T, dest, source, peeked);
        },
        else => {
            return DeserializeError.ExpectedNumber;
        },
    }
}

fn deserializeInteger(
    comptime T: type,
    dest: *T,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.expectInt(T);

    if (opts.precice_errors) {
        const peek = try common.peekNextTokenType(source, opts);

        switch (peek.token_type) {
            .number => {
                return deserializeIntegerAssume(T, dest, source, peek.ch);
            },
            else => {
                return DeserializeError.ExpectedNumber;
            },
        }
    } else {
        return deserializeIntegerAssume(
            T,
            dest,
            source,
            common.peekNext(source, opts) orelse return DeserializeError.ExpectedToken,
        );
    }
}

fn deserializeOptionalIntegerInferred(
    comptime T: type,
    dest: *?T,
    source: *Tokenizer,
    peek: u8,
    comptime token_type: TokenTypePrimitive,
) DeserializeError!void {
    common.expectInt(T);

    switch (token_type) {
        .number => {
            return deserializeIntegerAssume(T, &dest.*.?, source, peek);
        },
        .null => {
            return deserializeNullAssume(T, dest, source);
        },
        else => {
            return DeserializeError.ExpectedOptionalNumber;
        },
    }
}

fn deserializeOptionalInteger(
    comptime T: type,
    dest: *?T,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.expectInt(T);

    const peek = try common.peekNextTokenType(source, opts);

    switch (peek.token_type) {
        .number => {
            return deserializeIntegerAssume(T, &dest.*.?, source, peek.ch);
        },
        .null => {
            return deserializeNullAssume(T, dest, source);
        },
        else => {
            return DeserializeError.ExpectedOptionalNumber;
        },
    }
}

// deserializeInteger
// --------------------------------------------------

// --------------------------------------------------
// deserializeFloat

fn deserializeFloatAssume(
    comptime T: type,
    dest: *T,
    source: *Tokenizer,
    peek: u8,
) DeserializeError!void {
    common.expectFloat(T);

    const number = try source.takeTokenExpectPeek(peek, .number);

    dest.* = std.fmt.parseFloat(T, number) catch return Tokenizer.ParseError.InvalidNumber;
}

fn deserializeFloatInferred(
    comptime T: type,
    dest: *T,
    source: *Tokenizer,
    peek: u8,
    comptime token_type: TokenTypePrimitive,
) DeserializeError!void {
    common.expectFloat(T);

    switch (token_type) {
        .number => {
            return deserializeFloatAssume(T, dest, source, peek);
        },
        else => {
            return DeserializeError.ExpectedNumber;
        },
    }
}

fn deserializeFloat(
    comptime T: type,
    dest: *T,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.expectFloat(T);

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

fn deserializeOptionalFloatInferred(
    comptime T: type,
    dest: *?T,
    source: *Tokenizer,
    peek: u8,
    comptime token_type: TokenTypePrimitive,
) DeserializeError!void {
    common.expectFloat(T);

    switch (token_type) {
        .number => {
            return deserializeFloatAssume(T, &dest.*.?, source, peek.ch);
        },
        .null => {
            return deserializeNullAssume(T, dest, source);
        },
        else => {
            return DeserializeError.ExpectedOptionalNumber;
        },
    }
}

fn deserializeOptionalFloat(
    comptime T: type,
    dest: *?T,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.expectFloat(T);

    const peek = try common.peekNextTokenType(source, opts);

    switch (peek.token_type) {
        .number => {
            return deserializeFloatAssume(T, &dest.*.?, source, peek.ch);
        },
        .null => {
            return deserializeNullAssume(T, dest, source);
        },
        else => {
            return DeserializeError.ExpectedOptionalNumber;
        },
    }
}

// deserializeFloat
// --------------------------------------------------

// --------------------------------------------------
// deserializeString

fn deserializeStringAssume(
    dest: *[]const u8,
    source: *Tokenizer,
) DeserializeError!void {
    dest.* = try source.takeStringAssume();
}

fn deserializeStringInferred(
    dest: *[]const u8,
    source: *Tokenizer,
    comptime token_type: TokenTypePrimitive,
) DeserializeError!void {
    switch (token_type) {
        .string => {
            return deserializeStringAssume(dest, source);
        },
        else => {
            return DeserializeError.ExpectedString;
        },
    }
}

fn deserializeString(
    dest: *[]const u8,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    if (opts.precice_errors) {
        switch (try common.peekNextTokenTypeDiscard(source, opts)) {
            .string => {
                return deserializeStringAssume(dest, source);
            },
            else => {
                return DeserializeError.ExpectedString;
            },
        }
    } else {
        dest.* = common.nextTokenExpect(source, .string, opts);
    }
}

fn deserializeOptionalStringInferred(
    dest: *?[]const u8,
    source: *Tokenizer,
    comptime token_type: TokenTypePrimitive,
) DeserializeError!void {
    switch (token_type) {
        .string => {
            return deserializeStringAssume(&dest.*.?, source);
        },
        .null => {
            return deserializeNullAssume([]const u8, dest, source);
        },
        else => {
            return DeserializeError.ExpectedOptionalString;
        },
    }
}

fn deserializeOptionalString(
    dest: *?[]const u8,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    switch (try common.peekNextTokenTypeDiscard(source, opts)) {
        .string => {
            return deserializeStringAssume(&dest.*.?, source);
        },
        .null => {
            return deserializeNullAssume([]const u8, dest, source);
        },
        else => {
            return DeserializeError.ExpectedOptionalString;
        },
    }
}

// deserializeString
// --------------------------------------------------

// --------------------------------------------------
// deserializePointer

fn deserializePointerInferred(
    comptime T: type,
    dest: *T,
    source: *Tokenizer,
    comptime id_token_type: TokenTypePrimitive,
) DeserializeError!void {
    common.expectPointer(T);

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

fn deserializePointer(
    comptime T: type,
    dest: *T,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.expectPointer(T);

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

fn deserializeOptionalPointerInferred(
    comptime T: type,
    dest: *?T,
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

fn deserializeOptionalPointer(
    comptime T: type,
    dest: *?T,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.expectPointer(T);

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

fn deserializeArrayItem(
    comptime T: type,
    dest: *std.meta.Child(T),
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.expectArray(T);

    const Child = std.meta.Child(T);

    if (opts.precice_errors) {
        const peek = try common.peekNextTokenType(source, opts);

        switch (peek.token_type) {
            .comma => {
                if (common.arrayLenght(T) == 1) {
                    return DeserializeError.UnexpectedToken;
                }

                return DeserializeError.MissingArrayItem;
            },
            .array_end => {
                return DeserializeError.ArrayTooShort;
            },
            inline else => |token_type| {
                try deserializeInnerInferred(Child, dest, source, peek.ch, token_type, opts);
            },
        }
    } else {
        try deserializeInner(Child, dest, source, opts);
    }
}

fn consumeArrayTerminator(
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

fn consumeArraySeperator(
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

fn consumeEmptyArrayAssume(
    comptime T: type,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.expectArray(T);

    if (common.arrayLenght(T) > 0) {
        @compileError("Expected array with lenght 0!");
    }

    if (opts.precice_errors) {
        switch (try common.peekNextTokenTypeDiscard(source, opts)) {
            .array_end => {},
            .comma => {
                return DeserializeError.UnexpectedToken;
            },
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

fn consumeEmptyArrayInferred(
    comptime T: type,
    source: *Tokenizer,
    comptime id_token_type: TokenTypePrimitive,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.expectArray(T);

    if (common.arrayLenght(T) > 0) {
        @compileError("Expected array with lenght 0!");
    }

    switch (id_token_type) {
        .array_begin => {
            return consumeEmptyArrayAssume(T, source, id_token_type, opts);
        },
        else => {
            return DeserializeError.ExpectedArray;
        },
    }
}

fn consumeEmptyArray(
    comptime T: type,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.expectArray(T);

    if (common.arrayLenght(T) > 0) {
        @compileError("Expected array with lenght 0!");
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

    return consumeEmptyArrayAssume(T, source, opts);
}

fn deserializeOptionalEmptyArrayInferred(
    comptime T: type,
    dest: *?T,
    source: *Tokenizer,
    comptime id_token_type: TokenTypePrimitive,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.expectArray(T);

    if (common.arrayLenght(T) > 0) {
        @compileError("Expected array with lenght 0!");
    }

    switch (id_token_type) {
        .array_begin => {
            return consumeEmptyArrayAssume(T, source, opts);
        },
        .null => {
            return deserializeNullAssume(T, dest, source);
        },
        else => {
            return DeserializeError.ExpectedOptionalArray;
        },
    }
}

fn deserializeOptionalEmptyArray(
    comptime T: type,
    dest: *?T,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.expectArray(T);

    if (common.arrayLenght(T) > 0) {
        @compileError("Expected array with lenght 0!");
    }

    switch (try common.peekNextTokenTypeDiscard(source, opts)) {
        .array_begin => {
            return consumeEmptyArrayAssume(T, source, opts);
        },
        .null => {
            return deserializeNullAssume(T, dest, source);
        },
        else => {
            return DeserializeError.ExpectedOptionalArray;
        },
    }
}

fn deserializeArrayAssume(
    comptime T: type,
    dest: *T,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.expectArray(T);

    if (common.arrayLenght(T) == 0) {
        return consumeEmptyArrayAssume(T, source, opts);
    }

    {
        for (0..common.arrayLenght(T) - 1) |i| {
            try deserializeArrayItem(T, &dest.*[i], source, opts);

            try consumeArraySeperator(source, opts);
        }

        try deserializeArrayItem(T, &dest.*[common.arrayLenght(T) - 1], source, opts);
    }

    try consumeArrayTerminator(source, opts);
}

fn deserializeArrayInferred(
    comptime T: type,
    dest: *T,
    source: *Tokenizer,
    comptime id_token_type: TokenTypePrimitive,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.expectArray(T);

    if (common.arrayLenght(T) == 0) {
        return consumeEmptyArray(T, source, opts);
    }

    switch (id_token_type) {
        .array_begin => {
            return deserializeArrayAssume(T, dest, source, opts);
        },
        else => {
            return DeserializeError.ExpectedArray;
        },
    }
}

fn deserializeArray(
    comptime T: type,
    dest: *T,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.expectArray(T);

    if (common.arrayLenght(T) == 0) {
        return consumeEmptyArray(T, source, opts);
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

    return deserializeArrayAssume(T, dest, source, opts);
}

fn deserializeOptionalArrayInferred(
    comptime T: type,
    dest: *?T,
    source: *Tokenizer,
    comptime id_token_type: TokenTypePrimitive,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.expectArray(T);

    if (common.arrayLenght(T) == 0) {
        return deserializeOptionalEmptyArrayInferred(T, dest, source, id_token_type, opts);
    }

    switch (id_token_type) {
        .array_begin => {
            return deserializeArrayAssume(T, &dest.*.?, source, opts);
        },
        .null => {
            return deserializeNullAssume(T, dest, source);
        },
        else => {
            return DeserializeError.ExpectedArray;
        },
    }
}

fn deserializeOptionalArray(
    comptime T: type,
    dest: *?T,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.expectArray(T);

    if (common.arrayLenght(T) == 0) {
        return deserializeOptionalEmptyArray(T, dest, source, opts);
    }

    switch (try common.peekNextTokenTypeDiscard(source, opts)) {
        .array_begin => {
            return deserializeArrayAssume(T, &dest.*.?, source, opts);
        },
        .null => {
            return deserializeNullAssume(T, dest, source);
        },
        else => {
            return DeserializeError.ExpectedArray;
        },
    }
}

// deserializeArray
// --------------------------------------------------

// --------------------------------------------------
// deserializeStruct

fn firstStructFieldName(
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

fn takeStructFieldName(
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

fn nextStructFieldName(
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!?[]const u8 {
    switch (try common.peekNextTokenTypeDiscard(source, opts)) {
        .comma => {
            return takeStructFieldName(source, opts);
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

fn deserializeStructFieldValue(
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

fn visitFields(
    comptime T: type,
    dest: *T,
    seen: []bool,
) DeserializeError!void {
    common.expectStruct(T);

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

fn deserializeStructAssume(
    comptime T: type,
    dest: *T,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.expectStruct(T);

    const info = @typeInfo(T).@"struct";

    var seen = [1]bool{false} ** info.fields.len;

    { // Parse first field
        const field_name = try firstStructFieldName(source, opts) orelse return visitFields(T, dest, &seen);

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
        while (try nextStructFieldName(source, opts)) |field_name| {
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

    return visitFields(T, dest, &seen);
}

fn deserializeStructInferred(
    comptime T: type,
    dest: *T,
    source: *Tokenizer,
    comptime id_token_type: TokenTypePrimitive,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.expectStruct(T);

    switch (id_token_type) {
        .object_begin => {
            return deserializeStructAssume(T, dest, source, opts);
        },
        else => {
            return DeserializeError.ExpectedObject;
        },
    }
}

fn deserializeStruct(
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

    return deserializeStructAssume(T, dest, source, opts);
}

fn deserializeOptionalStructInferred(
    comptime T: type,
    dest: *?T,
    source: *Tokenizer,
    comptime id_token_type: TokenTypePrimitive,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.expectStruct(T);

    switch (id_token_type) {
        .object_begin => {
            return deserializeStructAssume(T, &dest.*.?, source, opts);
        },
        .null => {
            return deserializeNullAssume(T, dest, source);
        },
        else => {
            return DeserializeError.ExpectedOptionalObject;
        },
    }
}

fn deserializeOptionalStruct(
    comptime T: type,
    dest: *?T,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.expectStruct(T);

    switch (try common.peekNextTokenTypeDiscard(source, opts)) {
        .object_begin => {
            return deserializeStructAssume(T, &dest.*.?, source, opts);
        },
        .null => {
            return deserializeNullAssume(T, dest, source);
        },
        else => {
            return DeserializeError.ExpectedOptionalObject;
        },
    }
}

// deserializeStruct
// --------------------------------------------------

fn deserializeOptional(
    comptime T: type,
    dest: *T,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.expectOptional(T);

    const Child = std.meta.Child(T);

    switch (@typeInfo(Child)) {
        .bool => {
            try deserializeOptionalBoolean(dest, source, opts);
        },
        .int, .comptime_int => {
            try deserializeOptionalInteger(Child, dest, source, opts);
        },
        .float, .comptime_float => {
            try deserializeOptionalFloat(Child, dest, source, opts);
        },
        .pointer => {
            try deserializeOptionalPointer(Child, dest, source, opts);
        },
        .array => {
            try deserializeOptionalArray(Child, dest, source, opts);
        },
        .@"struct" => {
            try deserializeOptionalStruct(Child, dest, source, opts);
        },
        .optional => {
            try deserializeOptional(std.meta.Child(T), &dest.*.?, source, opts);
        },
        else => {
            @compileError("Unimplemented type!");
        },
    }
}

fn deserializeOptionalInferred(
    comptime T: type,
    dest: *T,
    source: *Tokenizer,
    peek: u8,
    comptime token_type: TokenTypePrimitive,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    common.expectOptional(T);

    const Child = std.meta.Child(T);

    switch (@typeInfo(Child)) {
        .bool => {
            try deserializeOptionalBooleanInferred(Child, dest, source, token_type);
        },
        .int, .comptime_int => {
            try deserializeOptionalIntegerInferred(Child, dest, source, peek, token_type);
        },
        .float, .comptime_float => {
            try deserializeOptionalFloatInferred(Child, dest, source, peek, token_type);
        },
        .pointer => {
            try deserializeOptionalPointerInferred(Child, dest, source, token_type);
        },
        .array => {
            try deserializeOptionalArrayInferred(Child, dest, source, token_type, opts);
        },
        .@"struct" => {
            try deserializeOptionalStructInferred(Child, dest, source, token_type, opts);
        },
        .optional => {
            try deserializeOptionalInferred(Child, &dest.*.?, source, peek, token_type, opts);
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
            try deserializeBooleanInferred(dest, source, token_type);
        },
        .int, .comptime_int => {
            try deserializeIntegerInferred(T, dest, source, peeked, token_type);
        },
        .float, .comptime_float => {
            try deserializeFloatInferred(T, dest, source, opts, token_type);
        },
        .pointer => {
            try deserializePointerInferred(T, dest, source, opts);
        },
        .array => {
            try deserializeArrayInferred(T, dest, source, token_type, opts);
        },
        .@"struct" => {
            try deserializeStructInferred(T, dest, source, token_type, opts);
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
    dest: *T,
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    switch (@typeInfo(T)) {
        .bool => {
            try deserializeBoolean(T, dest, source, opts);
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
    var dest = common.createUndefined(T);

    try deserializeInner(T, &dest, source, opts);

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
