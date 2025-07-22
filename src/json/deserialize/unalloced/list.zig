const std = @import("std");

const meta = @import("../meta.zig");
const types = @import("../token-types.zig");
const src = @import("../token-source.zig");

const Tokenizer = @import("../tokenizer.zig").Tokenizer;

const DeserializeOpts = @import("../opts.zig").DeserializeOpts;
const DeserializeError = @import("../errors.zig").DeserializeError;

const @"null" = @import("primitives.zig").null;
const list = @This();

const deserializeChildRecheck = @import("deserialize.zig").deserializeChildRecheck;
const deserializeChild = @import("deserialize.zig").deserializeChild;

pub fn consumeSeperator(
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    if (opts.precice_errors) {
        switch (try src.peekNextTokenTypeDiscard(source, opts)) {
            .comma => {},
            .array_end => {
                return DeserializeError.ArrayTooShort;
            },
            else => {
                return DeserializeError.MissingComma;
            },
        }
    } else {
        try src.nextTokenExpect(source, .comma, opts);
    }
}

pub fn deserializeItem(
    comptime T: type,
    dest: *std.meta.Child(T),
    source: *Tokenizer,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    meta.expectArray(T);

    const Child = std.meta.Child(T);

    if (opts.precice_errors) {
        const peek = try src.peekNextTokenType(source, opts);

        switch (peek.token_type) {
            .comma => {
                if (meta.arrayLenght(T) == 1) {
                    return DeserializeError.UnexpectedToken;
                }

                return DeserializeError.MissingArrayItem;
            },
            .array_end => {
                return DeserializeError.ArrayTooShort;
            },
            inline else => |token_type| {
                try deserializeChildRecheck(Child, dest, source, peek.ch, token_type, opts);
            },
        }
    } else {
        try deserializeChild(Child, dest, source, opts);
    }
}

pub const array = struct {
    pub fn consumeTerminator(
        source: *Tokenizer,
        comptime opts: DeserializeOpts,
    ) DeserializeError!void {
        switch (try src.peekNextTokenTypeDiscard(source, opts)) {
            .array_end => {},
            .comma => {
                if (opts.precice_errors) {
                    switch (try src.peekNextTokenTypeDiscard(source, opts)) {
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
                        try src.nextTokenExpect(source, .array_end, opts);
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

    pub fn consumeEmptyInner(
        comptime T: type,
        source: *Tokenizer,
        comptime opts: DeserializeOpts,
    ) DeserializeError!void {
        meta.expectArray(T);

        if (meta.arrayLenght(T) > 0) {
            @compileError("Expected array with lenght 0!");
        }

        if (opts.precice_errors) {
            switch (try src.peekNextTokenTypeDiscard(source, opts)) {
                .array_end => {},
                .comma => {
                    return DeserializeError.UnexpectedToken;
                },
                inline else => |token_type| {
                    if (src.tokenFitsType(std.meta.Child(T), token_type, opts)) {
                        return DeserializeError.ArrayTooLong;
                    }

                    return meta.expectedError(std.meta.Child(T));
                },
            }
        } else {
            try src.nextTokenExpect(source, .array_end, opts);
        }
    }

    pub fn consumeEmptyRecheck(
        comptime T: type,
        source: *Tokenizer,
        comptime id_token_type: types.Primitive,
        comptime opts: DeserializeOpts,
    ) DeserializeError!void {
        meta.expectArray(T);

        if (meta.arrayLenght(T) > 0) {
            @compileError("Expected array with lenght 0!");
        }

        switch (id_token_type) {
            .array_begin => {
                return consumeEmptyInner(T, source, id_token_type, opts);
            },
            else => {
                return DeserializeError.ExpectedArray;
            },
        }
    }

    pub fn consumeEmpty(
        comptime T: type,
        source: *Tokenizer,
        comptime opts: DeserializeOpts,
    ) DeserializeError!void {
        meta.expectArray(T);

        if (meta.arrayLenght(T) > 0) {
            @compileError("Expected array with lenght 0!");
        }

        if (opts.precice_errors) {
            switch (try src.peekNextTokenTypeDiscard(source, opts)) {
                .array_begin => {},
                else => {
                    return DeserializeError.ExpectedArray;
                },
            }
        } else {
            try src.nextTokenExpect(source, .array_begin, opts);
        }

        return consumeEmptyInner(T, source, opts);
    }

    pub fn deserializeEmptyNullableRecheck(
        comptime T: type,
        dest: *?T,
        source: *Tokenizer,
        comptime id_token_type: types.Primitive,
        comptime opts: DeserializeOpts,
    ) DeserializeError!void {
        meta.expectArray(T);

        if (meta.arrayLenght(T) > 0) {
            @compileError("Expected array with lenght 0!");
        }

        switch (id_token_type) {
            .array_begin => {
                return consumeEmptyInner(T, source, opts);
            },
            .null => {
                return @"null".deserializeAssume(T, dest, source);
            },
            else => {
                return DeserializeError.ExpectedOptionalArray;
            },
        }
    }

    pub fn deserializeEmptyNullable(
        comptime T: type,
        dest: *?T,
        source: *Tokenizer,
        comptime opts: DeserializeOpts,
    ) DeserializeError!void {
        meta.expectArray(T);

        if (meta.arrayLenght(T) > 0) {
            @compileError("Expected array with lenght 0!");
        }

        switch (try src.peekNextTokenTypeDiscard(source, opts)) {
            .array_begin => {
                return consumeEmptyInner(T, source, opts);
            },
            .null => {
                return @"null".deserializeAssume(T, dest, source);
            },
            else => {
                return DeserializeError.ExpectedOptionalArray;
            },
        }
    }

    fn deserializeInner(
        comptime T: type,
        dest: *T,
        source: *Tokenizer,
        comptime opts: DeserializeOpts,
    ) DeserializeError!void {
        meta.expectArray(T);

        if (meta.arrayLenght(T) == 0) {
            return consumeEmptyInner(T, source, opts);
        }

        {
            for (0..meta.arrayLenght(T) - 1) |i| {
                try deserializeItem(T, &dest.*[i], source, opts);

                try list.consumeSeperator(source, opts);
            }

            try deserializeItem(T, &dest.*[meta.arrayLenght(T) - 1], source, opts);
        }

        try array.consumeTerminator(source, opts);
    }

    pub fn deserializeRecheck(
        comptime T: type,
        dest: *T,
        source: *Tokenizer,
        comptime id_token_type: types.Primitive,
        comptime opts: DeserializeOpts,
    ) DeserializeError!void {
        meta.expectArray(T);

        if (meta.arrayLenght(T) == 0) {
            return consumeEmpty(T, source, opts);
        }

        switch (id_token_type) {
            .array_begin => {
                return deserializeInner(T, dest, source, opts);
            },
            else => {
                return DeserializeError.ExpectedArray;
            },
        }
    }

    pub fn deserialize(
        comptime T: type,
        dest: *T,
        source: *Tokenizer,
        comptime opts: DeserializeOpts,
    ) DeserializeError!void {
        meta.expectArray(T);

        if (meta.arrayLenght(T) == 0) {
            return array.consumeEmpty(T, source, opts);
        }

        if (opts.precice_errors) {
            switch (try src.peekNextTokenTypeDiscard(source, opts)) {
                .array_begin => {},
                else => {
                    return DeserializeError.ExpectedArray;
                },
            }
        } else {
            try src.nextTokenExpect(source, .array_begin, opts);
        }

        return array.deserializeInner(T, dest, source, opts);
    }

    pub fn deserializeNullableRecheck(
        comptime T: type,
        dest: *?T,
        source: *Tokenizer,
        comptime id_token_type: types.Primitive,
        comptime opts: DeserializeOpts,
    ) DeserializeError!void {
        meta.expectArray(T);

        if (meta.arrayLenght(T) == 0) {
            return array.deserializeEmptyNullableRecheck(T, dest, source, id_token_type, opts);
        }

        switch (id_token_type) {
            .array_begin => {
                return array.deserializeInner(T, &dest.*.?, source, opts);
            },
            .null => {
                return @"null".deserializeAssume(T, dest, source);
            },
            else => {
                return DeserializeError.ExpectedArray;
            },
        }
    }

    pub fn deserializeNullable(
        comptime T: type,
        dest: *?T,
        source: *Tokenizer,
        comptime opts: DeserializeOpts,
    ) DeserializeError!void {
        meta.expectArray(T);

        if (meta.arrayLenght(T) == 0) {
            return array.deserializeEmptyNullable(T, dest, source, opts);
        }

        switch (try src.peekNextTokenTypeDiscard(source, opts)) {
            .array_begin => {
                return array.deserializeInner(T, &dest.*.?, source, opts);
            },
            .null => {
                return @"null".deserializeAssume(T, dest, source);
            },
            else => {
                return DeserializeError.ExpectedArray;
            },
        }
    }
};
