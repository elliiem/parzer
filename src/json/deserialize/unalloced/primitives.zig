const std = @import("std");

const meta = @import("../meta.zig");
const types = @import("../token-types.zig");
const src = @import("../token-source.zig");

const Tokenizer = @import("../tokenizer.zig").Tokenizer;

const DeserializeOpts = @import("../opts.zig").DeserializeOpts;
const DeserializeError = @import("../errors.zig").DeserializeError;

pub const @"null" = struct {
    pub fn deserializeAssume(
        comptime T: type,
        dest: *?T,
        source: *Tokenizer,
    ) DeserializeError!void {
        defer dest.* = null;
        return source.consumeNullAssume();
    }
};

pub const @"bool" = struct {
    pub fn deserializeTrueAssume(
        dest: *bool,
        source: *Tokenizer,
    ) DeserializeError!void {
        defer dest.* = true;
        return source.consumeTrueAssume();
    }

    pub fn deserializeFalseAssume(
        dest: *bool,
        source: *Tokenizer,
    ) DeserializeError!void {
        defer dest.* = false;
        return source.consumeFalseAssume();
    }

    pub fn deserializeRecheck(
        dest: *bool,
        source: *Tokenizer,
        comptime token_type: types.Primitive,
    ) DeserializeError!void {
        switch (token_type) {
            .true => {
                return @"bool".deserializeTrueAssume(dest, source);
            },
            .false => {
                return @"bool".deserializeFalseAssume(dest, source);
            },
            else => {
                return DeserializeError.ExpectedBool;
            },
        }
    }

    pub fn deserialize(
        dest: *bool,
        source: *Tokenizer,
        comptime opts: DeserializeOpts,
    ) DeserializeError!void {
        if (opts.precice_errors) {
            switch (try src.peekNextTokenTypeDiscard(source, opts)) {
                .true => {
                    return @"bool".deserializeTrueAssume(dest, source);
                },
                .false => {
                    return @"bool".deserializeFalseAssume(dest, source);
                },
                else => {
                    return DeserializeError.ExpectedBool;
                },
            }
        } else {
            dest.* = try src.nextTokenExpect(source, .bool, opts);
        }
    }

    pub fn deserializeNullableRecheck(
        dest: *?bool,
        source: *Tokenizer,
        comptime token_type: types.Primitive,
    ) DeserializeError!void {
        switch (token_type) {
            .true => {
                return @"bool".deserializeTrueAssume(&dest.*.?, source);
            },
            .false => {
                return @"bool".deserializeFalseAssume(&dest.*.?, source);
            },
            .null => {
                return @"null".deserializeAssume(bool, dest, source);
            },
            else => {
                return DeserializeError.ExpectedOptionalBool;
            },
        }
    }

    pub fn deserializeNullable(
        dest: *?bool,
        source: *Tokenizer,
        comptime opts: DeserializeOpts,
    ) DeserializeError!void {
        switch (try src.peekNextTokenTypeDiscard(source, opts)) {
            .true => {
                return @"bool".deserializeTrueAssume(&dest.*.?, source);
            },
            .false => {
                return @"bool".deserializeFalseAssume(&dest.*.?, source);
            },
            .null => {
                return @"null".deserializeAssume(bool, dest, source);
            },
            else => {
                return DeserializeError.ExpectedOptionalBool;
            },
        }
    }
};

pub const int = struct {
    pub fn deserializePeeked(
        comptime T: type,
        dest: *T,
        source: *Tokenizer,
        peek: u8,
    ) DeserializeError!void {
        meta.expectInt(T);

        const number = try source.takeTokenExpectPeeked(peek, .number);

        dest.* = std.fmt.parseInt(T, number, 10) catch return Tokenizer.ParseError.InvalidNumber;
    }

    pub fn deserializeRecheck(
        comptime T: type,
        dest: *T,
        source: *Tokenizer,
        peeked: u8,
        comptime token_type: types.Primitive,
    ) DeserializeError!void {
        meta.expectInt(T);

        switch (token_type) {
            .number => {
                return int.deserializePeeked(T, dest, source, peeked);
            },
            else => {
                return DeserializeError.ExpectedNumber;
            },
        }
    }

    pub fn deserialize(
        comptime T: type,
        dest: *T,
        source: *Tokenizer,
        comptime opts: DeserializeOpts,
    ) DeserializeError!void {
        meta.expectInt(T);

        if (opts.precice_errors) {
            const peek = try src.peekNextTokenType(source, opts);

            switch (peek.token_type) {
                .number => {
                    return int.deserializePeeked(T, dest, source, peek.ch);
                },
                else => {
                    return DeserializeError.ExpectedNumber;
                },
            }
        } else {
            return int.deserializePeeked(
                T,
                dest,
                source,
                src.peekNext(source, opts) orelse return DeserializeError.ExpectedToken,
            );
        }
    }

    pub fn deserializeNullableRecheck(
        comptime T: type,
        dest: *?T,
        source: *Tokenizer,
        peek: u8,
        comptime token_type: types.Primitive,
    ) DeserializeError!void {
        meta.expectInt(T);

        switch (token_type) {
            .number => {
                return int.deserializePeeked(T, &dest.*.?, source, peek);
            },
            .null => {
                return @"null".deserializeAssume(T, dest, source);
            },
            else => {
                return DeserializeError.ExpectedOptionalNumber;
            },
        }
    }

    pub fn deserializeNullable(
        comptime T: type,
        dest: *?T,
        source: *Tokenizer,
        comptime opts: DeserializeOpts,
    ) DeserializeError!void {
        meta.expectInt(T);

        const peek = try src.peekNextTokenType(source, opts);

        switch (peek.token_type) {
            .number => {
                return int.deserializePeeked(T, &dest.*.?, source, peek.ch);
            },
            .null => {
                return @"null".deserializeAssume(T, dest, source);
            },
            else => {
                return DeserializeError.ExpectedOptionalNumber;
            },
        }
    }
};

pub const float = struct {
    pub fn deserializePeeked(
        comptime T: type,
        dest: *T,
        source: *Tokenizer,
        peek: u8,
    ) DeserializeError!void {
        meta.expectFloat(T);

        const number = try source.takeTokenExpectPeeked(peek, .number);

        dest.* = std.fmt.parseFloat(T, number) catch return Tokenizer.ParseError.InvalidNumber;
    }

    pub fn deserializeRecheck(
        comptime T: type,
        dest: *T,
        source: *Tokenizer,
        peek: u8,
        comptime token_type: types.Primitive,
    ) DeserializeError!void {
        meta.expectFloat(T);

        switch (token_type) {
            .number => {
                return float.deserializePeeked(T, dest, source, peek);
            },
            else => {
                return DeserializeError.ExpectedNumber;
            },
        }
    }

    pub fn deserialize(
        comptime T: type,
        dest: *T,
        source: *Tokenizer,
        comptime opts: DeserializeOpts,
    ) DeserializeError!void {
        meta.expectFloat(T);

        if (opts.precice_errors) {
            const peek = try src.peekNextTokenType(source, opts);

            switch (peek.token_type) {
                .number => {
                    return float.deserializePeeked(T, dest, source, peek.ch);
                },
                else => {
                    return DeserializeError.ExpectedNumber;
                },
            }
        } else {
            return float.deserializePeeked(
                T,
                dest,
                source,
                src.peekNext(source, opts) orelse return DeserializeError.ExpectedToken,
            );
        }
    }

    pub fn deserializeNullableRecheck(
        comptime T: type,
        dest: *?T,
        source: *Tokenizer,
        peek: u8,
        comptime token_type: types.Primitive,
    ) DeserializeError!void {
        meta.expectFloat(T);

        switch (token_type) {
            .number => {
                return float.deserializePeeked(T, &dest.*.?, source, peek.ch);
            },
            .null => {
                return @"null".deserializeAssume(T, dest, source);
            },
            else => {
                return DeserializeError.ExpectedOptionalNumber;
            },
        }
    }

    pub fn deserializeNullable(
        comptime T: type,
        dest: *?T,
        source: *Tokenizer,
        comptime opts: DeserializeOpts,
    ) DeserializeError!void {
        meta.expectFloat(T);

        const peek = try src.peekNextTokenType(source, opts);

        switch (peek.token_type) {
            .number => {
                return float.deserializePeeked(T, &dest.*.?, source, peek.ch);
            },
            .null => {
                return @"null".deserializeAssume(T, dest, source);
            },
            else => {
                return DeserializeError.ExpectedOptionalNumber;
            },
        }
    }
};

pub const string = struct {
    pub fn deserializeInner(
        dest: *[]const u8,
        source: *Tokenizer,
    ) DeserializeError!void {
        dest.* = try source.takeStringInner();
    }

    pub fn deserializeRecheck(
        dest: *[]const u8,
        source: *Tokenizer,
        comptime token_type: types.Primitive,
    ) DeserializeError!void {
        switch (token_type) {
            .string => {
                return string.deserializeInner(dest, source);
            },
            else => {
                return DeserializeError.ExpectedString;
            },
        }
    }

    pub fn deserializeString(
        dest: *[]const u8,
        source: *Tokenizer,
        comptime opts: DeserializeOpts,
    ) DeserializeError!void {
        if (opts.precice_errors) {
            switch (try src.peekNextTokenTypeDiscard(source, opts)) {
                .string => {
                    return string.deserializeInner(dest, source);
                },
                else => {
                    return DeserializeError.ExpectedString;
                },
            }
        } else {
            dest.* = src.nextTokenExpect(source, .string, opts);
        }
    }

    pub fn deserializeNullableRecheck(
        dest: *?[]const u8,
        source: *Tokenizer,
        comptime token_type: types.Primitive,
    ) DeserializeError!void {
        switch (token_type) {
            .string => {
                return string.deserializeInner(&dest.*.?, source);
            },
            .null => {
                return @"null".deserializeAssume([]const u8, dest, source);
            },
            else => {
                return DeserializeError.ExpectedOptionalString;
            },
        }
    }

    pub fn deserializeOptional(
        dest: *?[]const u8,
        source: *Tokenizer,
        comptime opts: DeserializeOpts,
    ) DeserializeError!void {
        switch (try src.peekNextTokenTypeDiscard(source, opts)) {
            .string => {
                return string.deserializeInner(&dest.*.?, source);
            },
            .null => {
                return @"null".deserializeAssume([]const u8, dest, source);
            },
            else => {
                return DeserializeError.ExpectedOptionalString;
            },
        }
    }
};
