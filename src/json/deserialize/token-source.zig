/// high level abstraction above the basic tokenizer
/// This makes it possible for users to create their own tokenizer without having to care about that while deserializing
/// NOTE: This is currently not implemented, prioritizing other stuff
const Tokenizer = @import("tokenizer.zig");

const DeserializeOpts = @import("opts.zig").DeserializeOpts;
const DeserializeError = @import("errors.zig").DeserializeError;

const types = @import("token-types.zig");

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
                source.skipTrueAssume();
            },
            .false => {
                source.skipFalseAssume();
            },
            .number => {
                source.skipNumberAssume();
            },
            .string => {
                source.skipStringInner();
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
                source.skipTrueAssume();
            },
            .false => {
                source.skipFalseAssume();
            },
            .number => {
                source.skipNumberAssume();
            },
            .string => {
                source.skipStringInner();
            },
            else => {
                continue;
            },
        }
    }
}

pub fn skipNextObject(
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
            source.skipTrueAssume();
        },
        .false => {
            source.skipFalseAssume();
        },
        .number => {
            source.skipNumberAssume();
        },
        .string => {
            source.skipStringInner();
        },
        else => {},
    }
}

pub const ObjectType = enum {
    number,
    string,
    bool,
    object,
    array,
};

pub fn skipNextObjectExpect(
    source: *Tokenizer,
    comptime expected: ObjectType,
    comptime opts: DeserializeOpts,
) DeserializeError!void {
    switch (expected) {
        .array => {
            switch (try peekNextTokenTypeDiscard(source, opts)) {
                .array_begin => {
                    try skipArrayInner(source, opts);
                },
                else => {
                    return DeserializeError.ExpectedArray;
                },
            }
        },
        .object => {
            switch (try peekNextTokenTypeDiscard(source, opts)) {
                .array_begin => {
                    try skipArrayInner(source, opts);
                },
                else => {
                    return DeserializeError.ExpectedArray;
                },
            }
        },
        inline else => |expected_inner| {
            if (opts.whitespace) {
                switch (expected_inner) {
                    .number => {
                        try source.skipNextTokenExpect(.number);
                    },
                    .string => {
                        try source.skipNextTokenExpect(.string);
                    },
                    .bool => {
                        try source.skipNextTokenExpect(.bool);
                    },
                }
            } else {
                switch (expected_inner) {
                    .number => {
                        try source.skipTokenExpect(.number);
                    },
                    .string => {
                        try source.skipTokenExpect(.string);
                    },
                    .bool => {
                        try source.skipTokenExpect(.bool);
                    },
                }
            }
        },
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
) DeserializeError!types.Token {
    if (opts.whitespace) {
        source.nextToken();
    } else {
        source.takeToken();
    }
}

pub fn nextTokenExpect(
    source: *Tokenizer,
    comptime expected: types.Default,
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
    comptime expected: types.Optional,
    comptime opts: DeserializeOpts,
) DeserializeError!Tokenizer.tokenValueTypeNullable(expected) {
    if (opts.whitespace) {
        return source.nextTokenExpectNullable(expected);
    } else {
        return source.takeTokenExpectNullable(expected);
    }
}

pub const Inferred = struct {
    token_type: types.Primitive,
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
) DeserializeError!types.Primitive {
    const peek = try peekNext(source, opts) orelse return DeserializeError.ExpectedToken;

    return Tokenizer.inferrTokenType(peek) orelse return DeserializeError.InvalidToken;
}
