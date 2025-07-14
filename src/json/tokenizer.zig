const std = @import("std");

const keywords = @import("keywords.zig");

const Tokenizer = @This();

source: []const u8,
i: usize = 0,

pub const ParseError = error{
    InvalidNumber,
    InvalidString,
    InvalidField,
    UnexpectedToken,
    InvalidToken,
    ExpectedToken,
};

// --------------------------------------------------
// Helpers

inline fn remaining(
    self: Tokenizer,
) usize {
    return self.source.len - self.i;
}

pub inline fn isSourceEmpty(
    self: Tokenizer,
) bool {
    return self.i >= self.source.len;
}

inline fn assertFilledSource(
    self: Tokenizer,
) void {
    std.debug.assert(self.i < self.source.len);
}

inline fn assertRemaining(
    self: Tokenizer,
    n: usize,
) void {
    std.debug.assert(self.remaining() >= n);
}

/// NOTE: Doesnt care if 'self.i' exeeds the lenght of 'self.source'
pub inline fn consumeChar(
    self: *Tokenizer,
) void {
    self.i += 1;
}

/// NOTE: Doesnt care if 'self.i' exeeds the lenght of 'self.source'
pub inline fn consumeChars(
    self: *Tokenizer,
    n: usize,
) void {
    self.i += n;
}

inline fn takeCharAssume(
    self: *Tokenizer,
) u8 {
    self.assertFilledSource();

    defer self.i += 1;

    return self.source[self.i];
}

pub inline fn takeChar(
    self: *Tokenizer,
) ?u8 {
    if (self.isSourceEmpty()) {
        return null;
    }

    return self.takeCharAssume();
}

inline fn peekCharAssume(
    self: Tokenizer,
) u8 {
    self.assertFilledSource();

    return self.source[self.i];
}

pub inline fn peekChar(
    self: *Tokenizer,
) ?u8 {
    if (self.isSourceEmpty()) {
        return null;
    }

    return self.peekCharAssume();
}

inline fn consumeLiteral(
    self: *Tokenizer,
    literal: []const u8,
) ParseError!void {
    const equals = (self.remaining() >= literal.len) and (std.mem.eql(u8, self.source[self.i .. self.i + literal.len], literal));

    if (!equals) {
        return ParseError.InvalidToken;
    }

    self.consumeChars(literal.len);
}

inline fn consumeComment(
    self: *Tokenizer,
) void {
    while (!self.isSourceEmpty()) {
        if (self.takeCharAssume() == '\n') {
            return;
        }
    }
}

pub fn consumeWhitespace(
    self: *Tokenizer,
) ParseError!?u8 {
    while (!self.isSourceEmpty()) {
        switch (self.takeCharAssume()) {
            '\t', '\n', '\r', ' ' => {},
            '/' => {
                if (self.isSourceEmpty() or self.takeCharAssume() != '/') {
                    return ParseError.InvalidToken;
                }

                self.consumeComment();
            },
            else => |ch| {
                return ch;
            },
        }
    }

    return null;
}

// Helpers
// --------------------------------------------------

// --------------------------------------------------
// Primitives

pub inline fn consumeNullAssume(
    self: *Tokenizer,
) ParseError!void {
    try self.consumeLiteral(keywords.NULL[1..]);
}

pub inline fn consumeTrueAssume(
    self: *Tokenizer,
) ParseError!void {
    try self.consumeLiteral(keywords.TRUE[1..]);
}

pub inline fn consumeFalseAssume(
    self: *Tokenizer,
) ParseError!void {
    try self.consumeLiteral(keywords.FALSE[1..]);
}

pub fn takeBool(
    self: *Tokenizer,
    ch: u8,
) ParseError!bool {
    switch (ch) {
        keywords.TRUE[0] => {
            self.consumeTrueAssume();
            return true;
        },
        keywords.FALSE[0] => {
            self.consumeFalseAssume();
            return false;
        },
        else => {},
    }
}

pub fn takeNullableBool(
    self: *Tokenizer,
    ch: u8,
) ParseError!?bool {
    switch (ch) {
        keywords.TRUE[0] => {
            self.consumeTrueAssume();
            return true;
        },
        keywords.FALSE[0] => {
            self.consumeFalseAssume();
            return false;
        },
        keywords.NULL[0] => {
            self.consumeNullAssume();
            return null;
        },
        else => {},
    }
}

fn consumeDecimal(
    self: *Tokenizer,
) ParseError!void {
    self.assertFilledSource();

    {
        switch (self.peekCharAssume()) {
            '0'...'9' => {},
            else => {
                return ParseError.InvalidNumber;
            },
        }

        self.consumeChar();
    }

    {
        while (!self.isSourceEmpty()) {
            switch (self.peekCharAssume()) {
                '0'...'9' => {},
                else => {
                    return;
                },
            }

            self.consumeChar();
        }
    }
}

fn consumeDecimalChecked(
    self: *Tokenizer,
) ParseError!void {
    if (self.isSourceEmpty()) {
        return ParseError.InvalidNumber;
    }

    return self.consumeDecimal();
}

fn consumeExponent(
    self: *Tokenizer,
) ParseError!void {
    self.assertFilledSource();

    switch (self.peekCharAssume()) {
        '-', '+' => {},
        '0'...'9' => {
            return self.consumeDecimal();
        },
        else => {
            return ParseError.InvalidNumber;
        },
    }

    self.consumeChar();

    return self.consumeDecimalChecked();
}

fn consumeExponentChecked(
    self: *Tokenizer,
) ParseError!void {
    if (self.isSourceEmpty()) {
        return ParseError.InvalidNumber;
    }

    return self.consumeExponent();
}

fn consumeNumberTrail(
    self: *Tokenizer,
) ParseError!void {
    while (!self.isSourceEmpty()) {
        switch (self.peekCharAssume()) {
            '0'...'9' => {
                self.consumeChar();
            },
            'e', 'E' => {
                self.consumeChar();
                return self.consumeExponent();
            },
            '.' => {
                self.consumeChar();
                return self.consumeDecimal();
            },
            else => {
                return;
            },
        }
    }
}

fn consumeNumberUnsigned(
    self: *Tokenizer,
) ParseError!void {
    self.assertFilledSource();

    switch (self.peekCharAssume()) {
        '1'...'9' => {
            self.consumeChar();
        },
        else => {
            return ParseError.InvalidNumber;
        },
    }

    return self.consumeNumberTrail();
}

fn consumeNumberUnsignedChecked(
    self: *Tokenizer,
) ParseError!void {
    if (self.isSourceEmpty()) {
        return ParseError.InvalidNumber;
    }

    return self.consumeNumberUnsigned();
}

fn consumeNumberTrailLeadingZero(
    self: *Tokenizer,
) ParseError!void {
    self.assertFilledSource();

    switch (self.peekCharAssume()) {
        '0'...'9' => {
            return ParseError.InvalidNumber;
        },
        'e', 'E' => {
            self.consumeChar();
            return self.consumeExponentChecked();
        },
        '.' => {
            self.consumeChar();
            return self.consumeDecimalChecked();
        },
        else => {
            return;
        },
    }
}

inline fn consumeNumberTrailLeadingZeroChecked(
    self: *Tokenizer,
) ParseError!void {
    if (self.isSourceEmpty()) {
        return ParseError.ExpectedToken;
    }

    return self.consumeNumberTrailLeadingZero();
}

pub fn takeNumber(
    self: *Tokenizer,
    ch: u8,
) ParseError![]const u8 {
    const start = self.i - 1;

    { // consume number remainder
        switch (ch) {
            '0' => {
                try self.consumeNumberTrailLeadingZeroChecked();
            },
            '1'...'9' => {
                try self.consumeNumberTrail();
            },
            '-' => {
                try self.consumeNumberUnsignedChecked();
            },
            else => {
                return ParseError.InvalidNumber;
            },
        }
    }

    const end = self.i;

    return self.source[start..end];
}

pub fn takeNullableNumber(
    self: *Tokenizer,
    ch: u8,
) ParseError!?[]const u8 {
    const start = self.i - 1;

    { // consume number remainder
        switch (ch) {
            '0' => {
                try self.consumeNumberTrailLeadingZeroChecked();
            },
            '1'...'9' => {
                try self.consumeNumberTrail();
            },
            '-' => {
                try self.consumeNumberUnsignedChecked();
            },
            keywords.NULL[0] => {
                try self.consumeNullAssume();
                return null;
            },
            else => {
                return ParseError.InvalidNumber;
            },
        }
    }

    const end = self.i;

    return self.source[start..end];
}

fn consumeStringInner(
    self: *Tokenizer,
) ParseError!void {
    while (!self.isSourceEmpty()) {
        if (self.takeCharAssume() == keywords.DQUOTE) {
            return;
        }
    }

    return ParseError.InvalidString;
}

fn consumeString(
    self: *Tokenizer,
) ParseError!void {
    self.assertFilledSource();

    if (self.takeCharAssume() != keywords.DQUOTE) {
        return ParseError.InvalidString;
    }

    return self.consumeStringInner();
}

pub fn takeStringInner(
    self: *Tokenizer,
) ParseError![]const u8 {
    self.assertFilledSource();

    const start = self.i;

    try self.consumeStringInner();

    const end = self.i - 1;

    return self.source[start..end];
}

pub fn takeString(
    self: *Tokenizer,
    ch: u8,
) ParseError![]const u8 {
    switch (ch) {
        keywords.DQUOTE => {
            return self.takeStringInner();
        },
        else => {
            return ParseError.InvalidString;
        },
    }
}

pub fn takeNullableString(
    self: *Tokenizer,
    ch: u8,
) ParseError!?[]const u8 {
    self.assertFilledSource();

    switch (ch) {
        keywords.DQUOTE => {
            return self.takeStringInner();
        },
        keywords.NULL[0] => {
            try self.consumeNullAssume();
            return null;
        },
        else => {
            return ParseError.UnexpectedToken;
        },
    }
}

fn consumeFieldTerminator(
    self: *Tokenizer,
) ParseError!void {
    if (self.isSourceEmpty() or self.takeCharAssume() != keywords.COLON) {
        return ParseError.InvalidField;
    }
}

pub fn takeFieldInner(
    self: *Tokenizer,
) ParseError![]const u8 {
    const name = self.takeStringInner();

    try self.consumeFieldTerminator();

    return name;
}

pub fn takeField(
    self: *Tokenizer,
    ch: u8,
) ParseError![]const u8 {
    const name = self.takeString(ch);

    if (self.isSourceEmpty() or self.takeCharAssume() != keywords.COLON) {
        return ParseError.InvalidField;
    }

    return name;
}

// Primitives
// --------------------------------------------------

pub const TokenTypePrimitive = enum {
    number,
    string,
    true,
    false,
    null,
    object_begin,
    object_end,
    array_begin,
    array_end,
    comma,
};

pub const TokenType = enum {
    number,
    string,
    bool,
    field,
    null,
    object_begin,
    object_end,
    array_begin,
    array_end,
    comma,
};

pub const Token = union(TokenType) {
    number: []const u8,
    string: []const u8,
    bool: bool,
    field: []const u8,
    null: void,
    object_begin: void,
    object_end: void,
    array_begin: void,
    array_end: void,
    comma: void,
};

pub fn tokenValueType(
    comptime token: TokenType,
) type {
    return switch (token) {
        .number => []const u8,
        .string => []const u8,
        .bool => bool,
        .field => []const u8,
        else => void,
    };
}

pub fn takeTokenPeeked(
    self: *Tokenizer,
    ch: u8,
) ParseError!Token {
    switch (ch) {
        '0'...'9', '-' => {
            return .{ .number = try self.takeNumber(ch) };
        },
        keywords.DQUOTE => {
            const value = try self.takeStringInner();

            const peeked = self.peekChar() orelse return .{
                .string = value,
            };

            switch (peeked) {
                keywords.COLON => {
                    self.consumeChar();

                    return .{
                        .field = value,
                    };
                },
                '\n', '\r', '\t', ' ' => {
                    self.consumeChar();

                    return .{
                        .string = value,
                    };
                },
                else => {
                    return .{
                        .string = value,
                    };
                },
            }
        },
        keywords.TRUE[0] => {
            try self.consumeTrueAssume();

            return .{
                .bool = true,
            };
        },
        keywords.FALSE[0] => {
            try self.consumeFalseAssume();

            return .{
                .bool = false,
            };
        },
        keywords.NULL[0] => {
            try self.consumeNullAssume();

            return .null;
        },
        keywords.OBJ_BEGIN => {
            return .object_begin;
        },
        keywords.OBJ_END => {
            return .object_end;
        },
        keywords.ARR_BEGIN => {
            return .array_begin;
        },
        keywords.ARR_END => {
            return .array_end;
        },
        keywords.COMMA => {
            return .comma;
        },
        else => {
            return ParseError.InvalidToken;
        },
    }
}

pub inline fn takeToken(
    self: *Tokenizer,
) ParseError!Token {
    const ch = self.takeChar() orelse return ParseError.ExpectedToken;

    return self.takeTokenPeeked(ch);
}

pub fn takeTokenExpectPeeked(
    self: *Tokenizer,
    ch: u8,
    comptime expected: TokenType,
) !tokenValueType(expected) {
    switch (expected) {
        .number => {
            return self.takeNumber(ch);
        },
        .string => {
            return self.takeString(ch);
        },
        .bool => {
            return self.takeBool(ch);
        },
        .field => {
            return self.takeField(ch);
        },
        .null => {
            return self.consumeTrueAssume();
        },
        .object_begin => {
            if (ch != keywords.OBJ_BEGIN) {
                return ParseError.UnexpectedToken;
            }
        },
        .object_end => {
            if (ch != keywords.OBJ_END) {
                return ParseError.UnexpectedToken;
            }
        },
        .array_begin => {
            if (ch != keywords.ARR_BEGIN) {
                return ParseError.UnexpectedToken;
            }
        },
        .array_end => {
            if (ch != keywords.ARR_END) {
                return ParseError.UnexpectedToken;
            }
        },
        .comma => {
            if (ch != keywords.COMMA) {
                return ParseError.UnexpectedToken;
            }
        },
    }
}

pub inline fn takeTokenExpect(
    self: *Tokenizer,
    comptime expected: TokenType,
) ParseError!tokenValueType(expected) {
    const ch = self.takeChar() orelse return ParseError.ExpectedToken;

    return self.takeTokenExpectPeeked(ch, expected);
}

pub const TokenTypeNullable = enum {
    number,
    string,
    bool,
    field,
    object_begin,
    object_end,
    array_begin,
    array_end,
    colon,
};

pub const NullableToken = union(TokenTypeNullable) {
    number: ?[]const u8,
    string: ?[]const u8,
    bool: ?bool,
    field: []const u8,
    object_begin: void,
    object_end: void,
    array_begin: void,
    array_end: void,
    colon: void,
};

pub fn tokenValueTypeNullable(
    comptime token: TokenTypeNullable,
) type {
    return switch (token) {
        .number => ?[]const u8,
        .string => ?[]const u8,
        .bool => ?bool,
        .field => []const u8,
        else => void,
    };
}

pub fn takeTokenExpectNullablePeeked(
    self: *Tokenizer,
    ch: u8,
    comptime expected: TokenTypeNullable,
) ParseError!tokenValueTypeNullable(expected) {
    switch (expected) {
        .number => {
            return self.takeNullableNumber(ch);
        },
        .string => {
            return self.takeNullableString(ch);
        },
        .bool => {
            return self.takeNullableBool(ch);
        },
        .field => {
            return self.takeField(ch);
        },
        else => {},
    }
}

pub inline fn takeTokenExpectNullable(
    self: *Tokenizer,
    comptime expected: TokenTypeNullable,
) ParseError!tokenValueTypeNullable(expected) {
    const ch = self.takeChar() orelse return ParseError.ExpectedToken;

    return self.takeTokenExpectNullablePeeked(ch, expected);
}

pub fn nextToken(
    self: *Tokenizer,
) ParseError!Token {
    const ch = try self.consumeWhitespace() orelse return ParseError.ExpectedToken;

    return self.takeTokenPeeked(ch);
}

pub fn nextTokenExpect(
    self: *Tokenizer,
    comptime expected: TokenType,
) ParseError!tokenValueType(expected) {
    const ch = try self.consumeWhitespace() orelse return ParseError.ExpectedToken;

    return self.takeTokenExpectPeeked(ch, expected);
}

pub fn nextTokenExpectNullable(
    self: *Tokenizer,
    comptime expected: TokenTypeNullable,
) ParseError!tokenValueTypeNullable(expected) {
    const ch = try self.consumeWhitespace() orelse return ParseError.ExpectedToken;

    return self.takeTokenExpectNullable(ch, expected);
}

pub fn inferrTokenType(
    ch: u8,
) ?TokenTypePrimitive {
    switch (ch) {
        // NOTE: Not sure if this makes sense
        inline else => |cmpt_ch| {
            return switch (cmpt_ch) {
                '0'...'9', '-' => .number,
                keywords.DQUOTE => .string,
                keywords.TRUE[0] => .true,
                keywords.FALSE[0] => .false,
                keywords.NULL[0] => .null,
                keywords.OBJ_BEGIN => .object_begin,
                keywords.OBJ_END => .object_end,
                keywords.ARR_BEGIN => .array_begin,
                keywords.ARR_END => .array_end,
                keywords.COMMA => .comma,
                else => null,
            };
        },
    }
}

// NOTE: Specific skip functions assume that the first character has been consumed already

pub fn skipNumberAssume(
    self: *Tokenizer,
) void {
    while (self.takeChar()) |ch| {
        switch (ch) {
            ' ', '\t', '\n', '\r' => {
                break;
            },
            keywords.COMMA, keywords.ARR_END, keywords.OBJ_END => {
                self.i -= 1;

                break;
            },
            else => {},
        }
    }
}

pub fn skipStringInner(
    self: *Tokenizer,
) void {
    while (self.takeChar()) |ch| {
        if (ch == keywords.DQUOTE) {
            if (!self.isSourceEmpty() and self.peekCharAssume() == keywords.COLON) {
                self.consumeChar();
            }

            return;
        }
    }
}

pub fn skipTrueAssume(
    self: *Tokenizer,
) void {
    self.consumeChars(keywords.TRUE.len - 1);
}

pub fn skipFalseAssume(
    self: *Tokenizer,
) void {
    self.consumeChars(keywords.FALSE.len - 1);
}

pub fn skipNullAssume(
    self: *Tokenizer,
) void {
    self.consumeChars(keywords.NULL.len - 1);
}

pub fn skipTokenPeeked(
    self: *Tokenizer,
    ch: u8,
) ParseError!void {
    switch (inferrTokenType(ch)) {
        .number => {
            self.skipNumberAssume();
        },
        .string => {
            self.skipStringInner();
        },
        .true => {
            self.consumeChars(keywords.TRUE.len - 1);
        },
        .false => {
            self.consumeChars(keywords.FALSE.len - 1);
        },
        else => {},
    }
}

pub fn skipTokenExpectPeeked(
    self: *Tokenizer,
    ch: u8,
    comptime expected: TokenType,
) ParseError!void {
    switch (inferrTokenType(ch) orelse return ParseError.InvalidToken) {
        .number => {
            if (expected != .number) {
                return ParseError.UnexpectedToken;
            }

            return self.skipNumberAssume();
        },
        .string => {
            if (expected != .string and expected != .field) {
                return ParseError.UnexpectedToken;
            }

            return self.skipStringInner();
        },
        .true => {
            if (expected != .bool) {
                return ParseError.UnexpectedToken;
            }

            return self.skipTrueAssume();
        },
        .false => {
            if (expected != .bool) {
                return ParseError.UnexpectedToken;
            }

            return self.skipFalseAssume();
        },
        .null => {
            if (expected != .null) {
                return ParseError.UnexpectedToken;
            }

            return self.skipNullAssume();
        },
        .object_begin => {
            if (expected != .object_begin) {
                return ParseError.UnexpectedToken;
            }
        },
        .object_end => {
            if (expected != .object_end) {
                return ParseError.UnexpectedToken;
            }
        },
        .array_begin => {
            if (expected != .array_begin) {
                return ParseError.UnexpectedToken;
            }
        },
        .array_end => {
            if (expected != .array_end) {
                return ParseError.UnexpectedToken;
            }
        },
        .comma => {
            if (expected != .comma) {
                return ParseError.UnexpectedToken;
            }
        },
    }
}

pub fn skipToken(
    self: *Tokenizer,
) ParseError!void {
    const ch = self.takeChar() orelse return;

    try self.skipTokenPeeked(ch);
}

pub fn skipTokenExpect(
    self: *Tokenizer,
    comptime expected: TokenType,
) ParseError!void {
    const ch = self.takeChar() orelse return;

    try self.skipTokenExpectPeeked(ch, expected);
}

pub fn skipNextToken(
    self: *Tokenizer,
) ParseError!void {
    const ch = try self.consumeWhitespace() orelse return;

    try self.skipTokenPeeked(ch);
}

pub fn skipNextTokenExpect(
    self: *Tokenizer,
    comptime expected: TokenType,
) ParseError!void {
    const ch = try self.consumeWhitespace() orelse return ParseError.ExpectedToken;

    try self.skipTokenExpectPeeked(ch, expected);
}
