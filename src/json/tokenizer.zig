const std = @import("std");

const keywords = @import("keywords.zig");

const Tokenizer = @This();

source: []const u8,
i: usize = 0,

pub const ParseError = error{
    InvalidNumber,
    ExpectedNumber,
    InvalidString,
    ExpectedString,
    InvalidField,
    ExpectedField,
    UnexpectedToken,
    InvalidToken,
    ExpectedToken,
};

// --------------------------------------------------
// Helpers

inline fn remaining(self: Tokenizer) usize {
    return self.source.len - self.i;
}

inline fn isSourceEmpty(self: Tokenizer) bool {
    return self.i >= self.source.len;
}

inline fn assertFilledSource(self: Tokenizer) void {
    std.debug.assert(self.i < self.source.len);
}

inline fn assertRemaining(self: Tokenizer, n: usize) void {
    std.debug.assert(self.remaining() >= n);
}

inline fn takeChar(self: *Tokenizer) u8 {
    self.assertFilledSource();

    defer self.i += 1;

    return self.source[self.i];
}

inline fn takeChars(self: *Tokenizer, n: usize) []const u8 {
    self.assertRemaining(n);

    defer self.i += n;
    return self.source[0..n];
}

inline fn consumeChar(self: *Tokenizer) void {
    self.i += 1;
}

inline fn consumeChars(self: *Tokenizer, n: usize) void {
    self.i += n;
}

inline fn peekChar(self: Tokenizer) u8 {
    self.assertFilledSource();

    return self.source[self.i];
}

inline fn peekChars(self: Tokenizer, n: usize) []const u8 {
    self.assertRemaining(n);

    return self.source[self.i..(self.i + n)];
}

inline fn peekEql(self: Tokenizer, str: []const u8) bool {
    if (self.remaining() < str.len) {
        return false;
    }

    return std.mem.eql(u8, self.peekChars(str.len), str);
}

/// Consumes the text of a comment
fn consumeComment(self: *Tokenizer) void {
    while (!self.isSourceEmpty()) {
        if (self.takeChar() == '\n') {
            return;
        }
    }
}

fn consumeWhitespace(self: *Tokenizer) ParseError!void {
    while (!self.isSourceEmpty()) {
        switch (self.peekChar()) {
            '\t', '\n', '\r', ' ' => {
                self.consumeChar();
            },
            '/' => {
                if (self.isSourceEmpty() or self.takeChar() != '/') {
                    return ParseError.InvalidToken;
                }

                self.consumeComment();
            },
            else => {
                return;
            },
        }
    }
}

// Helpers
// --------------------------------------------------

// --------------------------------------------------
// Primitives

fn consumeNull(self: *Tokenizer) ParseError!void {
    defer self.consumeChars(keywords.NULL.len);

    if (!self.peekEql(keywords.NULL)) {
        return ParseError.UnexpectedToken;
    }
}

fn consumeNullTrail(self: *Tokenizer) ParseError!void {
    defer self.consumeChars(keywords.NULL.len - 1);

    if (!self.peekEql(keywords.NULL[1..])) {
        return ParseError.InvalidToken;
    }
}

fn consumeTrue(self: *Tokenizer) ParseError!void {
    defer self.consumeChars(keywords.TRUE.len);

    if (!self.peekEql(keywords.TRUE)) {
        return ParseError.InvalidToken;
    }
}

fn consumeTrueTrail(self: *Tokenizer) ParseError!void {
    defer self.consumeChars(keywords.TRUE.len - 1);

    if (!self.peekEql(keywords.TRUE[1..])) {
        return ParseError.InvalidToken;
    }
}

fn consumeFalse(self: *Tokenizer) ParseError!void {
    defer self.consumeChars(keywords.FALSE.len);

    if (!self.peekEql(keywords.FALSE)) {
        return ParseError.InvalidToken;
    }
}

fn consumeFalseTrail(self: *Tokenizer) ParseError!void {
    defer self.consumeChars(keywords.FALSE.len - 1);

    if (!self.peekEql(keywords.FALSE[1..])) {
        return ParseError.InvalidToken;
    }
}

pub fn takeBool(self: *Tokenizer) ParseError!bool {
    self.assertFilledSource();

    switch (self.peekChar()) {
        keywords.TRUE[0] => {
            self.consumeChar();
            try self.consumeTrueTrail();
            return true;
        },
        keywords.FALSE[0] => {
            self.consumeChar();
            try self.consumeFalseTrail();
            return false;
        },
        else => {
            return ParseError.UnexpectedToken;
        },
    }
}

pub fn takeBoolChecked(self: *Tokenizer) ParseError!bool {
    if (self.isSourceEmpty()) {
        return ParseError.ExpectedToken;
    }

    return self.takeBool();
}

fn nextExpectBool(self: *Tokenizer) ParseError!bool {
    try self.consumeWhitespace();

    if (self.isSourceEmpty()) {
        return ParseError.InvalidToken;
    }

    return self.takeBool();
}

fn nextExpectBoolNullable(self: *Tokenizer) ParseError!?bool {
    try self.consumeWhitespace();

    if (self.isSourceEmpty()) {
        return ParseError.InvalidToken;
    }

    switch (self.peekChar()) {
        keywords.TRUE[0] => {
            self.consumeChar();
            try self.consumeTrueTrail();
            return true;
        },
        keywords.FALSE[0] => {
            self.consumeChar();
            try self.consumeFalseTrail();
            return false;
        },
        keywords.NULL[0] => {
            self.consumeChar();
            try self.consumeNullTrail();
            return null;
        },
        else => {
            return ParseError.InvalidToken;
        },
    }
}

fn consumeDecimal(self: *Tokenizer) ParseError!void {
    self.assertFilledSource();

    {
        switch (self.peekChar()) {
            '0'...'9' => {},
            else => {
                return ParseError.InvalidNumber;
            },
        }

        self.consumeChar();
    }

    {
        while (!self.isSourceEmpty()) {
            switch (self.peekChar()) {
                '0'...'9' => {},
                else => {
                    return;
                },
            }

            self.consumeChar();
        }
    }
}

fn consumeDecimalChecked(self: *Tokenizer) ParseError!void {
    if (self.isSourceEmpty()) {
        return ParseError.InvalidNumber;
    }

    return self.consumeDecimal();
}

fn consumeExponent(self: *Tokenizer) ParseError!void {
    self.assertFilledSource();

    switch (self.peekChar()) {
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

fn consumeExponentChecked(self: *Tokenizer) ParseError!void {
    if (self.isSourceEmpty()) {
        return ParseError.InvalidNumber;
    }

    return self.consumeExponent();
}

/// consumes a number after the first character has been consumed
fn consumeNumberRemainder(self: *Tokenizer) ParseError!void {
    while (!self.isSourceEmpty()) {
        switch (self.peekChar()) {
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

fn consumeNumberUnsigned(self: *Tokenizer) ParseError!void {
    self.assertFilledSource();

    switch (self.peekChar()) {
        '1'...'9' => {
            self.consumeChar();
        },
        else => {
            return ParseError.InvalidNumber;
        },
    }

    return self.consumeNumberRemainder();
}

fn consumeNumberUnsignedChecked(self: *Tokenizer) ParseError!void {
    if (self.isSourceEmpty()) {
        return ParseError.InvalidNumber;
    }

    return self.consumeNumberUnsigned();
}

fn consumeNumberLeadingZero(self: *Tokenizer) ParseError!void {
    self.assertFilledSource();

    switch (self.peekChar()) {
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

fn consumeNumber(self: *Tokenizer) ParseError!void {
    self.assertFilledSource();

    switch (self.peekChar()) {
        '0' => {
            self.consumeChar();
            return self.consumeNumberLeadingZero();
        },
        '1'...'9' => {
            self.consumeChar();
            return self.consumeNumberRemainder();
        },
        '-' => {
            self.consumeChar();
            return self.consumeNumberUnsignedChecked();
        },
        else => {
            return ParseError.InvalidNumber;
        },
    }
}

pub fn takeNumber(self: *Tokenizer) ParseError![]const u8 {
    self.assertFilledSource();

    const start = self.i;

    try self.consumeNumber();

    const end = self.i;

    return self.source[start..end];
}

pub fn takeNumberChecked(self: *Tokenizer) ParseError![]const u8 {
    if (self.isSourceEmpty()) {
        return ParseError.ExpectedNumber;
    }

    return self.takeNumber();
}

pub fn nextExpectNumber(self: *Tokenizer) ParseError![]const u8 {
    try self.consumeWhitespace();

    if (self.isSourceEmpty()) {
        return ParseError.ExpectedNumber;
    }

    return self.takeNumber();
}

pub fn nextExpectNumberNullable(self: *Tokenizer) ParseError!?[]const u8 {
    try self.consumeWhitespace();

    if (self.isSourceEmpty()) {
        return ParseError.ExpectedNumber;
    }

    switch (self.peekChar()) {
        '0'...'9', '-' => {
            return try self.takeNumber();
        },
        'n' => {
            try self.consumeNull();
            return null;
        },
        else => {
            return ParseError.InvalidNumber;
        },
    }
}

fn consumeStringTrail(self: *Tokenizer) ParseError!void {
    while (!self.isSourceEmpty()) {
        if (self.takeChar() == keywords.DQUOTE) {
            return;
        }
    }

    return ParseError.InvalidString;
}

fn takeStringTrail(self: *Tokenizer) ParseError![]const u8 {
    self.assertFilledSource();

    const start = self.i;

    try self.consumeStringTrail();

    const end = self.i - 1;

    return self.source[start..end];
}

fn consumeString(self: *Tokenizer) ParseError!void {
    self.assertFilledSource();

    if (self.takeChar() != keywords.DQUOTE) {
        return ParseError.ExpectedString;
    }

    return self.consumeStringTrail();
}

pub fn takeString(self: *Tokenizer) ParseError![]const u8 {
    self.assertFilledSource();

    const start = self.i + 1;

    try self.consumeString();

    const end = self.i - 1;

    return self.source[start..end];
}

pub fn takeStringChecked(self: *Tokenizer) ParseError![]const u8 {
    if (self.isSourceEmpty()) {
        return ParseError.ExpectedString;
    }

    return self.takeString();
}

pub fn nextExpectString(self: *Tokenizer) ParseError![]const u8 {
    try self.consumeWhitespace();

    if (self.isSourceEmpty()) {
        return ParseError.ExpectedString;
    }

    return self.takeString();
}

pub fn nextExpectStringNullable(self: *Tokenizer) ParseError!?[]const u8 {
    try self.consumeWhitespace();

    if (self.isSourceEmpty()) {
        return ParseError.InvalidString;
    }

    switch (self.takeChar()) {
        keywords.DQUOTE => {
            return self.takeStringTrail();
        },
        keywords.NULL[0] => {
            self.consumeNullTrail();
            return null;
        },
        else => {
            return ParseError.InvalidString;
        },
    }
}

// Primitives
// --------------------------------------------------

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

pub fn nextExpectField(self: *Tokenizer) ParseError![]const u8 {
    try self.consumeWhitespace();

    const name = try self.takeString();

    if (self.isSourceEmpty() or self.takeChar() != keywords.COLON) {
        return ParseError.InvalidField;
    }

    return name;
}

pub fn takeToken(self: Tokenizer) ParseError!Token {
    switch (self.peekChar()) {
        keywords.OBJ_BEGIN => {
            self.consumeChar();
            return .object_begin;
        },
        keywords.OBJ_END => {
            self.consumeChar();
            return .object_end;
        },
        keywords.ARR_BEGIN => {
            self.consumeChar();
            return .array_begin;
        },
        keywords.ARR_END => {
            self.consumeChar();
            return .array_end;
        },
        keywords.COMMA => {
            self.consumeChar();
            return .comma;
        },
        keywords.NULL[0] => {
            self.consumeChar();
            try self.consumeNullTrail();
            return .null;
        },
        keywords.DQUOTE => {
            self.consumeChar();
            const str = try self.takeStringTrail();

            if (!self.isSourceEmpty() and self.peekChar() == keywords.COLON) {
                self.consumeChar();

                return Token{ .field = str };
            } else {
                return Token{ .string = str };
            }
        },
        '0'...'9', '-' => {
            return Token{ .number = try self.takeNumber() };
        },
        else => {
            return ParseError.InvalidToken;
        },
    }
}

pub fn next(self: *Tokenizer) ParseError!?Token {
    try self.consumeWhitespace();

    if (self.isSourceEmpty()) {
        return null;
    }

    return takeToken();
}

pub fn nextExpectAny(self: *Tokenizer) ParseError!Token {
    return try self.next() orelse ParseError.ExpectedToken;
}

//  TODO: Implement this to actually consume stuff itself and not care about errors
pub fn skip(self: *Tokenizer) void {
    _ = self.next() catch {};
}

const Peek = struct {
    value: Token,
    i: usize,
};

pub fn peekToken(self: Tokenizer) ParseError!?Peek {
    var cloned = self;

    return .{ .value = cloned.next() orelse return null, .i = cloned.i };
}

pub fn peekTokenExpectAny(self: Tokenizer) ParseError!Peek {
    return try self.peekToken() orelse ParseError.ExpectedToken;
}

fn tokenValueType(comptime token: TokenType) type {
    return switch (token) {
        .number => []const u8,
        .string => []const u8,
        .bool => bool,
        .field => []const u8,
        else => void,
    };
}

pub fn takeTokenExpect(self: *Tokenizer, comptime expected: TokenType) !tokenValueType(expected) {
    switch (expected) {
        .number => {
            return self.takeNumberChecked();
        },
        .string => {
            return self.takeStringChecked();
        },
        .bool => {
            return self.takeBoolChecked();
        },
        .field => {
            return self.nextExpectField();
        },
        .null => {
            return self.consumeNull();
        },
        .object_begin => {
            if (self.takeChar() != keywords.OBJ_BEGIN) {
                return ParseError.UnexpectedToken;
            }
        },
        .object_end => {
            if (self.takeChar() != keywords.OBJ_END) {
                return ParseError.UnexpectedToken;
            }
        },
        .array_begin => {
            if (self.takeChar() != keywords.ARR_BEGIN) {
                return ParseError.UnexpectedToken;
            }
        },
        .array_end => {
            if (self.takeChar() != keywords.ARR_END) {
                return ParseError.UnexpectedToken;
            }
        },
        .comma => {
            if (self.takeChar() != keywords.COMMA) {
                return ParseError.UnexpectedToken;
            }
        },
    }
}

pub fn nextExpect(self: *Tokenizer, comptime expected: TokenType) !tokenValueType(expected) {
    try self.consumeWhitespace();

    return self.takeTokenExpect(expected);
}

fn tokenValueTypeNullable(comptime token: TokenType) type {
    return switch (token) {
        .number => ?[]const u8,
        .string => ?[]const u8,
        .bool => ?bool,
        .field => ?[]const u8,
        else => void,
    };
}

pub fn nextExpectNullable(self: *Tokenizer, comptime expected: TokenType) ?tokenValueTypeNullable(expected) {
    switch (expected) {
        .number => {
            return self.nextExpectNumberNullable();
        },
        .string => {
            return self.nextExpectStringNullable();
        },
        .bool => {
            return self.nextExpectBoolNullable();
        },
        .field => {
            return self.nextExpectField();
        },
        .null => {
            return self.consumeNull();
        },
        .object_begin => {
            self.consumeWhitespace();
            if (self.takeChar() != keywords.OBJ_BEGIN) {
                return ParseError.UnexpectedToken;
            }
        },
        .object_end => {
            self.consumeWhitespace();
            if (self.takeChar() != keywords.OBJ_END) {
                return ParseError.UnexpectedToken;
            }
        },
        .array_begin => {
            self.consumeWhitespace();
            if (self.takeChar() != keywords.ARR_BEGIN) {
                return ParseError.UnexpectedToken;
            }
        },
        .array_end => {
            self.consumeWhitespace();
            if (self.takeChar() != keywords.ARR_END) {
                return ParseError.UnexpectedToken;
            }
        },
        .comma => {
            self.consumeWhitespace();
            if (self.takeChar() != keywords.COMMA) {
                return ParseError.UnexpectedToken;
            }
        },
    }
}
