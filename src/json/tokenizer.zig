const std = @import("std");

const keywords = @import("keywords.zig");

const Tokenizer = @This();

source: []const u8,
i: usize,

pub const ParseError = error{
    InvalidNumber,
    InvalidToken,
    InvalidString,
};

const Token = union(enum) {
    Number: []const u8,
    String: []const u8,
    Boolean: bool,
    Null: void,
    ArrayBegin: void,
    ArrayEnd: void,
    ObjectBegin: void,
    ObjectEnd: void,
    Field: []const u8,
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
        return ParseError.InvalidToken;
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

pub fn nextExpectBool(self: *Tokenizer) ParseError!bool {
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
        else => {
            return ParseError.InvalidToken;
        },
    }
}

pub fn nextExpectBoolMaybeNull(self: *Tokenizer) ParseError!?bool {
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

fn consumeNumberTrail(self: *Tokenizer) ParseError!void {
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

fn consumeNumberTrailChecked(self: *Tokenizer) ParseError!void {
    if (self.isSourceEmpty()) {
        return ParseError.InvalidNumber;
    }

    return self.consumeNumberTrail();
}

fn consumeExponent(self: *Tokenizer) ParseError!void {
    self.assertFilledSource();

    switch (self.peekChar()) {
        '-', '+' => {},
        '0'...'9' => {
            return self.consumeNumberTrail();
        },
        else => {
            return ParseError.InvalidNumber;
        },
    }

    self.consumeChar();

    return self.consumeNumberTrailChecked();
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
                return self.consumeNumberTrail();
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

fn consumeNumberRemainderLeadingZero(self: *Tokenizer) ParseError!void {
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
            return self.consumeNumberTrailChecked();
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
            return self.consumeNumberRemainderLeadingZero();
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

fn consumeNumberChecked(self: *Tokenizer) ParseError!void {
    if (self.isSourceEmpty()) {
        return ParseError.InvalidNumber;
    }

    return self.consumeNumber();
}

fn takeNumber(self: *Tokenizer) ParseError![]const u8 {
    self.assertFilledSource();

    const start = self.i;

    try self.consumeNumber();

    const end = self.i;

    return self.source[start..end];
}

fn takeNumberChecked(self: *Tokenizer) ParseError![]const u8 {
    if (self.isSourceEmpty()) {
        return ParseError.InvalidNumber;
    }

    return self.takeNumber();
}

pub fn nextExpectNumber(self: *Tokenizer) ParseError![]const u8 {
    try self.consumeWhitespace();

    if (self.isSourceEmpty()) {
        return ParseError.InvalidNumber;
    }

    return self.takeNumber();
}

pub fn nextExpectNumberMaybeNull(self: *Tokenizer) ParseError!?[]const u8 {
    try self.consumeWhitespace();

    if (self.isSourceEmpty()) {
        return ParseError.InvalidNumber;
    }

    switch (self.peekChar()) {
        '0'...'9', '-' => {
            return try self.takeNumberChecked();
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

fn consumeString(self: *Tokenizer) ParseError!void {
    self.assertFilledSource();

    if (self.takeChar() != keywords.DQUOTE) {
        return ParseError.InvalidString;
    }

    return self.consumeStringTrail();
}

pub fn nextExpectString(self: *Tokenizer) ParseError![]const u8 {
    try self.consumeWhitespace();

    if (self.isSourceEmpty()) {
        return ParseError.InvalidString;
    }

    const start = self.i + 1;

    try self.consumeString();

    const end = self.i - 1;

    return self.source[start..end];
}

pub fn nextExpectStringMaybeNull(self: *Tokenizer) ParseError!?[]const u8 {
    try self.consumeWhitespace();

    if (self.isSourceEmpty()) {
        return ParseError.InvalidString;
    }

    const start = self.i + 1;

    switch (self.peekChar()) {
        keywords.DQUOTE => {
            self.consumeChar();
            try self.consumeStringTrail();
        },
        keywords.NULL[0] => {
            self.consumeChar();
            try self.consumeNullTrail();
            return null;
        },
        else => {
            return ParseError.InvalidString;
        },
    }

    const end = self.i - 1;

    return self.source[start..end];
}

// Primitives
// --------------------------------------------------

pub fn next(self: *Tokenizer) ParseError!?Token {
    if (self.isSourceEmpty()) {
        return null;
    }

    const ch = self.peekChar();

    switch (ch) {
        '1'...'9' => {},
    }
}
