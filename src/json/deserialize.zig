const std = @import("std");

const Tokenizer = @import("tokenizer.zig");

pub const DeserializeError = error{
    ArrayTooShort,
    ArrayTooLong,
} || Tokenizer.ParseError;

pub const DeserializeOpts = struct {
    allow_trailing_comma: bool = true,
    precice_errors: bool = false,
};

inline fn deserializeBool(source: *Tokenizer) !bool {
    return source.nextTokenExpect(.bool);
}

inline fn deserializeInt(comptime T: type, source: *Tokenizer) !T {
    const number = try source.nextTokenExpect(.number);

    return std.fmt.parseInt(T, number, 10) catch return Tokenizer.ParseError.InvalidNumber;
}

inline fn deserializeFloat(comptime T: type, source: *Tokenizer) !T {
    const number = try source.nextTokenExpect(.number);

    return std.fmt.parseFloat(T, number) catch return Tokenizer.ParseError.InvalidNumber;
}

inline fn deserializePointer(comptime T: type, source: *Tokenizer) !T {
    const info = @typeInfo(T).pointer;

    switch (info.size) {
        .slice => {
            if (comptime std.mem.eql(u8, @typeName(T), @typeName([]const u8))) {
                return source.nextTokenExpect(.string);
            }

            @compileError("Slices (exept strings ([]const u8)) are not allowed while parsing unallocated! Consider using deserializeAlloc().");
        },
        else => {
            @compileError("Pointers are not allowed while parsing unallocated! Consider using deserializeAlloc().");
        },
    }
}

inline fn deserializeEmptyArray(comptime T: type, source: *Tokenizer, comptime opts: DeserializeOpts) DeserializeError!void {
    const info = switch (@typeInfo(T)) {
        .array => |info| info,
        else => {
            unreachable;
        },
    };

    if (info.len > 0) {
        @compileError("Expected empty array!");
    }

    try source.nextTokenExpectChecked(.array_begin);

    if (opts.precice_errors) {
        switch (try source.nextTokenChecked()) {
            .array_end => {},
        }
    } else {
        try source.nextTokenExpectChecked(.array_end);
    }

    return;
}

fn deserializeArray(comptime T: type, array: *T, source: *Tokenizer, comptime opts: DeserializeOpts) DeserializeError!void {
    const info = switch (@typeInfo(T)) {
        .array => |info| info,
        else => {
            unreachable;
        },
    };

    if (info.len == 0) {
        return deserializeEmptyArray(T, source, opts);
    }

    try source.nextTokenExpectChecked(.array_begin);

    { // consume items
        for (0..info.len - 1) |i| {
            try deserializeField(info.child, &array[i], source, opts);

            switch (try source.nextTokenChecked()) {
                .comma => {},
                .array_end => {
                    return DeserializeError.ArrayTooShort;
                },
                else => {
                    return DeserializeError.UnexpectedToken;
                },
            }
        }

        try deserializeField(info.child, &array[info.len - 1], source, opts);
    }

    { // consume end
        if (opts.allow_trailing_comma) {
            switch (try source.nextTokenChecked()) {
                .array_end => {},
                .comma => {
                    try source.nextTokenExpectChecked(.array_end);
                },
                else => {
                    return DeserializeError.UnexpectedToken;
                },
            }
        } else {
            return source.nextTokenExpectChecked(.array_end);
        }
    }
}

pub inline fn deserializeField(comptime T: type, field: *T, source: *Tokenizer, comptime opts: DeserializeOpts) !void {
    switch (@typeInfo(T)) {
        .bool => {
            field.* = try deserializeBool(source);
        },
        .int => {
            field.* = try deserializeInt(T, source);
        },
        .float => {
            field.* = try deserializeFloat(T, source);
        },
        .pointer => {
            field.* = try deserializePointer(T, source);
        },
        .array => {
            return deserializeArray(T, field, source, opts);
        },
        else => {
            @compileError("Invalid type!");
        },
    }
}

pub fn deserialzeFromSource(comptime T: type, source: *Tokenizer, comptime opts: DeserializeOpts) !T {
    _ = opts;

    switch (@typeInfo(T)) {
        .bool => {
            return deserializeBool(source);
        },
        .int => {
            return deserializeInt(T, source);
        },
        .float => {
            return deserializeFloat(T, source);
        },
        .pointer => {
            return deserializePointer(T, source);
        },
        .array => {
            @compileError("Arrays are only allowed as fields when parsing unallocated! Consider using deserializeAlloc().");
        },
        else => {
            //  TODO: Better error message
            @compileError("Invalid type!");
        },
    }
}

pub fn deserialize(comptime T: type, json: []const u8, comptime opts: DeserializeOpts) !T {
    var source = Tokenizer{ .source = json };

    return deserialzeFromSource(T, &source, opts);
}
