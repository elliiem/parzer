const std = @import("std");

pub fn expectBoolean(
    comptime T: type,
) void {
    switch (@typeInfo(T)) {
        .bool => {},
        else => {
            @compileError("Expected T to be a boolean!");
        },
    }
}

pub fn expectInt(
    comptime T: type,
) void {
    switch (@typeInfo(T)) {
        .int, .comptime_int => {},
        else => {
            @compileError("Expected T to be a integer!");
        },
    }
}

pub fn expectFloat(
    comptime T: type,
) void {
    switch (@typeInfo(T)) {
        .float, .comptime_float => {},
        else => {
            @compileError("Expected T to be a float!");
        },
    }
}

pub fn expectPointer(
    comptime T: type,
) void {
    switch (@typeInfo(T)) {
        .pointer => {},
        else => {
            @compileError("Expected T to be a pointer!");
        },
    }
}

pub fn expectArray(
    comptime T: type,
) void {
    switch (@typeInfo(T)) {
        .array => {},
        else => {
            @compileError("Expected T to be a array!");
        },
    }
}

pub fn expectString(
    comptime T: type,
) void {
    if (!(T == []const u8)) {
        @compileError("Expected T to be a array!");
    }
}

pub fn expectOptional(
    comptime T: type,
) void {
    switch (@typeInfo(T)) {
        .optional => {},
        else => {
            @compileError("Expected T to be a optional!");
        },
    }
}

pub fn expectStruct(
    comptime T: type,
) void {
    switch (@typeInfo(T)) {
        .@"struct" => {},
        else => {
            @compileError("Expected T to be a struct!");
        },
    }
}

pub fn expectEnum(
    comptime T: type,
) void {
    switch (@typeInfo(T)) {
        .@"enum" => {},
        else => {
            @compileError("Expected T to be a enum!");
        },
    }
}

pub fn expectUnion(
    comptime T: type,
) void {
    switch (@typeInfo(T)) {
        .@"union" => {},
        else => {
            @compileError("Expected T to be a enum!");
        },
    }
}

pub fn arrayLenght(comptime T: type) comptime_int {
    expectArray(T);

    return @typeInfo(T).array.len;
}

pub inline fn createUndefined(
    comptime T: type,
) T {
    comptime {
        switch (@typeInfo(T)) {
            .bool,
            .int,
            .comptime_int,
            .float,
            .comptime_float,
            .pointer,
            .@"enum",
            .@"union",
            => {
                return undefined;
            },
            .array => {
                const Item = std.meta.Child(T);

                return [1]Item{createUndefined(Item)} ** arrayLenght(T);
            },
            .@"struct" => |info| {
                var value: T = undefined;

                for (info.fields) |field| {
                    @field(value, field.name) = createUndefined(field.type);
                }

                return value;
            },
            .optional => {
                return createUndefined(std.meta.Child(T));
            },
            else => {
                @compileError("Unimplemented type!");
            },
        }
    }
}
