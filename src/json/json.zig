const deserialize = @import("deserialize/deserialize.zig");

pub const DeserializeOpts = deserialize.DeserializeOpts;
pub const DeserializeError = deserialize.DeserializeError;

pub const deserializeUnalloced = deserialize.unalloced.deserialize;
