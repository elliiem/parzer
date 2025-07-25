pub const ParseError = error{
    InvalidNumber,
    InvalidString,
    InvalidField,
    UnexpectedToken,
    InvalidToken,
    ExpectedToken,
};

pub const DeserializeError = error{
    ExpectedBool,
    ExpectedOptionalBool,
    ExpectedNumber,
    ExpectedOptionalNumber,
    ExpectedString,
    ExpectedOptionalString,
    ExpectedField,
    ExpectedObject,
    ExpectedOptionalObject,
    ExpectedArray,
    ExpectedOptionalArray,
    ExpectedEnum,
    ExpectedOptionalEnum,
    ExpectedUnion,
    ExpectedOptionalUnion,
    ExpectedTag,
    UnknownTag,
    ArrayTooShort,
    ArrayTooLong,
    TrailingComma,
    MissingComma,
    MissingArrayItem,
    MissingField,
    UnknownField,
    DuplicateField,
    IllegalWhitespace,
} || ParseError;
