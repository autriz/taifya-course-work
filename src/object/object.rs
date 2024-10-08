use crate::ast::{Expression, Identifier, IdentifierType};

pub const TRUE: Object = Object::Value(Value::Boolean { value: true });
pub const FALSE: Object = Object::Value(Value::Boolean { value: false });
pub const NULL: Object = Object::Null;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer {
        value: i64
    },
    Float {
        value: f64,
    },
    String {
        value: String,
    },
    Boolean {
        value: bool
    },
}

impl Value {
    pub fn _type(&self) -> ValueType {
        match self {
            Self::Integer { .. } => ValueType::Integer,
            Self::Float { .. } => ValueType::Float,
            Self::String { .. } => ValueType::String,
            Self::Boolean { .. } => ValueType::Boolean
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueType {
    Integer,
    Float,
    String,
    Boolean
}

impl From<IdentifierType> for ValueType {
    fn from(value: IdentifierType) -> Self {
        match value {
            IdentifierType::Int => ValueType::Integer,
            IdentifierType::Float => ValueType::Float,
            IdentifierType::String => ValueType::String,
            IdentifierType::Bool => ValueType::Boolean,
            IdentifierType::Undefined => unreachable!("value type should not be undefined")
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Value(Value),
    Function {
        function: FunctionType,
    },
    Error {
        message: String,
    },
    Null
}

impl Object {
    pub fn _type(&self) -> ObjectType {
        match self {
            Self::Value(value) => ObjectType::Value(value._type()),
            Self::Function { .. } => ObjectType::Function,
            Self::Error { .. } => ObjectType::Error,
            Self::Null => ObjectType::Null
        }
    }
}

#[derive(Debug)]
pub enum ObjectType {
    Value(ValueType),
    Function,
    Error,
    Null
}

#[derive(Debug, Clone, PartialEq)]
pub enum FunctionType {
    Writeln {
        params: Vec<Expression>,
    },
    Readln {
        params: Vec<Identifier>
    },
}

