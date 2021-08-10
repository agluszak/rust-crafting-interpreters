pub enum Expr {
    Binary(Box<Expr>, BinaryOperator, Box<Expr>),
    Grouping(Box<Expr>),
    Literal(Literal),
    Unary(UnaryOperator, Box<Expr>),
    Variable(String),
}

pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    NotEqual,
    And,
    Or,
}

pub enum UnaryOperator {
    BooleanNegate,
    NumericNegate,
}

pub enum Literal {
    String(String),
    Number(f64),
    Boolean(bool),
    Nil,
}
