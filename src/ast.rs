#[derive(Debug, PartialEq)]
pub struct Program {
    pub code: Vec<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum LiteralExpression {
    Boolean(bool),
    Null,
    Number(f64),
    String(String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum PrimaryExpression {
    Literal(LiteralExpression),
}

impl From<LiteralExpression> for PrimaryExpression {
    fn from(expr: LiteralExpression) -> Self {
        PrimaryExpression::Literal(expr)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Multiply,
    Divide,
}

#[derive(Debug, PartialEq)]
pub struct BinaryExpression {
    pub operator: BinaryOperator,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Primary(PrimaryExpression),
    Binary(BinaryExpression),
}

impl From<PrimaryExpression> for Expression {
    fn from(expr: PrimaryExpression) -> Self {
        Expression::Primary(expr)
    }
}

impl From<LiteralExpression> for Expression {
    fn from(expr: LiteralExpression) -> Self {
        Expression::Primary(expr.into())
    }
}

impl From<BinaryExpression> for Expression {
    fn from(expr: BinaryExpression) -> Self {
        Expression::Binary(expr)
    }
}
