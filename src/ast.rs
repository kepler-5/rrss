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
pub struct SimpleIdentifier(pub String);
#[derive(Clone, Debug, PartialEq)]
pub struct CommonIdentifier(pub String, pub String);
#[derive(Clone, Debug, PartialEq)]
pub struct ProperIdentifier(pub Vec<String>);

#[derive(Clone, Debug, PartialEq)]
pub enum Identifier {
    Simple(SimpleIdentifier),
    Common(CommonIdentifier),
    Proper(ProperIdentifier),
}

impl From<SimpleIdentifier> for Identifier {
    fn from(expr: SimpleIdentifier) -> Self {
        Identifier::Simple(expr)
    }
}

impl From<CommonIdentifier> for Identifier {
    fn from(expr: CommonIdentifier) -> Self {
        Identifier::Common(expr)
    }
}

impl From<ProperIdentifier> for Identifier {
    fn from(expr: ProperIdentifier) -> Self {
        Identifier::Proper(expr)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum PrimaryExpression {
    Literal(LiteralExpression),
    Pronoun,
    Identifier(Identifier),
}

impl From<LiteralExpression> for PrimaryExpression {
    fn from(expr: LiteralExpression) -> Self {
        PrimaryExpression::Literal(expr)
    }
}

impl From<Identifier> for PrimaryExpression {
    fn from(expr: Identifier) -> Self {
        PrimaryExpression::Identifier(expr)
    }
}

impl From<SimpleIdentifier> for PrimaryExpression {
    fn from(expr: SimpleIdentifier) -> Self {
        PrimaryExpression::Identifier(expr.into())
    }
}

impl From<CommonIdentifier> for PrimaryExpression {
    fn from(expr: CommonIdentifier) -> Self {
        PrimaryExpression::Identifier(expr.into())
    }
}

impl From<ProperIdentifier> for PrimaryExpression {
    fn from(expr: ProperIdentifier) -> Self {
        PrimaryExpression::Identifier(expr.into())
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum UnaryOperator {
    Minus,
}

#[derive(Debug, PartialEq)]
pub struct UnaryExpression {
    pub operator: UnaryOperator,
    pub operand: Box<Expression>,
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
    Unary(UnaryExpression),
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

impl From<UnaryExpression> for Expression {
    fn from(expr: UnaryExpression) -> Self {
        Expression::Unary(expr)
    }
}

impl From<BinaryExpression> for Expression {
    fn from(expr: BinaryExpression) -> Self {
        Expression::Binary(expr)
    }
}
