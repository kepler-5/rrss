use std::iter::Peekable;

use derive_more::From;

#[cfg(test)]
mod tests;

macro_rules! bridging_from {
    (for $for_struct:ty: $($from_struct:ident),+) => {
        $(impl<T: Into<$from_struct>> From<T> for $for_struct {
            fn from(x: T) -> Self {
                Self::$from_struct(x.into())
            }
        })+
    };
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub code: Vec<Block>,
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
pub enum VariableName {
    Simple(SimpleIdentifier),
    Common(CommonIdentifier),
    Proper(ProperIdentifier),
}

impl From<SimpleIdentifier> for VariableName {
    fn from(expr: SimpleIdentifier) -> Self {
        VariableName::Simple(expr)
    }
}

impl From<CommonIdentifier> for VariableName {
    fn from(expr: CommonIdentifier) -> Self {
        VariableName::Common(expr)
    }
}

impl From<ProperIdentifier> for VariableName {
    fn from(expr: ProperIdentifier) -> Self {
        VariableName::Proper(expr)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Identifier {
    VariableName(VariableName),
    Pronoun,
}

bridging_from!(for Identifier: VariableName);

#[derive(Clone, Debug, PartialEq)]
pub struct ArraySubscript {
    pub array: Box<PrimaryExpression>,
    pub subscript: Box<PrimaryExpression>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionCall {
    pub name: VariableName,
    pub args: Vec<PrimaryExpression>,
}

#[derive(Clone, Debug, From, PartialEq)]
pub enum PrimaryExpression {
    Literal(LiteralExpression),
    #[from(ignore)]
    Identifier(Identifier),
    ArraySubscript(ArraySubscript),
    FunctionCall(FunctionCall),
}

bridging_from!(for PrimaryExpression: Identifier);

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum UnaryOperator {
    Minus,
    Not,
}

#[derive(Clone, Debug, PartialEq)]
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
    And,
    Or,
    Nor,
    Eq,
    NotEq,
    Greater,
    GreaterEq,
    Less,
    LessEq,
}

#[derive(Clone, Debug, PartialEq)]
pub struct BinaryExpression {
    pub operator: BinaryOperator,
    pub lhs: Box<Expression>,
    pub rhs: Box<ExpressionList>,
}

#[derive(Clone, Debug, From, PartialEq)]
pub enum Expression {
    #[from(ignore)]
    PrimaryExpression(PrimaryExpression),
    BinaryExpression(BinaryExpression),
    UnaryExpression(UnaryExpression),
}

bridging_from!(for Expression: PrimaryExpression);

#[derive(Clone, Debug, PartialEq)]
pub struct ExpressionList {
    pub first: Expression,
    pub rest: Vec<Expression>,
}

impl ExpressionList {
    pub fn has_multiple(&self) -> bool {
        !self.rest.is_empty()
    }
}

impl<E: Into<Expression>> From<E> for ExpressionList {
    fn from(e: E) -> Self {
        Self {
            first: e.into(),
            rest: Vec::new(),
        }
    }
}

#[derive(Clone, Debug, From, PartialEq)]
pub enum AssignmentLHS {
    #[from(ignore)]
    Identifier(Identifier),
    ArraySubscript(ArraySubscript),
}

bridging_from!(for AssignmentLHS: Identifier);

#[derive(Clone, Debug, PartialEq)]
pub struct Assignment {
    pub dest: AssignmentLHS,
    pub value: ExpressionList,
    pub operator: Option<BinaryOperator>, // for compound assignments
}

#[derive(Clone, Debug, PartialEq)]
pub enum PoeticNumberLiteralElem {
    Word(String),
    WordSuffix(String),
    Dot,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PoeticNumberLiteral {
    pub elems: Vec<PoeticNumberLiteralElem>,
}

#[derive(Debug, From, PartialEq)]
pub enum PoeticNumberAssignmentRHS {
    #[from(ignore)]
    Expression(Expression),
    PoeticNumberLiteral(PoeticNumberLiteral),
}

bridging_from!(for PoeticNumberAssignmentRHS: Expression);

#[derive(Debug, PartialEq)]
pub struct PoeticNumberAssignment {
    pub dest: AssignmentLHS,
    pub rhs: PoeticNumberAssignmentRHS,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PoeticStringAssignment {
    pub dest: AssignmentLHS,
    pub rhs: String,
}

#[derive(Debug, From, PartialEq)]
pub enum PoeticAssignment {
    Number(PoeticNumberAssignment),
    String(PoeticStringAssignment),
}

#[derive(Debug, PartialEq)]
pub struct If {
    pub condition: Expression,
    pub then_block: Block,
    pub else_block: Option<Block>,
}

#[derive(Debug, PartialEq)]
pub struct While {
    pub condition: Expression,
    pub block: Block,
}

#[derive(Debug, PartialEq)]
pub struct Until {
    pub condition: Expression,
    pub block: Block,
}

#[derive(Debug, PartialEq)]
pub struct Inc {
    pub dest: Identifier,
    pub amount: usize,
}

#[derive(Debug, PartialEq)]
pub struct Dec {
    pub dest: Identifier,
    pub amount: usize,
}

#[derive(Debug, PartialEq)]
pub struct Input {
    pub dest: Option<AssignmentLHS>,
}

#[derive(Debug, PartialEq)]
pub struct Output {
    pub value: Expression,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum MutationOperator {
    Cut,
    Join,
    Cast,
}

#[derive(Debug, PartialEq)]
pub struct Mutation {
    pub operator: MutationOperator,
    pub operand: PrimaryExpression,
    pub dest: Option<AssignmentLHS>,
    pub param: Option<Expression>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum RoundingDirection {
    Up,
    Down,
    Nearest,
}

#[derive(Debug, PartialEq)]
pub struct Rounding {
    pub direction: RoundingDirection,
    pub operand: Expression,
}

#[derive(Debug, From, PartialEq)]
pub enum ArrayPushRHS {
    #[from(ignore)]
    ExpressionList(ExpressionList),
    PoeticNumberLiteral(PoeticNumberLiteral),
}

bridging_from!(for ArrayPushRHS: ExpressionList);

#[derive(Debug, PartialEq)]
pub struct ArrayPush {
    pub array: PrimaryExpression,
    pub value: ArrayPushRHS,
}

#[derive(Debug, PartialEq)]
pub struct ArrayPop {
    pub array: PrimaryExpression,
    pub dest: Option<AssignmentLHS>,
}

#[derive(Debug, PartialEq)]
pub struct Return {
    pub value: Expression,
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub name: VariableName,
    pub params: Vec<VariableName>,
    pub body: Block,
}

#[derive(Debug, From, PartialEq)]
pub enum Statement {
    Assignment(Assignment),
    #[from(ignore)]
    PoeticAssignment(PoeticAssignment),
    If(If),
    While(While),
    Until(Until),
    Inc(Inc),
    Dec(Dec),
    Input(Input),
    Output(Output),
    Mutation(Mutation),
    Rounding(Rounding),
    Continue,
    Break,
    ArrayPush(ArrayPush),
    ArrayPop(ArrayPop),
    Return(Return),
    Function(Function),
    FunctionCall(FunctionCall),
}

bridging_from!(for Statement: PoeticAssignment);

#[derive(Debug, PartialEq)]
pub struct StatementWithLine(pub Statement, pub usize);

#[derive(Debug, PartialEq)]
pub struct Block(pub Vec<StatementWithLine>);

impl Block {
    pub fn empty() -> Self {
        Self(Vec::new())
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

/////////////// PoeticNumberLiteral::compute_value

#[derive(Clone, Debug, PartialEq, Eq)]
enum PoeticNumberLiteralIteratorItem<'a> {
    Word(&'a String),
    SuffixedWord(&'a String, Vec<&'a String>),
    Dot,
}

struct PoeticNumberLiteralIterator<'a, T>
where
    T: Iterator<Item = &'a PoeticNumberLiteralElem>,
{
    iter: Peekable<T>,
}

impl<'a, T> PoeticNumberLiteralIterator<'a, T>
where
    T: Iterator<Item = &'a PoeticNumberLiteralElem>,
{
    fn new(iter: T) -> Self {
        Self {
            iter: iter.peekable(),
        }
    }

    fn greedily_match_suffixes(&mut self) -> Vec<&'a String> {
        let extract_suffix = |elem: &'a PoeticNumberLiteralElem| match &elem {
            PoeticNumberLiteralElem::WordSuffix(s) => Some(s),
            _ => None,
        };
        // TODO clean up with itertools take_while_ref?
        let mut suffixes = vec![extract_suffix(self.iter.next().unwrap()).unwrap()];
        while let Some(suffix) = self.iter.peek().and_then(|e| extract_suffix(e)) {
            self.iter.next();
            suffixes.push(suffix);
        }
        suffixes
    }
}

impl<'a, T> Iterator for PoeticNumberLiteralIterator<'a, T>
where
    T: Iterator<Item = &'a PoeticNumberLiteralElem>,
{
    type Item = PoeticNumberLiteralIteratorItem<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|e| match e {
            PoeticNumberLiteralElem::Dot => PoeticNumberLiteralIteratorItem::Dot,
            PoeticNumberLiteralElem::Word(s) => self
                .iter
                .peek()
                .filter(|e| matches!(e, PoeticNumberLiteralElem::WordSuffix(_)))
                .is_some()
                .then(|| {
                    PoeticNumberLiteralIteratorItem::SuffixedWord(s, self.greedily_match_suffixes())
                })
                .unwrap_or_else(|| PoeticNumberLiteralIteratorItem::Word(s)),
            PoeticNumberLiteralElem::WordSuffix(_) => unreachable!(),
        })
    }
}

fn position_or_end<I, F, T>(mut iter: I, pred: F) -> usize
where
    I: Iterator<Item = T>,
    F: Fn(&T) -> bool,
{
    let mut n = 0usize;
    while let Some(val) = iter.next() {
        if pred(&val) {
            break;
        } else {
            n += 1
        }
    }
    n
}

impl PoeticNumberLiteral {
    fn iter(&self) -> PoeticNumberLiteralIterator<impl Iterator<Item = &PoeticNumberLiteralElem>> {
        PoeticNumberLiteralIterator::new(self.elems.iter())
    }

    fn ten_to_the(n: i32) -> f64 {
        // TODO optimize small N's with lookup table?
        10.0f64.powi(n)
    }

    pub fn compute_value(&self) -> f64 {
        let exponent =
            position_or_end(self.iter(), |e| *e == PoeticNumberLiteralIteratorItem::Dot) as i32 - 1;
        self.iter()
            .filter(|e| *e != PoeticNumberLiteralIteratorItem::Dot)
            .enumerate()
            .map(|(idx, item)| {
                let length = match item {
                    PoeticNumberLiteralIteratorItem::Dot => unreachable!(),
                    PoeticNumberLiteralIteratorItem::Word(s) => s.len(),
                    PoeticNumberLiteralIteratorItem::SuffixedWord(s0, s1) => {
                        s1.iter().map(|x| x.len()).fold(s0.len(), |a, b| a + b)
                    }
                };
                (length % 10) as f64 * Self::ten_to_the(exponent - idx as i32)
            })
            .sum()
    }
}