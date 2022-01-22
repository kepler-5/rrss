use std::{hint::unreachable_unchecked, iter::once, iter::Peekable, sync::Arc};

use crate::frontend::source_range::SourceRange;

use derive_more::{From, IsVariant};

use super::source_range::{Line, Range, SourceLocation};

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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WithLoc<T>(pub T, pub SourceLocation);

impl<T> WithLoc<T> {
    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> WithLoc<U> {
        WithLoc(f(self.0), self.1)
    }
    pub fn as_ref(&self) -> WithLoc<&T> {
        WithLoc(&self.0, self.1)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WithRange<T>(pub T, pub SourceRange);

impl<T> WithRange<T> {
    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> WithRange<U> {
        WithRange(f(self.0), self.1)
    }
    pub fn as_ref(&self) -> WithRange<&T> {
        WithRange(&self.0, self.1.clone())
    }
}

impl<T> Range for WithRange<T> {
    fn range(&self) -> SourceRange {
        self.1.clone()
    }
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SimpleIdentifier(pub String);
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CommonIdentifier(pub String, pub String);
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
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
impl From<WithRange<SimpleIdentifier>> for WithRange<VariableName> {
    fn from(expr: WithRange<SimpleIdentifier>) -> Self {
        expr.map(Into::into)
    }
}

impl From<CommonIdentifier> for VariableName {
    fn from(expr: CommonIdentifier) -> Self {
        VariableName::Common(expr)
    }
}
impl From<WithRange<CommonIdentifier>> for WithRange<VariableName> {
    fn from(expr: WithRange<CommonIdentifier>) -> Self {
        expr.map(Into::into)
    }
}

impl From<ProperIdentifier> for VariableName {
    fn from(expr: ProperIdentifier) -> Self {
        VariableName::Proper(expr)
    }
}
impl From<WithRange<ProperIdentifier>> for WithRange<VariableName> {
    fn from(expr: WithRange<ProperIdentifier>) -> Self {
        expr.map(Into::into)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Identifier {
    VariableName(VariableName),
    Pronoun,
}

bridging_from!(for Identifier: VariableName);

impl<T: Into<VariableName>> From<WithRange<T>> for WithRange<Identifier> {
    fn from(x: WithRange<T>) -> Self {
        x.map(|x| Identifier::VariableName(x.into()))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ArraySubscript {
    pub array: Box<PrimaryExpression>,
    pub subscript: Box<PrimaryExpression>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionCall {
    pub name: WithRange<VariableName>,
    pub args: Vec<PrimaryExpression>,
}

#[derive(Clone, Debug, From, PartialEq)]
pub enum PrimaryExpression {
    Literal(WithRange<LiteralExpression>),
    #[from(ignore)]
    Identifier(WithRange<Identifier>),
    ArraySubscript(ArraySubscript),
    FunctionCall(FunctionCall),
}

impl<T: Into<Identifier>> From<WithRange<T>> for PrimaryExpression {
    fn from(x: WithRange<T>) -> Self {
        PrimaryExpression::Identifier(x.map(Into::into))
    }
}

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

    pub fn iter(&self) -> impl Iterator<Item = &Expression> {
        once(&self.first).chain(self.rest.iter())
    }

    pub fn len(&self) -> usize {
        1 + self.rest.len()
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
    Identifier(WithRange<Identifier>),
    ArraySubscript(ArraySubscript),
}

impl<T: Into<Identifier>> From<WithRange<T>> for AssignmentLHS {
    fn from(x: WithRange<T>) -> Self {
        AssignmentLHS::Identifier(x.map(Into::into))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ArrayPopExpr {
    pub array: PrimaryExpression,
}

impl<P: Into<PrimaryExpression>> From<P> for ArrayPopExpr {
    fn from(p: P) -> Self {
        Self { array: p.into() }
    }
}

#[derive(Clone, Debug, From, PartialEq)]
pub enum AssignmentRHS {
    #[from(ignore)]
    ExpressionList(ExpressionList),
    ArrayPop(ArrayPopExpr),
}

bridging_from!(for AssignmentRHS: ExpressionList);

#[derive(Clone, Debug, PartialEq)]
pub struct Assignment {
    pub dest: AssignmentLHS,
    pub value: AssignmentRHS,
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
    pub dest: WithRange<Identifier>,
    pub amount: isize,
}

#[derive(Debug, PartialEq)]
pub struct Dec {
    pub dest: WithRange<Identifier>,
    pub amount: isize,
}

#[derive(Debug, From, PartialEq)]
pub enum InputDest {
    Some(AssignmentLHS),
    None(SourceLocation),
}

impl InputDest {
    pub fn opt(&self) -> Option<&AssignmentLHS> {
        match self {
            InputDest::Some(x) => Some(&x),
            InputDest::None(_) => None,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Input {
    pub dest: InputDest,
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
    pub value: Option<ArrayPushRHS>,
}

#[derive(Debug, PartialEq)]
pub struct ArrayPop {
    pub expr: ArrayPopExpr,
    pub dest: Option<AssignmentLHS>,
}

#[derive(Debug, PartialEq)]
pub struct Return {
    pub value: Expression,
}

#[derive(Debug, PartialEq)]
pub struct FunctionData {
    pub params: Vec<WithRange<VariableName>>,
    pub body: Block,
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub name: WithRange<VariableName>,
    pub data: Arc<FunctionData>,
}

#[derive(Debug, PartialEq)]
pub struct Continue(pub SourceRange);

#[derive(Debug, PartialEq)]
pub struct Break(pub SourceRange);

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
    Continue(Continue),
    Break(Break),
    ArrayPush(ArrayPush),
    ArrayPop(ArrayPop),
    Return(Return),
    Function(Function),
    FunctionCall(FunctionCall),
}

bridging_from!(for Statement: PoeticAssignment);

#[derive(Debug, IsVariant, PartialEq)]
pub enum Block {
    Empty(SourceLocation),
    NonEmpty(Vec<Statement>),
}

impl Block {
    pub fn new(loc: SourceLocation, statements: Vec<Statement>) -> Self {
        if statements.is_empty() {
            Self::Empty(loc)
        } else {
            Self::NonEmpty(statements)
        }
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
                    PoeticNumberLiteralIteratorItem::Dot => unsafe { unreachable_unchecked() },
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

/////////////// impl Range

fn head_tail_range(head: &impl Range, tail: &Vec<impl Range>) -> SourceRange {
    let head_range = head.range();
    if let Some(last) = tail.last() {
        head_range.concat(last.range())
    } else {
        head_range
    }
}

impl Range for PrimaryExpression {
    fn range(&self) -> SourceRange {
        match self {
            PrimaryExpression::Literal(x) => x.range(),
            PrimaryExpression::Identifier(x) => x.range(),
            PrimaryExpression::ArraySubscript(x) => x.range(),
            PrimaryExpression::FunctionCall(x) => x.range(),
        }
    }
}

impl Range for ArraySubscript {
    fn range(&self) -> SourceRange {
        self.array.range().concat(self.subscript.range())
    }
}

impl Range for FunctionCall {
    fn range(&self) -> SourceRange {
        head_tail_range(&self.name, &self.args)
    }
}

impl Range for Expression {
    fn range(&self) -> SourceRange {
        match self {
            Expression::PrimaryExpression(x) => x.range(),
            Expression::BinaryExpression(x) => x.range(),
            Expression::UnaryExpression(x) => x.range(),
        }
    }
}

impl Range for BinaryExpression {
    fn range(&self) -> SourceRange {
        self.lhs.range().concat(self.rhs.range())
    }
}

impl Range for UnaryExpression {
    fn range(&self) -> SourceRange {
        self.operand.range()
    }
}

impl Range for ExpressionList {
    fn range(&self) -> SourceRange {
        head_tail_range(&self.first, &self.rest)
    }
}

impl Range for AssignmentLHS {
    fn range(&self) -> SourceRange {
        match self {
            AssignmentLHS::Identifier(x) => x.range(),
            AssignmentLHS::ArraySubscript(x) => x.range(),
        }
    }
}

impl Range for ArrayPopExpr {
    fn range(&self) -> SourceRange {
        self.array.range()
    }
}

impl Range for AssignmentRHS {
    fn range(&self) -> SourceRange {
        match self {
            AssignmentRHS::ExpressionList(e) => e.range(),
            AssignmentRHS::ArrayPop(p) => p.range(),
        }
    }
}

/////////////// impl Line

impl Line for Statement {
    fn line(&self) -> u32 {
        match self {
            Statement::Assignment(x) => x.line(),
            Statement::PoeticAssignment(x) => x.line(),
            Statement::If(x) => x.line(),
            Statement::While(x) => x.line(),
            Statement::Until(x) => x.line(),
            Statement::Inc(x) => x.line(),
            Statement::Dec(x) => x.line(),
            Statement::Input(x) => x.line(),
            Statement::Output(x) => x.line(),
            Statement::Mutation(x) => x.line(),
            Statement::Rounding(x) => x.line(),
            Statement::Continue(x) => x.line(),
            Statement::Break(x) => x.line(),
            Statement::ArrayPush(x) => x.line(),
            Statement::ArrayPop(x) => x.line(),
            Statement::Return(x) => x.line(),
            Statement::Function(x) => x.line(),
            Statement::FunctionCall(x) => x.line(),
        }
    }
}

impl Line for Assignment {
    fn line(&self) -> u32 {
        self.value.line()
    }
}

impl Line for PoeticAssignment {
    fn line(&self) -> u32 {
        match self {
            PoeticAssignment::Number(x) => x.line(),
            PoeticAssignment::String(x) => x.line(),
        }
    }
}

impl Line for PoeticNumberAssignment {
    fn line(&self) -> u32 {
        self.dest.line()
    }
}

impl Line for PoeticStringAssignment {
    fn line(&self) -> u32 {
        self.dest.line()
    }
}

impl Line for If {
    fn line(&self) -> u32 {
        self.condition.line()
    }
}

impl Line for While {
    fn line(&self) -> u32 {
        self.condition.line()
    }
}

impl Line for Until {
    fn line(&self) -> u32 {
        self.condition.line()
    }
}

impl Line for Inc {
    fn line(&self) -> u32 {
        self.dest.line()
    }
}

impl Line for Dec {
    fn line(&self) -> u32 {
        self.dest.line()
    }
}

impl Line for Input {
    fn line(&self) -> u32 {
        match &self.dest {
            InputDest::Some(dest) => dest.line(),
            InputDest::None(loc) => loc.line,
        }
    }
}

impl Line for Output {
    fn line(&self) -> u32 {
        self.value.line()
    }
}

impl Line for Mutation {
    fn line(&self) -> u32 {
        self.operand.line()
    }
}

impl Line for Rounding {
    fn line(&self) -> u32 {
        self.operand.line()
    }
}

impl Line for Continue {
    fn line(&self) -> u32 {
        self.0.line()
    }
}

impl Line for Break {
    fn line(&self) -> u32 {
        self.0.line()
    }
}

impl Line for ArrayPush {
    fn line(&self) -> u32 {
        self.array.line()
    }
}

impl Line for ArrayPop {
    fn line(&self) -> u32 {
        self.expr.line()
    }
}

impl Line for Return {
    fn line(&self) -> u32 {
        self.value.line()
    }
}

impl Line for Function {
    fn line(&self) -> u32 {
        self.name.line()
    }
}

impl Line for Block {
    fn line(&self) -> u32 {
        match self {
            Block::Empty(loc) => loc.line,
            Block::NonEmpty(s) => s.first().unwrap().line(),
        }
    }
}
