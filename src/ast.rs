use std::iter::Peekable;

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
pub enum Identifier {
    Simple(SimpleIdentifier),
    Common(CommonIdentifier),
    Proper(ProperIdentifier),
    Pronoun,
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
    Identifier(Identifier),
}

impl From<LiteralExpression> for PrimaryExpression {
    fn from(expr: LiteralExpression) -> Self {
        PrimaryExpression::Literal(expr)
    }
}

impl<I: Into<Identifier>> From<I> for PrimaryExpression {
    fn from(id: I) -> Self {
        PrimaryExpression::Identifier(id.into())
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
    pub rhs: Box<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Primary(PrimaryExpression),
    Binary(BinaryExpression),
    Unary(UnaryExpression),
}

impl<E: Into<PrimaryExpression>> From<E> for Expression {
    fn from(expr: E) -> Self {
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

#[derive(Debug, PartialEq)]
pub struct Assignment {
    pub dest: Identifier,
    pub value: Box<Expression>,
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

#[derive(Debug, PartialEq)]
pub enum PoeticNumberAssignmentRHS {
    Expression(Box<Expression>),
    PoeticNumberLiteral(PoeticNumberLiteral),
}

impl From<Box<Expression>> for PoeticNumberAssignmentRHS {
    fn from(e: Box<Expression>) -> Self {
        PoeticNumberAssignmentRHS::Expression(e)
    }
}

impl From<PoeticNumberLiteral> for PoeticNumberAssignmentRHS {
    fn from(p: PoeticNumberLiteral) -> Self {
        PoeticNumberAssignmentRHS::PoeticNumberLiteral(p)
    }
}

#[derive(Debug, PartialEq)]
pub struct PoeticNumberAssignment {
    pub dest: Identifier,
    pub rhs: PoeticNumberAssignmentRHS,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PoeticStringAssignment {
    pub dest: Identifier,
    pub rhs: String,
}

#[derive(Debug, PartialEq)]
pub enum PoeticAssignment {
    Number(PoeticNumberAssignment),
    String(PoeticStringAssignment),
}

impl From<PoeticNumberAssignment> for PoeticAssignment {
    fn from(p: PoeticNumberAssignment) -> Self {
        PoeticAssignment::Number(p)
    }
}

impl From<PoeticStringAssignment> for PoeticAssignment {
    fn from(p: PoeticStringAssignment) -> Self {
        PoeticAssignment::String(p)
    }
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
    pub dest: Option<Identifier>,
}

#[derive(Debug, PartialEq)]
pub struct Output {
    pub value: Box<Expression>,
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
    pub operand: Expression,
    pub dest: Option<Identifier>,
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

#[derive(Debug, PartialEq)]
pub enum Statement {
    Assignment(Assignment),
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
}

impl From<Assignment> for Statement {
    fn from(a: Assignment) -> Self {
        Statement::Assignment(a)
    }
}

impl<P: Into<PoeticAssignment>> From<P> for Statement {
    fn from(p: P) -> Self {
        Statement::PoeticAssignment(p.into())
    }
}

impl From<If> for Statement {
    fn from(i: If) -> Self {
        Statement::If(i)
    }
}

impl From<While> for Statement {
    fn from(w: While) -> Self {
        Statement::While(w)
    }
}

impl From<Until> for Statement {
    fn from(u: Until) -> Self {
        Statement::Until(u)
    }
}

impl From<Inc> for Statement {
    fn from(i: Inc) -> Self {
        Statement::Inc(i)
    }
}

impl From<Dec> for Statement {
    fn from(d: Dec) -> Self {
        Statement::Dec(d)
    }
}

impl From<Input> for Statement {
    fn from(i: Input) -> Self {
        Statement::Input(i)
    }
}

impl From<Output> for Statement {
    fn from(o: Output) -> Self {
        Statement::Output(o)
    }
}

impl From<Mutation> for Statement {
    fn from(m: Mutation) -> Self {
        Statement::Mutation(m)
    }
}

impl From<Rounding> for Statement {
    fn from(r: Rounding) -> Self {
        Statement::Rounding(r)
    }
}

#[derive(Debug, PartialEq)]
pub struct StatementWithLine(pub Statement, pub usize);

#[derive(Debug, PartialEq)]
pub struct Block(pub Vec<StatementWithLine>);

impl Block {
    pub fn empty() -> Self {
        Self(Vec::new())
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

fn find_or_end<I, F, T>(mut iter: I, pred: F) -> usize
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
            find_or_end(self.iter(), |e| *e == PoeticNumberLiteralIteratorItem::Dot) as i32 - 1;
        self.iter()
            .filter(|e| *e != PoeticNumberLiteralIteratorItem::Dot)
            .enumerate()
            .map(|(idx, item)| {
                let length = match item {
                    PoeticNumberLiteralIteratorItem::Dot => unreachable!(),
                    PoeticNumberLiteralIteratorItem::Word(s) => s.len(),
                    PoeticNumberLiteralIteratorItem::SuffixedWord(s0, s1) => {
                        s0.len() + s1.iter().fold(0, |a, x| a + x.len())
                    }
                };
                (length % 10) as f64 * Self::ten_to_the(exponent - idx as i32)
            })
            .fold(0.0, |a, b| a + b)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn poetic_number_literal_iterator() {
        fn items<'a>(
            elems: &'a Vec<PoeticNumberLiteralElem>,
        ) -> Vec<PoeticNumberLiteralIteratorItem<'a>> {
            PoeticNumberLiteralIterator::new(elems.iter()).collect()
        }

        assert_eq!(
            items(&vec![PoeticNumberLiteralElem::Dot]),
            vec![PoeticNumberLiteralIteratorItem::Dot]
        );
        assert_eq!(
            items(&vec![PoeticNumberLiteralElem::Word("foo".into())]),
            vec![PoeticNumberLiteralIteratorItem::Word(&"foo".into())]
        );
        assert_eq!(
            items(&vec![
                PoeticNumberLiteralElem::Word("foo".into()),
                PoeticNumberLiteralElem::WordSuffix("'s".into())
            ]),
            vec![PoeticNumberLiteralIteratorItem::SuffixedWord(
                &"foo".into(),
                vec![&"'s".into()],
            )]
        );
    }

    #[test]
    fn test_find_or_end() {
        assert_eq!(find_or_end(vec![].into_iter(), |x: &i32| *x == 1), 0);
        assert_eq!(find_or_end(vec![1, 2, 3].into_iter(), |x| *x == 1), 0);
        assert_eq!(find_or_end(vec![1, 2, 3].into_iter(), |x| *x == 2), 1);
        assert_eq!(find_or_end(vec![1, 2, 3].into_iter(), |x| *x == 3), 2);
        assert_eq!(find_or_end(vec![1, 2, 3].into_iter(), |x| *x == 4), 3);
        assert_eq!(find_or_end(vec![1, 2, 3].into_iter(), |x| *x == 9), 3);
    }
}
