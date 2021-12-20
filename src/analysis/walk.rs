use std::iter;

use crate::frontend::ast::*;

#[cfg(test)]
mod tests;

pub trait Combine {
    fn combine(self, other: Self) -> Self;
}

impl Combine for () {
    fn combine(self, _: Self) -> Self {
        ()
    }
}

pub fn combine_all<I, T, E>(mut iter: I) -> std::result::Result<T, E>
where
    I: Iterator<Item = std::result::Result<T, E>>,
    T: Combine + Default,
{
    iter.try_fold(T::default(), |acc, x| x.map(|x| acc.combine(x)))
}

#[inline]
pub fn leaf<T, U: Default, E>(_: T) -> std::result::Result<U, E> {
    Ok(Default::default())
}

pub trait Visitor {
    type Output: Combine + Default;
    type Error;

    fn visit_program(&mut self, p: &Program) -> Result<Self> {
        combine_all(p.code.iter().map(|b| self.visit_block(b)))
    }

    fn visit_block(&mut self, b: &Block) -> Result<Self> {
        combine_all(b.0.iter().map(|sl| self.visit_statement(&sl.0)))
    }

    fn visit_statement(&mut self, s: &Statement) -> Result<Self> {
        match s {
            Statement::Assignment(a) => self.visit_assignment(a),
            Statement::PoeticAssignment(p) => self.visit_poetic_assignment(p),
            Statement::If(i) => self.visit_if(i),
            Statement::While(w) => self.visit_while(w),
            Statement::Until(u) => self.visit_until(u),
            Statement::Inc(i) => self.visit_inc(i),
            Statement::Dec(d) => self.visit_dec(d),
            Statement::Input(i) => self.visit_input(i),
            Statement::Output(o) => self.visit_output(o),
            Statement::Mutation(m) => self.visit_mutation(m),
            Statement::Rounding(r) => self.visit_rounding(r),
            Statement::Continue => self.visit_continue(),
            Statement::Break => self.visit_break(),
            Statement::ArrayPush(a) => self.visit_array_push(a),
            Statement::ArrayPop(a) => self.visit_array_pop(a),
            Statement::Return(r) => self.visit_return(r),
            Statement::Function(f) => self.visit_function(f),
            Statement::FunctionCall(f) => self.visit_function_call(f),
        }
    }

    // Statements
    fn visit_assignment(&mut self, a: &Assignment) -> Result<Self> {
        Ok(self
            .visit_assignment_lhs(&a.dest)?
            .combine(
                a.operator
                    .map_or_else(|| leaf(()), |o| self.visit_binary_operator(o))?,
            )
            .combine(self.visit_expression_list(&a.value)?))
    }
    fn visit_poetic_assignment(&mut self, p: &PoeticAssignment) -> Result<Self> {
        match p {
            PoeticAssignment::Number(a) => self.visit_poetic_number_assignment(a),
            PoeticAssignment::String(a) => self.visit_poetic_string_assignment(a),
        }
    }
    fn visit_if(&mut self, i: &If) -> Result<Self> {
        Ok(self
            .visit_expression(&i.condition)?
            .combine(self.visit_block(&i.then_block)?)
            .combine(
                i.else_block
                    .as_ref()
                    .map_or_else(|| leaf(()), |b| self.visit_block(b))?,
            ))
    }
    fn visit_while(&mut self, w: &While) -> Result<Self> {
        Ok(self
            .visit_expression(&w.condition)?
            .combine(self.visit_block(&w.block)?))
    }
    fn visit_until(&mut self, u: &Until) -> Result<Self> {
        Ok(self
            .visit_expression(&u.condition)?
            .combine(self.visit_block(&u.block)?))
    }
    fn visit_inc(&mut self, i: &Inc) -> Result<Self> {
        self.visit_identifier(&i.dest)
    }
    fn visit_dec(&mut self, d: &Dec) -> Result<Self> {
        self.visit_identifier(&d.dest)
    }
    fn visit_input(&mut self, i: &Input) -> Result<Self> {
        i.dest
            .as_ref()
            .map_or_else(|| leaf(()), |lhs| self.visit_assignment_lhs(lhs))
    }
    fn visit_output(&mut self, o: &Output) -> Result<Self> {
        self.visit_expression(&o.value)
    }
    fn visit_mutation(&mut self, m: &Mutation) -> Result<Self> {
        Ok(self
            .visit_mutation_operator(m.operator)?
            .combine(self.visit_primary_expression(&m.operand)?)
            .combine(
                m.dest
                    .as_ref()
                    .map_or_else(|| leaf(()), |dest| self.visit_assignment_lhs(dest))?,
            )
            .combine(
                m.param
                    .as_ref()
                    .map_or_else(|| leaf(()), |param| self.visit_expression(param))?,
            ))
    }
    fn visit_rounding(&mut self, r: &Rounding) -> Result<Self> {
        Ok(self
            .visit_rounding_direction(r.direction)?
            .combine(self.visit_expression(&r.operand)?))
    }
    fn visit_continue(&mut self) -> Result<Self> {
        leaf(())
    }
    fn visit_break(&mut self) -> Result<Self> {
        leaf(())
    }
    fn visit_array_push(&mut self, a: &ArrayPush) -> Result<Self> {
        Ok(self
            .visit_primary_expression(&a.array)?
            .combine(self.visit_array_push_rhs(&a.value)?))
    }
    fn visit_array_pop(&mut self, a: &ArrayPop) -> Result<Self> {
        Ok(self.visit_primary_expression(&a.array)?.combine(
            a.dest
                .as_ref()
                .map_or_else(|| leaf(()), |dest| self.visit_assignment_lhs(dest))?,
        ))
    }
    fn visit_return(&mut self, r: &Return) -> Result<Self> {
        self.visit_expression(&r.value)
    }
    fn visit_function(&mut self, f: &Function) -> Result<Self> {
        Ok(combine_all(
            iter::once(self.visit_variable_name(&f.name))
                .chain(f.params.iter().map(|p| self.visit_variable_name(p))),
        )?
        .combine(self.visit_block(&f.body)?))
    }
    fn visit_function_call(&mut self, f: &FunctionCall) -> Result<Self> {
        combine_all(
            iter::once(self.visit_variable_name(&f.name))
                .chain(f.args.iter().map(|e| self.visit_primary_expression(e))),
        )
    }

    // Statement helper types
    fn visit_assignment_lhs(&mut self, a: &AssignmentLHS) -> Result<Self> {
        match a {
            AssignmentLHS::Identifier(i) => self.visit_identifier(i),
            AssignmentLHS::ArraySubscript(a) => self.visit_array_subsript(a),
        }
    }
    fn visit_poetic_number_assignment(&mut self, a: &PoeticNumberAssignment) -> Result<Self> {
        Ok(self
            .visit_assignment_lhs(&a.dest)?
            .combine(self.visit_poetic_number_assignment_rhs(&a.rhs)?))
    }
    fn visit_poetic_string_assignment(&mut self, a: &PoeticStringAssignment) -> Result<Self> {
        self.visit_assignment_lhs(&a.dest)
    }
    fn visit_poetic_number_assignment_rhs(
        &mut self,
        p: &PoeticNumberAssignmentRHS,
    ) -> Result<Self> {
        match p {
            PoeticNumberAssignmentRHS::Expression(e) => self.visit_expression(e),
            PoeticNumberAssignmentRHS::PoeticNumberLiteral(p) => {
                self.visit_poetic_number_literal(p)
            }
        }
    }
    fn visit_poetic_number_literal(&mut self, p: &PoeticNumberLiteral) -> Result<Self> {
        combine_all(
            p.elems
                .iter()
                .map(|p| self.visit_poetic_number_literal_elem(p)),
        )
    }
    fn visit_poetic_number_literal_elem(&mut self, p: &PoeticNumberLiteralElem) -> Result<Self> {
        leaf(p)
    }
    fn visit_array_push_rhs(&mut self, a: &ArrayPushRHS) -> Result<Self> {
        match a {
            ArrayPushRHS::ExpressionList(e) => self.visit_expression_list(e),
            ArrayPushRHS::PoeticNumberLiteral(p) => self.visit_poetic_number_literal(p),
        }
    }

    // Operators
    fn visit_binary_operator(&mut self, o: BinaryOperator) -> Result<Self> {
        leaf(o)
    }
    fn visit_mutation_operator(&mut self, o: MutationOperator) -> Result<Self> {
        leaf(o)
    }
    fn visit_unary_operator(&mut self, o: UnaryOperator) -> Result<Self> {
        leaf(o)
    }
    fn visit_rounding_direction(&mut self, r: RoundingDirection) -> Result<Self> {
        leaf(r)
    }

    // Expressions
    fn visit_expression_list(&mut self, e: &ExpressionList) -> Result<Self> {
        combine_all(
            iter::once(self.visit_expression(&e.first))
                .chain(e.rest.iter().map(|e| self.visit_expression(e))),
        )
    }
    fn visit_expression(&mut self, e: &Expression) -> Result<Self> {
        match e {
            Expression::PrimaryExpression(e) => self.visit_primary_expression(e),
            Expression::BinaryExpression(e) => self.visit_binary_expression(e),
            Expression::UnaryExpression(e) => self.visit_unary_expression(e),
        }
    }
    fn visit_primary_expression(&mut self, e: &PrimaryExpression) -> Result<Self> {
        match e {
            PrimaryExpression::Literal(e) => self.visit_literal_expression(e),
            PrimaryExpression::Identifier(i) => self.visit_identifier(i),
            PrimaryExpression::ArraySubscript(a) => self.visit_array_subsript(a),
            PrimaryExpression::FunctionCall(f) => self.visit_function_call(f),
        }
    }
    fn visit_binary_expression(&mut self, e: &BinaryExpression) -> Result<Self> {
        Ok(self
            .visit_expression(&e.lhs)?
            .combine(self.visit_binary_operator(e.operator)?)
            .combine(self.visit_expression_list(&e.rhs)?))
    }
    fn visit_unary_expression(&mut self, e: &UnaryExpression) -> Result<Self> {
        Ok(self
            .visit_unary_operator(e.operator)?
            .combine(self.visit_expression(&e.operand)?))
    }
    fn visit_array_subsript(&mut self, a: &ArraySubscript) -> Result<Self> {
        Ok(self
            .visit_primary_expression(&a.array)?
            .combine(self.visit_primary_expression(&a.subscript)?))
    }
    fn visit_literal_expression(&mut self, e: &LiteralExpression) -> Result<Self> {
        leaf(e)
    }

    // Identifiers
    fn visit_identifier(&mut self, i: &Identifier) -> Result<Self> {
        match i {
            Identifier::VariableName(n) => self.visit_variable_name(n),
            Identifier::Pronoun => self.visit_pronoun(),
        }
    }
    fn visit_pronoun(&mut self) -> Result<Self> {
        leaf(())
    }
    fn visit_variable_name(&mut self, n: &VariableName) -> Result<Self> {
        match n {
            VariableName::Simple(n) => self.visit_simple_identifier(n),
            VariableName::Common(n) => self.visit_common_identifier(n),
            VariableName::Proper(n) => self.visit_proper_identifier(n),
        }
    }
    fn visit_simple_identifier(&mut self, n: &SimpleIdentifier) -> Result<Self> {
        leaf(n)
    }
    fn visit_common_identifier(&mut self, n: &CommonIdentifier) -> Result<Self> {
        leaf(n)
    }
    fn visit_proper_identifier(&mut self, n: &ProperIdentifier) -> Result<Self> {
        leaf(n)
    }
}

pub type Result<T> = std::result::Result<<T as Visitor>::Output, <T as Visitor>::Error>;
