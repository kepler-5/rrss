use crate::frontend::ast::{
    AssignmentLHS, CommonIdentifier, Identifier, PrimaryExpression, ProperIdentifier,
    SimpleIdentifier, VariableName,
};

#[cfg(test)]
mod tests;

pub trait Render {
    fn render(&self) -> String;
}

impl Render for SimpleIdentifier {
    fn render(&self) -> String {
        self.0.clone()
    }
}

impl Render for CommonIdentifier {
    fn render(&self) -> String {
        format!("{} {}", self.0, self.1)
    }
}

impl Render for ProperIdentifier {
    fn render(&self) -> String {
        self.0.join(" ")
    }
}

impl Render for VariableName {
    fn render(&self) -> String {
        match self {
            VariableName::Simple(s) => s.render(),
            VariableName::Common(c) => c.render(),
            VariableName::Proper(p) => p.render(),
        }
    }
}

impl Render for Identifier {
    fn render(&self) -> String {
        match self {
            Identifier::VariableName(v) => v.render(),
            Identifier::Pronoun => "<pronoun>".into(),
        }
    }
}

impl Render for AssignmentLHS {
    fn render(&self) -> String {
        match self {
            AssignmentLHS::Identifier(i) => i.0.render(),
            AssignmentLHS::ArraySubscript(_) => "<expression>".into(),
        }
    }
}

impl Render for PrimaryExpression {
    fn render(&self) -> String {
        match self {
            PrimaryExpression::Identifier(i) => i.0.render(),
            PrimaryExpression::Literal(_) => "<literal>".into(),
            PrimaryExpression::ArraySubscript(_) => "<expression>".into(),
            PrimaryExpression::FunctionCall(_) => "<expression>".into(),
            PrimaryExpression::ArrayPop(_) => "<expression>".into(),
        }
    }
}
