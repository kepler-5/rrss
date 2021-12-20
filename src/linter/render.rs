use crate::frontend::ast::{
    AssignmentLHS, CommonIdentifier, Identifier, ProperIdentifier, SimpleIdentifier, VariableName,
};

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
            AssignmentLHS::Identifier(i) => i.render(),
            AssignmentLHS::ArraySubscript(_) => "<expression>".into(),
        }
    }
}
