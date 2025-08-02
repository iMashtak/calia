use cgp::prelude::*;

#[cgp_component(OperatorChecker)]
pub trait HasOperator<Content> {
    const LEVEL: u64;

    fn build_operator() -> String;
}

pub struct AndOperatorClause;

pub struct OrOperatorClause;

pub struct LtOperatorClause;

pub struct GtOperatorClause;

pub struct EqOperatorClause;