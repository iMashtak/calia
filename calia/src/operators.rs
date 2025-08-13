use std::marker::PhantomData;

use cgp::prelude::*;

#[cgp_component(OperatorChecker)]
pub trait HasOperator<Content> {
    const LEVEL: i32;

    fn build_operator(&self, _code: PhantomData<Content>) -> String;
}

pub struct OrOperatorClause;
pub struct AndOperatorClause;
pub struct EqOperatorClause;
pub struct LtOperatorClause;
pub struct GtOperatorClause;
pub struct LikeOperatorClause;
pub struct ILikeOperatorClause;
pub struct OverlapsOperatorClause;
pub struct BetweenOperatorClause;
pub struct InOperatorClause;
pub struct IsOperatorClause;
pub struct PlusOperatorClause;
pub struct MinusOperatorClause;
pub struct MulOperatorClause;
pub struct DivOperatorClause;
pub struct ModOperatorClause;
pub struct ExpOperatorClause;

pub struct NotOperatorClause;
