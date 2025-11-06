use std::marker::PhantomData;

use calia::prelude::*;
use cgp::prelude::*;

pub struct PostgreSql<Version>(pub PhantomData<Version>);

// --- operators ---

#[cgp_provider]
impl<Context, Version> OperatorChecker<Context, NotOperatorClause> for PostgreSql<Version> {
    const LEVEL: i32 = 40;

    fn build_operator(_context: &Context, _code: PhantomData<NotOperatorClause>) -> String {
        format!("not")
    }
}

#[cgp_provider]
impl<Context, Version> OperatorChecker<Context, OrOperatorClause> for PostgreSql<Version> {
    const LEVEL: i32 = 10;

    fn build_operator(_context: &Context, _code: PhantomData<OrOperatorClause>) -> String {
        format!("or")
    }
}

#[cgp_provider]
impl<Context, Version> OperatorChecker<Context, AndOperatorClause> for PostgreSql<Version> {
    const LEVEL: i32 = 30;

    fn build_operator(_context: &Context, _code: PhantomData<AndOperatorClause>) -> String {
        format!("and")
    }
}

#[cgp_provider]
impl<Context, Version> OperatorChecker<Context, EqOperatorClause> for PostgreSql<Version> {
    const LEVEL: i32 = 50;

    fn build_operator(_context: &Context, _code: PhantomData<EqOperatorClause>) -> String {
        format!("=")
    }
}

#[cgp_provider]
impl<Context, Version> OperatorChecker<Context, LtOperatorClause> for PostgreSql<Version> {
    const LEVEL: i32 = 70;

    fn build_operator(_context: &Context, _code: PhantomData<LtOperatorClause>) -> String {
        format!("<")
    }
}

#[cgp_provider]
impl<Context, Version> OperatorChecker<Context, GtOperatorClause> for PostgreSql<Version> {
    const LEVEL: i32 = 70;

    fn build_operator(_context: &Context, _code: PhantomData<GtOperatorClause>) -> String {
        format!(">")
    }
}

#[cgp_provider]
impl<Context, Version> OperatorChecker<Context, LikeOperatorClause> for PostgreSql<Version> {
    const LEVEL: i32 = 90;

    fn build_operator(_context: &Context, _code: PhantomData<LikeOperatorClause>) -> String {
        format!("like")
    }
}

#[cgp_provider]
impl<Context, Version> OperatorChecker<Context, ILikeOperatorClause> for PostgreSql<Version> {
    const LEVEL: i32 = 90;

    fn build_operator(_context: &Context, _code: PhantomData<ILikeOperatorClause>) -> String {
        format!("ilike")
    }
}

#[cgp_provider]
impl<Context, Version> OperatorChecker<Context, OverlapsOperatorClause> for PostgreSql<Version> {
    const LEVEL: i32 = 110;

    fn build_operator(_context: &Context, _code: PhantomData<OverlapsOperatorClause>) -> String {
        format!("overlaps")
    }
}

#[cgp_provider]
impl<Context, Version> OperatorChecker<Context, InOperatorClause> for PostgreSql<Version> {
    const LEVEL: i32 = 150;

    fn build_operator(_context: &Context, _code: PhantomData<InOperatorClause>) -> String {
        format!("in")
    }
}

#[cgp_provider]
impl<Context, Version> OperatorChecker<Context, IsOperatorClause> for PostgreSql<Version> {
    const LEVEL: i32 = 190;

    fn build_operator(_context: &Context, _code: PhantomData<IsOperatorClause>) -> String {
        format!("is")
    }
}

#[cgp_provider]
impl<Context, Version> OperatorChecker<Context, PlusOperatorClause> for PostgreSql<Version> {
    const LEVEL: i32 = 210;

    fn build_operator(_context: &Context, _code: PhantomData<PlusOperatorClause>) -> String {
        format!("+")
    }
}

#[cgp_provider]
impl<Context, Version> OperatorChecker<Context, MinusOperatorClause> for PostgreSql<Version> {
    const LEVEL: i32 = 210;

    fn build_operator(_context: &Context, _code: PhantomData<MinusOperatorClause>) -> String {
        format!("-")
    }
}

#[cgp_provider]
impl<Context, Version> OperatorChecker<Context, MulOperatorClause> for PostgreSql<Version> {
    const LEVEL: i32 = 230;

    fn build_operator(_context: &Context, _code: PhantomData<MulOperatorClause>) -> String {
        format!("*")
    }
}

#[cgp_provider]
impl<Context, Version> OperatorChecker<Context, DivOperatorClause> for PostgreSql<Version> {
    const LEVEL: i32 = 230;

    fn build_operator(_context: &Context, _code: PhantomData<DivOperatorClause>) -> String {
        format!("/")
    }
}

#[cgp_provider]
impl<Context, Version> OperatorChecker<Context, ModOperatorClause> for PostgreSql<Version> {
    const LEVEL: i32 = 230;

    fn build_operator(_context: &Context, _code: PhantomData<ModOperatorClause>) -> String {
        format!("%")
    }
}

#[cgp_provider]
impl<Context, Version> OperatorChecker<Context, ExpOperatorClause> for PostgreSql<Version> {
    const LEVEL: i32 = 250;

    fn build_operator(_context: &Context, _code: PhantomData<ExpOperatorClause>) -> String {
        format!("^")
    }
}

// --- functions ---

#[cgp_provider]
impl<Context, Version> FunctionChecker<Context, Symbol!("concat")> for PostgreSql<Version> {}
