use std::marker::PhantomData;

use calia::prelude::*;
use calia_sqlx::Sqlx;
use cgp::prelude::*;

dialect! {MyDialect as Sqlx db PostgreSql version "17.5"}

table! {ScopeTable as scope (id, name, age)}

query! {SelectScopes,
    select s.name as name
    from ScopeTable as s
    where 1
}

expression! {SampleExpression [s is ScopeTable] s.age or 5}

query! {SelectTemplate,
    select s.id as id
    from ScopeTable as s
    where (|Sub| and |Sub2|) and ((5 or 7))
}

fn main() {
    let result = MyDialect.build_select(PhantomData::<SelectScopes>);
    println!("{}", result);
    let result = MyDialect.build_expression(PhantomData::<SampleExpression>);
    println!("{}", result);
    let result =
        MyDialect.build_select(PhantomData::<SelectTemplate<SampleExpression, TrueExpression>>);
    println!("{}", result);
}

pub struct PostgreSql<Version>(pub PhantomData<Version>);

#[cgp_provider]
impl<Context, Version> OperatorChecker<Context, LtOperatorClause> for PostgreSql<Version> {
    const LEVEL: u64 = 1;

    fn build_operator() -> String {
        format!("<")
    }
}

#[cgp_provider]
impl<Context, Version> OperatorChecker<Context, GtOperatorClause> for PostgreSql<Version> {
    const LEVEL: u64 = 1;

    fn build_operator() -> String {
        format!(">")
    }
}

#[cgp_provider]
impl<Context, Version> OperatorChecker<Context, EqOperatorClause> for PostgreSql<Version> {
    const LEVEL: u64 = 2;

    fn build_operator() -> String {
        format!("=")
    }
}

#[cgp_provider]
impl<Context, Version> OperatorChecker<Context, AndOperatorClause> for PostgreSql<Version> {
    const LEVEL: u64 = 3;

    fn build_operator() -> String {
        format!("and")
    }
}

#[cgp_provider]
impl<Context, Version> OperatorChecker<Context, OrOperatorClause> for PostgreSql<Version> {
    const LEVEL: u64 = 4;

    fn build_operator() -> String {
        format!("or")
    }
}

#[cgp_provider]
impl<Context, Version> FunctionChecker<Context, symbol!("concat")> for PostgreSql<Version> {}
