use std::marker::PhantomData;

use cgp::prelude::*;

#[cgp_component(SelectClauseBuilder)]
pub trait CanBuildSelect<Code> {
    fn build_select(&self, code: PhantomData<Code>) -> String;
}

#[cgp_component(FromClauseBuilder)]
pub trait CanBuildFromClause<Code> {
    fn build_from(&self, code: PhantomData<Code>) -> String;
}

#[cgp_component(WhereClauseBuilder)]
pub trait CanBuildWhereClause<Code> {
    fn build_where(&self, code: PhantomData<Code>) -> Option<String>;
}

#[cgp_component(TargetClauseBuilder)]
pub trait CanBuildTargetClause<Code> {
    fn build_target(&self, code: PhantomData<Code>) -> String;
}

#[cgp_component(TargetStructClauseFieldsCollector)]
pub trait CanCollectTargetStructClauseFields<Code> {
    fn collect_target_struct_clause_fields(
        &self,
        code: PhantomData<Code>,
        collection: &mut Vec<String>,
    );
}

#[cgp_component(ProjectionBuilder)]
pub trait CanBuildProjection<Code> {
    fn build_projection(&self, code: PhantomData<Code>) -> String;
}

#[cgp_component(ExpressionBuilder)]
pub trait CanBuildExpression<Code> {
    type Type;

    fn level(&self) -> u64;

    fn build_expression(&self, code: PhantomData<Code>) -> String;
}

#[cgp_component(FunctionCallArgsCollector)]
pub trait CanCollectFunctionCallArgs<Code> {
    type ArgTypes;

    fn collect_function_call_args(&self, code: PhantomData<Code>, collection: &mut Vec<String>);
}