use std::marker::PhantomData;

use cgp::prelude::*;

#[cgp_component(SelectClauseBuilder)]
pub trait CanBuildSelectClause<Code> {
    fn build_select(&self, code: PhantomData<Code>) -> String;
}

#[cgp_component(ProjectionClauseBuilder)]
pub trait CanBuildProjectionClause<Code> {
    fn build_projection(&self, code: PhantomData<Code>) -> String;
}

#[cgp_component(FromClauseBuilder)]
pub trait CanBuildFromClause<Code> {
    fn build_from(&self, code: PhantomData<Code>) -> Option<String>;
}

#[cgp_component(BindingClauseBuilder)]
pub trait CanBuildBindingClause<Code> {
    fn build_binding(&self, code: PhantomData<Code>) -> String;
}

#[cgp_component(BindingsCollector)]
pub trait CanCollectBindings<Code> {
    fn collect_bindings(&self, code: PhantomData<Code>, collection: &mut Vec<String>);
}

#[cgp_component(WhereClauseBuilder)]
pub trait CanBuildWhereClause<Code> {
    fn build_where(&self, code: PhantomData<Code>) -> Option<String>;
}

#[cgp_component(ExpressionClauseBuilder)]
pub trait CanBuildExpressionClause<Code> {
    fn level(&self) -> u64;

    fn build_expression(&self, code: PhantomData<Code>) -> String;
}

#[cgp_component(FunctionCallArgsCollector)]
pub trait CanCollectFunctionCallArgs<Code> {
    fn collect_function_call_args(&self, code: PhantomData<Code>, collection: &mut Vec<String>);
}