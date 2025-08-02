use std::{fmt::Display, marker::PhantomData};

use calia_macro::{define_function_call_args_impls, define_target_struct_clause_fields_impls};
use cgp::prelude::*;
use calia::prelude::*;

#[cgp_context]
struct MyDialect;

delegate_components! {
    MyDialectComponents {
        [
            SelectClauseBuilderComponent,
            TargetClauseBuilderComponent,
            FromClauseBuilderComponent,
            WhereClauseBuilderComponent,
            TargetStructClauseFieldsCollectorComponent,
            FunctionCallArgsCollectorComponent,
            ProjectionBuilderComponent,
            ExpressionBuilderComponent,
        ]: PseudoCode,
        [
            OperatorCheckerComponent
        ]: PostgreSql,
    }
}

calia_macro::table! {ScopeTable as scope (id, name, age)}

calia_macro::query! {SelectScopes,
    select {
        id: s.id,
        name: concat(s.name, s.name),
    }
    from ScopeTable as s
    where (s.id and (s.id or 10)) and (((5) or 7))
}

fn main() {
    let result = MyDialect.build_select(PhantomData::<SelectScopes>);
    println!("{}", result);
}

pub struct PseudoCode;

#[cgp_provider]
impl<Context, Target, From, Where> SelectClauseBuilder<Context, SelectClause<Target, From, Where>>
    for PseudoCode
where
    Context: CanBuildTargetClause<Target> + CanBuildFromClause<From> + CanBuildWhereClause<Where>,
{
    fn build_select(
        context: &Context,
        _code: PhantomData<SelectClause<Target, From, Where>>,
    ) -> String {
        let target_clause = context.build_target(PhantomData);
        let from_clause = context.build_from(PhantomData);
        let where_clause = context.build_where(PhantomData);
        let mut result = String::new();
        result.push_str(format!("select {} from {}", target_clause, from_clause).as_str());
        if let Some(where_clause) = where_clause {
            result.push_str(format!(" where {}", where_clause).as_str());
        }
        result
    }
}

#[cgp_provider]
impl<Context, Table, Alias> FromClauseBuilder<Context, FromClause<Table, Alias>> for PseudoCode
where
    Table: IsTable,
    Alias: Default + Display,
{
    fn build_from(_context: &Context, _code: PhantomData<FromClause<Table, Alias>>) -> String {
        format!("{} as {}", Table::Name::default(), Alias::default())
    }
}

#[cgp_provider]
impl<Context> WhereClauseBuilder<Context, ()> for PseudoCode
{
    fn build_where(
        _context: &Context,
        _code: PhantomData<()>,
    ) -> Option<String> {
        None
    }
}

#[cgp_provider]
impl<Context, Expression> WhereClauseBuilder<Context, WhereClause<Expression>> for PseudoCode
where
    Context: CanBuildExpression<Expression>,
{
    fn build_where(
        context: &Context,
        _code: PhantomData<WhereClause<Expression>>,
    ) -> Option<String> {
        let expression = context.build_expression(PhantomData);
        Some(expression)
    }
}

#[cgp_provider]
impl<Context, Fields> TargetClauseBuilder<Context, TargetStructClause<Fields>> for PseudoCode
where
    Context: CanCollectTargetStructClauseFields<Fields>,
{
    fn build_target(context: &Context, _code: PhantomData<TargetStructClause<Fields>>) -> String {
        let mut fields = Vec::new();
        context.collect_target_struct_clause_fields(PhantomData, &mut fields);
        format!("{}", fields.join(", "))
    }
}

define_target_struct_clause_fields_impls! {32, PseudoCode}

#[cgp_provider]
impl<Context, Name, Expression>
    ProjectionBuilder<Context, TargetStructFieldClause<Name, Expression>> for PseudoCode
where
    Context: CanBuildExpression<Expression>,
    Name: Default + Display,
{
    fn build_projection(
        context: &Context,
        _code: PhantomData<TargetStructFieldClause<Name, Expression>>,
    ) -> String {
        let expression = context.build_expression(PhantomData);
        let name = Name::default();
        format!("{} as {}", expression, name)
    }
}

#[cgp_provider]
impl<Context, Table, Alias, Name>
    ExpressionBuilder<Context, FieldReferenceClause<Table, Alias, Name>> for PseudoCode
where
    Table: IsTable + HasTypedField<Name>,
    Alias: Default + Display,
    Name: Default + Display,
{
    type Type = <Table as HasTypedField<Name>>::Type;

    fn level(_context: &Context) -> u64 {
        return 0;
    }

    fn build_expression(
        _context: &Context,
        _code: PhantomData<FieldReferenceClause<Table, Alias, Name>>,
    ) -> String {
        format!("{}.{}", Alias::default(), Name::default())
    }
}

#[cgp_provider]
impl<Context, Name, Args> ExpressionBuilder<Context, FunctionCallClause<Name, Args>> for PseudoCode
where
    Context: CanCollectFunctionCallArgs<Args>,
    Name: Default + Display,
{
    type Type = ();

    fn level(_context: &Context) -> u64 {
        return 0;
    }

    fn build_expression(
        context: &Context,
        _code: PhantomData<FunctionCallClause<Name, Args>>,
    ) -> String {
        let mut args = Vec::new();
        context.collect_function_call_args(PhantomData, &mut args);
        format!("{}({})", Name::default(), args.join(", "))
    }
}

#[cgp_provider]
impl<Context, Content> ExpressionBuilder<Context, StringClause<Content>> for PseudoCode
where
    Content: Default + Display,
{
    type Type = ();

    fn level(_context: &Context) -> u64 {
        return 0;
    }

    fn build_expression(_context: &Context, _code: PhantomData<StringClause<Content>>) -> String {
        format!("\"{}\"", Content::default())
    }
}

#[cgp_provider]
impl<Context, Content> ExpressionBuilder<Context, IntegerClause<Content>> for PseudoCode
where
    Content: Default + Display,
{
    type Type = ();

    fn level(_context: &Context) -> u64 {
        return 0;
    }

    fn build_expression(_context: &Context, _code: PhantomData<IntegerClause<Content>>) -> String {
        format!("{}", Content::default())
    }
}

#[cgp_provider]
impl<Context, Left, Operator, Right>
    ExpressionBuilder<Context, BinaryOperatorCallClause<Left, Operator, Right>> for PseudoCode
where
    Context: CanBuildExpression<Left> + CanBuildExpression<Right> + HasOperator<Operator>,
{
    type Type = ();

    fn level(_context: &Context) -> u64 {
        return <Context as HasOperator<Operator>>::LEVEL;
    }

    fn build_expression(
        context: &Context,
        _code: PhantomData<BinaryOperatorCallClause<Left, Operator, Right>>,
    ) -> String {
        let left_level = <Context as CanBuildExpression<Left>>::level(context);
        let right_level = <Context as CanBuildExpression<Right>>::level(context);
        let operator_level = <Context as HasOperator<Operator>>::LEVEL;
        let mut left = context.build_expression(PhantomData::<Left>);
        let mut right = context.build_expression(PhantomData::<Right>);
        let operator = <Context as HasOperator<Operator>>::build_operator();
        if left_level > operator_level {
            left = format!("({})", left);
        }
        if right_level > operator_level {
            right = format!("({})", right);
        }
        format!("{} {} {}", left, operator, right)
    }
}

define_function_call_args_impls! {32, PseudoCode}

pub struct PostgreSql;

#[cgp_provider]
impl<Context> OperatorChecker<Context, LtOperatorClause> for PostgreSql {
    const LEVEL: u64 = 1;

    fn build_operator() -> String {
        format!("<")
    }
}

#[cgp_provider]
impl<Context> OperatorChecker<Context, GtOperatorClause> for PostgreSql {
    const LEVEL: u64 = 1;

    fn build_operator() -> String {
        format!(">")
    }
}

#[cgp_provider]
impl<Context> OperatorChecker<Context, EqOperatorClause> for PostgreSql {
    const LEVEL: u64 = 2;

    fn build_operator() -> String {
        format!("=")
    }
}

#[cgp_provider]
impl<Context> OperatorChecker<Context, AndOperatorClause> for PostgreSql {
    const LEVEL: u64 = 3;

    fn build_operator() -> String {
        format!("and")
    }
}

#[cgp_provider]
impl<Context> OperatorChecker<Context, OrOperatorClause> for PostgreSql {
    const LEVEL: u64 = 4;

    fn build_operator() -> String {
        format!("or")
    }
}
