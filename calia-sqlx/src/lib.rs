use std::{fmt::Display, marker::PhantomData};

use calia::prelude::*;
use cgp::prelude::*;

pub struct Sqlx;

#[cgp_provider]
impl<Context, Projection, From, Where>
    SelectClauseBuilder<Context, SelectClause<Projection, From, Where>> for Sqlx
where
    Context: CanBuildProjectionClause<Projection>
        + CanBuildFromClause<From>
        + CanBuildWhereClause<Where>,
{
    fn build_select(
        context: &Context,
        _code: PhantomData<SelectClause<Projection, From, Where>>,
    ) -> String {
        let projection_clause = context.build_projection(PhantomData);
        let from_clause = context.build_from(PhantomData);
        let where_clause = context.build_where(PhantomData);
        let mut result = String::new();
        result.push_str(format!("select {}", projection_clause).as_str());
        if let Some(from_clause) = from_clause {
            result.push_str(format!(" from {}", from_clause).as_str());
        }
        if let Some(where_clause) = where_clause {
            result.push_str(format!(" where {}", where_clause).as_str());
        }
        result
    }
}

#[cgp_provider]
impl<Context, Bindings> ProjectionClauseBuilder<Context, ProjectionClause<Bindings>> for Sqlx
where
    Context: CanCollectBindings<Bindings>,
{
    fn build_projection(
        context: &Context,
        _code: PhantomData<ProjectionClause<Bindings>>,
    ) -> String {
        let mut bindings = Vec::new();
        context.collect_bindings(PhantomData, &mut bindings);
        bindings.join(", ")
    }
}

#[cgp_provider]
impl<Context> FromClauseBuilder<Context, ()> for Sqlx {
    fn build_from(_context: &Context, _code: PhantomData<()>) -> Option<String> {
        None
    }
}

#[cgp_provider]
impl<Context, Bindings> FromClauseBuilder<Context, FromClause<Bindings>> for Sqlx
where
    Context: CanCollectBindings<Bindings>,
{
    fn build_from(context: &Context, _code: PhantomData<FromClause<Bindings>>) -> Option<String> {
        let mut bindings = Vec::new();
        context.collect_bindings(PhantomData, &mut bindings);
        Some(bindings.join(", "))
    }
}

#[cgp_provider]
impl<Context> BindingsCollector<Context, Nil> for Sqlx {
    fn collect_bindings(
        _context: &Context,
        _code: PhantomData<Nil>,
        _collection: &mut Vec<String>,
    ) {
    }
}

#[cgp_provider]
impl<Context, Head, Tail> BindingsCollector<Context, Cons<Head, Tail>> for Sqlx
where
    Context: CanBuildBindingClause<Head>,
    Sqlx: BindingsCollector<Context, Tail>,
{
    fn collect_bindings(
        context: &Context,
        _code: PhantomData<Cons<Head, Tail>>,
        collection: &mut Vec<String>,
    ) {
        collection.push(context.build_binding(PhantomData::<Head>));
        Sqlx::collect_bindings(context, PhantomData, collection)
    }
}

#[cgp_provider]
impl<Context, Expression, Alias> BindingClauseBuilder<Context, BindingClause<Expression, Alias>>
    for Sqlx
where
    Context: CanBuildExpressionClause<Expression>,
    Alias: Default + Display,
{
    fn build_binding(
        context: &Context,
        _code: PhantomData<BindingClause<Expression, Alias>>,
    ) -> String {
        let expression = context.build_expression(PhantomData);
        let alias = Alias::default();
        if alias.to_string() == "" {
            expression
        } else {
            format!("{} as {}", expression, alias)
        }
    }
}

#[cgp_provider]
impl<Context> WhereClauseBuilder<Context, ()> for Sqlx {
    fn build_where(_context: &Context, _code: PhantomData<()>) -> Option<String> {
        None
    }
}

#[cgp_provider]
impl<Context, Expression> WhereClauseBuilder<Context, WhereClause<Expression>> for Sqlx
where
    Context: CanBuildExpressionClause<Expression>,
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
impl<Context, Table, Alias, Name>
    ExpressionClauseBuilder<Context, FieldReferenceClause<Table, Alias, Name>> for Sqlx
where
    Table: IsTable + HasTypedField<Name>,
    Alias: Default + Display,
    Name: Default + Display,
{
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
impl<Context, Name, Args> ExpressionClauseBuilder<Context, FunctionCallClause<Name, Args>> for Sqlx
where
    Context: CanCollectFunctionCallArgs<Args> + HasFunction<Name>,
    Name: Default + Display,
{
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
impl<Context, Content> ExpressionClauseBuilder<Context, StringClause<Content>> for Sqlx
where
    Content: Default + Display,
{
    fn level(_context: &Context) -> u64 {
        return 0;
    }

    fn build_expression(_context: &Context, _code: PhantomData<StringClause<Content>>) -> String {
        format!("\"{}\"", Content::default())
    }
}

#[cgp_provider]
impl<Context, Content> ExpressionClauseBuilder<Context, IntegerClause<Content>> for Sqlx
where
    Content: Default + Display,
{
    fn level(_context: &Context) -> u64 {
        return 0;
    }

    fn build_expression(_context: &Context, _code: PhantomData<IntegerClause<Content>>) -> String {
        format!("{}", Content::default())
    }
}

#[cgp_provider]
impl<Context, Table> ExpressionClauseBuilder<Context, TableReferenceClause<Table>> for Sqlx
where
    Table: IsTable,
{
    fn level(_context: &Context) -> u64 {
        return 0;
    }

    fn build_expression(_context: &Context, _code: PhantomData<TableReferenceClause<Table>>) -> String {
        format!("{}", Table::Name::default())
    }
}

#[cgp_provider]
impl<Context, Left, Operator, Right>
    ExpressionClauseBuilder<Context, BinaryOperatorClause<Left, Operator, Right>> for Sqlx
where
    Context: CanBuildExpressionClause<Left> + CanBuildExpressionClause<Right> + HasOperator<Operator>,
{
    fn level(_context: &Context) -> u64 {
        return <Context as HasOperator<Operator>>::LEVEL;
    }

    fn build_expression(
        context: &Context,
        _code: PhantomData<BinaryOperatorClause<Left, Operator, Right>>,
    ) -> String {
        let left_level = <Context as CanBuildExpressionClause<Left>>::level(context);
        let right_level = <Context as CanBuildExpressionClause<Right>>::level(context);
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

#[cgp_provider]
impl<Context> FunctionCallArgsCollector<Context, Nil> for Sqlx
where
    Context:,
{
    fn collect_function_call_args(
        _context: &Context,
        _code: PhantomData<Nil>,
        _collection: &mut Vec<String>,
    ) {
    }
}

#[cgp_provider]
impl<Context, Head, Tail> FunctionCallArgsCollector<Context, Cons<Head, Tail>> for Sqlx
where
    Context: CanBuildExpressionClause<Head>,
    Sqlx: FunctionCallArgsCollector<Context, Tail>,
{
    fn collect_function_call_args(
        context: &Context,
        _code: PhantomData<Cons<Head, Tail>>,
        collection: &mut Vec<String>,
    ) {
        collection.push(context.build_expression(PhantomData::<Head>));
        Sqlx::collect_function_call_args(context, PhantomData, collection)
    }
}
