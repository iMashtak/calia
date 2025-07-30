use std::{fmt::Display, marker::PhantomData};

use calia_macro::{define_function_call_args_impls, define_target_struct_clause_fields_impls};
use cgp::prelude::*;

#[cgp_context]
struct MyDialect;

delegate_components! {
    MyDialectComponents {
        [
            SelectClauseBuilderComponent,
            TargetClauseBuilderComponent,
            FromClauseBuilderComponent,
            TargetStructClauseFieldsCollectorComponent,
            FunctionCallArgsCollectorComponent,
            ProjectionBuilderComponent,
            ExpressionBuilderComponent,
        ]: PseudoCode,
    }
}

calia_macro::table! {ScopeTable as scope (id, name, age)}

calia_macro::query! {SelectScopes,
    select {
        id: s.id,
        name: concat(s.name, s.name),
    }
    from ScopeTable as s
    where s.name "=" "115" and s.age "<" 115
}

fn main() {
    let result = MyDialect.build_select(PhantomData::<SelectScopes>);
    println!("{}", result);
}

trait IsTable {
    type Name: Default + Display;
}

trait HasTypedField<Name> {
    type Type;
}

pub struct PseudoCode;

struct SelectClause<Target, From, Where = ()>(pub PhantomData<(Target, From, Where)>);

#[cgp_component(SelectClauseBuilder)]
trait CanBuildSelect<Code> {
    fn build_select(&self, code: PhantomData<Code>) -> String;
}

#[cgp_provider]
impl<Context, Target, From, Where> SelectClauseBuilder<Context, SelectClause<Target, From, Where>>
    for PseudoCode
where
    Context: CanBuildTargetClause<Target> + CanBuildFromClause<From> + CanBuildExpression<Where>,
{
    fn build_select(
        context: &Context,
        _code: PhantomData<SelectClause<Target, From, Where>>,
    ) -> String {
        let target_clause = context.build_target(PhantomData);
        let from_clause = context.build_from(PhantomData);
        let where_clause = context.build_expression(PhantomData);
        let mut result = String::new();
        result.push_str(format!("select {} from {}", target_clause, from_clause).as_str());
        if let Some(where_clause) = where_clause {
            result.push_str(format!(" where {}", where_clause).as_str());
        }
        result
    }
}

struct FromClause<Table, Alias>(pub PhantomData<(Table, Alias)>);

#[cgp_component(FromClauseBuilder)]
trait CanBuildFromClause<Code> {
    fn build_from(&self, code: PhantomData<Code>) -> String;
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

struct FieldReferenceClause<Table, Alias, Name>(pub PhantomData<(Table, Alias, Name)>);

#[cgp_component(TargetClauseBuilder)]
trait CanBuildTargetClause<Code> {
    fn build_target(&self, code: PhantomData<Code>) -> String;
}

struct TargetStructClause<Fields>(pub PhantomData<Fields>);

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

#[cgp_component(TargetStructClauseFieldsCollector)]
trait CanCollectTargetStructClauseFields<Code> {
    fn collect_target_struct_clause_fields(
        &self,
        code: PhantomData<Code>,
        collection: &mut Vec<String>,
    );
}

define_target_struct_clause_fields_impls! {32, PseudoCode}

#[cgp_component(ProjectionBuilder)]
trait CanBuildProjection<Code> {
    fn build_projection(&self, code: PhantomData<Code>) -> String;
}

struct TargetStructFieldClause<Name, Expression>(pub PhantomData<(Name, Expression)>);

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

#[cgp_component(ExpressionBuilder)]
trait CanBuildExpression<Code> {
    type Type;
    const LEVEL: u64;

    fn build_expression(&self, code: PhantomData<Code>) -> String;
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
    const LEVEL: u64 = 0;

    fn build_expression(
        _context: &Context,
        _code: PhantomData<FieldReferenceClause<Table, Alias, Name>>,
    ) -> String {
        format!("{}.{}", Alias::default(), Name::default())
    }
}

struct FunctionCallClause<Name, Args>(pub PhantomData<(Name, Args)>);

#[cgp_provider]
impl<Context, Name, Args> ExpressionBuilder<Context, FunctionCallClause<Name, Args>> for PseudoCode
where
    Context: CanCollectFunctionCallArgs<Args>,
    Name: Default + Display,
{
    type Type = ();
    const LEVEL: u64 = 0;

    fn build_expression(
        context: &Context,
        _code: PhantomData<FunctionCallClause<Name, Args>>,
    ) -> String {
        let mut args = Vec::new();
        context.collect_function_call_args(PhantomData, &mut args);
        format!("{}({})", Name::default(), args.join(", "))
    }
}

struct StringClause<Content>(pub PhantomData<Content>);

#[cgp_provider]
impl<Context, Content> ExpressionBuilder<Context, StringClause<Content>> for PseudoCode
where
    Content: Default + Display,
{
    type Type = ();
    const LEVEL: u64 = 0;

    fn build_expression(_context: &Context, _code: PhantomData<StringClause<Content>>) -> String {
        format!("\"{}\"", Content::default())
    }
}

struct IntegerClause<Content>(pub PhantomData<Content>);

#[cgp_provider]
impl<Context, Content> ExpressionBuilder<Context, IntegerClause<Content>> for PseudoCode
where
    Content: Default + Display,
{
    type Type = ();
    const LEVEL: u64 = 0;

    fn build_expression(_context: &Context, _code: PhantomData<IntegerClause<Content>>) -> String {
        format!("{}", Content::default())
    }
}

struct BinaryOperatorCallClause<Left, Operator, Right>(pub PhantomData<(Left, Operator, Right)>);

#[cgp_provider]
impl<Context, Left, Operator, Right>
    ExpressionBuilder<Context, BinaryOperatorCallClause<Left, Operator, Right>> for PseudoCode
where
    Context: CanBuildExpression<Left> + CanBuildExpression<Right> + HasOperator<Operator>,
    Operator: Default + Display,
{
    type Type = ();
    const LEVEL: u64 = 0;

    fn build_expression(
        context: &Context,
        _code: PhantomData<BinaryOperatorCallClause<Left, Operator, Right>>,
    ) -> String {
        let left_level = <Context as CanBuildExpression<Left>>::LEVEL;
        let right_level = <Context as CanBuildExpression<Right>>::LEVEL;
        let operator_level = <Context as HasOperator<Operator>>::LEVEL;
        let mut left = context.build_expression(PhantomData::<Left>);
        let mut right = context.build_expression(PhantomData::<Right>);
        let operator = Operator::default().to_string();
        if left_level < operator_level {
            left = format!("({})", left);
        }
        if right_level < operator_level {
            right = format!("({})", right);
        }
        format!("{} {} {}", left, operator, right)
    }
}

#[cgp_component(OperatorChecker)]
trait HasOperator<Content> {
    const LEVEL: u64;
}

struct AndOperatorClause;

struct OrOperatorClause;

struct PostgreSql;

#[cgp_provider]
impl<Context> OperatorChecker<Context, AndOperatorClause> for PostgreSql {
    const LEVEL: u64 = 1;
}

#[cgp_provider]
impl<Context> OperatorChecker<Context, OrOperatorClause> for PostgreSql {
    const LEVEL: u64 = 2;
}

#[cgp_component(FunctionCallArgsCollector)]
trait CanCollectFunctionCallArgs<Code> {
    type ArgTypes;

    fn collect_function_call_args(&self, code: PhantomData<Code>, collection: &mut Vec<String>);
}

define_function_call_args_impls! {32, PseudoCode}
