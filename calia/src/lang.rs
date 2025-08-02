use std::marker::PhantomData;

pub struct SelectClause<Target, From, Where = ()>(pub PhantomData<(Target, From, Where)>);

pub struct FromClause<Table, Alias>(pub PhantomData<(Table, Alias)>);

pub struct WhereClause<Expression>(pub PhantomData<Expression>);

pub struct FieldReferenceClause<Table, Alias, Name>(pub PhantomData<(Table, Alias, Name)>);

pub struct TargetStructClause<Fields>(pub PhantomData<Fields>);

pub struct TargetStructFieldClause<Name, Expression>(pub PhantomData<(Name, Expression)>);

pub struct FunctionCallClause<Name, Args>(pub PhantomData<(Name, Args)>);

pub struct StringClause<Content>(pub PhantomData<Content>);

pub struct IntegerClause<Content>(pub PhantomData<Content>);

pub struct BinaryOperatorCallClause<Left, Operator, Right>(pub PhantomData<(Left, Operator, Right)>);