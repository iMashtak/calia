use std::marker::PhantomData;

pub struct NamedParameterClause<Name>(pub PhantomData<Name>);

pub struct UnnamedParameterClause(pub PhantomData<()>);

pub struct SelectClause<Projection, From = (), Where = ()>(pub PhantomData<(Projection, From, Where)>);

pub struct ProjectionClause<Bindings>(pub PhantomData<Bindings>);

pub struct FromClause<Bindings>(pub PhantomData<Bindings>);

pub struct BindingClause<Expression, Alias>(pub PhantomData<(Expression, Alias)>);

pub struct TableReferenceClause<Table>(pub PhantomData<Table>);

pub struct WhereClause<Expression>(pub PhantomData<Expression>);

pub struct FieldReferenceClause<Table, Alias, Name>(pub PhantomData<(Table, Alias, Name)>);

pub struct FunctionCallClause<Name, Args>(pub PhantomData<(Name, Args)>);

pub struct StringClause<Content>(pub PhantomData<Content>);

pub struct IntegerClause<Content>(pub PhantomData<Content>);

pub struct BinaryOperatorClause<Left, Operator, Right>(pub PhantomData<(Left, Operator, Right)>);