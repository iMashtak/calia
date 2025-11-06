use std::marker::PhantomData;

use calia::prelude::*;
use calia_postgresql::PostgreSql;
use calia_sqlx::{Sqlx, SqlxPlaceholderSyntax};
use cgp::prelude::*;

dialect! {
    MyDialect {
        sqlx_placeholder_syntax: SqlxPlaceholderSyntax,
    } as Sqlx
    db PostgreSql version "17.5"
}

table! {ScopeTable as scope (id, name, age)}

table! {UserTable as user (id, name, age)}

query! {SelectScopes,
    select s.id as name
    from ScopeTable as s
    where (select u.id as id from UserTable as u where u.age < s.age) = 0
    and s.name = ?1 and s.age like "%some%"
}

expression! {SampleExpression [s is ScopeTable] s.age = ""}

query! {SelectTemplate,
    select s.id as id
    from ScopeTable as s
    where (|Sub| and |Sub2|) and ((-5 or 7))
}

fn main() {
    let dialect = MyDialect {
        sqlx_placeholder_syntax: SqlxPlaceholderSyntax::QuestionMark,
    };
    let result = dialect.build_select(PhantomData::<SelectScopes>);
    println!("{}", result);
    let result = dialect.build_expression(PhantomData::<SampleExpression>);
    println!("{}", result);
    let result =
        dialect.build_select(PhantomData::<SelectTemplate<SampleExpression, TrueExpression>>);
    println!("{}", result);
}
