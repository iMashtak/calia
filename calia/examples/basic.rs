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

expression! {SampleExpression [s is ScopeTable] s.age @Op ""}

query! {SelectTemplate,
    select s.id as id
    from ScopeTable as s
    where (not |Sub| and |Sub|) and (not (-5 @Op 7))
}

query2! {
    SelectTemplateX => 
    select table.id as alias, 0 = (not 4 = 5) as str
    from ScopeTable as table
}

fn main() {
    let dialect = MyDialect {
        sqlx_placeholder_syntax: SqlxPlaceholderSyntax::QuestionMark,
    };

    let result = build!(use dialect for select in SelectScopes);
    println!("{}", result);

    let result = build!(use dialect for expression in SampleExpression<EqOperatorClause>);
    println!("{}", result);

    let result = build!(use dialect for select in SelectTemplate<SampleExpression<EqOperatorClause>, AndOperatorClause>);
    println!("{}", result);

    let result = build!(use dialect for select in SelectTemplateX);
    println!("{}", result);
}
