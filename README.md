# Calia

SQL query builder. Compile-time query AST definition, run-time query translation to specified format.

> Early stage of development

The main idea is to give devs an ability to write deeply compile-time checked (as far as Rust type system allows) SQL queries in dialect that covers all popular SQL databases. In addition we want compile-time check that selected database supports such SQL verb, or function, or operator or etc.

Client-side code should heavily use macros to get an ability to write as less as possible code. Example:

```rust
// insert required usages
use calia::prelude::*;
use calia_postgresql::PostgreSql;
use calia_sqlx::{Sqlx, SqlxPlaceholderSyntax};
use cgp::prelude::*;

// define sql dialect that should be used for query translation
dialect! {
    Dialect {
        sqlx_placeholder_syntax: SqlxPlaceholderSyntax,
    } as Sqlx
    db PostgreSql version "17.5"
}

// define database schema
table! {User as users (id, name, surname, age)}

// define query
query! {GetUsersWithAgeLessThan18AndNameEqualToAlicia,
    select
        user.id as id,
        user.name as name,
        user.surname as surname,
        user.age as age
    from User as user
    where user.age < 18
    and user.name = "Alicia"
}

fn main() {
    // create dialect instance with specified fields values
    let dialect = Dialect {
        sqlx_placeholder_syntax: SqlxPlaceholderSyntax::QuestionMark,
    };

    // use dialect to build defined `select` query in the form of `sqlx` string
    let query: String = build!(use dialect for select in GetUsersWithAgeLessThan18AndNameEqualToAlicia);
}
```

## Implemented features

1. *Query templating.* For now there is only an ability to substitute expressions in `where` section.
    ```rust
    // define expression for substitution
    expression! {UserAgeLessThan18 [user is User] user.age < 18}

    // define query with substitution point `|Condition|`
    query! {GetUsersWithAnyConditionAndNameEqualToAlice,
        select
            user.id as id,
            user.name as name,
            user.surname as surname,
            user.age as age
        from User as user
        where |Condition|
        and user.name = "Alice"
    }

    fn main() {
        ...

        // substitute expression as generic parameter of query type
        let query: String = build!(use dialect for select 
            in GetUsersWithAnyConditionAndNameEqualToAlice<UserAgeLessThan18>
        );
    }
    ```

    There are two predefined expressions: `TrueExpression` and `FalseExpression` - that can be used to ignore some substitution points in query.

2. *Custom operators.* It is almost impossible to cover every operator in every database with `query!` macro syntax. So there is a need to make some extension points for custom operators. It works just like query templating but with different syntax to avoid ambiguity:
    ```rust
    // define expression with operator substitution point
    expression! {UserAgeComparedTo18 [user is User] user.age @Op 18}

    fn main() {
        ...

        // substitute expression as generic parameter of query type
        let expression: String = build!(use dialect for expression 
            in UserAgeComparedTo18<EqOperatorClause>
        );
    }
    ```

## Planned features

Obviously it needs to implement all parts of `SELECT` query and all other queries.

Also it is desired to implement not only name-checking of SQL-functions but also cheking of their argument types since it matters for SQL queries. Currently I am struggling with Rust type system that do not allow me to use [impl specialization](https://github.com/rust-lang/rust/issues/31844).

## Whats inside

This library is heavily relies on [`cgp`](https://github.com/contextgeneric/cgp) project.
