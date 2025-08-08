# Calia

SQL query builder. Compile-time query AST definition, run-time query translation to specified format.

> Early stage of development

The main idea is to give devs an ability to write deeply compile-time checked (as far as Rust type system allows) SQL queries in dialect that covers all popular SQL databases. In addition we want compile-time check that selected database supports such SQL verb, or function, or operator or etc.

Client-side code should heavily use macros to get an ability to write as less as possible code. Example:

```rust
// define sql dialect that should be used for query translation
dialect! {Dialect as Sqlx db PostgreSql version "17.5"}

// define database schema
table! {User as users (id, name, surname, age)}

// define query
query! {GetUsersWithAgeLessThan18AndNameEqualToAlice,
    select {
        id: user.id,
        name: user.name,
        surname: user.surname,
        age: user.age,
    }
    from User as user
    where user.age < 18
    and user.name = "Alice"
}

fn main() {
    // use dialect to build defined `select` query in the form of `sqlx` string
    let query = Dialect.build_select(PhantomData::<GetUsersWithAgeLessThan18AndNameEqualToAlice>);
}
```

## Implemented features

1. *Query templating.* For now there is only an ability to substitute expressions in `where` section.
    ```rust
    // define expression for substitution
    expression! {UserAgeLessThan18 [user is User] user.age < 18}

    // define query with substitution point `|Condition|`
    query! {GetUsersWithAnyConditionAndNameEqualToAlice,
        select {
            id: user.id,
            name: user.name,
            surname: user.surname,
            age: user.age,
        }
        from User as user
        where |Condition|
        and user.name = "Alice"
    }

    fn main() {
        // substitute expression as generic parameter of query type
        let query = Dialect.build_select(PhantomData::<
            GetUsersWithAnyConditionAndNameEqualToAlice<UserAgeLessThan18>
        >);
    }
    ```

    There two predefined expressions: `TrueExpression` and `FalseExpression` - that can be used to ignore some substitution points in query.

## Planned features

1. All SQL queries with verification that db supports specified verbs
2. Parameter bindings in queries
3. `sqlx`-strings generation
4. `sea-query` structs generation
5. PostgreSQL, MySQL, SQLite verb/function/operators checking support

## Whats inside

This library is heavily relies on [`cgp`](https://github.com/contextgeneric/cgp) project.
