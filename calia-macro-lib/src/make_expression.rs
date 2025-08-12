use indexmap::IndexMap;
use proc_macro2::TokenStream;
use quote::quote;
use syn::{bracketed, parse::{Parse, ParseStream}, parse2, Ident, Token, Type};

use crate::{expressions::ExpressionClause, keywords::keyword, CgpSqlContext, ToCgpSql as _};

struct ExpressionSubstitution {
    assign: Type,
    aliases: Vec<AliasClause>,
    expression: ExpressionClause,
}

impl Parse for ExpressionSubstitution {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let assign: Type = input.parse()?;
        let content;
        bracketed!(content in input);
        let alias_clauses = content.parse_terminated(AliasClause::parse, Token![,])?;
        let aliases = alias_clauses.into_iter().collect();
        let expression = input.parse()?;
        Ok(ExpressionSubstitution {
            assign,
            aliases,
            expression,
        })
    }
}

struct AliasClause {
    name: Ident,
    table: Type,
}

impl Parse for AliasClause {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name: Ident = input.parse()?;
        input.parse::<keyword::is>()?;
        let table: Type = input.parse()?;
        Ok(AliasClause { name, table })
    }
}

pub fn make_expression(body: TokenStream) -> TokenStream {
    let mut context = CgpSqlContext {
        aliases: IndexMap::new(),
        substitutions: IndexMap::new(),
    };
    let sub = parse2::<ExpressionSubstitution>(body).unwrap();
    let assign = sub.assign;
    let aliases = sub.aliases;
    for alias in aliases {
        context.aliases.insert(alias.name, alias.table);
    }
    let expression = sub.expression;
    let expression = expression.to_cgp_sql(&mut context);
    quote! {
        pub type #assign = #expression;
    }
}