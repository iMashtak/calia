use indexmap::IndexMap;
use proc_macro2::TokenStream;
use quote::quote;
use syn::{parse::{Parse, ParseStream}, parse2, Token, Type};

use crate::{expressions::SelectClause, keywords::keyword, CgpSqlContext, ToCgpSql};

struct QueryDef {
    assign: Type,
    clause: QueryClause,
}

enum QueryClause {
    Select(SelectClause),
    Update(),
    Insert(),
    Delete(),
}

impl Parse for QueryDef {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let assign: Type = input.parse()?;
        input.parse::<Token![,]>()?;
        let clause: QueryClause = input.parse()?;
        Ok(QueryDef {
            assign,
            clause,
        })
    }
}

impl Parse for QueryClause {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(keyword::select) {
            let result: SelectClause = input.parse()?;
            Ok(QueryClause::Select(result))
        } else {
            Err(input.error("unexpected start of query"))
        }
    }
}

impl ToCgpSql for QueryClause {
    fn to_cgp_sql(&self, context: &mut CgpSqlContext) -> TokenStream {
        match self {
            Self::Select(clause) => clause.to_cgp_sql(context),
            _ => todo!()
        }
    }
}

pub fn make_query(body: TokenStream) -> TokenStream {
    let mut context = CgpSqlContext {
        aliases: IndexMap::new(),
        substitutions: IndexMap::new(),
    };
    let query = parse2::<QueryDef>(body).unwrap();
    let assign = query.assign;
    let query_clause = query.clause.to_cgp_sql(&mut context);
    if context.substitutions.is_empty() {
        quote! {
            pub type #assign = #query_clause;
        }
    } else {
        let subs = context.substitutions.keys();
        quote! {
            pub type #assign<#(#subs),*> = #query_clause;
        }
    }
}
