use std::fmt::Display;

use chumsky::{
    DefaultExpected,
    error::{Error, RichReason},
    input::Stream,
    label::LabelError,
    prelude::*,
    util::Maybe,
};
use indexmap::IndexMap;
use proc_macro2::{Span, TokenStream, TokenTree, token_stream::IntoIter};
use quote::{ToTokens, quote};
use syn::Type;

use crate::{
    CgpSqlContext, ToCgpSql,
    expressions::{
        BindingClause, BindingsClause, ExpressionClause, FieldReferenceClause, FromClause,
        PrimaryExpressionClause, ProjectionClause, SelectClause, TableReferenceClause,
    },
};

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

#[derive(Debug)]
struct Messaged<'src> {
    pub message: String,
    pub simple: Simple<'src, TokenTree, SimpleSpan>,
}

impl<'src> Messaged<'src> {
    pub fn new(
        message: impl Into<String>,
        simple: Simple<'src, TokenTree, SimpleSpan>,
    ) -> Messaged<'src> {
        Self {
            message: message.into(),
            simple,
        }
    }
}

impl<'src> Error<'src, Stream<IntoIter>> for Messaged<'src> {}

impl<'src, L> LabelError<'src, Stream<IntoIter>, L> for Messaged<'src> {
    fn expected_found<E: IntoIterator<Item = L>>(
        _expected: E,
        found: Option<chumsky::util::MaybeRef<'src, <Stream<IntoIter> as Input>::Token>>,
        span: <Stream<IntoIter> as Input>::Span,
    ) -> Self {
        Self::new("".to_string(), Simple::new(found, span))
    }
}

fn parser<'src>() -> impl Parser<'src, Stream<IntoIter>, QueryDef, extra::Err<Messaged<'src>>> {
    let ident = any().try_map(|x: TokenTree, s: SimpleSpan| match &x {
        TokenTree::Ident(i) => Ok(i.clone()),
        _ => Err(Messaged::new("expected identifier", Simple::new(Some(Maybe::Val(x)), s))),
    });
    let comma = select! {
        TokenTree::Punct(x) => x.as_char() == ','
    }
    .filter(|x| *x);
    let dot = select! {
        TokenTree::Punct(x) => x.as_char() == '.'
    }
    .filter(|x| *x);
    let select_keyword = select! {
        TokenTree::Ident(x) => x.to_string() == "select"
    }
    .filter(|x| *x);
    let from_keyword = select! {
        TokenTree::Ident(x) => x.to_string() == "from"
    }
    .filter(|x| *x);
    let as_keyword = any().try_map(|x: TokenTree, s: SimpleSpan| match &x {
        TokenTree::Ident(i) if i.to_string() == "as" => Ok(x),
        _ => Err(Messaged::new("expected 'as' keyword", Simple::new(Some(Maybe::Val(x)), s))),
    });

    let projection_clause = ident
        .then_ignore(dot)
        .then(ident)
        .then_ignore(as_keyword.clone())
        .then(ident)
        .separated_by(comma)
        .allow_trailing()
        .collect::<Vec<_>>()
        .map(|xs| {
            let bindings = xs
                .into_iter()
                .map(|((table, column), alias)| BindingClause {
                    expression: ExpressionClause::Primary(PrimaryExpressionClause::FieldReference(
                        FieldReferenceClause {
                            alias: table,
                            field: column,
                        },
                    )),
                    alias,
                })
                .collect::<Vec<_>>();
            ProjectionClause {
                bindings: BindingsClause { values: bindings },
            }
        });

    let from_clause = ident
        .then_ignore(as_keyword)
        .then(ident)
        .separated_by(comma)
        .allow_trailing()
        .collect::<Vec<_>>()
        .map(|xs| {
            let bindings = xs
                .into_iter()
                .map(|(table, alias)| BindingClause {
                    expression: ExpressionClause::Primary(PrimaryExpressionClause::TableReference(
                        TableReferenceClause {
                            ty: Type::Verbatim(table.to_token_stream()),
                        },
                    )),
                    alias,
                })
                .collect::<Vec<_>>();
            FromClause {
                bindings: BindingsClause { values: bindings },
            }
        });

    let clause = empty()
        .then_ignore(select_keyword)
        .then(projection_clause)
        .then_ignore(from_keyword)
        .then(from_clause)
        .map(|((_, projection_clause), from_clause)| SelectClause {
            projection_clause,
            from_clause: Some(from_clause),
            where_clause: None,
        });

    let assign = any()
        .filter(|x: &TokenTree| matches!(x, TokenTree::Ident(_)))
        .map(|x: TokenTree| match x {
            TokenTree::Ident(x) => Type::Verbatim(x.to_token_stream()),
            _ => unreachable!(),
        });

    assign
        .then_ignore(comma)
        .then(clause)
        .map(|(assign, clause)| QueryDef {
            assign,
            clause: QueryClause::Select(clause),
        })
}

impl ToCgpSql for QueryClause {
    fn to_cgp_sql(&self, context: &mut CgpSqlContext) -> TokenStream {
        match self {
            Self::Select(clause) => clause.to_cgp_sql(context),
            _ => todo!(),
        }
    }
}

pub fn make_query2(body: TokenStream) -> TokenStream {
    let stream = Stream::from_iter(body.into_iter());
    let query = parser().parse(stream).into_result();
    if let Err(errs) = query {
        let mut error: Option<syn::Error> = None;
        for err in errs {
            if let Some(x) = err.simple.found() {
                if let Some(ref mut error) = error {
                    error.combine(syn::Error::new(x.span(), err.message));
                } else {
                    error = Some(syn::Error::new(x.span(), err.message))
                }
            } else {
                if let Some(ref mut error) = error {
                    error.combine(syn::Error::new(Span::call_site(), err.message));
                } else {
                    error = Some(syn::Error::new(Span::call_site(), err.message))
                }
            }
        }
        return error.unwrap().into_compile_error();
    }
    let query = query.unwrap();
    let mut context = CgpSqlContext {
        aliases: IndexMap::new(),
        substitutions: IndexMap::new(),
    };
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
