use chumsky::{
    error::Error,
    input::Stream,
    label::LabelError,
    pratt::{infix, left, prefix},
    prelude::*,
    util::Maybe,
};
use indexmap::IndexMap;
use proc_macro2::{Delimiter, Span, TokenStream, TokenTree, token_stream::IntoIter};
use quote::{ToTokens, quote};
use syn::{Ident, Lit, Token, Type};

use crate::{
    CgpSqlContext, ToCgpSql,
    expressions::{
        BinaryOperatorClause, BindingClause, BindingsClause, ExpressionClause,
        FieldReferenceClause, FromClause, IntegerClause, PrimaryExpressionClause, ProjectionClause,
        SelectClause, StringClause, TableReferenceClause, UnaryPrefixOperatorClause,
    },
};

macro_rules! define_parser {
    ($var: ident, $keyword: literal, $message: literal) => {
        let $var = any().try_map(|x: TokenTree, s: SimpleSpan| {
            let parsed = syn::parse2::<Ident>(x.to_token_stream());
            match parsed {
                Ok(p) if p.to_string() == $keyword => Ok(x),
                _ => Err(Messaged::new($message, Simple::new(Some(Maybe::Val(x)), s))),
            }
        });
    };

    ($var: ident, $keyword: tt, $message: literal) => {
        let $var = any().try_map(|x: TokenTree, s: SimpleSpan| {
            let parsed = syn::parse2::<Token![$keyword]>(x.to_token_stream());
            match parsed {
                Ok(_) => Ok(x),
                _ => Err(Messaged::new($message, Simple::new(Some(Maybe::Val(x)), s))),
            }
        });
    };

    ($var: ident, $parsing: ty, $ident: ident => $body: expr, $message: literal) => {
        let $var = any().try_map(|x: TokenTree, s: SimpleSpan| {
            let parsed = syn::parse2::<$parsing>(x.to_token_stream());
            match parsed {
                Ok($ident) => $body,
                Err(_) => Err(Messaged::new($message, Simple::new(Some(Maybe::Val(x)), s))),
            }
        });
    };

    ($var: ident, $parsing: ty, $pattern: pat => $body: expr, $message: literal) => {
        let $var = any().try_map(|x: TokenTree, s: SimpleSpan| {
            let parsed = syn::parse2::<$parsing>(x.to_token_stream());
            match parsed {
                Ok(parsed) => match parsed {
                    $pattern => $body,
                    _ => Err(Messaged::new($message, Simple::new(Some(Maybe::Val(x)), s))),
                },
                Err(_) => Err(Messaged::new($message, Simple::new(Some(Maybe::Val(x)), s))),
            }
        });
    };
}

macro_rules! define_infix {
    ($assoc: expr, $op: ident, $clause: ty) => {
        infix(
            $assoc,
            $op,
            |l: ExpressionClause, _, r: ExpressionClause, _| {
                ExpressionClause::BinaryOperator(BinaryOperatorClause {
                    left: Box::new(l),
                    right: Box::new(r),
                    operator: quote! {$clause},
                    custom_ty: None,
                })
            },
        )
    };
}

struct QueryDef {
    assign: Type,
    clause: QueryClause,
}

enum QueryClause {
    Select(SelectClause),
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

fn parser() -> impl Parser<'static, Stream<IntoIter>, QueryDef, extra::Err<Messaged<'static>>> {
    define_parser!(ident, Ident, p => Ok(p), "expected identifier");
    define_parser!(ty, Type, p => Ok(p), "expected type");
    define_parser!(string_lit, Lit, Lit::Str(p) => Ok(p), "expected string");
    define_parser!(int_lit, Lit, Lit::Int(p) => Ok(p), "expected integer");
    define_parser!(select_keyword, "select", "expected 'select' keyword");
    define_parser!(from_keyword, "from", "expected 'from' keyword");
    define_parser!(not_keyword, "not", "expected 'not' keyword");
    define_parser!(as_keyword, as, "expected 'as' keyword");
    define_parser!(dot, ., "expected '.' symbol");
    define_parser!(comma, ,, "expected ',' symbol");
    define_parser!(eq, =, "expected '=' symbol");
    // define_parser!(lt, <, "expected '<' symbol");
    define_parser!(gt, >, "expected '>' symbol");
    define_parser!(at, @, "expected '@' symbol");
    let arrow = eq.then(gt);
    let custom_operator_clause = at.then(ty);

    let field_reference_clause = ident
        .then_ignore(dot)
        .then(ident)
        .map(|(alias, field)| FieldReferenceClause { alias, field });

    let primary_expression = string_lit
        .map(|x| {
            ExpressionClause::Primary(PrimaryExpressionClause::String(StringClause { value: x }))
        })
        .or(int_lit.map(|x| {
            ExpressionClause::Primary(PrimaryExpressionClause::Integer(IntegerClause { value: x }))
        }))
        .or(field_reference_clause
            .map(|x| ExpressionClause::Primary(PrimaryExpressionClause::FieldReference(x))))
        .map(|x| ExpressionClause::Primary(PrimaryExpressionClause::Expression(Box::new(x))));

    let parens = custom(|input| {
        let before = input.cursor();
        if let Some(next) = input.next() {
            match next {
                TokenTree::Group(group) if matches!(group.delimiter(), Delimiter::Parenthesis) => {
                    Ok(Stream::from_iter(group.stream().into_iter()))
                }
                _ => Err(Messaged::new(
                    "expression must be in parens",
                    Simple::new(Some(Maybe::Val(next)), input.span_since(&before)),
                )),
            }
        } else {
            Err(Messaged::new(
                "not found next tokens",
                Simple::new(None, input.span_since(&before)),
            ))
        }
    });

    let expression_clause = recursive(|rec| {
        let atom = primary_expression.or(rec.nested_in(parens));
        atom.pratt((
            define_infix!(left(1), eq, EqOperatorClause),
            infix(
                left(8),
                custom_operator_clause,
                |l: ExpressionClause, (_, custom_ty), r: ExpressionClause, _| {
                    ExpressionClause::BinaryOperator(BinaryOperatorClause {
                        left: Box::new(l),
                        right: Box::new(r),
                        operator: quote! {#custom_ty},
                        custom_ty: Some(custom_ty),
                    })
                },
            ),
            prefix(0, not_keyword, |_, r: ExpressionClause, _| {
                ExpressionClause::UnaryPrefixOperator(UnaryPrefixOperatorClause {
                    operator: quote! {NotOperatorClause},
                    value: Box::new(r),
                })
            }),
        ))
    });

    let projection_clause = expression_clause
        .then_ignore(as_keyword.clone())
        .then(ident)
        .separated_by(comma)
        .allow_trailing()
        .collect::<Vec<_>>()
        .map(|xs| {
            let bindings = xs
                .into_iter()
                .map(|(expression, alias)| BindingClause { expression, alias })
                .collect::<Vec<_>>();
            ProjectionClause {
                bindings: BindingsClause { values: bindings },
            }
        });

    let from_clause = ty
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
                        TableReferenceClause { ty: table },
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

    ty.then_ignore(arrow)
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
