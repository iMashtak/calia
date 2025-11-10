use proc_macro2::TokenStream;
use quote::{ToTokens as _, quote};
use syn::{
    Ident, LitInt, LitStr, Token, Type, parenthesized,
    parse::{Parse, ParseBuffer, ParseStream},
    parse2,
    punctuated::Punctuated,
    spanned::Spanned,
    token,
};

use crate::{CgpSqlContext, ToCgpSql, keywords::keyword};

// --- structure ---

#[derive(Clone, Debug)]
pub struct SelectClause {
    pub projection_clause: ProjectionClause,
    pub from_clause: Option<FromClause>,
    pub where_clause: Option<WhereClause>,
}

#[derive(Clone, Debug)]
pub struct ProjectionClause {
    pub bindings: BindingsClause,
}

#[derive(Clone, Debug)]
pub struct FromClause {
    pub bindings: BindingsClause,
}

#[derive(Clone, Debug)]
pub struct BindingsClause {
    pub values: Vec<BindingClause>,
}

#[derive(Clone, Debug)]
pub struct BindingClause {
    pub expression: ExpressionClause,
    pub alias: Ident,
}

#[derive(Clone, Debug)]
pub struct WhereClause {
    pub condition: ExpressionClause,
}

#[derive(Clone, Debug)]
pub enum ExpressionClause {
    Primary(PrimaryExpressionClause),
    UnaryPrefixOperator(UnaryPrefixOperatorClause),
    UnaryPostfixOperator(UnaryPostfixOperatorClause),
    BinaryOperator(BinaryOperatorClause),
    Case(CaseClause),
}

#[derive(Clone, Debug)]
pub enum PrimaryExpressionClause {
    Null,
    Parameter(ParameterClause),
    String(StringClause),
    Integer(IntegerClause),
    FieldReference(FieldReferenceClause),
    Function(FunctionClause),
    SubstitutionPoint(SubstitutionPointClause),
    Select(Box<SelectClause>),
    Expression(Box<ExpressionClause>),
    TableReference(TableReferenceClause),
}

#[derive(Clone, Debug)]
pub struct ParameterClause {
    pub name: Option<Ident>,
    pub number: Option<LitInt>,
}

#[derive(Clone, Debug)]
pub struct StringClause {
    pub value: LitStr,
}

#[derive(Clone, Debug)]
pub struct IntegerClause {
    pub value: LitInt,
}

#[derive(Clone, Debug)]
pub struct FieldReferenceClause {
    pub alias: Ident,
    pub field: Ident,
}

#[derive(Clone, Debug)]
pub struct FunctionClause {
    pub name: Ident,
    pub args: Vec<ExpressionClause>,
}

#[derive(Clone, Debug)]
pub struct SubstitutionPointClause {
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub struct TableReferenceClause {
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub struct UnaryPrefixOperatorClause {
    pub operator: TokenStream,
    pub value: Box<ExpressionClause>,
}

#[derive(Clone, Debug)]
pub struct UnaryPostfixOperatorClause {
    pub operator: TokenStream,
    pub value: Box<ExpressionClause>,
}

#[derive(Clone, Debug)]
pub struct BinaryOperatorClause {
    pub left: Box<ExpressionClause>,
    pub operator: TokenStream,
    pub right: Box<ExpressionClause>,
    pub custom_ty: Option<Type>,
}

#[derive(Clone, Debug)]
pub struct CaseClause {
    pub expression: Option<Box<ExpressionClause>>,
    pub when_then: Vec<(ExpressionClause, ExpressionClause)>,
    pub otherwise: Option<Box<ExpressionClause>>,
}

// query parsing

impl Parse for SelectClause {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if !input.peek(keyword::select) {
            return Err(input.error("expected 'select' keyword"));
        }
        input.parse::<keyword::select>()?;
        let projection_clause: ProjectionClause;
        let tokens = copy_until(input, |inner| inner.peek(keyword::from))?;
        projection_clause = parse2(tokens)?;
        let mut from_clause: Option<FromClause> = Option::None;
        if input.peek(keyword::from) {
            let tokens = copy_until(input, |inner| inner.peek(Token![where]))?;
            from_clause = Some(parse2(tokens)?);
        }
        let mut where_clause: Option<WhereClause> = Option::None;
        if input.peek(Token![where]) {
            let tokens = copy_until(input, |inner| inner.is_empty())?;
            where_clause = Some(parse2(tokens)?);
        }
        Ok(SelectClause {
            projection_clause,
            from_clause,
            where_clause,
        })
    }
}

impl ToCgpSql for SelectClause {
    fn to_cgp_sql(&self, context: &mut CgpSqlContext) -> TokenStream {
        let mut from_tokens = None;
        if let Some(from_clause) = &self.from_clause {
            from_tokens = Some(from_clause.to_cgp_sql(context));
        }
        let projection_tokens = self.projection_clause.to_cgp_sql(context);
        let mut where_tokens = None;
        if let Some(where_clause) = &self.where_clause {
            where_tokens = Some(where_clause.to_cgp_sql(context));
        }
        let mut content = Vec::new();
        content.push(projection_tokens);
        if let Some(from_tokens) = from_tokens {
            content.push(quote! {,});
            content.push(from_tokens);
        }
        if let Some(where_tokens) = where_tokens {
            content.push(quote! {,});
            content.push(where_tokens);
        }
        let content: TokenStream = content.into_iter().collect();
        quote! {SelectClause<#content>}
    }
}

impl Parse for ProjectionClause {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let bindings: BindingsClause = input.parse()?;
        Ok(ProjectionClause { bindings })
    }
}

impl ToCgpSql for ProjectionClause {
    fn to_cgp_sql(&self, context: &mut CgpSqlContext) -> TokenStream {
        let bindings = self.bindings.to_cgp_sql(context);
        quote! {ProjectionClause<#bindings>}
    }
}

impl Parse for BindingsClause {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut values = Vec::new();
        let mut first = true;
        while !input.is_empty() {
            if first {
                first = false;
            } else {
                input.parse::<Token![,]>()?;
            }
            let binding: BindingClause = input.parse()?;
            values.push(binding);
        }
        Ok(BindingsClause { values })
    }
}

impl ToCgpSql for BindingsClause {
    fn to_cgp_sql(&self, context: &mut CgpSqlContext) -> TokenStream {
        let mut result = Vec::new();
        for value in &self.values {
            result.push(value.to_cgp_sql(context));
        }
        quote! {Product![#(#result),*]}
    }
}

impl Parse for BindingClause {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content = copy_until(input, |x| x.peek(Token![as]))?;
        let expression: ExpressionClause = parse2(content)?;
        input.parse::<Token![as]>()?;
        let alias: Ident = input.parse()?;
        Ok(BindingClause { expression, alias })
    }
}

impl ToCgpSql for BindingClause {
    fn to_cgp_sql(&self, context: &mut CgpSqlContext) -> TokenStream {
        if let ExpressionClause::Primary(PrimaryExpressionClause::TableReference(table)) =
            &self.expression
        {
            context.aliases.insert(self.alias.clone(), table.ty.clone());
        }
        let expression = self.expression.to_cgp_sql(context);
        let alias = &self.alias;
        let alias = LitStr::new(alias.to_string().as_str(), alias.span());
        quote! {BindingClause<#expression, Symbol!(#alias)>}
    }
}

impl Parse for FromClause {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input.parse::<keyword::from>()?;
        let bindings: BindingsClause = input.parse()?;
        Ok(FromClause { bindings })
    }
}

impl ToCgpSql for FromClause {
    fn to_cgp_sql(&self, context: &mut CgpSqlContext) -> TokenStream {
        let bindings = self.bindings.to_cgp_sql(context);
        quote! {FromClause<#bindings>}
    }
}

impl Parse for WhereClause {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input.parse::<Token![where]>()?;
        let condition: ExpressionClause = input.parse()?;
        Ok(WhereClause { condition })
    }
}

impl ToCgpSql for WhereClause {
    fn to_cgp_sql(&self, context: &mut CgpSqlContext) -> TokenStream {
        let condition = self.condition.to_cgp_sql(context);
        quote! {WhereClause<#condition>}
    }
}

// --- expression parsing ---

#[derive(Clone, Debug)]
enum S {
    Atom(PrimaryExpressionClause),
    Cons(TokenStream, Vec<S>, OperatorPosition, Option<Type>),
}

#[derive(Clone, Debug)]
enum OperatorPosition {
    Prefix,
    Infix,
    Postfix,
}

impl Parse for ExpressionClause {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        fn parse_to_s_expression(input: &ParseBuffer, min_binding_power: i32) -> syn::Result<S> {
            let mut left_s = if input.peek(Token![!]) || input.peek(keyword::not) {
                if input.peek(Token![!]) {
                    input.parse::<Token![!]>()?;
                } else if input.peek(keyword::not) {
                    input.parse::<keyword::not>()?;
                }
                let right_binding_power = 41;
                let right_s = parse_to_s_expression(input, right_binding_power)?;
                S::Cons(
                    quote! {NotOperatorClause},
                    vec![right_s],
                    OperatorPosition::Prefix,
                    None,
                )
            } else {
                let left: PrimaryExpressionClause = input.parse()?;
                S::Atom(left)
            };
            loop {
                if input.is_empty() {
                    break;
                }
                let mut custom_ty = None;
                let (operator, left_binding_power, right_binding_power) = if input.peek(keyword::or)
                {
                    (quote! {OrOperatorClause}, 10, 11)
                } else if input.peek(keyword::and) {
                    (quote! {AndOperatorClause}, 30, 31)
                } else if input.peek(Token![=]) {
                    (quote! {EqOperatorClause}, 50, 51)
                } else if input.peek(Token![<]) {
                    (quote! {LtOperatorClause}, 70, 71)
                } else if input.peek(Token![>]) {
                    (quote! {GtOperatorClause}, 70, 71)
                } else if input.peek(keyword::like) {
                    (quote! {LikeOperatorClause}, 90, 91)
                } else if input.peek(keyword::ilike) {
                    (quote! {ILikeOperatorClause}, 90, 91)
                } else if input.peek(keyword::overlaps) {
                    (quote! {OverlapsOperatorClause}, 110, 111)
                } else if input.peek(Token![in]) {
                    (quote! {InOperatorClause}, 150, 151)
                } else if input.peek(keyword::is) {
                    (quote! {IsOperatorClause}, 190, 191)
                } else if input.peek(Token![+]) {
                    (quote! {PlusOperatorClause}, 210, 211)
                } else if input.peek(Token![-]) {
                    (quote! {MinusOperatorClause}, 210, 211)
                } else if input.peek(Token![*]) {
                    (quote! {MulOperatorClause}, 230, 231)
                } else if input.peek(Token![/]) {
                    (quote! {DivOperatorClause}, 230, 231)
                } else if input.peek(Token![%]) {
                    (quote! {ModOperatorClause}, 230, 231)
                } else if input.peek(Token![^]) {
                    (quote! {ExpOperatorClause}, 250, 251)
                } else if input.peek(Token![@]) && input.peek2(Ident) {
                    let input = input.fork();
                    input.parse::<Token![@]>()?;
                    let op = input.parse::<Type>()?;
                    custom_ty = Some(op.clone());
                    (quote! {#op}, 80, 81)
                } else {
                    return Err(input.error("unknown operator"));
                };
                if left_binding_power < min_binding_power {
                    break;
                }
                if input.peek(keyword::or) {
                    input.parse::<keyword::or>()?;
                } else if input.peek(keyword::and) {
                    input.parse::<keyword::and>()?;
                } else if input.peek(Token![=]) {
                    input.parse::<Token![=]>()?;
                } else if input.peek(Token![<]) {
                    input.parse::<Token![<]>()?;
                } else if input.peek(Token![>]) {
                    input.parse::<Token![>]>()?;
                } else if input.peek(keyword::like) {
                    input.parse::<keyword::like>()?;
                } else if input.peek(keyword::ilike) {
                    input.parse::<keyword::ilike>()?;
                } else if input.peek(keyword::overlaps) {
                    input.parse::<keyword::overlaps>()?;
                } else if input.peek(Token![in]) {
                    input.parse::<Token![in]>()?;
                } else if input.peek(keyword::is) {
                    input.parse::<keyword::is>()?;
                } else if input.peek(Token![+]) {
                    input.parse::<Token![+]>()?;
                } else if input.peek(Token![-]) {
                    input.parse::<Token![-]>()?;
                } else if input.peek(Token![*]) {
                    input.parse::<Token![*]>()?;
                } else if input.peek(Token![/]) {
                    input.parse::<Token![/]>()?;
                } else if input.peek(Token![%]) {
                    input.parse::<Token![%]>()?;
                } else if input.peek(Token![^]) {
                    input.parse::<Token![^]>()?;
                } else if input.peek(Token![@]) && input.peek2(Ident) {
                    input.parse::<Token![@]>()?;
                    input.parse::<Type>()?;
                } else {
                    return Err(input.error("unknown operator"));
                };
                let right_s = parse_to_s_expression(input, right_binding_power)?;
                left_s = S::Cons(
                    operator,
                    vec![left_s, right_s],
                    OperatorPosition::Infix,
                    custom_ty,
                );
            }
            Ok(left_s)
        }

        fn convert_to_def(s: S) -> syn::Result<ExpressionClause> {
            match s {
                S::Atom(clause) => Ok(ExpressionClause::Primary(clause)),
                S::Cons(operator, args, position, custom_ty) => {
                    if args.len() == 1 {
                        let value = convert_to_def(args[0].clone())?;
                        match position {
                            OperatorPosition::Prefix => Ok(ExpressionClause::UnaryPrefixOperator(
                                UnaryPrefixOperatorClause {
                                    operator,
                                    value: Box::new(value),
                                },
                            )),
                            OperatorPosition::Postfix => {
                                Ok(ExpressionClause::UnaryPostfixOperator(
                                    UnaryPostfixOperatorClause {
                                        operator,
                                        value: Box::new(value),
                                    },
                                ))
                            }
                            _ => unreachable!(),
                        }
                    } else if args.len() == 2 {
                        let left = convert_to_def(args[0].clone())?;
                        let right = convert_to_def(args[1].clone())?;
                        Ok(ExpressionClause::BinaryOperator(BinaryOperatorClause {
                            left: Box::new(left),
                            operator,
                            right: Box::new(right),
                            custom_ty,
                        }))
                    } else {
                        Err(syn::Error::new(
                            operator.span(),
                            "unexpected operator arity",
                        ))
                    }
                }
            }
        }

        let s = parse_to_s_expression(input, 0)?;
        let result = convert_to_def(s)?;
        Ok(result)
    }
}

impl ToCgpSql for ExpressionClause {
    fn to_cgp_sql(&self, context: &mut CgpSqlContext) -> TokenStream {
        match self {
            Self::Primary(x) => x.to_cgp_sql(context),
            Self::BinaryOperator(x) => x.to_cgp_sql(context),
            Self::UnaryPrefixOperator(x) => x.to_cgp_sql(context),
            Self::UnaryPostfixOperator(x) => x.to_cgp_sql(context),
            _ => todo!(),
        }
    }
}

impl ToCgpSql for UnaryPrefixOperatorClause {
    fn to_cgp_sql(&self, context: &mut CgpSqlContext) -> TokenStream {
        let operator = &self.operator;
        let value = self.value.to_cgp_sql(context);
        quote! {
            UnaryPrefixOperatorClause<#operator, #value>
        }
    }
}

impl ToCgpSql for UnaryPostfixOperatorClause {
    fn to_cgp_sql(&self, context: &mut CgpSqlContext) -> TokenStream {
        let operator = &self.operator;
        let value = self.value.to_cgp_sql(context);
        quote! {
            UnaryPostfixOperatorClause<#operator, #value>
        }
    }
}

impl ToCgpSql for BinaryOperatorClause {
    fn to_cgp_sql(&self, context: &mut CgpSqlContext) -> TokenStream {
        let left = self.left.as_ref().to_cgp_sql(context);
        let operator = &self.operator;
        let right = self.right.as_ref().to_cgp_sql(context);
        if let Some(ty) = self.custom_ty.clone() {
            if !context.substitutions.contains_key(&ty) {
                context.substitutions.insert(ty, ());
            }
        }
        quote! {
            BinaryOperatorClause<#left, #operator, #right>
        }
    }
}

impl Parse for PrimaryExpressionClause {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(keyword::null) {
            input.parse::<keyword::null>()?;
            return Ok(PrimaryExpressionClause::Null);
        }
        if input.peek(Token![?]) {
            return Ok(PrimaryExpressionClause::Parameter(input.parse()?));
        }
        if input.peek(LitStr) {
            return Ok(PrimaryExpressionClause::String(input.parse()?));
        }
        if input.peek(LitInt) {
            return Ok(PrimaryExpressionClause::Integer(input.parse()?));
        }
        if input.peek(Ident) && input.peek2(Token![.]) {
            return Ok(PrimaryExpressionClause::FieldReference(input.parse()?));
        }
        if input.peek(Ident) && input.peek2(token::Paren) {
            return Ok(PrimaryExpressionClause::Function(input.parse()?));
        }
        if input.peek(Token![|]) && input.peek2(Ident) {
            return Ok(PrimaryExpressionClause::SubstitutionPoint(input.parse()?));
        }
        if input.peek(keyword::select) {
            return Ok(PrimaryExpressionClause::Select(Box::new(input.parse()?)));
        }
        if input.peek(Ident) {
            return Ok(PrimaryExpressionClause::TableReference(input.parse()?));
        }
        if input.peek(token::Paren) {
            let content;
            parenthesized!(content in input);
            return Ok(PrimaryExpressionClause::Expression(Box::new(
                content.parse()?,
            )));
        }
        Err(input.error("unknown expression"))
    }
}

impl ToCgpSql for PrimaryExpressionClause {
    fn to_cgp_sql(&self, context: &mut CgpSqlContext) -> TokenStream {
        match self {
            PrimaryExpressionClause::Null => quote! {NullValueClause},
            PrimaryExpressionClause::Parameter(x) => x.to_cgp_sql(context),
            PrimaryExpressionClause::String(x) => x.to_cgp_sql(context),
            PrimaryExpressionClause::Integer(x) => x.to_cgp_sql(context),
            PrimaryExpressionClause::FieldReference(x) => x.to_cgp_sql(context),
            PrimaryExpressionClause::Function(x) => x.to_cgp_sql(context),
            PrimaryExpressionClause::SubstitutionPoint(x) => x.to_cgp_sql(context),
            PrimaryExpressionClause::Select(x) => x.to_cgp_sql(context),
            PrimaryExpressionClause::Expression(x) => x.to_cgp_sql(context),
            PrimaryExpressionClause::TableReference(x) => x.to_cgp_sql(context),
        }
    }
}

impl Parse for ParameterClause {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input.parse::<Token![?]>()?;
        if input.peek(LitInt) {
            let number: LitInt = input.parse()?;
            Ok(ParameterClause {
                name: None,
                number: Some(number),
            })
        } else if input.peek(Ident) {
            let name: Ident = input.parse()?;
            Ok(ParameterClause {
                name: Some(name),
                number: None,
            })
        } else {
            Ok(ParameterClause {
                name: None,
                number: None,
            })
        }
    }
}

impl ToCgpSql for ParameterClause {
    fn to_cgp_sql(&self, _context: &mut CgpSqlContext) -> TokenStream {
        match (&self.name, &self.number) {
            (Some(name), None) => {
                let name = LitStr::new(name.to_string().as_str(), name.span());
                quote! {NamedParameterClause<Symbol!(#name)>}
            }
            (None, Some(number)) => {
                let number = LitStr::new(number.base10_digits(), number.span());
                quote! {NumberedParameterClause<Symbol!(#number)>}
            }
            (None, None) => {
                quote! {UnmarkedParameterClause}
            }
            _ => unreachable!(),
        }
    }
}

impl Parse for StringClause {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let value: LitStr = input.parse()?;
        Ok(StringClause { value })
    }
}

impl ToCgpSql for StringClause {
    fn to_cgp_sql(&self, _context: &mut CgpSqlContext) -> TokenStream {
        let value = &self.value;
        quote! {
            StringClause<Symbol!(#value)>
        }
    }
}

impl Parse for IntegerClause {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let value: LitInt = input.parse()?;
        Ok(IntegerClause { value })
    }
}

impl ToCgpSql for IntegerClause {
    fn to_cgp_sql(&self, _context: &mut CgpSqlContext) -> TokenStream {
        let value = &self.value;
        let value = LitStr::new(value.base10_digits(), value.span());
        quote! {
            IntegerClause<Symbol!(#value)>
        }
    }
}

impl Parse for FieldReferenceClause {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let alias: Ident = input.parse()?;
        input.parse::<Token![.]>()?;
        let field: Ident = input.parse()?;
        Ok(FieldReferenceClause { alias, field })
    }
}

impl ToCgpSql for FieldReferenceClause {
    fn to_cgp_sql(&self, context: &mut CgpSqlContext) -> TokenStream {
        let alias_str = self.alias.to_string();
        let table = context
            .aliases
            .get(&self.alias)
            .expect(format!("Undeclared alias '{}'", alias_str).as_str());
        let alias = LitStr::new(alias_str.as_str(), self.alias.span());
        let name = LitStr::new(self.field.to_string().as_str(), self.field.span());
        quote! {
            FieldReferenceClause<#table, Symbol!(#alias), Symbol!(#name)>
        }
    }
}

impl Parse for FunctionClause {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name: Ident = input.parse()?;
        let content;
        parenthesized!(content in input);
        let args: Punctuated<ExpressionClause, token::Comma> =
            Punctuated::parse_terminated(&content)?;
        let args = args.into_iter().collect();
        Ok(FunctionClause { name, args })
    }
}

impl ToCgpSql for FunctionClause {
    fn to_cgp_sql(&self, context: &mut CgpSqlContext) -> TokenStream {
        let name = LitStr::new(self.name.to_string().as_str(), self.name.span());
        let mut args: Vec<TokenStream> = Vec::new();
        for arg in &self.args {
            args.push(arg.to_cgp_sql(context));
        }
        quote! {
            FunctionCallClause<Symbol!(#name), Product![#(#args),*]>
        }
    }
}

impl Parse for SubstitutionPointClause {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input.parse::<Token![|]>()?;
        let ty: Type = input.parse()?;
        input.parse::<Token![|]>()?;
        Ok(SubstitutionPointClause { ty })
    }
}

impl ToCgpSql for SubstitutionPointClause {
    fn to_cgp_sql(&self, context: &mut CgpSqlContext) -> TokenStream {
        let ty = &self.ty;
        if !context.substitutions.contains_key(&ty.clone()) {
            context.substitutions.insert(ty.clone(), ());
        }
        quote! {#ty}
    }
}

impl Parse for TableReferenceClause {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ty: Type = input.parse()?;
        Ok(TableReferenceClause { ty })
    }
}

impl ToCgpSql for TableReferenceClause {
    fn to_cgp_sql(&self, _context: &mut CgpSqlContext) -> TokenStream {
        let ty = &self.ty;
        quote! {TableReferenceClause<#ty>}
    }
}

// --- utils ---

fn copy_until(input: &ParseBuffer, until: fn(&ParseBuffer) -> bool) -> syn::Result<TokenStream> {
    let mut result = Vec::new();
    while !input.is_empty() {
        if input.peek(token::Paren) && input.peek2(keyword::select) {
            let content;
            parenthesized!(content in input);
            let content = content.cursor().token_stream();
            result.push(quote! {(#content)});
        }
        if until(input) {
            break;
        }
        input.step(|cursor| {
            let (token, next) = cursor
                .token_tree()
                .ok_or_else(|| input.error("Unexpected end of input"))?;
            result.push(token.into_token_stream());
            Ok(((), next))
        })?;
    }
    Ok(result.into_iter().collect())
}
