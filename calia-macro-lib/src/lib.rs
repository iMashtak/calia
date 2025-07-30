use indexmap::IndexMap;
use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    Ident, LitInt, LitStr, Token, Type, braced, parenthesized,
    parse::{Parse, ParseBuffer, ParseStream},
    parse2,
    punctuated::Punctuated,
    token,
};

#[cfg(test)]
mod tests;

#[derive(Debug)]
enum QueryClause {
    Select(SelectClause),
}

impl Parse for QueryClause {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let query_type: Ident = input.parse()?;
        let query_type = query_type.to_string();
        match query_type.as_str() {
            "select" => Ok(QueryClause::Select(input.parse()?)),
            _ => panic!("Unknown keyword '{}'", query_type),
        }
    }
}

impl ToCgpQuery for QueryClause {
    fn to_cgp_query(&self, context: &mut CgpQueryContext) -> TokenStream {
        match self {
            QueryClause::Select(x) => x.to_cgp_query(context),
        }
    }
}

#[derive(Debug)]
struct SelectClause {
    target_clause: SelectTargetClause,
    from_clause: SelectFromClause,
    where_clause: Option<SelectWhereClause>,
}

impl Parse for SelectClause {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let target_clause: SelectTargetClause = input.parse()?;
        input.parse::<keyword::from>()?;
        let from_clause: SelectFromClause = input.parse()?;
        let mut where_clause: Option<SelectWhereClause> = Option::None;
        if input.peek(Token![where]) {
            input.parse::<Token![where]>()?;
            where_clause = Some(input.parse()?);
        }
        Ok(SelectClause {
            target_clause,
            from_clause,
            where_clause,
        })
    }
}

impl ToCgpQuery for SelectClause {
    fn to_cgp_query(&self, context: &mut CgpQueryContext) -> TokenStream {
        let from_tokens: TokenStream = self.from_clause.to_cgp_query(context);
        let target_tokens: TokenStream = self.target_clause.to_cgp_query(context);
        let where_tokens: TokenStream = match &self.where_clause {
            Some(x) => x.to_cgp_query(context),
            None => quote! {()},
        };
        quote! {
            SelectClause<#target_tokens, #from_tokens, #where_tokens>
        }
    }
}

#[derive(Debug)]
enum SelectTargetClause {
    Struct(SelectTargetStructClause),
}

impl Parse for SelectTargetClause {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(token::Brace) {
            Ok(SelectTargetClause::Struct(input.parse()?))
        } else {
            Err(input.error("Expected to have projection definition"))
        }
    }
}

impl ToCgpQuery for SelectTargetClause {
    fn to_cgp_query(&self, context: &mut CgpQueryContext) -> TokenStream {
        match self {
            SelectTargetClause::Struct(x) => x.to_cgp_query(context),
        }
    }
}

#[derive(Debug)]
struct SelectTargetStructClause {
    mapping: IndexMap<Ident, ExpressionClause>,
}

impl Parse for SelectTargetStructClause {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        braced!(content in input);
        let pairs: Punctuated<SelectTargetStructPairClause, token::Comma> =
            Punctuated::parse_terminated(&content)?;
        let mut mapping: IndexMap<Ident, ExpressionClause> = IndexMap::new();
        for pair in pairs {
            mapping.insert(pair.name, pair.expression);
        }
        Ok(SelectTargetStructClause { mapping })
    }
}

impl ToCgpQuery for SelectTargetStructClause {
    fn to_cgp_query(&self, context: &mut CgpQueryContext) -> TokenStream {
        let mut pairs: Vec<TokenStream> = Vec::new();
        for (name, expression) in &self.mapping {
            let name = LitStr::new(name.to_string().as_str(), name.span());
            let expression: TokenStream = expression.to_cgp_query(context);
            pairs.push(quote! {
                TargetStructFieldClause<symbol!(#name), #expression>
            });
        }
        quote! {
            TargetStructClause<Product![#(#pairs),*]>
        }
    }
}

struct SelectTargetStructPairClause {
    name: Ident,
    expression: ExpressionClause,
}

impl Parse for SelectTargetStructPairClause {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name: Ident = input.parse()?;
        input.parse::<Token![:]>()?;
        let expression: ExpressionClause = input.parse()?;
        Ok(SelectTargetStructPairClause { name, expression })
    }
}

#[derive(Debug)]
enum ExpressionClause {
    String(StringClause),
    Integer(IntegerClause),
    FieldReference(FieldReferenceClause),
    FunctionCall(FunctionCallClause),
}

impl Parse for ExpressionClause {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(LitStr) {
            Ok(ExpressionClause::String(input.parse()?))
        } else if input.peek(LitInt) {
            Ok(ExpressionClause::Integer(input.parse()?))
        } else if input.peek(Ident) && input.peek2(Token![.]) {
            Ok(ExpressionClause::FieldReference(input.parse()?))
        } else if input.peek(Ident) && input.peek2(token::Paren) {
            Ok(ExpressionClause::FunctionCall(input.parse()?))
        } else {
            Err(input.error("Invalid expression"))
        }
    }
}

impl ToCgpQuery for ExpressionClause {
    fn to_cgp_query(&self, context: &mut CgpQueryContext) -> TokenStream {
        match self {
            ExpressionClause::String(x) => x.to_cgp_query(context),
            ExpressionClause::Integer(x) => x.to_cgp_query(context),
            ExpressionClause::FieldReference(x) => x.to_cgp_query(context),
            ExpressionClause::FunctionCall(x) => x.to_cgp_query(context),
        }
    }
}

#[derive(Debug)]
struct StringClause {
    value: LitStr,
}

impl Parse for StringClause {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let value: LitStr = input.parse()?;
        Ok(StringClause { value })
    }
}

impl ToCgpQuery for StringClause {
    fn to_cgp_query(&self, _context: &mut CgpQueryContext) -> TokenStream {
        let value = &self.value;
        quote! {
            StringClause<symbol!(#value)>
        }
    }
}

#[derive(Debug)]
struct IntegerClause {
    value: LitInt,
}

impl Parse for IntegerClause {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let value: LitInt = input.parse()?;
        Ok(IntegerClause { value })
    }
}

impl ToCgpQuery for IntegerClause {
    fn to_cgp_query(&self, _context: &mut CgpQueryContext) -> TokenStream {
        let value = &self.value;
        let value = LitStr::new(value.base10_digits(), value.span());
        quote! {
            IntegerClause<symbol!(#value)>
        }
    }
}

#[derive(Debug)]
struct FieldReferenceClause {
    alias: Ident,
    name: Ident,
}

impl Parse for FieldReferenceClause {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let alias: Ident = input.parse()?;
        input.parse::<Token![.]>()?;
        let name: Ident = input.parse()?;
        Ok(FieldReferenceClause { alias, name })
    }
}

impl ToCgpQuery for FieldReferenceClause {
    fn to_cgp_query(&self, context: &mut CgpQueryContext) -> TokenStream {
        let alias_str = self.alias.to_string();
        let table = context
            .aliases
            .get(&self.alias)
            .expect(format!("Undeclared alias '{}'", alias_str).as_str());
        let alias = LitStr::new(alias_str.as_str(), self.alias.span());
        let name = LitStr::new(self.name.to_string().as_str(), self.name.span());
        quote! {
            FieldReferenceClause<#table, symbol!(#alias), symbol!(#name)>
        }
    }
}

#[derive(Debug)]
struct FunctionCallClause {
    name: Ident,
    args: Vec<ExpressionClause>,
}

impl Parse for FunctionCallClause {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name: Ident = input.parse()?;
        let content;
        parenthesized!(content in input);
        let args: Punctuated<ExpressionClause, token::Comma> =
            Punctuated::parse_terminated(&content)?;
        let args = args.into_iter().collect();
        Ok(FunctionCallClause { name, args })
    }
}

impl ToCgpQuery for FunctionCallClause {
    fn to_cgp_query(&self, context: &mut CgpQueryContext) -> TokenStream {
        let name = LitStr::new(self.name.to_string().as_str(), self.name.span());
        let mut args: Vec<TokenStream> = Vec::new();
        for arg in &self.args {
            args.push(arg.to_cgp_query(context));
        }
        quote! {
            FunctionCallClause<symbol!(#name), Product![#(#args),*]>
        }
    }
}

#[derive(Debug)]
struct SelectFromClause {
    table: Type,
    alias: Ident,
}

impl Parse for SelectFromClause {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let table: Type = input.parse()?;
        input.parse::<Token![as]>()?;
        let alias: Ident = input.parse()?;
        Ok(SelectFromClause { table, alias })
    }
}

impl ToCgpQuery for SelectFromClause {
    fn to_cgp_query(&self, context: &mut CgpQueryContext) -> TokenStream {
        context
            .aliases
            .insert(self.alias.clone(), self.table.clone());
        let table = &self.table;
        let alias = LitStr::new(self.alias.to_string().as_str(), self.alias.span());
        quote! {
            FromClause<#table, symbol!(#alias)>
        }
    }
}

#[derive(Debug)]
enum SelectWhereClause {
    Raw(SelectWhereRawClause),
    And(SelectWhereAndClause),
    Or(SelectWhereOrClause),
}

fn parse_where_content<'a>(input: &ParseBuffer<'a>) -> syn::Result<ParseBuffer<'a>> {
    let start = input.fork();
    while !input.is_empty() {
        if input.peek(keyword::group) || input.peek(keyword::order) {
            break;
        }
        input.step(|cursor| {
            let (_token, next) = cursor
                .token_tree()
                .ok_or_else(|| input.error("Unexpected end of input"))?;
            Ok(((), next))
        })?;
    }
    Ok(start)
}

enum WhereOperator {
    And,
    Or,
    None,
}

fn parse_where_item_content<'a>(
    input: &ParseBuffer<'a>,
) -> syn::Result<(ParseBuffer<'a>, WhereOperator)> {
    let start = input.fork();
    let mut operator = WhereOperator::None;
    while !input.is_empty() {
        if input.peek(keyword::and) {
            operator = WhereOperator::And;
            break;
        }
        if input.peek(keyword::or) {
            operator = WhereOperator::Or;
            break;
        }
        if input.peek(keyword::group) || input.peek(keyword::order) {
            break;
        }
        input.step(|cursor| {
            let (_token, next) = cursor
                .token_tree()
                .ok_or_else(|| input.error("Unexpected end of input"))?;
            Ok(((), next))
        })?;
    }
    Ok((start, operator))
}

fn parse_where_clause_rec<'a>(input: &ParseBuffer<'a>) -> syn::Result<SelectWhereClause> {
    if input.peek(token::Paren) {
        let content;
        parenthesized!(content in input);
        let left_where_clause = parse_where_clause_rec(&content)?;
        if input.peek(keyword::and) {
            input.parse::<keyword::and>()?;
            let right_where_clause = parse_where_clause_rec(input)?;
            Ok(SelectWhereClause::And(SelectWhereAndClause {
                left: Box::new(left_where_clause),
                right: Box::new(right_where_clause),
            }))
        } else if input.peek(keyword::or) {
            input.parse::<keyword::or>()?;
            let right_where_clause = parse_where_clause_rec(input)?;
            Ok(SelectWhereClause::Or(SelectWhereOrClause {
                left: Box::new(left_where_clause),
                right: Box::new(right_where_clause),
            }))
        } else {
            Ok(left_where_clause)
        }
    } else {
        let (content, operator) = parse_where_item_content(input)?;
        match operator {
            WhereOperator::And => {
                let left = SelectWhereRawClause::parse(&content)?;
                content.parse::<keyword::and>()?;
                let right = parse_where_clause_rec(&content)?;
                Ok(SelectWhereClause::And(SelectWhereAndClause {
                    left: Box::new(SelectWhereClause::Raw(left)),
                    right: Box::new(right),
                }))
            }
            WhereOperator::Or => {
                let left = SelectWhereRawClause::parse(&content)?;
                content.parse::<keyword::or>()?;
                let right = parse_where_clause_rec(&content)?;
                Ok(SelectWhereClause::Or(SelectWhereOrClause {
                    left: Box::new(SelectWhereClause::Raw(left)),
                    right: Box::new(right),
                }))
            }
            WhereOperator::None => Ok(SelectWhereClause::Raw(content.parse()?)),
        }
    }
}

impl Parse for SelectWhereClause {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content = parse_where_content(input)?;
        parse_where_clause_rec(&content)
    }
}

impl ToCgpQuery for SelectWhereClause {
    fn to_cgp_query(&self, context: &mut CgpQueryContext) -> TokenStream {
        match self {
            SelectWhereClause::Raw(x) => x.to_cgp_query(context),
            SelectWhereClause::And(x) => x.to_cgp_query(context),
            SelectWhereClause::Or(x) => x.to_cgp_query(context),
        }
    }
}

#[derive(Debug)]
struct SelectWhereRawClause {
    var: ExpressionClause,
    operator: LitStr,
    val: ExpressionClause,
}

impl Parse for SelectWhereRawClause {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let var: ExpressionClause = input.parse()?;
        let operator: LitStr = input.parse()?;
        let val: ExpressionClause = input.parse()?;
        Ok(SelectWhereRawClause { var, operator, val })
    }
}

impl ToCgpQuery for SelectWhereRawClause {
    fn to_cgp_query(&self, context: &mut CgpQueryContext) -> TokenStream {
        let var = self.var.to_cgp_query(context);
        let operator = &self.operator;
        let val = self.val.to_cgp_query(context);
        quote! {
            RawWhereClause<#var, symbol!(#operator), #val>
        }
    }
}

#[derive(Debug)]
struct SelectWhereAndClause {
    left: Box<SelectWhereClause>,
    right: Box<SelectWhereClause>,
}

impl ToCgpQuery for SelectWhereAndClause {
    fn to_cgp_query(&self, context: &mut CgpQueryContext) -> TokenStream {
        let left = self.left.to_cgp_query(context);
        let right = self.right.to_cgp_query(context);
        quote! {
            AndWhereClause<#left, #right>
        }
    }
}

#[derive(Debug)]
struct SelectWhereOrClause {
    left: Box<SelectWhereClause>,
    right: Box<SelectWhereClause>,
}

impl ToCgpQuery for SelectWhereOrClause {
    fn to_cgp_query(&self, context: &mut CgpQueryContext) -> TokenStream {
        let left = self.left.to_cgp_query(context);
        let right = self.right.to_cgp_query(context);
        quote! {
            OrWhereClause<#left, #right>
        }
    }
}

mod keyword {
    use syn::custom_keyword;

    custom_keyword!(and);
    custom_keyword!(or);
    custom_keyword!(from);
    custom_keyword!(by);
    custom_keyword!(group);
    custom_keyword!(order);
    custom_keyword!(limit);
    custom_keyword!(offset);
}

trait ToCgpQuery {
    fn to_cgp_query(&self, context: &mut CgpQueryContext) -> TokenStream;
}

struct CgpQueryContext {
    aliases: IndexMap<Ident, Type>,
}

struct Query {
    assign: Type,
    query_clause: QueryClause,
}

impl Parse for Query {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let assign: Type = input.parse()?;
        input.parse::<Token![,]>()?;
        let query_clause: QueryClause = input.parse()?;
        Ok(Query {
            assign,
            query_clause,
        })
    }
}

pub fn make_query(body: TokenStream) -> TokenStream {
    let mut context = CgpQueryContext {
        aliases: IndexMap::new(),
    };
    let query = parse2::<Query>(body).unwrap();
    let assign = query.assign;
    let query_clause = query.query_clause.to_cgp_query(&mut context);
    quote! {
        type #assign = #query_clause;
    }
}

struct TableDef {
    ty: Type,
    name: Ident,
    columns: Vec<Ident>,
}

impl Parse for TableDef {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ty: Type = input.parse()?;
        input.parse::<Token![as]>()?;
        let name: Ident = input.parse()?;
        let content;
        parenthesized!(content in input);
        let columns: Punctuated<Ident, token::Comma> = Punctuated::parse_terminated(&content)?;
        let columns = columns.into_iter().collect();
        Ok(TableDef { ty, name, columns })
    }
}

pub fn make_table(body: TokenStream) -> TokenStream {
    let table_def: TableDef = parse2(body).unwrap();
    let table_ty = table_def.ty;
    let table_name = table_def.name;
    let table_name = LitStr::new(table_name.to_string().as_str(), table_name.span());

    let mut has_typed_field_impls: Vec<TokenStream> = Vec::new();
    for name in &table_def.columns {
        let name = LitStr::new(name.to_string().as_str(), name.span());
        has_typed_field_impls.push(quote! {
            impl HasTypedField<symbol!(#name)> for ScopeTable {
                type Type = ();
            }
        });
    }
    quote! {
        struct #table_ty;

        impl IsTable for #table_ty {
            type Name = symbol!(#table_name);
        }

        #(#has_typed_field_impls)*
    }
}
