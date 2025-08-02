use indexmap::IndexMap;
use proc_macro2::TokenStream;
use quote::{ToTokens, quote};
use syn::{
    Error, Ident, LitInt, LitStr, Token, Type, braced, bracketed, parenthesized,
    parse::{Parse, ParseBuffer, ParseStream},
    parse2,
    punctuated::Punctuated,
    token,
};

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
    where_clause: Option<ExpressionClause>,
}

impl Parse for SelectClause {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let target_clause: SelectTargetClause = input.parse()?;
        input.parse::<keyword::from>()?;
        let from_clause: SelectFromClause = input.parse()?;
        let mut where_clause: Option<ExpressionClause> = Option::None;
        if input.peek(Token![where]) {
            input.parse::<Token![where]>()?;
            let mut where_expression = Vec::new();
            while !input.is_empty() {
                if input.peek(keyword::group)
                    || input.peek(keyword::order)
                    || input.peek(keyword::limit)
                    || input.peek(keyword::offset)
                {
                    break;
                }
                input.step(|cursor| {
                    let (token, next) = cursor
                        .token_tree()
                        .ok_or_else(|| input.error("Unexpected end of input"))?;
                    where_expression.push(token.into_token_stream());
                    Ok(((), next))
                })?;
            }
            let where_expression: TokenStream = where_expression.into_iter().collect();
            let where_expression: ExpressionClause = parse2(where_expression)?;
            where_clause = Some(where_expression);
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
            Some(x) => {
                let expression = x.to_cgp_query(context);
                quote! {WhereClause<#expression>}
            }
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
        let pairs = content.parse_terminated(SelectTargetStructPairClause::parse, token::Comma)?;
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
enum SimpleExpressionClause {
    String(StringClause),
    Integer(IntegerClause),
    FieldReference(FieldReferenceClause),
    FunctionCall(FunctionCallClause),
    SubstitutionPoint(SubstitutionPointClause),
}

impl Parse for SimpleExpressionClause {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(token::Paren) {
            let inner;
            parenthesized!(inner in input);
            return inner.parse();
        }
        if input.peek(LitStr) {
            Ok(SimpleExpressionClause::String(input.parse()?))
        } else if input.peek(LitInt) {
            Ok(SimpleExpressionClause::Integer(input.parse()?))
        } else if input.peek(Ident) && input.peek2(Token![.]) {
            Ok(SimpleExpressionClause::FieldReference(input.parse()?))
        } else if input.peek(Ident) && input.peek2(token::Paren) {
            Ok(SimpleExpressionClause::FunctionCall(input.parse()?))
        } else if input.peek(Token![|]) {
            Ok(SimpleExpressionClause::SubstitutionPoint(input.parse()?))
        } else {
            return Err(syn::Error::new(input.span(), "unknown kind of expression"));
        }
    }
}

impl ToCgpQuery for SimpleExpressionClause {
    fn to_cgp_query(&self, context: &mut CgpQueryContext) -> TokenStream {
        match self {
            SimpleExpressionClause::String(x) => x.to_cgp_query(context),
            SimpleExpressionClause::Integer(x) => x.to_cgp_query(context),
            SimpleExpressionClause::FieldReference(x) => x.to_cgp_query(context),
            SimpleExpressionClause::FunctionCall(x) => x.to_cgp_query(context),
            SimpleExpressionClause::SubstitutionPoint(x) => x.to_cgp_query(context),
        }
    }
}

#[derive(Debug)]
struct SubstitutionPointClause {
    ty: Type,
}

impl Parse for SubstitutionPointClause {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input.parse::<Token![|]>()?;
        let ty: Type = input.parse()?;
        input.parse::<Token![|]>()?;
        Ok(SubstitutionPointClause { ty })
    }
}

impl ToCgpQuery for SubstitutionPointClause {
    fn to_cgp_query(&self, context: &mut CgpQueryContext) -> TokenStream {
        let ty = &self.ty;
        if !context.substitutions.contains_key(&ty.clone()) {
            context.substitutions.insert(ty.clone(), ());
        }
        quote! {#ty}
    }
}

#[derive(Debug)]
enum ExpressionClause {
    SimpleExpression(SimpleExpressionClause),
    BinaryOperatorCall(BinaryOperatorCallClause),
}

impl Parse for ExpressionClause {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        fn peek_and_parse_binary_operator(content: &ParseBuffer) -> Option<TokenStream> {
            if content.peek(keyword::and) {
                let operator = Some(quote! {AndOperatorClause});
                content.parse::<keyword::and>().unwrap();
                return operator;
            }
            if content.peek(keyword::or) {
                let operator = Some(quote! {OrOperatorClause});
                content.parse::<keyword::or>().unwrap();
                return operator;
            }
            if content.peek(token::Lt) {
                let operator = Some(quote! {LtOperatorClause});
                content.parse::<token::Lt>().unwrap();
                return operator;
            }
            if content.peek(token::Gt) {
                let operator = Some(quote! {GtOperatorClause});
                content.parse::<token::Gt>().unwrap();
                return operator;
            }
            if content.peek(token::Eq) {
                let operator = Some(quote! {EqOperatorClause});
                content.parse::<token::Eq>().unwrap();
                return operator;
            }
            None
        }

        fn parse_inner(content: &ParseBuffer) -> syn::Result<ExpressionClause> {
            if content.peek(token::Paren) {
                let inner;
                parenthesized!(inner in content);
                let left = parse_inner(&inner)?;
                if content.is_empty() {
                    return Ok(left);
                }
                if let Some(operator) = peek_and_parse_binary_operator(content) {
                    let right: ExpressionClause = content.parse()?;
                    return Ok(ExpressionClause::BinaryOperatorCall(
                        BinaryOperatorCallClause {
                            left: Box::new(left),
                            operator,
                            right: Box::new(right),
                        },
                    ));
                }
                return Err(syn::Error::new(content.span(), "expected operator"));
            } else {
                let mut left = Vec::new();
                let mut operator = None;
                while !content.is_empty() {
                    if content.peek(Token![,]) {
                        break;
                    }
                    if let Some(op) = peek_and_parse_binary_operator(content) {
                        operator = Some(op);
                        break;
                    }
                    content.step(|cursor| {
                        let (token, next) = cursor
                            .token_tree()
                            .ok_or_else(|| content.error("Unexpected end of input"))?;
                        left.push(token.into_token_stream());
                        Ok(((), next))
                    })?;
                }
                let left: TokenStream = left.into_iter().collect();
                let left: SimpleExpressionClause = parse2(left)?;
                let left = ExpressionClause::SimpleExpression(left);
                if operator.is_none() {
                    return Ok(left);
                }
                let operator = operator.unwrap();
                let right: ExpressionClause = content.parse()?;
                return Ok(ExpressionClause::BinaryOperatorCall(
                    BinaryOperatorCallClause {
                        left: Box::new(left),
                        operator,
                        right: Box::new(right),
                    },
                ));
            };
        }

        parse_inner(input)
    }
}

impl ToCgpQuery for ExpressionClause {
    fn to_cgp_query(&self, context: &mut CgpQueryContext) -> TokenStream {
        match self {
            ExpressionClause::SimpleExpression(x) => x.to_cgp_query(context),
            ExpressionClause::BinaryOperatorCall(x) => x.to_cgp_query(context),
        }
    }
}

#[derive(Debug)]
struct BinaryOperatorCallClause {
    left: Box<ExpressionClause>,
    operator: TokenStream,
    right: Box<ExpressionClause>,
}

impl Parse for BinaryOperatorCallClause {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(token::Paren) {
            let content;
            parenthesized!(content in input);
            let left: ExpressionClause = content.parse()?;
            let operator = if content.peek(keyword::and) {
                quote! {AndOperatorClause}
            } else if content.peek(keyword::or) {
                quote! {OrOperatorClause}
            } else if content.peek(token::Lt) {
                quote! {LtOperatorClause}
            } else if content.peek(token::Gt) {
                quote! {GtOperatorClause}
            } else if content.peek(token::Eq) {
                quote! {EqOperatorClause}
            } else {
                return Err(Error::new(content.span(), "unknown operator"));
            };
            let right: ExpressionClause = input.parse()?;
            return Ok(BinaryOperatorCallClause {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            });
        } else {
            let mut left = Vec::new();
            let operator: Option<TokenStream>;
            loop {
                if input.peek(keyword::and) {
                    operator = Some(quote! {AndOperatorClause});
                    input.parse::<keyword::and>()?;
                    break;
                }
                if input.peek(keyword::or) {
                    operator = Some(quote! {OrOperatorClause});
                    input.parse::<keyword::or>()?;
                    break;
                }
                if input.peek(token::Lt) {
                    operator = Some(quote! {LtOperatorClause});
                    input.parse::<token::Lt>()?;
                    break;
                }
                if input.peek(token::Gt) {
                    operator = Some(quote! {GtOperatorClause});
                    input.parse::<token::Gt>()?;
                    break;
                }
                if input.peek(token::Eq) {
                    operator = Some(quote! {EqOperatorClause});
                    input.parse::<token::Eq>()?;
                    break;
                }
                input.step(|cursor| {
                    let (token, next) = cursor
                        .token_tree()
                        .ok_or_else(|| input.error("Unexpected end of input"))?;
                    left.push(token.into_token_stream());
                    Ok(((), next))
                })?;
            }
            if operator.is_none() {
                return Err(syn::Error::new(input.span(), "Not found operator"));
            }
            let left: TokenStream = left.into_iter().collect();
            let left: ExpressionClause = parse2(left)
                .map_err(|x| syn::Error::new(x.span(), format!("Cannot parse left: {}", x)))?;
            let right: ExpressionClause = input.parse()?;
            return Ok(BinaryOperatorCallClause {
                left: Box::new(left),
                operator: operator.unwrap(),
                right: Box::new(right),
            });
        }
    }
}

impl ToCgpQuery for BinaryOperatorCallClause {
    fn to_cgp_query(&self, context: &mut CgpQueryContext) -> TokenStream {
        let left = self.left.as_ref().to_cgp_query(context);
        let operator = &self.operator;
        let right = self.right.as_ref().to_cgp_query(context);
        quote! {
            BinaryOperatorCallClause<#left, #operator, #right>
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

    custom_keyword!(db);
    custom_keyword!(version);

    custom_keyword!(is);
}

trait ToCgpQuery {
    fn to_cgp_query(&self, context: &mut CgpQueryContext) -> TokenStream;
}

struct CgpQueryContext {
    aliases: IndexMap<Ident, Type>,
    substitutions: IndexMap<Type, ()>,
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
        substitutions: IndexMap::new(),
    };
    let query = parse2::<Query>(body).unwrap();
    let assign = query.assign;
    let query_clause = query.query_clause.to_cgp_query(&mut context);
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

#[derive(Debug)]
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

#[derive(Debug)]
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
    let mut context = CgpQueryContext {
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
    let expression = expression.to_cgp_query(&mut context);
    quote! {
        pub type #assign = #expression;
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
        pub struct #table_ty;

        impl IsTable for #table_ty {
            type Name = symbol!(#table_name);
        }

        #(#has_typed_field_impls)*
    }
}

#[derive(Debug)]
struct DialectDef {
    ty: Ident,
    provider: Type,
    db: Type,
    version: LitStr,
}

impl Parse for DialectDef {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ty: Ident = input.parse()?;
        input.parse::<Token![as]>()?;
        let provider: Type = input.parse()?;
        input.parse::<keyword::db>()?;
        let db: Type = input.parse()?;
        input.parse::<keyword::version>()?;
        let version: LitStr = input.parse()?;
        Ok(DialectDef {
            ty,
            provider,
            db,
            version,
        })
    }
}

pub fn make_dialect(body: TokenStream) -> TokenStream {
    let def: DialectDef = parse2(body).unwrap();
    let ty = def.ty;
    let ty_components = Ident::new((ty.to_string() + "Components").as_str(), ty.span());
    let provider = def.provider;
    let db = def.db;
    let version = def.version;

    quote! {
        #[cgp_context]
        pub struct #ty;

        delegate_components! {
            #ty_components {
                [
                    SelectClauseBuilderComponent,
                    TargetClauseBuilderComponent,
                    FromClauseBuilderComponent,
                    WhereClauseBuilderComponent,
                    TargetStructClauseFieldsCollectorComponent,
                    FunctionCallArgsCollectorComponent,
                    ProjectionBuilderComponent,
                    ExpressionBuilderComponent,
                ]: #provider,
                [
                    OperatorCheckerComponent,
                    FunctionCheckerComponent,
                ]: #db<symbol!(#version)>,
            }
        }
    }
}
