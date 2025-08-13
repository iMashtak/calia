use indexmap::IndexMap;
use proc_macro2::TokenStream;
use quote::quote;
use syn::{braced, parse::{Parse, ParseStream}, parse2, token, Ident, LitStr, Token, Type};
use crate::keywords::keyword;

struct DialectDef {
    ty: Ident,
    params: IndexMap<Ident, Type>,
    provider: Type,
    db: Type,
    version: LitStr,
}

impl Parse for DialectDef {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ty: Ident = input.parse()?;
        let inner;
        braced!(inner in input);
        let mut params = IndexMap::new();
        let punctuated = inner.parse_terminated(|inner| {
            let ident: Ident = inner.parse()?;
            inner.parse::<Token![:]>()?;
            let ty: Type = inner.parse()?;
            Ok((ident, ty))
        }, token::Comma)?;
        for (ident, ty) in punctuated {
            params.insert(ident, ty);
        }
        input.parse::<Token![as]>()?;
        let provider: Type = input.parse()?;
        input.parse::<keyword::db>()?;
        let db: Type = input.parse()?;
        input.parse::<keyword::version>()?;
        let version: LitStr = input.parse()?;
        Ok(DialectDef {
            ty,
            params,
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
    let mut params = Vec::new();
    for (ident, ty) in def.params {
        let param = quote! {#ident: #ty};
        params.push(param);
    }
    let provider = def.provider;
    let db = def.db;
    let version = def.version;

    quote! {
        #[derive(HasField)]
        #[cgp_context]
        pub struct #ty {
            #(#params),*
        }

        delegate_components! {
            #ty_components {
                [
                    SelectClauseBuilderComponent,
                    ProjectionClauseBuilderComponent,
                    FromClauseBuilderComponent,
                    BindingClauseBuilderComponent,
                    BindingsCollectorComponent,
                    WhereClauseBuilderComponent,
                    ExpressionClauseBuilderComponent,
                    FunctionCallArgsCollectorComponent,
                ]: #provider,
                [
                    OperatorCheckerComponent,
                    FunctionCheckerComponent,
                ]: #db<symbol!(#version)>,
            }
        }
    }
}