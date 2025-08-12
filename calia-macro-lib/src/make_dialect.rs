use proc_macro2::TokenStream;
use quote::quote;
use syn::{parse::{Parse, ParseStream}, parse2, Ident, LitStr, Token, Type};
use crate::keywords::keyword;

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