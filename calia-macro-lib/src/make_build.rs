use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    Ident, Token, Type,
    parse::{Parse, ParseStream}, parse2,
};

struct BuildDef {
    dialect: Ident,
    kind: Ident,
    ty: Type,
}

impl Parse for BuildDef {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input.parse::<Token![use]>()?;
        let dialect: Ident = input.parse()?;
        input.parse::<Token![for]>()?;
        let kind: Ident = input.parse()?;
        input.parse::<Token![in]>()?;
        let ty: Type = input.parse()?;
        Ok(BuildDef { dialect, kind, ty })
    }
}

pub fn make_build(body: TokenStream) -> TokenStream {
    let def: BuildDef = parse2(body).unwrap();
    let kind = def.kind;
    let method = Ident::new(format!("build_{}", kind).as_str(), kind.span());
    let dialect = def.dialect;
    let ty = def.ty;
    quote! {
        #dialect.#method(PhantomData::<#ty>)
    }
}
