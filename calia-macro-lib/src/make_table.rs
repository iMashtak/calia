use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    Ident, LitStr, Token, Type, parenthesized,
    parse::{Parse, ParseStream},
    parse2,
    punctuated::Punctuated,
    token,
};

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
            impl HasTypedField<symbol!(#name)> for #table_ty {
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
