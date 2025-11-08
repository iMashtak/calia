use proc_macro::TokenStream;

#[proc_macro]
pub fn query(body: TokenStream) -> TokenStream {
    calia_macro_lib::make_query(body.into()).into()
}

#[proc_macro]
pub fn expression(body: TokenStream) -> TokenStream {
    calia_macro_lib::make_expression(body.into()).into()
}

#[proc_macro]
pub fn table(body: TokenStream) -> TokenStream {
    calia_macro_lib::make_table(body.into()).into()
}

#[proc_macro]
pub fn dialect(body: TokenStream) -> TokenStream {
    calia_macro_lib::make_dialect(body.into()).into()
}

#[proc_macro]
pub fn build(body: TokenStream) -> TokenStream {
    calia_macro_lib::make_build(body.into()).into()
}