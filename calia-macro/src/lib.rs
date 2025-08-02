use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{
    Ident, LitInt, Token, Type,
    parse::{Parse, ParseStream},
    parse_macro_input,
};

struct RepeatedDefinitions {
    count: LitInt,
    ty: Type,
}

impl Parse for RepeatedDefinitions {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let count: LitInt = input.parse()?;
        input.parse::<Token![,]>()?;
        let ty: Type = input.parse()?;
        Ok(RepeatedDefinitions { count, ty })
    }
}

#[proc_macro]
pub fn define_target_struct_clause_fields_impls(body: TokenStream) -> TokenStream {
    let RepeatedDefinitions { count, ty } = parse_macro_input!(body as RepeatedDefinitions);
    let count: usize = count.base10_parse().unwrap();

    let mut defs: Vec<TokenStream> = Vec::new();
    for i in 0..count + 1 {
        let types: Vec<Ident> = (0..i).map(|j| format_ident!("F{}", j)).collect();
        let result: TokenStream = quote! {
            #[cgp_provider]
            impl<Context, #(#types),*>
                TargetStructClauseFieldsCollector<Context, Product![#(#types),*]> for #ty
            where
                Context: #(CanBuildProjection<#types>)+*
            {
                fn collect_target_struct_clause_fields(
                    context: &Context,
                    _code: PhantomData<Product![#(#types),*]>,
                    collection: &mut Vec<String>,
                ) {
                    #(collection.push(context.build_projection(PhantomData::<#types>)));*
                }
            }
        }
        .into();
        defs.push(result);
    }
    defs.into_iter().collect()
}

#[proc_macro]
pub fn define_function_call_args_impls(body: TokenStream) -> TokenStream {
    let RepeatedDefinitions { count, ty } = parse_macro_input!(body as RepeatedDefinitions);
    let count: usize = count.base10_parse().unwrap();

    let mut defs: Vec<TokenStream> = Vec::new();
    for i in 0..count + 1 {
        let types: Vec<Ident> = (0..i).map(|j| format_ident!("F{}", j)).collect();
        let result: TokenStream = quote! {
            #[cgp_provider]
            impl<Context, #(#types),*>
                FunctionCallArgsCollector<Context, Product![#(#types),*]> for #ty
            where
                Context: #(CanBuildExpression<#types>)+*
            {
                fn collect_function_call_args(
                    context: &Context,
                    _code: PhantomData<Product![#(#types),*]>,
                    collection: &mut Vec<String>,
                ) {
                    #(collection.push(context.build_expression(PhantomData::<#types>)));*
                }
            }
        }
        .into();
        defs.push(result);
    }
    defs.into_iter().collect()
}

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