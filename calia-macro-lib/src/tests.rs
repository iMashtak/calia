use proc_macro2::TokenStream;
use quote::quote;
use syn::parse2;

use crate::QueryClause;

#[test]
fn test_query_parsing() -> syn::Result<()> {
    let body: TokenStream = quote! {
        select ScopeView {
            id: scope.id,
            name: concat(scope.name, scope.name),
        }
        from ScopeTable as scope
        where (scope.name "=" "name" or scope.name "=" "other")
        and scope.id "in" "115"
    };
    let parsed: QueryClause = parse2(body)?;
    println!("{:?}", parsed);
    Ok(())
}
