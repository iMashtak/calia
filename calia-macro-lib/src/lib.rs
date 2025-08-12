use indexmap::IndexMap;
use proc_macro2::TokenStream;

mod keywords;
mod expressions;

mod make_dialect;
pub use make_dialect::make_dialect;

mod make_table;
pub use make_table::make_table;

mod make_query;
pub use make_query::make_query;

mod make_expression;
pub use make_expression::make_expression;
use syn::{Ident, Type};

struct CgpSqlContext {
    aliases: IndexMap<Ident, Type>,
    substitutions: IndexMap<Type, ()>,
}

trait ToCgpSql {
    fn to_cgp_sql(&self, context: &mut CgpSqlContext) -> TokenStream;
}