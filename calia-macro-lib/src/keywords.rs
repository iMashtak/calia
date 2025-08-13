pub mod keyword {
    use syn::custom_keyword;

    custom_keyword!(select);
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
    custom_keyword!(like);
    custom_keyword!(ilike);
    custom_keyword!(overlaps);
    custom_keyword!(between);
    custom_keyword!(not);

    custom_keyword!(null);
}