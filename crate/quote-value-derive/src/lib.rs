use {
    proc_macro::TokenStream,
    proc_macro2::Span,
    quote::quote,
    syn::{
        Data,
        DataEnum,
        DataStruct,
        DeriveInput,
        Field,
        Fields,
        FieldsNamed,
        FieldsUnnamed,
        Ident,
        Variant,
        parse_macro_input,
    },
};

fn fields_pat(fields: &Fields) -> proc_macro2::TokenStream {
    match fields {
        Fields::Unit => quote!(),
        Fields::Unnamed(FieldsUnnamed { unnamed, .. }) => {
            let field_idents = unnamed.iter()
                .enumerate()
                .map(|(idx, _)| Ident::new(&format!("__field{}", idx), Span::call_site()));
            quote!((#(#field_idents,)*))
        }
        Fields::Named(FieldsNamed { named, .. }) => {
            let field_idents = named.iter()
                .map(|Field { ident, .. }| ident);
            quote!({ #(#field_idents,)* })
        }
    }
}

fn quote_fields(fields: &Fields) -> (proc_macro2::TokenStream, proc_macro2::TokenStream) {
    match fields {
        Fields::Unit => (quote!(), quote!()),
        Fields::Unnamed(FieldsUnnamed { unnamed, .. }) => {
            let field_names = (0..unnamed.len()).map(|idx| Ident::new(&format!("__field{}", idx), Span::call_site())).collect::<Vec<_>>();
            let field_defs = field_names.iter().zip(unnamed)
                .map(|(ident, Field { ty, .. })| quote!(let #ident = <#ty as ::quote_value::QuoteValue>::quote(#ident);));
            (
                quote!(#(#field_defs)*),
                quote!((#(##field_names,)*)),
            )
        }
        Fields::Named(FieldsNamed { named, .. }) => {
            let field_names = named.iter().map(|Field { ident, .. }| ident);
            let field_defs = named.iter()
                .map(|Field { ident, ty, .. }| quote!(let #ident = <#ty as ::quote_value::QuoteValue>::quote(#ident);));
            (
                quote!(#(#field_defs)*),
                quote!({ #(#field_names: ##field_names,)* }),
            )
        }
    }
}

#[proc_macro_derive(QuoteValue)]
pub fn derive_quote_value(input: TokenStream) -> TokenStream {
    let DeriveInput { ident: ty, generics, data, .. } = parse_macro_input!(input as DeriveInput);
    if generics.lt_token.is_some() || generics.where_clause.is_some() { return quote!(compile_error!("generics not supported in derive(QuoteValue)")).into() } //TODO
    let impl_body = match data {
        Data::Struct(DataStruct { fields, .. }) => {
            let fields_pat = fields_pat(&fields);
            let (outer, inner) = quote_fields(&fields);
            quote! {
                let #ty #fields_pat = self;
                #outer
                ::quote_value::quote::quote!(#ty #inner)
            }
        }
        Data::Enum(DataEnum { variants, .. }) => {
            let match_arms = variants.iter()
                .map(|Variant { ident: var, fields, .. }| {
                    let fields_pat = fields_pat(&fields);
                    let (outer, inner) = quote_fields(&fields);
                    quote!(#ty::#var #fields_pat => {
                        #outer
                        ::quote_value::quote::quote!(#ty::#var #inner)
                    })
                });
            quote! {
                match self {
                    #(#match_arms)*
                }
            }
        }
        Data::Union(_) => return quote!(compile_error!("unions not supported in derive(QuoteValue)")).into(),
    };
    TokenStream::from(quote! {
        impl ::quote_value::QuoteValue for #ty {
            fn quote(&self) -> ::quote_value::proc_macro2::TokenStream {
                #impl_body
            }
        }
    })
}
