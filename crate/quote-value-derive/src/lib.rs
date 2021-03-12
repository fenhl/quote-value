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
        Path,
        PathSegment,
        Token,
        TraitBound,
        TraitBoundModifier,
        TypeParamBound,
        Variant,
        WherePredicate,
        parenthesized,
        parse::{
            Parse,
            ParseStream,
            Result,
        },
        parse_macro_input,
        punctuated::Punctuated,
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

enum QuoteValueAttr {
    Where(Punctuated<WherePredicate, Token![,]>),
}

impl Parse for QuoteValueAttr {
    fn parse(input: ParseStream<'_>) -> Result<QuoteValueAttr> {
        let lookahead = input.lookahead1();
        Ok(if lookahead.peek(Token![where]) {
            let _ = input.parse::<Token![where]>()?;
            let content;
            parenthesized!(content in input);
            QuoteValueAttr::Where(content.parse_terminated(WherePredicate::parse)?)
        } else {
            return Err(lookahead.error())
        })
    }
}

#[proc_macro_derive(QuoteValue, attributes(quote_value))]
pub fn derive_quote_value(input: TokenStream) -> TokenStream {
    let DeriveInput { attrs, ident: ty, generics, data, .. } = parse_macro_input!(input as DeriveInput);
    let mut quote_value_attrs = attrs.into_iter().filter(|attr| attr.path.get_ident().map_or(false, |ident| ident == "quote_value")).fuse();
    let impl_generics = match (quote_value_attrs.next(), quote_value_attrs.next()) {
        (None, _) => {
            let mut impl_generics = generics.clone();
            for param in impl_generics.type_params_mut() {
                param.colon_token.get_or_insert_with(<Token![:]>::default);
                param.bounds.push(TypeParamBound::Trait(TraitBound {
                    paren_token: None,
                    modifier: TraitBoundModifier::None,
                    lifetimes: None,
                    path: Path {
                        leading_colon: Some(<Token![::]>::default()),
                        segments: vec![PathSegment::from(Ident::new("quote_value", Span::call_site())), PathSegment::from(Ident::new("QuoteValue", Span::call_site()))].into_iter().collect(),
                    },
                }));
            }
            impl_generics
        }
        (Some(attr), None) => match attr.parse_args() {
            Ok(QuoteValueAttr::Where(predicates)) => {
                let mut impl_generics = generics.clone();
                impl_generics.make_where_clause().predicates.extend(predicates);
                impl_generics
            }
            Err(e) => return e.to_compile_error().into(),
        },
        (Some(_), Some(_)) => return quote!(compile_error!("found multiple quote_value attributes")).into(),
    };
    let (impl_generics, ty_generics, where_clause) = impl_generics.split_for_impl();
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
        impl #impl_generics ::quote_value::QuoteValue for #ty #ty_generics #where_clause {
            fn quote(&self) -> ::quote_value::proc_macro2::TokenStream {
                #impl_body
            }
        }
    })
}
