use {
    std::{
        collections::{
            BTreeMap,
            BTreeSet,
            HashMap,
            HashSet,
        },
        ffi::OsString,
        rc::Rc,
        sync::Arc,
    },
    quote::quote,
    crate::QuoteValue,
};

macro_rules! impl_quote_value_via_to_tokens {
    ($($ty:ty,)*) => {
        $(
            impl QuoteValue for $ty {
                fn quote(&self) -> proc_macro2::TokenStream {
                    quote!(#self)
                }
            }
        )*
    };
}

impl_quote_value_via_to_tokens! {
    i8, i16, i32, i64, i128, isize,
    u8, u16, u32, u64, u128, usize,
    f32, f64,
    char,
    bool,
}

macro_rules! impl_quote_value_tuple {
    ($($ty:ident),+) => {
        impl<$($ty: QuoteValue),+> QuoteValue for ($($ty,)+) {
            #[allow(non_snake_case)]
            fn quote(&self) -> proc_macro2::TokenStream {
                let ($($ty,)+) = self;
                $(
                    let $ty = $ty.quote();
                )+
                quote!(($(#$ty,)+))
            }
        }
    };
}

impl_quote_value_tuple!(A);
impl_quote_value_tuple!(A, B);
impl_quote_value_tuple!(A, B, C);
impl_quote_value_tuple!(A, B, C, D);
impl_quote_value_tuple!(A, B, C, D, E);
impl_quote_value_tuple!(A, B, C, D, E, F);
impl_quote_value_tuple!(A, B, C, D, E, F, G);
impl_quote_value_tuple!(A, B, C, D, E, F, G, H);
impl_quote_value_tuple!(A, B, C, D, E, F, G, H, I);
impl_quote_value_tuple!(A, B, C, D, E, F, G, H, I, J);
impl_quote_value_tuple!(A, B, C, D, E, F, G, H, I, J, K);
impl_quote_value_tuple!(A, B, C, D, E, F, G, H, I, J, K, L);

impl QuoteValue for () {
    fn quote(&self) -> proc_macro2::TokenStream {
        quote!(())
    }
}

impl<'a> QuoteValue for &'a str {
    fn quote(&self) -> proc_macro2::TokenStream {
        quote!(#self)
    }
}

impl QuoteValue for String {
    fn quote(&self) -> proc_macro2::TokenStream {
        quote!(::std::string::ToString::to_string(#self))
    }
}

impl<T: QuoteValue> QuoteValue for Box<T> {
    fn quote(&self) -> proc_macro2::TokenStream {
        let inner = T::quote(&self);
        quote!(::std::boxed::Box::new(#inner))
    }
}

impl<T: QuoteValue> QuoteValue for Rc<T> {
    fn quote(&self) -> proc_macro2::TokenStream {
        let inner = T::quote(&self);
        quote!(::std::rc::Rc::new(#inner))
    }
}

impl<T: QuoteValue> QuoteValue for Arc<T> {
    fn quote(&self) -> proc_macro2::TokenStream {
        let inner = T::quote(&self);
        quote!(::std::sync::Arc::new(#inner))
    }
}

impl<T: QuoteValue> QuoteValue for Option<T> {
    fn quote(&self) -> proc_macro2::TokenStream {
        match self {
            Some(x) => {
                let x = x.quote();
                quote!(::core::option::Option::Some(#x))
            }
            None => quote!(::core::option::Option::None),
        }
    }
}

impl<T: QuoteValue, E: QuoteValue> QuoteValue for Result<T, E> {
    fn quote(&self) -> proc_macro2::TokenStream {
        match self {
            Ok(x) => {
                let x = x.quote();
                quote!(::core::result::Result::Ok(#x))
            }
            Err(e) => {
                let e = e.quote();
                quote!(::core::result::Result::Err(#e))
            }
        }
    }
}

impl<T: QuoteValue> QuoteValue for Vec<T> {
    fn quote(&self) -> proc_macro2::TokenStream {
        let elts = self.iter().map(|elt| elt.quote());
        quote!(::std::vec![#(#elts,)*])
    }
}

impl<T: QuoteValue> QuoteValue for BTreeSet<T> {
    fn quote(&self) -> proc_macro2::TokenStream {
        let inserts = self.iter().map(|elt| {
            let elt = elt.quote();
            quote!(set.insert(#elt);)
        });
        quote!({
            let mut set = ::std::collections::BTreeSet::new();
            #(#inserts)*
            set
        })
    }
}

impl<T: QuoteValue> QuoteValue for HashSet<T> {
    fn quote(&self) -> proc_macro2::TokenStream {
        let len = self.len();
        let inserts = self.iter().map(|elt| {
            let elt = elt.quote();
            quote!(set.insert(#elt);)
        });
        quote!({
            let mut set = ::std::collections::HashSet::with_capacity(#len);
            #(#inserts)*
            set
        })
    }
}

impl<K: QuoteValue, V: QuoteValue> QuoteValue for BTreeMap<K, V> {
    fn quote(&self) -> proc_macro2::TokenStream {
        let inserts = self.iter().map(|(k, v)| {
            let k = k.quote();
            let v = v.quote();
            quote!(map.insert(#k, #v);)
        });
        quote!({
            let mut map = ::std::collections::BTreeMap::new();
            #(#inserts)*
            map
        })
    }
}

impl<K: QuoteValue, V: QuoteValue> QuoteValue for HashMap<K, V> {
    fn quote(&self) -> proc_macro2::TokenStream {
        let len = self.len();
        let inserts = self.iter().map(|(k, v)| {
            let k = k.quote();
            let v = v.quote();
            quote!(map.insert(#k, #v);)
        });
        quote!({
            let mut map = ::std::collections::HashMap::with_capacity(#len);
            #(#inserts)*
            map
        })
    }
}

#[cfg(unix)]
impl QuoteValue for OsString {
    fn quote(&self) -> proc_macro2::TokenStream {
        let buf = std::os::unix::OsStringExt::into_vec(self.clone()).quote();
        quote!(<::std::ffi::OsString as ::std::os::unix::ffi::OsStringExt>::from_vec(#buf))
    }
}

#[cfg(windows)]
impl QuoteValue for OsString {
    fn quote(&self) -> proc_macro2::TokenStream {
        let buf = std::os::windows::ffi::OsStrExt::encode_wide(&self[..]).collect::<Vec<_>>().quote();
        quote!(<::std::ffi::OsString as ::std::os::windows::ffi::OsStringExt>::from_wide(&#buf))
    }
}
