mod impls;

pub use quote_value_derive::QuoteValue;
#[doc(hidden)] pub use { // used in proc macro
    proc_macro2,
    quote,
};

/// Allows serializing a values of implementing types to Rust token streams.
///
/// It is similar to [`quote::ToTokens`] with the added guarantee that the tokens parse to an expression of type `Self`.
pub trait QuoteValue {
    fn quote(&self) -> proc_macro2::TokenStream;
}
