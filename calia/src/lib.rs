use std::fmt::Display;

mod components;
mod functions;
mod lang;
mod operators;

pub mod prelude;

pub trait IsTable {
    type Name: Default + Display;
}

pub trait HasTypedField<Name> {
    type Type;
}