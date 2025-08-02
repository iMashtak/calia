use std::fmt::Display;

use cgp::prelude::*;

#[cgp_component(FunctionChecker)]
pub trait HasFunction<Name>
where
    Name: Default + Display,
{
}