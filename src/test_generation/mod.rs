use crate::types::Context;

mod actions;
mod boilerplate;
mod structs;

pub fn generate_tests(ctx: Context) -> String {
    format!(
        "{}{}{}{}",
        structs::translate_structs(ctx.clone()),
        boilerplate::TEST_HEADER,
        actions::translate_actions(ctx),
        boilerplate::TEST_FOOTER
    )
}
