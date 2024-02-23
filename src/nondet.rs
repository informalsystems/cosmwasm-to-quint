use itertools::Itertools;

use crate::types::{Constructor, Context};

pub trait NondetValue {
    fn nondet_value(&self, ctx: &Context, name: &str) -> String;
}

impl NondetValue for Constructor {
    fn nondet_value(&self, _ctx: &Context, ident: &str) -> String {
        let values_for_fields = self
            .fields
            .iter()
            .map(|field| field.nondet_value.clone())
            .collect_vec()
            .join("\n  ");

        let msg_params = if self.fields.is_empty() {
            "".to_string()
        } else {
            let field_params = self
                .fields
                .iter()
                .map(|field| {
                    let name = field.name.clone();
                    format!("{name}: {name}")
                })
                .collect_vec()
                .join(", ");

            format!("({{ {field_params} }})")
        };

        format!(
            "
    {}
    pure val {} = {}{}
",
            values_for_fields, ident, self.name, msg_params
        )
    }
}
