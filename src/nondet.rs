use std::collections::HashMap;

use itertools::Itertools;

use crate::translate::Translatable;
use crate::types::{Constructor, Context};

pub trait NondetValue {
    fn nondet_value(&self, ctx: &mut Context, ident: &str) -> String;
}

impl NondetValue for Constructor {
    fn nondet_value(&self, _ctx: &mut Context, ident: &str) -> String {
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

impl NondetValue for rustc_hir::Ty<'_> {
    fn nondet_value(&self, ctx: &mut Context, ident: &str) -> String {
        let ty = self.translate(ctx);

        let nondet_values_by_type = HashMap::from([
            ("str", "Set(\"s1\", \"s2\", \"s3\").oneOf()".to_string()),
            ("int", "0.to(MAX_AMOUNT).oneOf()".to_string()),
            // TODO list for other types
        ]);
        if ty == "List[int]" {
            // FIXME make this work with all types
            return format!(
                "
    val possibilities = 1.to(10).map(i => SomeInt(i)).union(Set(NoneInt))
    nondet v1 = possibilities.oneOf()
    nondet v2 = possibilities.oneOf()
    nondet v3 = possibilities.oneOf()
    val {ident}: {ty} = [v1, v2, v3].foldl([], (acc, v) => match v {{
      | SomeInt(i) => acc.append(i)
      | NoneInt => acc
    }})
"
            );
        }
        format!(
            "nondet {}: {} = {}",
            ident,
            ty,
            nondet_values_by_type
                .get(ty.as_str())
                .unwrap_or(&format!("nondet_value_for_type({ty})"))
        )
    }
}
