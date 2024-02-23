use std::collections::HashMap;

use itertools::Itertools;

use crate::translate::Translatable;
use crate::types::{Constructor, Context};

/// Trait for generating a nondeterministic value for something.
pub trait NondetValue {
    fn nondet_value(&self, ctx: &mut Context, ident: &str) -> String;
}

impl NondetValue for Constructor {
    fn nondet_value(&self, _ctx: &mut Context, ident: &str) -> String {
        if self.fields.is_empty() {
            // If the constuctor has no field, it can only contain one value, and that is deterministic
            return format!("pure val {} = {}", ident, self.name);
        }

        // Each field needs its own named nondet value, in the form of
        // `nondet field_name: field_type = something.oneOf()`
        let values_for_fields = self
            .fields
            .iter()
            .map(|field| field.nondet_value.clone())
            .collect_vec()
            .join("\n  ");

        // And these values should be assembled into a record to be given to the constructor
        let record_fields = self
            .fields
            .iter()
            .map(|field| format!("{name}: {name}", name = field.name.clone()))
            .collect_vec()
            .join(", ");

        format!(
            "
    {}
    pure val {} = {}({{ {} }})
",
            values_for_fields, ident, self.name, record_fields
        )
    }
}

impl NondetValue for rustc_hir::Ty<'_> {
    fn nondet_value(&self, ctx: &mut Context, ident: &str) -> String {
        let ty = self.translate(ctx);

        let nondet_values_by_type = HashMap::from([
            ("str", "Set(\"s1\", \"s2\", \"s3\").oneOf()".to_string()),
            ("int", "0.to(MAX_AMOUNT).oneOf()".to_string()),
            ("bool", "Bool.oneOf()".to_string()),
        ]);

        if ty == "List[int]" {
            // FIXME make this work with all types
            // Since Quint doesn't allow `oneOf` to be called on iterators, we generate a list of up to 3 elements.
            // We can generalize this when we have type-level polymorphism
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

        // Form primitive types, we can use the predefined values
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
