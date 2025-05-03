use std::sync::Arc;

use indexmap::IndexMap;

use crate::location::Span;

use super::{
    language::{GlobalName, LocalName},
    process,
    types::{Context, Type, TypeDefs, TypeError},
};

#[derive(Clone, Debug)]
pub struct Program<Expr> {
    pub type_defs: Vec<TypeDef>,
    pub declarations: Vec<Declaration>,
    pub definitions: Vec<Definition<Expr>>,
}

#[derive(Debug, Clone)]
pub struct CheckedProgram {
    pub type_defs: TypeDefs,
    pub declarations: IndexMap<GlobalName, Declaration>,
    pub definitions: IndexMap<GlobalName, Definition<Arc<process::Expression<Type>>>>,
}

#[derive(Clone, Debug)]
pub struct TypeDef {
    pub span: Span,
    pub name: GlobalName,
    pub params: Vec<LocalName>,
    pub typ: Type,
}

#[derive(Clone, Debug)]
pub struct Declaration {
    pub span: Span,
    pub name: GlobalName,
    pub typ: Type,
}

#[derive(Clone, Debug)]
pub struct Definition<Expr> {
    pub span: Span,
    pub name: GlobalName,
    pub expression: Expr,
}

impl Program<Arc<process::Expression<()>>> {
    pub fn type_check(&self) -> Result<CheckedProgram, TypeError> {
        let type_defs = TypeDefs::new_with_validation(
            self.type_defs
                .iter()
                .map(|d| (&d.span, &d.name, &d.params, &d.typ)),
        )?;

        let mut unchecked_definitions = IndexMap::new();
        for Definition {
            span,
            name,
            expression,
        } in &self.definitions
        {
            if let Some((span1, _)) =
                unchecked_definitions.insert(name.clone(), (span.clone(), expression.clone()))
            {
                return Err(TypeError::NameAlreadyDefined(
                    span.clone(),
                    span1.clone(),
                    name.clone(),
                ));
            }
        }

        let mut declarations = IndexMap::new();
        for Declaration { span, name, typ } in &self.declarations {
            if !unchecked_definitions.contains_key(name) {
                return Err(TypeError::DeclaredButNotDefined(span.clone(), name.clone()));
            }
            if let Some((span1, _)) = declarations.insert(name.clone(), (span.clone(), typ.clone()))
            {
                return Err(TypeError::NameAlreadyDeclared(
                    span.clone(),
                    span1,
                    name.clone(),
                ));
            }
        }

        let names_to_check = unchecked_definitions
            .iter()
            .map(|(name, (span, _))| (span.clone(), name.clone()))
            .collect::<Vec<_>>();

        let mut context = Context::new(type_defs, declarations, unchecked_definitions);
        for (span, name) in names_to_check {
            context.check_definition(&span, &name)?;
        }

        Ok(CheckedProgram {
            type_defs: context.get_type_defs().clone(),
            declarations: context
                .get_declarations()
                .into_iter()
                .map(|(name, (span, typ))| (name.clone(), Declaration { span, name, typ }))
                .collect(),
            definitions: context
                .get_checked_definitions()
                .into_iter()
                .map(|(name, (span, expression))| {
                    (
                        name.clone(),
                        Definition {
                            span,
                            name,
                            expression,
                        },
                    )
                })
                .collect(),
        })
    }
}

impl<Expr> Default for Program<Expr> {
    fn default() -> Self {
        Self {
            type_defs: Vec::new(),
            declarations: Vec::new(),
            definitions: Vec::new(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct NameWithType(pub String, pub Type);

pub struct TypeOnHover {
    sorted_pairs: Vec<(Span, NameWithType)>,
}

impl TypeOnHover {
    pub fn new(program: &CheckedProgram) -> Self {
        let mut pairs = Vec::new();

        for (_, definition) in &program.definitions {
            definition
                .expression
                .types_at_spans(&mut |span, name, typ| pairs.push((span, NameWithType(name, typ))));
        }

        pairs.sort_by_key(|(span, _)| span.start.offset);
        pairs.dedup_by_key(|(span, _)| span.start.offset);

        Self {
            sorted_pairs: pairs,
        }
    }
}

impl TypeOnHover {
    pub fn query(&self, row: usize, column: usize) -> Option<NameWithType> {
        // find index with the greatest start that is <= than (row, column)
        let (mut lo, mut hi) = (0, self.sorted_pairs.len());
        while lo + 1 < hi {
            let mi = (lo + hi) / 2;
            let mp = self.sorted_pairs[mi].0.start;
            if mp.row < row || (mp.row == row && mp.column <= column) {
                lo = mi;
            } else {
                hi = mi;
            }
        }

        let (span, typ) = &self.sorted_pairs[lo];

        // check if queried (row, column) is in the found span
        if row < span.start.row || (row == span.start.row && column < span.start.column) {
            return None;
        }
        if span.end.row < row || (span.end.row == row && span.end.column < column) {
            return None;
        }

        // found a good span
        Some(typ.clone())
    }
}
