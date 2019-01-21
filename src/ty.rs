// TODO:
//
// * static dispatch cannot use SELF_TYPE

use super::ast;
use failure::{bail, ResultExt};
use id_arena::{Arena, ArenaBehavior};
use std::collections::{HashMap, HashSet};

#[derive(Default)]
pub struct Environment {
    types: Arena<Type, TypesArenaBehavior>,

    // Maps from a class to a map of that class's methods keyed by method name.
    class_inherited_methods: Option<HashMap<ast::NodeId, HashMap<ast::StringId, ast::NodeId>>>,

    // Maps from a class to a map of that class's attributes keyed by attribute name.
    class_inherited_attributes: Option<HashMap<ast::NodeId, HashMap<ast::StringId, ast::NodeId>>>,

    current_class: Option<ast::NodeId>,
    data_scope: Vec<HashMap<ast::Identifier, Type>>,
    method_scope: Vec<HashMap<ast::Identifier, Type>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Nominal(NominalType),
    Method(MethodType),
    SelfType(SelfType),
    Bottom(BottomType),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NominalType(ast::TypeIdentifier);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MethodType {
    params: Vec<Type>,
    ret: Box<Type>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SelfType;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BottomType;

struct TypesArenaBehavior;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
struct TypeId(usize);

impl From<ast::NodeId> for TypeId {
    #[inline]
    fn from(id: ast::NodeId) -> TypeId {
        TypeId(id.index())
    }
}

impl<'a> From<&'a ast::NodeId> for TypeId {
    #[inline]
    fn from(id: &'a ast::NodeId) -> TypeId {
        TypeId(id.index())
    }
}

impl ArenaBehavior for TypesArenaBehavior {
    type Id = TypeId;

    #[inline]
    fn new_id(_arena_id: u32, index: usize) -> TypeId {
        TypeId(index)
    }

    #[inline]
    fn index(id: TypeId) -> usize {
        id.0
    }

    #[inline]
    fn arena_id(_id: TypeId) -> u32 {
        0
    }

    #[inline]
    fn new_arena_id() -> u32 {
        0
    }
}

impl Environment {
    pub(crate) fn alloc(&mut self, id: ast::NodeId) {
        assert_eq!(self.types.len(), id.index());
        self.types.alloc(Type::Bottom(BottomType));
    }

    pub fn get(&self, id: ast::NodeId) -> &Type {
        &self.types[id.into()]
    }

    pub fn get_nominal(&self, id: ast::NodeId) -> &NominalType {
        self.get(id).unwrap_nominal()
    }

    pub fn get_method(&self, id: ast::NodeId) -> &MethodType {
        self.get(id).unwrap_method()
    }

    fn get_data_id_ty(&self, id: ast::Identifier) -> Result<TypeId, failure::Error> {
        unimplemented!("get_data_id_ty")
    }

    fn get_method_id_ty(&self, id: ast::Identifier) -> Result<TypeId, failure::Error> {
        unimplemented!("get_method_id_ty")
    }

    /// Is `a` a subtype of `b`?
    fn is_subtype(&self, a: TypeId, b: TypeId) -> bool {
        unimplemented!("is_subtype")
    }

    fn define_method_types(&mut self, ctx: &ast::Context) -> Result<(), failure::Error> {
        for (id, node) in ctx.nodes() {
            if let ast::Node::Method {
                formals, ty, name, ..
            } = node
            {
                assert!(self.get(id).is_bottom());

                if ctx.interned_str_ref(name.0) == "self" {
                    bail!("Cannot use `self` as a method name");
                }

                let mut param_ids = HashSet::new();
                let mut params = vec![];
                for &(p_id, ty_id) in formals {
                    if !param_ids.insert(p_id) {
                        bail!(
                            "Cannot have duplicate formal parameter names in method `{}`",
                            ctx.interned_str_ref(name.0)
                        );
                    }

                    let ty_name = ctx.interned_str_ref(ty_id.0);
                    if let "SELF_TYPE" = ty_name {
                        bail!("Cannot use `SELF_TYPE` in parameters");
                    }

                    let p_name = ctx.interned_str_ref(p_id.0);
                    if let "self" = p_name {
                        bail!("Cannot use `self` as a parameter name");
                    }

                    if ctx.get_class_by_name(ty_id).is_none() {
                        bail!(
                            "`{}` method takes parameter of undefined class `{}`",
                            ctx.interned_str_ref(name.0),
                            ty_name
                        );
                    }
                    params.push(Type::Nominal(NominalType(ty_id)));
                }

                let ret = Box::new(match ctx.interned_str_ref(ty.0) {
                    "SELF_TYPE" => Type::SelfType(SelfType),
                    r => {
                        if ctx.get_class_by_name(*ty).is_some() {
                            Type::Nominal(NominalType(*ty))
                        } else {
                            bail!(
                                "`{}` method returns undefined class `{}`",
                                ctx.interned_str_ref(name.0),
                                r
                            );
                        }
                    }
                });

                self.types[id.into()] = Type::Method(MethodType { params, ret });
            }
        }

        Ok(())
    }

    fn add_self_methods(
        &self,
        ctx: &ast::Context,
        methods: &mut HashMap<ast::StringId, ast::NodeId>,
        class: ast::NodeId,
    ) -> Result<(), failure::Error> {
        let mut this_class_methods = HashSet::new();

        let features = match ctx.node_ref(class) {
            ast::Node::Class { ref features, .. } => features,
            _ => panic!("adding methods of non-class?"),
        };
        for f in features {
            match ctx.node_ref(*f) {
                ast::Node::Method { name, .. } => {
                    if !this_class_methods.insert(name) {
                        bail!(
                            "Duplicate method definitions for method `{}`",
                            ctx.interned_str_ref(name.0)
                        );
                    }
                    if let Some(overridden) = methods.insert(name.0, *f) {
                        if self.get(overridden) != self.get(*f) {
                            bail!("method overrides must have the same type");
                        }
                    }
                }
                _ => {}
            }
        }

        Ok(())
    }

    fn create_class_inherited_methods(
        &self,
        ctx: &ast::Context,
    ) -> Result<HashMap<ast::NodeId, HashMap<ast::StringId, ast::NodeId>>, failure::Error> {
        let mut map = HashMap::new();

        let graph = ast::InheritanceGraph::new(ctx);
        let mut dfs = petgraph::visit::DfsPostOrder::empty(&graph);
        for (id, node) in ctx.nodes() {
            if let ast::Node::Class { .. } = node {
                dfs.stack.push(id);
            }
        }

        let object = ctx.get_object_class();
        let mut methods = HashMap::new();
        self.add_self_methods(ctx, &mut methods, object)
            .expect("object methods should always type check");
        map.insert(object, methods);
        dfs.discovered.insert(object);
        dfs.finished.insert(object);

        while let Some((id, ast::Node::Class { parent, .. })) =
            dfs.next(&graph).map(|id| (id, ctx.node_ref(id)))
        {
            let parent = parent.map_or_else(|| ctx.get_object_class(), |p| ctx.class_by_name(p));
            let mut methods = map[&parent].clone();
            self.add_self_methods(ctx, &mut methods, id)?;
            map.insert(id, methods);
        }

        Ok(map)
    }

    fn define_attribute_types(&mut self, ctx: &ast::Context) -> Result<(), failure::Error> {
        for (id, node) in ctx.nodes() {
            if let ast::Node::Property { name, ty, .. } = node {
                assert!(self.get(id).is_bottom());

                if ctx.interned_str_ref(name.0) == "self" {
                    bail!("Cannot define attribute named `self`");
                }

                if ctx.get_class_by_name(*ty).is_none() {
                    bail!(
                        "Cannot have an attribute of an undefined class `{}`",
                        ctx.interned_str_ref(ty.0)
                    );
                }

                self.types[id.into()] = if ctx.interned_str_ref(ty.0) == "SELF_TYPE" {
                    Type::SelfType(SelfType)
                } else {
                    Type::Nominal(NominalType(*ty))
                };
            }
        }

        Ok(())
    }

    fn add_self_attributes(
        &self,
        ctx: &ast::Context,
        attributes: &mut HashMap<ast::StringId, ast::NodeId>,
        class: ast::NodeId,
    ) -> Result<(), failure::Error> {
        let mut this_class_attributes = HashSet::new();

        let features = match ctx.node_ref(class) {
            ast::Node::Class { ref features, .. } => features,
            _ => panic!("adding attributes of non-class?"),
        };
        for f in features {
            match ctx.node_ref(*f) {
                ast::Node::Property { name, .. } => {
                    if !this_class_attributes.insert(name) {
                        bail!(
                            "Duplicate attribute definitions for attribute `{}`",
                            ctx.interned_str_ref(name.0)
                        );
                    }

                    if attributes.insert(name.0, *f).is_some() {
                        bail!("Cannot override attributes");
                    }
                }
                _ => {}
            }
        }

        Ok(())
    }

    fn create_class_inherited_attributes(
        &self,
        ctx: &ast::Context,
    ) -> Result<HashMap<ast::NodeId, HashMap<ast::StringId, ast::NodeId>>, failure::Error> {
        let mut map: HashMap<ast::NodeId, HashMap<ast::StringId, ast::NodeId>> = HashMap::new();

        let graph = ast::InheritanceGraph::new(ctx);
        let mut dfs = petgraph::visit::DfsPostOrder::empty(&graph);
        for (id, node) in ctx.nodes() {
            if let ast::Node::Class { .. } = node {
                dfs.stack.push(id);
            }
        }

        let object = ctx.get_object_class();
        map.insert(object, Default::default());
        dfs.discovered.insert(object);
        dfs.finished.insert(object);

        while let Some((id, ast::Node::Class { parent, .. })) =
            dfs.next(&graph).map(|id| (id, ctx.node_ref(id)))
        {
            let parent = parent.map_or_else(|| ctx.get_object_class(), |p| ctx.class_by_name(p));
            let mut attributes = map[&parent].clone();
            self.add_self_attributes(ctx, &mut attributes, id)?;
            map.insert(id, attributes);
        }

        Ok(map)
    }

    pub fn check_signatures(&mut self, ctx: &ast::Context) -> Result<(), failure::Error> {
        self.define_method_types(ctx)?;
        self.class_inherited_methods = Some(self.create_class_inherited_methods(ctx)?);

        self.define_attribute_types(ctx)?;
        self.class_inherited_attributes = Some(self.create_class_inherited_attributes(ctx)?);

        Ok(())
    }

    pub fn check_bodies(&mut self, ctx: &ast::Context) -> Result<(), failure::Error> {
        for (id, node) in ctx.nodes() {
            if let ast::Node::Class {
                name: _,
                parent: _,
                features,
            } = node
            {
                self.current_class = Some(id);
                self.method_scope.push(
                    self.class_inherited_methods
                        .as_ref()
                        .expect("should have called `create_class_inherited_methods` already")[&id]
                        .iter()
                        .map(|(k, v)| (ast::Identifier(*k), self.get(*v).clone()))
                        .collect(),
                );
                self.data_scope.push(
                    self.class_inherited_attributes
                        .as_ref()
                        .expect("should have called `create_class_inherited_attributes` already")
                        [&id]
                        .iter()
                        .map(|(k, v)| (ast::Identifier(*k), self.get(*v).clone()))
                        .collect(),
                );

                for f in features {
                    match ctx.node_ref(*f) {
                        ast::Node::Method { formals, expr, .. } => {
                            self.data_scope.push(
                                formals
                                    .iter()
                                    .map(|(k, v)| (*k, Type::Nominal(NominalType(*v))))
                                    .collect(),
                            );
                            self.check_expr(ctx, *expr)?;
                        }
                        ast::Node::Property {
                            expr: Some(expr), ..
                        } => self.check_expr(ctx, *expr)?,
                        // TODO: handle expr.is_none() case...
                        f => panic!("bad feature of class? {:?}", f),
                    }
                }

                self.data_scope.clear();
                self.method_scope.clear();
                self.current_class = None;
            }
        }
        Ok(())
    }

    fn check_expr_expected_ty(
        &mut self,
        ctx: &ast::Context,
        expr: ast::NodeId,
        expected: &Type,
    ) -> Result<(), failure::Error> {
        self.check_expr(ctx, expr)?;
        if &self.types[expr.into()] != expected {
            failure::bail!(
                "expected {:?}, found {:?}",
                &self.types[expr.into()],
                &expected
            );
        }
        Ok(())
    }

    fn check_expr(
        &mut self,
        ctx: &ast::Context,
        this_expr: ast::NodeId,
    ) -> Result<(), failure::Error> {
        assert!(self.get(this_expr).is_bottom());
        let this_expr_ty: TypeId = this_expr.into();

        match ctx.node_ref(this_expr) {
            ast::Node::IntegerConst(_) => {
                self.types[this_expr_ty] = Type::builtin(ctx, "Int");
                Ok(())
            }
            ast::Node::StringConst(_) => {
                self.types[this_expr_ty] = Type::builtin(ctx, "String");
                Ok(())
            }
            ast::Node::BoolConst(_) => {
                self.types[this_expr_ty] = Type::builtin(ctx, "Bool");
                Ok(())
            }
            ast::Node::Assign { id, expr } => {
                let id_ty = self.get_data_id_ty(*id)?;
                self.check_expr(ctx, *expr)?;
                if !self.is_subtype(this_expr_ty, id_ty) {
                    failure::bail!(
                        "bad assign: expected {:?}, found {:?}",
                        &self.types[this_expr_ty],
                        &self.types[id_ty]
                    );
                }
                self.types[this_expr_ty] = self.types[expr.into()].clone();
                Ok(())
            }
            ast::Node::Not { expr } => {
                let bool = Type::builtin(ctx, "Bool");
                self.check_expr_expected_ty(ctx, *expr, &bool)
                    .context("bad `not` expression")?;
                self.types[this_expr_ty] = bool;
                Ok(())
            }
            ast::Node::LessThanEqual { lhs, rhs } | ast::Node::LessThan { lhs, rhs } => {
                let int = Type::builtin(ctx, "Int");
                self.check_expr_expected_ty(ctx, *lhs, &int)
                    .context("bad left-hand side for `<=`")?;
                self.check_expr_expected_ty(ctx, *rhs, &int)
                    .context("bad right-hand side for `<=`")?;
                self.types[this_expr_ty] = Type::builtin(ctx, "bool");
                Ok(())
            }
            ast::Node::Equal { lhs, rhs } => {
                let string = Type::builtin(ctx, "String");
                let int = Type::builtin(ctx, "Int");
                let bool = Type::builtin(ctx, "Bool");

                self.check_expr(ctx, *lhs)?;
                let lhs_ty = &self.types[lhs.into()];
                if !(lhs_ty == &string || lhs_ty == &int || lhs_ty == &bool) {
                    bail!(
                        "bad left-hand side to `=` expression; expected `String`, `Int`, or \
                         `Bool`, but found {:?}",
                        lhs_ty
                    );
                }

                self.check_expr(ctx, *rhs)?;
                let rhs_ty = &self.types[rhs.into()];
                if !(rhs_ty == &string || rhs_ty == &int || rhs_ty == &bool) {
                    bail!(
                        "bad right-hand side to `=` expression; expected `String`, `Int`, or \
                         `Bool`, but found {:?}",
                        rhs_ty
                    );
                }

                if self.types[lhs.into()] != self.types[rhs.into()] {
                    bail!(
                        "bad `=` expression; left-hand side = {:?} but right-hand side = {:?}",
                        &self.types[lhs.into()],
                        &self.types[rhs.into()]
                    )
                }

                Ok(())
            }
            ast::Node::Add { lhs, rhs }
            | ast::Node::Sub { lhs, rhs }
            | ast::Node::Mul { lhs, rhs }
            | ast::Node::Div { lhs, rhs } => {
                let int = Type::builtin(ctx, "Int");
                self.check_expr_expected_ty(ctx, *lhs, &int)
                    .context("bad left-hand side of binary arithmetic expression")?;
                self.check_expr_expected_ty(ctx, *rhs, &int)
                    .context("bad left-hand side of binary arithmetic expression")?;
                self.types[this_expr_ty] = int;
                Ok(())
            }
            ast::Node::IsVoid { expr } => {
                self.check_expr(ctx, *expr)?;
                self.types[this_expr_ty] = Type::builtin(ctx, "Bool");
                Ok(())
            }
            ast::Node::Negate { expr } => {
                let int = Type::builtin(ctx, "Int");
                self.check_expr_expected_ty(ctx, *expr, &int)
                    .context("bad negation expression")?;
                self.types[this_expr_ty] = int;
                Ok(())
            }
            // ast::Node::Dispatch {
            //     receiver,
            //     cast: Option<TypeIdentifier>,
            //     method: Identifier,
            //     args: Vec<NodeId>,
            // },
            ast::Node::LetIn { id, ty, expr, body } => {
                unimplemented!("let/in");
                Ok(())
            }
            // ast::Node::IfThenElse {
            //     condition,
            //     consequent,
            //     alternative,
            // },
            // ast::Node::While {
            //     condition,
            //     body,
            // },
            // ast::Node::Block {
            //     exprs: Vec<NodeId>,
            // },
            // ast::Node::Case {
            //     expr,
            //     cases: Vec<(Identifier, TypeIdentifier, NodeId)>,
            // },
            // ast::Node::New {
            //     ty: TypeIdentifier,
            // },
            // ast::Node::VariableReference {
            //     id: Identifier,
            // },
            _ => unimplemented!("check_expr"),
        }
    }
}

macro_rules! is_get_and_unwrap {
    ( $is_name:ident, $get_name:ident , $unwrap_name:ident, $t:ty, $variant:ident ) => {
        pub fn $get_name(&self) -> Option<&$t> {
            match self {
                Type::$variant(t) => Some(t),
                _ => None,
            }
        }

        pub fn $is_name(&self) -> bool {
            self.$get_name().is_some()
        }

        pub fn $unwrap_name(&self) -> &$t {
            self.$get_name().unwrap()
        }
    }
}

impl Type {
    is_get_and_unwrap!(is_nominal, nominal, unwrap_nominal, NominalType, Nominal);
    is_get_and_unwrap!(is_method, method, unwrap_method, MethodType, Method);
    is_get_and_unwrap!(
        is_self_type,
        self_type,
        unwrap_self_type,
        SelfType,
        SelfType
    );
    is_get_and_unwrap!(is_bottom, bottom, unwrap_bottom, BottomType, Bottom);

    fn builtin(ctx: &ast::Context, builtin: &str) -> Type {
        let s = ctx.already_interned(builtin);
        Type::Nominal(NominalType(ast::TypeIdentifier(s)))
    }
}
