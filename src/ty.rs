// TODO:
//
// * static dispatch cannot use SELF_TYPE

use super::ast;
use failure::bail;
use id_arena::{Arena, ArenaBehavior};
use std::collections::{HashMap, HashSet};

#[derive(Default)]
pub struct Environment {
    types: Arena<Type, TypesArenaBehavior>,

    // Maps from a class to a map of that class's methods keyed by method name.
    class_inherited_methods: Option<HashMap<ast::NodeId, HashMap<ast::StringId, ast::NodeId>>>,

    // Maps from a class to a map of that class's attributes keyed by attribute name.
    class_inherited_attributes: Option<HashMap<ast::NodeId, HashMap<ast::StringId, ast::NodeId>>>,
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

                    if let Some(overridden) = attributes.insert(name.0, *f) {
                        if self.get(overridden) != self.get(*f) {
                            bail!("Attribute overrides must have the same type");
                        }
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
}
