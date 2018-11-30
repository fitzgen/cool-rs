use super::ast;
use id_arena::{Arena, ArenaBehavior};

#[derive(Default)]
pub struct Environment {
    types: Arena<Type, TypesArenaBehavior>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Primary(PrimaryType),
    Method(MethodType),
    Bottom(BottomType),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PrimaryType {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MethodType {}

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

    pub fn get_primary(&self, id: ast::NodeId) -> &PrimaryType {
        self.get(id).unwrap_primary()
    }

    pub fn get_method(&self, id: ast::NodeId) -> &MethodType {
        self.get(id).unwrap_method()
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
    is_get_and_unwrap!(is_primary, primary, unwrap_primary, PrimaryType, Primary);
    is_get_and_unwrap!(is_method, method, unwrap_method, MethodType, Method);
    is_get_and_unwrap!(is_bottom, bottom, unwrap_bottom, BottomType, Bottom);
}
