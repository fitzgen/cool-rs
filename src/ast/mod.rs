mod dump;
pub use self::dump::DumpAst;

use crate::ty;
use id_arena::{Arena, Id};
use std::collections::HashMap;

#[derive(Default)]
pub struct Context {
    idents: Arena<String>,
    already_interned: HashMap<String, StringId>,

    nodes: Arena<Node>,
    env: ty::Environment,
}

pub type StringId = Id<String>;
pub type NodeId = Id<Node>;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TypeIdentifier(pub StringId);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Identifier(pub StringId);

pub enum Node {
    Class {
        name: TypeIdentifier,
        parent: Option<TypeIdentifier>,
        features: Vec<NodeId>,
    },
    Method {
        name: Identifier,
        formals: Vec<(Identifier, TypeIdentifier)>,
        ty: TypeIdentifier,
        expr: NodeId,
    },
    Property {
        name: Identifier,
        ty: TypeIdentifier,
        expr: Option<NodeId>,
    },
    Assign {
        id: Identifier,
        expr: NodeId,
    },
    Not {
        expr: NodeId,
    },
    LessThanEqual {
        lhs: NodeId,
        rhs: NodeId,
    },
    LessThan {
        lhs: NodeId,
        rhs: NodeId,
    },
    Equal {
        lhs: NodeId,
        rhs: NodeId,
    },
    Add {
        lhs: NodeId,
        rhs: NodeId,
    },
    Sub {
        lhs: NodeId,
        rhs: NodeId,
    },
    Mul {
        lhs: NodeId,
        rhs: NodeId,
    },
    Div {
        lhs: NodeId,
        rhs: NodeId,
    },
    IsVoid {
        expr: NodeId,
    },
    Negate {
        expr: NodeId,
    },
    Dispatch {
        receiver: NodeId,
        cast: Option<TypeIdentifier>,
        method: Identifier,
        args: Vec<NodeId>,
    },
    LetIn {
        id: Identifier,
        ty: TypeIdentifier,
        expr: Option<NodeId>,
        body: NodeId,
    },
    IfThenElse {
        condition: NodeId,
        consequent: NodeId,
        alternative: NodeId,
    },
    While {
        condition: NodeId,
        body: NodeId,
    },
    Block {
        exprs: Vec<NodeId>,
    },
    Case {
        expr: NodeId,
        cases: Vec<(Identifier, TypeIdentifier, NodeId)>,
    },
    New {
        ty: TypeIdentifier,
    },
    VariableReference {
        id: Identifier,
    },
    IntegerConst(i64),
    StringConst(StringId),
    BoolConst(bool),
}

impl Context {
    pub fn intern<S: AsRef<str> + Into<String>>(&mut self, s: S) -> StringId {
        if let Some(id) = self.already_interned.get(s.as_ref()) {
            return *id;
        }

        let s = s.into();
        let id = self.idents.alloc(s.clone());
        self.already_interned.insert(s, id);
        id
    }

    pub fn interned_str_ref(&self, id: StringId) -> &str {
        &self.idents[id]
    }

    pub fn new_node(&mut self, node: Node) -> NodeId {
        let id = self.nodes.alloc(node);
        self.env.alloc(id);
        id
    }

    pub fn node_ref(&self, id: NodeId) -> &Node {
        &self.nodes[id]
    }

    pub fn nodes(&self) -> &Arena<Node> {
        &self.nodes
    }

    pub fn new_type_identifier(&mut self, id: &str) -> TypeIdentifier {
        let id = self.intern(id);
        TypeIdentifier(id)
    }

    pub fn new_identifier(&mut self, id: &str) -> Identifier {
        let id = self.intern(id);
        Identifier(id)
    }

    pub fn new_string_const(&mut self, s: String) -> NodeId {
        let s = self.intern(s);
        self.new_node(Node::StringConst(s))
    }

    pub fn env(&self) -> &ty::Environment {
        &self.env
    }

    pub fn env_mut(&mut self) -> &mut ty::Environment {
        &mut self.env
    }
}