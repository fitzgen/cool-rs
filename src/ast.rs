use id_arena::{Arena, Id};
use std::collections::HashMap;

#[derive(Default)]
pub struct Context {
    idents: Arena<String>,
    already_interned: HashMap<String, StringId>,

    nodes: Arena<Node>,
}

pub type StringId = Id<String>;
pub type NodeId = Id<Node>;

pub struct TypeIdentifier(pub StringId);
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
        bindings: Vec<(Identifier, TypeIdentifier, Option<NodeId>)>,
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

    pub fn node_ref(&self, id: NodeId) -> &Node {
        &self.nodes[id]
    }
}

impl Context {
    pub fn new_class(
        &mut self,
        name: TypeIdentifier,
        parent: Option<TypeIdentifier>,
        features: Vec<NodeId>,
    ) -> NodeId {
        self.nodes.alloc(Node::Class {
            name,
            parent,
            features,
        })
    }

    pub fn new_type_identifier(&mut self, id: &str) -> TypeIdentifier {
        let id = self.intern(id);
        TypeIdentifier(id)
    }

    pub fn new_identifier(&mut self, id: &str) -> Identifier {
        let id = self.intern(id);
        Identifier(id)
    }

    pub fn new_method(
        &mut self,
        name: Identifier,
        formals: Vec<(Identifier, TypeIdentifier)>,
        ty: TypeIdentifier,
        expr: NodeId,
    ) -> NodeId {
        self.nodes.alloc(Node::Method {
            name,
            formals,
            ty,
            expr,
        })
    }

    pub fn new_property(
        &mut self,
        name: Identifier,
        ty: TypeIdentifier,
        expr: Option<NodeId>,
    ) -> NodeId {
        self.nodes.alloc(Node::Property { name, ty, expr })
    }

    pub fn new_assign(&mut self, id: Identifier, expr: NodeId) -> NodeId {
        self.nodes.alloc(Node::Assign { id, expr })
    }

    pub fn new_not(&mut self, expr: NodeId) -> NodeId {
        self.nodes.alloc(Node::Not { expr })
    }

    pub fn new_less_than_equal(&mut self, lhs: NodeId, rhs: NodeId) -> NodeId {
        self.nodes.alloc(Node::LessThanEqual { lhs, rhs })
    }

    pub fn new_less_than(&mut self, lhs: NodeId, rhs: NodeId) -> NodeId {
        self.nodes.alloc(Node::LessThan { lhs, rhs })
    }

    pub fn new_equal(&mut self, lhs: NodeId, rhs: NodeId) -> NodeId {
        self.nodes.alloc(Node::Equal { lhs, rhs })
    }

    pub fn new_add(&mut self, lhs: NodeId, rhs: NodeId) -> NodeId {
        self.nodes.alloc(Node::Add { lhs, rhs })
    }

    pub fn new_sub(&mut self, lhs: NodeId, rhs: NodeId) -> NodeId {
        self.nodes.alloc(Node::Sub { lhs, rhs })
    }

    pub fn new_mul(&mut self, lhs: NodeId, rhs: NodeId) -> NodeId {
        self.nodes.alloc(Node::Mul { lhs, rhs })
    }

    pub fn new_div(&mut self, lhs: NodeId, rhs: NodeId) -> NodeId {
        self.nodes.alloc(Node::Div { lhs, rhs })
    }

    pub fn new_isvoid(&mut self, expr: NodeId) -> NodeId {
        self.nodes.alloc(Node::IsVoid { expr })
    }

    pub fn new_negate(&mut self, expr: NodeId) -> NodeId {
        self.nodes.alloc(Node::Negate { expr })
    }

    pub fn new_dispatch(
        &mut self,
        receiver: NodeId,
        cast: Option<TypeIdentifier>,
        method: Identifier,
        args: Vec<NodeId>,
    ) -> NodeId {
        self.nodes.alloc(Node::Dispatch {
            receiver,
            cast,
            method,
            args,
        })
    }

    pub fn new_let_in(
        &mut self,
        bindings: Vec<(Identifier, TypeIdentifier, Option<NodeId>)>,
        body: NodeId,
    ) -> NodeId {
        self.nodes.alloc(Node::LetIn { bindings, body })
    }

    pub fn new_if_then_else(
        &mut self,
        condition: NodeId,
        consequent: NodeId,
        alternative: NodeId,
    ) -> NodeId {
        self.nodes.alloc(Node::IfThenElse {
            condition,
            consequent,
            alternative,
        })
    }

    pub fn new_while(&mut self, condition: NodeId, body: NodeId) -> NodeId {
        self.nodes.alloc(Node::While { condition, body })
    }

    pub fn new_block(&mut self, exprs: Vec<NodeId>) -> NodeId {
        self.nodes.alloc(Node::Block { exprs })
    }

    pub fn new_case(
        &mut self,
        expr: NodeId,
        cases: Vec<(Identifier, TypeIdentifier, NodeId)>,
    ) -> NodeId {
        self.nodes.alloc(Node::Case { expr, cases })
    }

    pub fn new_new(&mut self, ty: TypeIdentifier) -> NodeId {
        self.nodes.alloc(Node::New { ty })
    }

    pub fn new_variable_reference(&mut self, id: Identifier) -> NodeId {
        self.nodes.alloc(Node::VariableReference { id })
    }

    pub fn new_integer_const(&mut self, c: i64) -> NodeId {
        self.nodes.alloc(Node::IntegerConst(c))
    }

    pub fn new_string_const(&mut self, s: String) -> NodeId {
        let s = self.intern(s);
        self.nodes.alloc(Node::StringConst(s))
    }

    pub fn new_bool_const(&mut self, b: bool) -> NodeId {
        self.nodes.alloc(Node::BoolConst(b))
    }
}
