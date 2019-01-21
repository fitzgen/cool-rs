mod inheritance_graph;
pub use self::inheritance_graph::InheritanceGraph;

mod dump;
pub use self::dump::DumpAst;

use crate::frontend;
use crate::ty;
use failure::{bail, format_err};
use id_arena::{Arena, Id};
use std::collections::HashMap;

pub struct Context {
    idents: Arena<String>,
    already_interned: HashMap<String, StringId>,

    nodes: Arena<Node>,

    env: Option<ty::Environment>,
    class_name_to_node: Option<HashMap<TypeIdentifier, NodeId>>,
    object_class: Option<NodeId>,
    int_class: Option<NodeId>,
    string_class: Option<NodeId>,
    bool_class: Option<NodeId>,
    subclasses_map: Option<HashMap<NodeId, Vec<NodeId>>>,
}

impl Default for Context {
    fn default() -> Context {
        Context {
            idents: Default::default(),
            already_interned: Default::default(),
            nodes: Default::default(),
            env: Some(Default::default()),
            class_name_to_node: None,
            object_class: None,
            int_class: None,
            string_class: None,
            bool_class: None,
            subclasses_map: None,
        }
    }
}

pub type StringId = Id<String>;
pub type NodeId = Id<Node>;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct TypeIdentifier(pub StringId);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Identifier(pub StringId);

#[derive(Debug)]
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

    pub fn get_already_interned(&self, s: &str) -> Option<StringId> {
        self.already_interned.get(s).cloned()
    }

    pub fn already_interned(&self, s: &str) -> StringId {
        self.get_already_interned(s).unwrap()
    }

    pub fn new_node(&mut self, node: Node) -> NodeId {
        let id = self.nodes.alloc(node);
        self.env_mut().alloc(id);
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
        self.env.as_ref().unwrap()
    }

    pub fn env_mut(&mut self) -> &mut ty::Environment {
        self.env.as_mut().unwrap()
    }

    fn create_class_name_to_node_map(
        &self,
    ) -> Result<HashMap<TypeIdentifier, NodeId>, failure::Error> {
        let mut map = HashMap::new();
        for (id, node) in self.nodes.iter() {
            if let Node::Class { name, .. } = *node {
                let class_name = self.interned_str_ref(name.0);
                if "SELF_TYPE" == class_name {
                    bail!("Cannot define SELF_TYPE as a new class");
                }

                if map.insert(name, id).is_some() {
                    bail!("Redefinition of class `{}`", class_name);
                }
            }
        }
        Ok(map)
    }

    pub fn get_class_by_name(&self, class_name: TypeIdentifier) -> Option<NodeId> {
        self.class_name_to_node
            .as_ref()
            .expect("should have called `create_class_name_to_node_map` already")
            .get(&class_name)
            .cloned()
    }

    pub fn class_by_name(&self, class_name: TypeIdentifier) -> NodeId {
        self.get_class_by_name(class_name)
            .expect("no class with given class name")
    }

    fn create_subclasses_map(&self) -> Result<HashMap<NodeId, Vec<NodeId>>, failure::Error> {
        let mut map = HashMap::new();
        for (id, node) in self.nodes.iter() {
            if let Node::Class {
                parent: Some(parent),
                ..
            } = node
            {
                let parent = self.get_class_by_name(*parent).ok_or_else(|| {
                    format_err!(
                        "Cannot inherit from undefined class `{}`",
                        self.interned_str_ref(parent.0)
                    )
                })?;
                map.entry(parent).or_insert_with(Vec::default).push(id);
            }
        }
        Ok(map)
    }

    pub fn get_subclasses(&self, class: NodeId) -> &[NodeId] {
        self.subclasses_map
            .as_ref()
            .expect("should have called `create_subclasses_map` already")
            .get(&class)
            .map(|v| &v[..])
            .unwrap_or(&[])
    }

    pub fn class_name(&self, class: NodeId) -> Option<&str> {
        self.nodes[class]
            .class_name()
            .map(|t| self.interned_str_ref(t.0))
    }

    pub fn get_object_class(&self) -> NodeId {
        self.object_class
            .expect("should have called `create_class_name_to_node_map` already")
    }

    pub fn get_int_class(&self) -> NodeId {
        self.int_class
            .expect("should have called `create_class_name_to_node_map` already")
    }

    pub fn get_string_class(&self) -> NodeId {
        self.string_class
            .expect("should have called `create_class_name_to_node_map` already")
    }

    pub fn get_bool_class(&self) -> NodeId {
        self.bool_class
            .expect("should have called `create_class_name_to_node_map` already")
    }

    pub fn type_check(&mut self) -> Result<(), failure::Error> {
        self.add_self_hosted_builtins();

        self.class_name_to_node = Some(self.create_class_name_to_node_map()?);
        let object = self.intern("Object");
        self.object_class = Some(self.class_by_name(TypeIdentifier(object)));
        let int = self.intern("Int");
        self.int_class = Some(self.class_by_name(TypeIdentifier(int)));
        let string = self.intern("String");
        self.string_class = Some(self.class_by_name(TypeIdentifier(string)));
        let bool = self.intern("Bool");
        self.bool_class = Some(self.class_by_name(TypeIdentifier(bool)));

        self.subclasses_map = Some(self.create_subclasses_map()?);
        self.check_inheritance()?;

        let mut env = self.env.take().unwrap();
        env.check_signatures(self)?;
        env.check_bodies(self)?;
        self.env = Some(env);
        // TODO: check for Main::main
        Ok(())
    }

    fn add_self_hosted_builtins(&mut self) {
        let source = include_str!("../builtins.cool");
        let mut lexer = frontend::lexer::Lexer::new(source);
        let parser = frontend::parser::ProgramParser::new();
        parser
            .parse(self, &mut lexer)
            .expect("self-hosted code should never fail to parse");
    }

    fn check_inheritance(&self) -> Result<(), failure::Error> {
        // Ensure that nothing inherits from the builtins that you can't extend.
        for (_, node) in self.nodes.iter() {
            if let Node::Class {
                parent: Some(parent),
                ..
            } = node
            {
                match self.interned_str_ref(parent.0) {
                    c @ "SELF_TYPE" | c @ "Int" | c @ "String" | c @ "Bool" => {
                        bail!("Cannot inherit from `{}`", c)
                    }
                    _ => {}
                }
            }
        }

        // Check for cycles in the inheritance graph.
        let inheritance_graph = inheritance_graph::InheritanceGraph::new(self);
        let mut sccs = petgraph::algo::kosaraju_scc(&inheritance_graph);
        sccs.retain(|scc| scc.len() > 1);
        if sccs.is_empty() {
            return Ok(());
        }

        let mut msg = String::new();
        for scc in sccs {
            let names: Vec<String> = scc
                .into_iter()
                .map(|id| self.class_name(id).unwrap().to_string())
                .collect();
            let names: String = names.join(", ");
            msg.push_str(&format!(
                "Found inheritance cycle involving classes: {}\n",
                names
            ));
        }
        bail!("{}", msg)
    }
}

impl Node {
    pub fn class_name(&self) -> Option<TypeIdentifier> {
        match self {
            Node::Class { name, .. } => Some(*name),
            _ => None,
        }
    }
}
