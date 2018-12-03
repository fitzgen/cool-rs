use petgraph::visit;
use std::collections::HashSet;
use std::vec;

pub struct InheritanceGraph<'a> {
    ctx: &'a super::Context,
}

impl<'a> InheritanceGraph<'a> {
    #[inline]
    pub fn new(ctx: &'a super::Context) -> InheritanceGraph<'a> {
        InheritanceGraph { ctx }
    }
}

impl<'a> visit::GraphBase for InheritanceGraph<'a> {
    type EdgeId = ();
    type NodeId = super::NodeId;
}

impl<'a> visit::Visitable for InheritanceGraph<'a> {
    type Map = HashSet<super::NodeId>;

    #[inline]
    fn visit_map(&self) -> Self::Map {
        HashSet::with_capacity(self.ctx.nodes().len())
    }

    #[inline]
    fn reset_map(&self, map: &mut Self::Map) {
        map.clear();
    }
}

pub struct Neighbors<'a> {
    ctx: &'a super::Context,
    node: super::NodeId,
}

impl<'a> Iterator for Neighbors<'a> {
    type Item = super::NodeId;

    #[inline]
    fn next(&mut self) -> Option<super::NodeId> {
        match self.ctx.node_ref(self.node) {
            super::Node::Class {
                name: _,
                parent: Some(parent),
                features: _,
            } => {
                self.node = self.ctx.class_by_name(*parent);
                Some(self.node)
            }
            _ => None,
        }
    }
}

impl<'a> visit::IntoNeighbors for &'a InheritanceGraph<'a> {
    type Neighbors = Neighbors<'a>;

    #[inline]
    fn neighbors(self, node: super::NodeId) -> Neighbors<'a> {
        Neighbors {
            ctx: self.ctx,
            node,
        }
    }
}

pub struct NeighborsDirected {
    iter: vec::IntoIter<super::NodeId>,
}

impl Iterator for NeighborsDirected {
    type Item = super::NodeId;

    #[inline]
    fn next(&mut self) -> Option<super::NodeId> {
        self.iter.next()
    }
}

impl<'a> visit::IntoNeighborsDirected for &'a InheritanceGraph<'a> {
    type NeighborsDirected = NeighborsDirected;

    fn neighbors_directed(
        self,
        node: super::NodeId,
        direction: petgraph::Direction,
    ) -> Self::NeighborsDirected {
        match direction {
            petgraph::Direction::Outgoing => NeighborsDirected {
                iter: visit::IntoNeighbors::neighbors(self, node)
                    .collect::<Vec<_>>()
                    .into_iter(),
            },
            petgraph::Direction::Incoming => NeighborsDirected {
                iter: self.ctx.get_subclasses(node).to_vec().into_iter(),
            },
        }
    }
}

pub struct NodeIdentifiers {
    iter: vec::IntoIter<super::NodeId>,
}

impl Iterator for NodeIdentifiers {
    type Item = super::NodeId;

    #[inline]
    fn next(&mut self) -> Option<super::NodeId> {
        self.iter.next()
    }
}

impl<'a> visit::IntoNodeIdentifiers for &'a InheritanceGraph<'a> {
    type NodeIdentifiers = NodeIdentifiers;

    fn node_identifiers(self) -> Self::NodeIdentifiers {
        let mut classes = vec![];
        for (id, node) in self.ctx.nodes() {
            if let super::Node::Class { .. } = node {
                classes.push(id);
            }
        }
        NodeIdentifiers {
            iter: classes.into_iter(),
        }
    }
}

impl<'a> visit::NodeCount for InheritanceGraph<'a> {
    #[inline]
    fn node_count(&self) -> usize {
        self.ctx.nodes().len()
    }
}
