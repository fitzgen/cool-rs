use crate::ast;

pub trait DumpAst {
    fn dump_ast(&self, ctx: &ast::Context, s: &mut String, depth: usize);
}

fn indent(s: &mut String, depth: usize) {
    for _ in 0..depth {
        s.push_str("  ");
    }
}

fn indented_line(s: &mut String, depth: usize, body: &str) {
    indent(s, depth);
    s.push_str(body);
    s.push('\n');
}

impl DumpAst for Vec<ast::NodeId> {
    fn dump_ast(&self, ctx: &ast::Context, s: &mut String, depth: usize) {
        assert_eq!(depth, 0);
        s.push_str("_program\n");
        for nid in self {
            nid.dump_ast(ctx, s, depth + 1);
        }
    }
}

impl DumpAst for ast::NodeId {
    fn dump_ast(&self, ctx: &ast::Context, s: &mut String, depth: usize) {
        match ctx.node_ref(*self) {
            ast::Node::Class {
                name,
                parent,
                features,
            } => {
                indented_line(s, depth, "_class");
                let depth = depth + 1;
                indented_line(s, depth, ctx.interned_str_ref(name.0));
                indented_line(
                    s,
                    depth,
                    parent.map_or("Object", |p| ctx.interned_str_ref(p.0)),
                );
                indented_line(s, depth, "(");
                for f in features {
                    f.dump_ast(ctx, s, depth);
                }
                indented_line(s, depth, ")");
            }
            ast::Node::Method {
                name,
                formals,
                ty,
                expr,
            } => {
                indented_line(s, depth, "_method");
                let depth = depth + 1;
                indented_line(s, depth, ctx.interned_str_ref(name.0));
                for &(id, ty) in formals {
                    indented_line(s, depth, "_formal");
                    let depth = depth + 1;
                    indented_line(s, depth, ctx.interned_str_ref(id.0));
                    indented_line(s, depth, ctx.interned_str_ref(ty.0));
                }
                indented_line(s, depth, ctx.interned_str_ref(ty.0));
                expr.dump_ast(ctx, s, depth);
            }
            ast::Node::Property { name, ty, expr } => {
                indented_line(s, depth, "_attr");
                let depth = depth + 1;
                indented_line(s, depth, ctx.interned_str_ref(name.0));
                indented_line(s, depth, ctx.interned_str_ref(ty.0));
                match expr {
                    None => indented_line(s, depth, "_no_expr"),
                    Some(e) => {
                        e.dump_ast(ctx, s, depth);
                    }
                }
                indented_line(s, depth, ": _no_type");
            }
            ast::Node::Assign { id, expr } => {
                indented_line(s, depth, "_assign");
                indented_line(s, depth + 1, ctx.interned_str_ref(id.0));
                expr.dump_ast(ctx, s, depth + 1);
                indented_line(s, depth, ": _no_type");
            }
            ast::Node::Not { expr } => {
                indented_line(s, depth, "_comp");
                expr.dump_ast(ctx, s, depth + 1);
                indented_line(s, depth, ": _no_type");
            }
            ast::Node::LessThanEqual { lhs, rhs } => {
                indented_line(s, depth, "_lte");
                lhs.dump_ast(ctx, s, depth + 1);
                rhs.dump_ast(ctx, s, depth + 1);
                indented_line(s, depth, ": _no_type");
            }
            ast::Node::LessThan { lhs, rhs } => {
                indented_line(s, depth, "_lt");
                lhs.dump_ast(ctx, s, depth + 1);
                rhs.dump_ast(ctx, s, depth + 1);
                indented_line(s, depth, ": _no_type");
            }
            ast::Node::Equal { lhs, rhs } => {
                indented_line(s, depth, "_eq");
                lhs.dump_ast(ctx, s, depth + 1);
                rhs.dump_ast(ctx, s, depth + 1);
                indented_line(s, depth, ": _no_type");
            }
            ast::Node::Add { lhs, rhs } => {
                indented_line(s, depth, "_plus");
                {
                    let depth = depth + 1;
                    lhs.dump_ast(ctx, s, depth);
                    rhs.dump_ast(ctx, s, depth);
                }
                indented_line(s, depth, ": _no_type");
            }
            ast::Node::Sub { lhs, rhs } => {
                indented_line(s, depth, "_sub");
                {
                    let depth = depth + 1;
                    lhs.dump_ast(ctx, s, depth);
                    rhs.dump_ast(ctx, s, depth);
                }
                indented_line(s, depth, ": _no_type");
            }
            ast::Node::Mul { lhs, rhs } => {
                indented_line(s, depth, "_mul");
                {
                    let depth = depth + 1;
                    lhs.dump_ast(ctx, s, depth);
                    rhs.dump_ast(ctx, s, depth);
                }
                indented_line(s, depth, ": _no_type");
            }
            ast::Node::Div { lhs, rhs } => {
                indented_line(s, depth, "_divide");
                {
                    let depth = depth + 1;
                    lhs.dump_ast(ctx, s, depth);
                    rhs.dump_ast(ctx, s, depth);
                }
                indented_line(s, depth, ": _no_type");
            }
            ast::Node::IsVoid { expr } => {
                indented_line(s, depth, "_isvoid");
                expr.dump_ast(ctx, s, depth + 1);
                indented_line(s, depth, ": _no_type");
            }
            ast::Node::Negate { expr } => {
                indented_line(s, depth, "_neg");
                expr.dump_ast(ctx, s, depth + 1);
                indented_line(s, depth, ": _no_type");
            }
            ast::Node::Dispatch {
                receiver,
                cast,
                method,
                args,
            } => {
                indented_line(
                    s,
                    depth,
                    if cast.is_some() {
                        "_static_dispatch"
                    } else {
                        "_dispatch"
                    },
                );
                receiver.dump_ast(ctx, s, depth + 1);
                if let Some(ty) = *cast {
                    indented_line(s, depth + 1, ctx.interned_str_ref(ty.0));
                }
                indented_line(s, depth + 1, ctx.interned_str_ref(method.0));
                indented_line(s, depth + 1, "(");
                for a in args {
                    a.dump_ast(ctx, s, depth + 1);
                }
                indented_line(s, depth + 1, ")");
                indented_line(s, depth, ": _no_type");
            }
            ast::Node::LetIn { id, ty, expr, body } => {
                indented_line(s, depth, "_let");
                indented_line(s, depth + 1, ctx.interned_str_ref(id.0));
                indented_line(s, depth + 1, ctx.interned_str_ref(ty.0));
                if let Some(e) = expr {
                    e.dump_ast(ctx, s, depth + 1);
                } else {
                    indented_line(s, depth + 1, "_no_expr");
                    indented_line(s, depth + 1, ": _no_type");
                }
                body.dump_ast(ctx, s, depth + 1);
                indented_line(s, depth, ": _no_type");
            }
            ast::Node::IfThenElse {
                condition,
                consequent,
                alternative,
            } => {
                indented_line(s, depth, "_cond");
                condition.dump_ast(ctx, s, depth + 1);
                consequent.dump_ast(ctx, s, depth + 1);
                alternative.dump_ast(ctx, s, depth + 1);
                indented_line(s, depth, ": _no_type");
            }
            ast::Node::While { condition, body } => {
                indented_line(s, depth, "_loop");
                condition.dump_ast(ctx, s, depth + 1);
                body.dump_ast(ctx, s, depth + 1);
                indented_line(s, depth, ": _no_type");
            }
            ast::Node::Block { exprs } => {
                indented_line(s, depth, "_block");
                for e in exprs {
                    e.dump_ast(ctx, s, depth + 1);
                }
                indented_line(s, depth, ": _no_type");
            }
            ast::Node::Case { expr, cases } => {
                indented_line(s, depth, "_typcase");
                expr.dump_ast(ctx, s, depth + 1);
                for &(id, ty, expr) in cases {
                    indented_line(s, depth + 1, "_branch");
                    indented_line(s, depth + 2, ctx.interned_str_ref(id.0));
                    indented_line(s, depth + 2, ctx.interned_str_ref(ty.0));
                    expr.dump_ast(ctx, s, depth + 2);
                }
                indented_line(s, depth, ": _no_type");
            }
            ast::Node::New { ty } => {
                indented_line(s, depth, "_new");
                indented_line(s, depth + 1, ctx.interned_str_ref(ty.0));
                indented_line(s, depth, ": _no_type");
            }
            ast::Node::VariableReference { id } => {
                indented_line(s, depth, "_object");
                indented_line(s, depth + 1, ctx.interned_str_ref(id.0));
                indented_line(s, depth, ": _no_type");
            }
            ast::Node::IntegerConst(i) => {
                indented_line(s, depth, "_int");
                indented_line(s, depth + 1, &format!("{}", i));
                indented_line(s, depth, ": _no_type");
            }
            ast::Node::StringConst(st) => {
                indented_line(s, depth, "_string");
                indented_line(
                    s,
                    depth + 1,
                    &format!("\"{}\"", ctx.interned_str_ref(*st).replace("\n", "\\n")),
                );
                indented_line(s, depth, ": _no_type");
            }
            ast::Node::BoolConst(b) => {
                indented_line(s, depth, "_bool");
                indented_line(
                    s,
                    depth + 1,
                    match b {
                        true => "1",
                        false => "0",
                    },
                );
                indented_line(s, depth, ": _no_type");
            }
        }
    }
}
