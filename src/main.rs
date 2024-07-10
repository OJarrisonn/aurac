use lalrpop_util::lalrpop_mod;

pub mod ir;
pub mod ast;
lalrpop_mod!(
    #[allow(clippy::ptr_arg)]
    #[rustfmt::skip]
    pub grammar
);

fn main() {
    let src = include_str!("../example/aura/val.aura");
    let ast = grammar::ModuleParser::new().parse(src);
    match ast {
        Ok(ast) => {
            println!("{:#?}", ast);
        }
        Err(e) => {
            println!("{}", e);
        }

    }
}
