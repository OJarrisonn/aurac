use std::io::Write;

use ir::{expr::{Call, Expr, Literal}, identifier::Identifier, c::{CIR, Statement}, Compilable};

mod ir;

fn main() {
    let program = CIR {
        includes: vec![],
        
        atoms: vec!["a".into(), "b".into()],

        main: vec![
            Statement::Bind { ident: Identifier::Value("msg".into()), type_: Identifier::Type("String".into()), expr: Expr::Literal(Literal::String("Hello, World!".into())) },
            Statement::Expr(Expr::Call(Call {
                symbol: "print".into(),
                args: vec![
                    Expr::Identifier(Identifier::Value("msg".into()))
                ]
            }))
        ],
    };

    let mut file = std::fs::File::create("target/c/out.c").unwrap();
    file.write_all(program.compile().as_bytes()).unwrap();
}
