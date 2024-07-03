use std::io::Write;

use ir::{c::{expr::{Call, Expr, Literal, Value}, identifier::Identifier, Statement, CIR}, Compilable};

mod ir;

fn main() {
    let program = CIR {
        includes: vec![],
        
        atoms: vec!["a".into(), "b".into()],

        main: vec![
            Statement::Bind { ident: Identifier("msg".into()), expr: Expr {
                type_: Identifier("String".into()),
                value: Value::Literal(Literal::String("Hello, World!".into()))} 
            },
            Statement::Expr(Expr {
                type_: Identifier("Void".into()),
                value: Value::Call(Call {
                    symbol: "print".into(),
                    args: vec![
                        Value::Identifier(Identifier("msg".into()))
                        ]
                    })
                }
            )
        ],
    };

    let mut file = std::fs::File::create("target/c/out.c").unwrap();
    file.write_all(program.compile().as_bytes()).unwrap();
}
