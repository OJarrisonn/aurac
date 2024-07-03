use std::io::Write;

use ir::{c::{expr::{Call, Expr, Literal, Scope, Value}, identifier::Identifier, FunctionHeader, Statement, StaticBind, CIR}, Compilable};

pub mod ir;

fn main() {
    let program = CIR {
        includes: vec![],
        atoms: vec![],
        statics: vec![
            StaticBind::Function {
                header: FunctionHeader {
                    ident: Identifier("hello_world".into()),
                    args: vec![],
                    output: Identifier("Void".into())
                },
                body: Scope(vec![
                    Statement::Bind { 
                        ident: Identifier("message".into()), 
                        expr: Expr {
                            value: Value::Literal(Literal::String("Hello, world!\\n".to_string())),
                            type_: Identifier("String".into()),
                        }
                    },
                    Statement::Expr(Expr {
                        value: Value::Call(Call {
                            symbol: "print".into(),
                            args: vec![
                                Value::Identifier(Identifier("message".into())),
                            ],
                        }),
                        type_: Identifier("Void".into()),
                    }),
                ]),
            }
        ],
        main: StaticBind::Function {
            header: FunctionHeader {
                ident: Identifier("main".into()),
                args: vec![],
                output: Identifier("Int".into()),
            },
            body: Scope(vec![
                Statement::Expr(Expr {
                    value: Value::Call(Call {
                        symbol: "hello_world".into(),
                        args: vec![],
                    }),
                    type_: Identifier("Int".into()),
                }),
                Statement::Return(Value::Literal(Literal::Int(0))),
            ]),
        },
    };

    let mut file = std::fs::File::create("target/c/out.c").unwrap();
    file.write_all(program.compile().as_bytes()).unwrap();
}
