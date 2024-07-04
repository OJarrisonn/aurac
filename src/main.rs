use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub grammar);
pub mod ir;

fn main() {
    assert!(dbg!(grammar::ValParser::new().parse("val ten Int = 10")).is_ok());
    assert!(dbg!(grammar::FnParser::new().parse("fn add(a Int, b Int) -> Int { v = 10; return v }")).is_ok());
}
