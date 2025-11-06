use time_warp_unified::compiler::TempleCodeCompiler;

#[test]
fn compile_to_c_basic_program() {
    let src = r#"
10 LET X = 5
20 PRINT "Value:", X
30 IF X > 3 THEN PRINT "Greater"
40 END
"#;
    let comp = TempleCodeCompiler::new();
    let c = comp.compile_to_c(src).expect("compile_to_c failed");
    assert!(c.contains("double V_X"));
    assert!(c.contains("line_10"));
    assert!(c.contains("line_20"));
    assert!(c.contains("printf(\"%g\", V_X)"));
    assert!(c.contains("if (V_X > 3"));
}
