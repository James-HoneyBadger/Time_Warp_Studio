// Re-export main modules for testing
pub mod app;
pub mod audio;
pub mod compiler;
pub mod game;
pub mod graphics;
pub mod interpreter;
pub mod iot;
pub mod languages;
pub mod ml;
pub mod plugins;
pub mod ui;
pub mod utils;

#[cfg(test)]
mod tests {
    #[cfg(feature = "audio")]
    use super::audio::AudioMixer;
    use super::utils::ExpressionEvaluator;
    use std::collections::HashMap;

    #[test]
    fn test_expression_evaluator_basic() {
        let eval = ExpressionEvaluator::new();
        assert_eq!(eval.evaluate("2 + 3").unwrap(), 5.0);
        assert_eq!(eval.evaluate("10 - 4").unwrap(), 6.0);
        assert_eq!(eval.evaluate("3 * 4").unwrap(), 12.0);
        assert_eq!(eval.evaluate("15 / 3").unwrap(), 5.0);
    }

    #[test]
    fn test_expression_evaluator_precedence() {
        let eval = ExpressionEvaluator::new();
        assert_eq!(eval.evaluate("2 + 3 * 4").unwrap(), 14.0);
        assert_eq!(eval.evaluate("(2 + 3) * 4").unwrap(), 20.0);
        assert_eq!(eval.evaluate("2 ^ 3").unwrap(), 8.0);
    }

    #[test]
    fn test_expression_evaluator_functions() {
        let eval = ExpressionEvaluator::new();

        // Test SIN(0) - should be close to 0
        let sin_result = eval.evaluate("SIN(0)");
        println!("SIN(0) = {:?}", sin_result);
        assert!(sin_result.is_ok());
        assert!((sin_result.unwrap() - 0.0).abs() < 0.0001);

        // Test ABS(-5) - should be 5
        let abs_result = eval.evaluate("ABS(-5)");
        println!("ABS(-5) = {:?}", abs_result);
        assert!(abs_result.is_ok());
        assert_eq!(abs_result.unwrap(), 5.0);

        // Test SQRT(16) - should be 4
        let sqrt_result = eval.evaluate("SQRT(16)");
        println!("SQRT(16) = {:?}", sqrt_result);
        assert!(sqrt_result.is_ok());
        assert_eq!(sqrt_result.unwrap(), 4.0);

        // Test INT(3.7) - should be 3
        let int_result = eval.evaluate("INT(3.7)");
        println!("INT(3.7) = {:?}", int_result);
        assert!(int_result.is_ok());
        assert_eq!(int_result.unwrap(), 3.0);

        // Test ROUND(3.5) - should be 4
        let round_result = eval.evaluate("ROUND(3.5)");
        println!("ROUND(3.5) = {:?}", round_result);
        assert!(round_result.is_ok());
        assert_eq!(round_result.unwrap(), 4.0);
    }

    #[test]
    fn test_expression_evaluator_variables() {
        let mut vars = HashMap::new();
        vars.insert("X".to_string(), 10.0);
        vars.insert("Y".to_string(), 5.0);
        let eval = ExpressionEvaluator::with_variables(vars);
        assert_eq!(eval.evaluate("X + Y").unwrap(), 15.0);
        assert_eq!(eval.evaluate("X * 2 + Y").unwrap(), 25.0);
        assert_eq!(eval.evaluate("SQRT(X) + Y").unwrap(), 8.16227766016838);
    }

    #[test]
    fn test_expression_evaluator_complex() {
        let mut vars = HashMap::new();
        vars.insert("A".to_string(), 3.0);
        vars.insert("B".to_string(), 4.0);
        let eval = ExpressionEvaluator::with_variables(vars);

        // Pythagorean theorem
        let result = eval.evaluate("SQRT(A^2 + B^2)").unwrap();
        assert!((result - 5.0).abs() < 0.0001);
    }

    #[test]
    fn test_audio_mixer_creation() {
        #[cfg(feature = "audio")]
        {
            let mixer = AudioMixer::new();
            mixer.beep(); // Should not crash
        }
    }

    #[test]
    fn test_audio_mixer_music_string() {
        #[cfg(feature = "audio")]
        {
            let mixer = AudioMixer::new();
            let result = mixer.play_music_string("C4 D4 E4");
            assert!(result.is_ok());
        }
    }

    #[test]
    fn test_interpreter_creation() {
        use super::interpreter::Interpreter;
        let interp = Interpreter::new();
        assert_eq!(interp.variables.len(), 0);
        assert_eq!(interp.output.len(), 0);
    }

    #[test]
    fn test_interpreter_expression_evaluation() {
        use super::interpreter::Interpreter;
        let mut interp = Interpreter::new();
        interp.variables.insert("X".to_string(), 10.0);

        let result = interp.evaluate_expression("X * 2 + 5").unwrap();
        assert_eq!(result, 25.0);
    }

    #[test]
    fn test_interpreter_text_interpolation() {
        use super::interpreter::Interpreter;
        let mut interp = Interpreter::new();
        interp.variables.insert("NAME".to_string(), 42.0);
        interp
            .string_variables
            .insert("GREETING".to_string(), "Hello".to_string());

        let result = interp.interpolate_text("*GREETING* world! The answer is *NAME*");
        assert_eq!(result, "Hello world! The answer is 42");
    }

    #[test]
    fn test_pilot_text_command() {
        use super::graphics::TurtleState;
        use super::interpreter::Interpreter;
        use super::languages::pilot;

        let mut interp = Interpreter::new();
        let mut turtle = TurtleState::new();

        let result = pilot::execute(&mut interp, "T:Hello World", &mut turtle);
        assert!(result.is_ok());
        assert_eq!(interp.output.len(), 1);
        assert_eq!(interp.output[0], "Hello World");
    }

    #[test]
    fn test_pilot_use_command() {
        use super::graphics::TurtleState;
        use super::interpreter::Interpreter;
        use super::languages::pilot;

        let mut interp = Interpreter::new();
        let mut turtle = TurtleState::new();

        let result = pilot::execute(&mut interp, "U:X=10", &mut turtle);
        assert!(result.is_ok());
        assert_eq!(interp.variables.get("X"), Some(&10.0));
    }

    #[test]
    fn test_pilot_compute_and_yes() {
        use super::graphics::TurtleState;
        use super::interpreter::Interpreter;
        use super::languages::pilot;

        let mut interp = Interpreter::new();
        let mut turtle = TurtleState::new();

        // Set up a variable
        interp.variables.insert("X".to_string(), 10.0);

        // C: stores condition
        pilot::execute(&mut interp, "C:X>5", &mut turtle).unwrap();
        assert_eq!(interp.stored_condition, Some(true));

        // Y: checks stored condition
        pilot::execute(&mut interp, "Y:", &mut turtle).unwrap();
        assert_eq!(interp.match_flag, true);
        assert_eq!(interp.last_match_set, true);
    }

    #[test]
    fn test_pilot_conditional_text() {
        use super::graphics::TurtleState;
        use super::interpreter::Interpreter;
        use super::languages::pilot;

        let mut interp = Interpreter::new();
        let mut turtle = TurtleState::new();

        interp.variables.insert("SCORE".to_string(), 85.0);

        // Test Y: with condition
        pilot::execute(&mut interp, "Y:SCORE>80", &mut turtle).unwrap();
        pilot::execute(&mut interp, "T:Great job!", &mut turtle).unwrap();

        assert_eq!(interp.output.len(), 1);
        assert_eq!(interp.output[0], "Great job!");
    }
}
