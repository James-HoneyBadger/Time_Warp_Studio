use crate::app::TimeWarpApp;
use eframe::egui;

pub fn render(_app: &TimeWarpApp, ui: &mut egui::Ui) {
    ui.heading("Time Warp IDE - Help");
    ui.separator();

    egui::ScrollArea::vertical().show(ui, |ui| {
        ui.heading("Quick Start");
        ui.label("Time Warp supports three educational programming languages:");
        ui.add_space(10.0);
        ui.heading("User Input");
        ui.label("When a program requests input (BASIC INPUT or PILOT A:), the IDE shows an 📝 prompt in the Output tab.");
        ui.label("Type your response and press Enter or click Submit to resume execution. The value is stored as a number if possible, otherwise as text.");
        ui.add_space(10.0);

        ui.heading("PILOT Language");
        ui.label("T:text - Display text");
        ui.label("A:var - Accept input");
        ui.label("U:var=value - Set variable");
        ui.label("C:condition - Compute condition");
        ui.label("Y:condition - Execute if true");
        ui.label("N:condition - Execute if false");
        ui.label("J:label - Jump to label");
        ui.label("L:label - Define label");
        ui.label("E: - End program");
        ui.label("SCREEN mode[, w, h] - Set screen (BASIC-style command; Logo follows selected mode)");
        ui.add_space(10.0);

        ui.heading("BASIC Language");
        ui.label("PRINT \"text\" - Display text");
        ui.label("INPUT var - Get user input (blocking)");
        ui.label("LET var$ = INKEY$ - Get key press (non-blocking)");
        ui.label("PRINT INKEY$ - Print last key pressed if any");
        ui.label("SCREEN mode[, w, h] - Set text/graphics screen (0=text, 1=640x480, 2=1024x768)");
        ui.label("CLS - Clear text screen and reset cursor");
        ui.label("LOCATE row, col - Move text cursor (1-based)");
        ui.label("LET var = value - Set variable");
        ui.label("GOTO line - Jump to line number");
        ui.label("IF condition THEN command - Conditional");
        ui.label("FOR var = start TO end - Loop");
        ui.label("NEXT var - End loop");
        ui.label("GOSUB line - Call subroutine");
        ui.label("RETURN - Return from subroutine");
        ui.label("END - End program");
        ui.add_space(10.0);

        ui.heading("Logo Language");
        ui.label("FORWARD n - Move turtle forward");
        ui.label("BACK n - Move turtle backward");
        ui.label("LEFT n - Turn left n degrees");
        ui.label("RIGHT n - Turn right n degrees");
        ui.label("PENUP - Lift pen");
        ui.label("PENDOWN - Lower pen");
        ui.label("CLEARSCREEN - Clear graphics");
        ui.label("HOME - Return to center");
        ui.label("SETXY x y - Set position");
        ui.label("REPEAT n [commands] - Repeat commands");
        ui.add_space(10.0);

        ui.heading("Example Programs");
        ui.label("See the examples/ directory for 32 sample programs in each language.");
        ui.label("PILOT: pilot_quiz.pilot, pilot_adventure.pilot, pilot_dragon_adventure.pilot, ...");
        ui.label("BASIC: basic_guess.bas, basic_hangman.bas, basic_inkey_demo.bas, basic_screen_modes.bas, ...");
        ui.label("Logo: logo_star.logo, logo_flower.logo, logo_snowman.logo, logo_koch_snowflake.logo, ...");
        ui.label("Graphics can be saved via View → Save Canvas as PNG…");
        ui.label("For detailed guidance, see USER_GUIDE.md → 'How to run [language] examples'.");
    });
}
