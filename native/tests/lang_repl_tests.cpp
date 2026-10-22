// SPDX-License-Identifier: MIT
//
// The REPL, watch expressions and data breakpoints.

#include "lang/support.hpp"

namespace tw::lang_tests {

// ---------------------------------------------------------------------------
// REPL, watch expressions and data breakpoints
// ---------------------------------------------------------------------------

void test_repl_snippet_keeps_bindings() {
    RecordingHost host;
    Interpreter interpreter{Language::Basic, host};
    interpreter.load_program("10 PRINT \"program\"\n");
    interpreter.run();
    TW_CHECK_EQ(interpreter.output().size(), std::size_t{1});

    TW_CHECK(interpreter.run_snippet("LET X = 5").empty());
    TW_CHECK(interpreter.has_variable("X"));
    TW_CHECK_EQ(interpreter.number("X"), 5.0);

    // The second submission sees the first one's binding, and the program text
    // that was loaded before the REPL started is not run again.
    const std::string output = interpreter.run_snippet("PRINT X");
    TW_CHECK_EQ(output, std::string{"5\n"});
    TW_CHECK_EQ(interpreter.output().size(), std::size_t{1});
}

void test_basic_question_mark_is_print() {
    RecordingHost host;
    Interpreter interpreter{Language::Basic, host};
    interpreter.load_program("10 ? 6 * 7\n");
    interpreter.run();
    TW_CHECK(interpreter.error().empty());
    TW_CHECK_EQ(interpreter.output().size(), std::size_t{1});
    TW_CHECK_EQ(interpreter.output().at(0), std::string{"42"});
}

void test_repl_new_clears_basic_bindings() {
    RecordingHost host;
    ReplSession session{Language::Basic, host};
    TW_CHECK(session.execute("LET X = 7").empty());
    TW_CHECK(session.variables().count("X") == 1);

    const std::string cleared = session.execute("  new  ");
    TW_CHECK(cleared.find("cleared") != std::string::npos);
    TW_CHECK(session.variables().empty());
    TW_CHECK(session.error().empty());

    // A different word is still an error, not a second spelling of NEW.
    const std::string typo = session.execute("NEWS");
    TW_CHECK(typo.find("\u274C") != std::string::npos);
}

void test_watch_expressions_use_the_paused_bindings() {
    std::map<std::string, timewarp::lang::Value> variables;
    variables["X"] = timewarp::lang::Value{5.0};
    variables["TITLE"] = timewarp::lang::Value{std::string{"Ada"}};

    const timewarp::lang::WatchResult arithmetic =
        timewarp::lang::evaluate_watch("X * 2 + 1", variables);
    TW_CHECK(arithmetic.ok);
    TW_CHECK_EQ(arithmetic.value, std::string{"11"});

    // Case-insensitive, like the engine's own variable lookup.
    const timewarp::lang::WatchResult lower =
        timewarp::lang::evaluate_watch("x + 1", variables);
    TW_CHECK(lower.ok);
    TW_CHECK_EQ(lower.value, std::string{"6"});

    const timewarp::lang::WatchResult text =
        timewarp::lang::evaluate_watch("TITLE", variables);
    TW_CHECK(text.ok);
    TW_CHECK_EQ(text.value, std::string{"Ada"});

    const timewarp::lang::WatchResult missing =
        timewarp::lang::evaluate_watch("NOPE", variables);
    TW_CHECK(!missing.ok);
    TW_CHECK_EQ(missing.value, std::string{"?"});
    TW_CHECK(!missing.error.empty());

    const timewarp::lang::WatchResult divided =
        timewarp::lang::evaluate_watch("1 / 0", variables);
    TW_CHECK(!divided.ok);
}

void test_sql_statement_reaches_the_database_callback() {
    RecordingHost host;
    Interpreter interpreter{Language::Basic, host};
    std::string seen;
    interpreter.set_sql_callback([&seen](std::string_view sql) {
        seen = std::string{sql};
        return std::string{"TABLE\n"};
    });
    interpreter.load_program("10 SQL SELECT * FROM scores\n");
    interpreter.run();
    TW_CHECK_EQ(seen, std::string{"SELECT * FROM scores"});
    TW_CHECK_EQ(interpreter.output().size(), std::size_t{1});
    TW_CHECK_EQ(interpreter.output().front(), std::string{"TABLE"});

    RecordingHost plain_host;
    Interpreter plain{Language::Focal, plain_host};
    plain.load_program("01.10 SQL SELECT 1\n");
    plain.run();
    TW_CHECK_EQ(plain.output().size(), std::size_t{1});
    TW_CHECK(plain.output().front().find("no database is open") != std::string::npos);

    // Logo reaches the same callback through its own command loop; SQL is the
    // rest of the line, not a reporter split into Logo words.
    RecordingHost logo_host;
    Interpreter logo{Language::Logo, logo_host};
    std::string logo_seen;
    logo.set_sql_callback([&logo_seen](std::string_view sql) {
        logo_seen = std::string{sql};
        return std::string{"LOGO TABLE\n"};
    });
    logo.load_program("SQL SELECT * FROM scores\n");
    logo.run();
    TW_CHECK_EQ(logo_seen, std::string{"SELECT * FROM scores"});
    TW_CHECK_EQ(logo.output().size(), std::size_t{1});
    TW_CHECK_EQ(logo.output().front(), std::string{"LOGO TABLE"});

    // Smalltalk asks through its `SQL` class: `Transcript show: (SQL run: '...')`.
    RecordingHost st_host;
    Interpreter smalltalk{Language::Smalltalk, st_host};
    std::string st_seen;
    smalltalk.set_sql_callback([&st_seen](std::string_view sql) {
        st_seen = std::string{sql};
        return std::string{"ST TABLE"};
    });
    smalltalk.load_program("Transcript show: (SQL run: 'SELECT * FROM scores').\n");
    smalltalk.run();
    TW_CHECK_EQ(st_seen, std::string{"SELECT * FROM scores"});
    TW_CHECK_EQ(smalltalk.output().size(), std::size_t{1});
    TW_CHECK_EQ(smalltalk.output().front(), std::string{"ST TABLE"});
}

void test_data_breakpoint_pauses_on_a_variable_change() {
    RecordingHost host;
    Interpreter interpreter{Language::Basic, host};
    interpreter.load_program("10 LET X = 1\n20 LET X = 2\n30 PRINT X\n");

    timewarp::lang::DebugSession session;
    session.set_data_breakpoints({"X"});
    std::mutex mutex;
    std::condition_variable changed;
    std::vector<int> pauses;
    std::string changed_name;
    session.on_data_changed = [&](int line, const std::string& name) {
        {
            const std::lock_guard<std::mutex> lock{mutex};
            pauses.push_back(line);
            changed_name = name;
        }
        changed.notify_all();
    };
    interpreter.set_debug_session(&session);

    std::thread worker{[&] { interpreter.run(); }};
    const auto wait_for = [&](std::size_t count) {
        std::unique_lock lock{mutex};
        return changed.wait_for(lock, std::chrono::seconds{5},
                                [&] { return pauses.size() >= count; });
    };

    TW_CHECK(wait_for(1));
    TW_CHECK_EQ(pauses.at(0), 10);
    TW_CHECK_EQ(changed_name, std::string{"X"});
    TW_CHECK_EQ(interpreter.number("X"), 1.0);
    TW_CHECK(interpreter.output().empty());

    session.request_continue();
    TW_CHECK(wait_for(2));
    TW_CHECK_EQ(pauses.at(1), 20);
    TW_CHECK_EQ(interpreter.number("X"), 2.0);

    session.request_continue();
    worker.join();
    TW_CHECK_EQ(interpreter.output().size(), std::size_t{1});
    TW_CHECK_EQ(interpreter.output().front(), std::string{"2"});
    TW_CHECK(!session.paused());
}

void test_data_breakpoint_absent_when_not_watched() {
    RecordingHost host;
    Interpreter interpreter{Language::Basic, host};
    interpreter.load_program("10 LET X = 1\n20 LET Y = 2\n");
    timewarp::lang::DebugSession session;
    session.set_data_breakpoints({"X"});
    std::mutex mutex;
    std::condition_variable changed;
    int pauses = 0;
    session.on_data_changed = [&](int, const std::string&) {
        {
            const std::lock_guard<std::mutex> lock{mutex};
            ++pauses;
        }
        changed.notify_all();
    };
    interpreter.set_debug_session(&session);
    std::thread worker{[&] { interpreter.run(); }};
    {
        std::unique_lock lock{mutex};
        (void)changed.wait_for(lock, std::chrono::seconds{5}, [&] { return pauses > 0; });
    }
    session.request_continue();
    worker.join();
    // X changed once, at line 10. Y's assignment is not watched and must not
    // produce a second pause.
    TW_CHECK_EQ(pauses, 1);
}


void test_logo_data_breakpoint_pauses_at_a_statement_boundary() {
    RecordingHost host;
    Interpreter interpreter{Language::Logo, host};
    interpreter.load_program("MAKE \"X 1\nMAKE \"X 2\nPRINT :X\n");

    timewarp::lang::DebugSession session;
    session.set_data_breakpoints({"X"});
    std::mutex mutex;
    std::condition_variable changed;
    std::vector<int> pauses;
    session.on_data_changed = [&](int line, const std::string& name) {
        {
            const std::lock_guard<std::mutex> lock{mutex};
            pauses.push_back(line);
        }
        changed.notify_all();
        (void)name;
    };
    interpreter.set_debug_session(&session);

    std::thread worker{[&] { interpreter.run(); }};
    const auto wait_for = [&](std::size_t count) {
        std::unique_lock lock{mutex};
        return changed.wait_for(lock, std::chrono::seconds{5},
                                [&] { return pauses.size() >= count; });
    };

    // The pause point is the boundary *after* the statement, so the first change
    // (X = 1) is reported before line 2 and the second (X = 2) before line 3.
    TW_CHECK(wait_for(1));
    TW_CHECK_EQ(pauses.at(0), 2);
    TW_CHECK_EQ(interpreter.number("X"), 1.0);
    session.request_continue();
    TW_CHECK(wait_for(2));
    TW_CHECK_EQ(pauses.at(1), 3);
    TW_CHECK_EQ(interpreter.number("X"), 2.0);
    session.request_continue();
    worker.join();
    TW_CHECK_EQ(interpreter.output().size(), std::size_t{1});
    TW_CHECK_EQ(interpreter.output().front(), std::string{"2"});
}

}  // namespace tw::lang_tests
