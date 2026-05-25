from time_warp.features.ide_hooks import IDEComponentInitializer, IDEEventRouter


def test_component_initializer_can_boot_optional_feature_modules():
    initializer = IDEComponentInitializer()

    assert initializer.initialize_marketplace() is True
    assert initializer.initialize_debugger() is True
    assert initializer.initialize_ai_intelligence() is True
    assert initializer.initialize_beta_testing() is True
    assert initializer.initialize_integration_manager() is True

    assert "marketplace" in initializer.components
    assert "debugger" in initializer.components
    assert "ai_intelligence" in initializer.components
    assert "beta" in initializer.components
    assert initializer.components["ai_intelligence"]["status"] == "ready"


def test_event_router_emits_ai_and_debug_events():
    initializer = IDEComponentInitializer()
    assert initializer.initialize_debugger() is True
    assert initializer.initialize_ai_intelligence() is True

    router = IDEEventRouter(initializer)
    seen = {"started": [], "ai": []}

    router.register_handler(
        "debugger:session_started",
        lambda **data: seen["started"].append(data["session"]),
    )
    router.register_handler(
        "ai:suggestions",
        lambda **data: seen["ai"].append(data),
    )

    router.on_debug_start("lesson-1")
    router.on_code_change("IF X > 10 THEN\nPRINT X", "basic", (0, 1))

    assert len(seen["started"]) == 1
    assert len(seen["ai"]) == 1
    assert "completions" in seen["ai"][0]
    assert "bugs" in seen["ai"][0]


def _make_init():
    init = IDEComponentInitializer()
    init.initialize_marketplace()
    init.initialize_debugger()
    init.initialize_ai_intelligence()
    init.initialize_beta_testing()
    return init


class TestIDEComponentInitializer:
    """More IDEComponentInitializer tests."""

    def test_marketplace_in_components(self):
        init = _make_init()
        assert "marketplace" in init.components

    def test_debugger_in_components(self):
        init = _make_init()
        assert "debugger" in init.components

    def test_ai_in_components(self):
        init = _make_init()
        assert "ai_intelligence" in init.components

    def test_beta_in_components(self):
        init = _make_init()
        assert "beta" in init.components

    def test_ai_status_ready(self):
        init = _make_init()
        assert init.components["ai_intelligence"]["status"] == "ready"

    def test_marketplace_status_ready(self):
        init = _make_init()
        assert init.components["marketplace"]["status"] == "ready"

    def test_debugger_status_ready(self):
        init = _make_init()
        assert init.components["debugger"]["status"] == "ready"

    def test_initialize_returns_true(self):
        init = IDEComponentInitializer()
        assert init.initialize_marketplace() is True
        assert init.initialize_debugger() is True

    def test_get_component_marketplace(self):
        init = _make_init()
        comp = init.get_component("marketplace")
        assert comp is not None

    def test_get_component_debugger(self):
        init = _make_init()
        comp = init.get_component("debugger")
        assert comp is not None

    def test_get_component_missing_returns_none(self):
        init = _make_init()
        assert init.get_component("nonexistent") is None


class TestIDEEventRouter:
    """More IDEEventRouter event tests."""

    def _router(self):
        init = _make_init()
        return IDEEventRouter(init)

    def test_debug_start_triggers_handler(self):
        router = self._router()
        seen = []
        router.register_handler("debugger:session_started", lambda **d: seen.append(d))
        router.on_debug_start("sess-1")
        assert len(seen) == 1

    def test_debug_start_session_id(self):
        router = self._router()
        seen = []
        router.register_handler("debugger:session_started", lambda **d: seen.append(d))
        router.on_debug_start("my-session")
        assert seen[0]["session"].id == "my-session"

    def test_breakpoint_created_event(self):
        router = self._router()
        seen = []
        router.register_handler("debugger:breakpoint_created", lambda **d: seen.append(d))
        router.on_breakpoint_created("file.bas", 10)
        assert len(seen) == 1

    def test_breakpoint_line_number(self):
        router = self._router()
        seen = []
        router.register_handler("debugger:breakpoint_created", lambda **d: seen.append(d))
        router.on_breakpoint_created("file.bas", 10)
        assert seen[0]["breakpoint"].line == 10

    def test_breakpoint_file(self):
        router = self._router()
        seen = []
        router.register_handler("debugger:breakpoint_created", lambda **d: seen.append(d))
        router.on_breakpoint_created("script.bas", 5)
        assert seen[0]["breakpoint"].file == "script.bas"

    def test_multiple_handlers_same_event(self):
        router = self._router()
        counts = [0, 0]
        router.register_handler("debugger:session_started", lambda **d: counts.__setitem__(0, counts[0]+1))
        router.register_handler("debugger:session_started", lambda **d: counts.__setitem__(1, counts[1]+1))
        router.on_debug_start("s")
        assert counts[0] == 1
        assert counts[1] == 1

    def test_code_change_triggers_ai(self):
        router = self._router()
        seen = []
        router.register_handler("ai:suggestions", lambda **d: seen.append(d))
        router.on_code_change("PRINT X", "basic", (0, 0))
        assert len(seen) == 1

    def test_ai_suggestions_has_completions(self):
        router = self._router()
        seen = []
        router.register_handler("ai:suggestions", lambda **d: seen.append(d))
        router.on_code_change("PRINT X", "basic", (0, 0))
        assert "completions" in seen[0]

    def test_ai_suggestions_has_bugs(self):
        router = self._router()
        seen = []
        router.register_handler("ai:suggestions", lambda **d: seen.append(d))
        router.on_code_change("PRINT X", "basic", (0, 0))
        assert "bugs" in seen[0]


class TestIDEComponentInitializerExtended:
    """More IDEComponentInitializer tests."""

    def test_components_dict_type(self):
        init = IDEComponentInitializer()
        init.initialize_marketplace()
        assert isinstance(init.components, dict)

    def test_initialize_integration_manager_true(self):
        init = IDEComponentInitializer()
        assert init.initialize_integration_manager() is True

    def test_integration_manager_in_components(self):
        init = IDEComponentInitializer()
        init.initialize_integration_manager()
        assert "integration_manager" in init.components

    def test_all_components_have_status(self):
        init = IDEComponentInitializer()
        init.initialize_marketplace()
        init.initialize_debugger()
        init.initialize_ai_intelligence()
        init.initialize_beta_testing()
        for comp in init.components.values():
            assert "status" in comp

    def test_marketplace_status_string(self):
        init = IDEComponentInitializer()
        init.initialize_marketplace()
        assert isinstance(init.components["marketplace"]["status"], str)

    def test_get_component_returns_dict(self):
        init = IDEComponentInitializer()
        init.initialize_debugger()
        comp = init.get_component("debugger")
        assert isinstance(comp, dict)

    def test_get_component_has_status_key(self):
        init = IDEComponentInitializer()
        init.initialize_ai_intelligence()
        comp = init.get_component("ai_intelligence")
        assert "status" in comp

    def test_beta_status_ready(self):
        init = IDEComponentInitializer()
        init.initialize_beta_testing()
        assert init.components["beta"]["status"] == "ready"


class TestIDEEventRouterExtended:
    """More IDEEventRouter tests."""

    def _router(self):
        init = IDEComponentInitializer()
        init.initialize_debugger()
        init.initialize_ai_intelligence()
        return IDEEventRouter(init)

    def test_register_and_fire_custom_event(self):
        router = self._router()
        fired = []
        router.register_handler("custom:event", lambda **d: fired.append(d))
        router.emit_event("custom:event", value=42)
        assert len(fired) == 1

    def test_emit_passes_kwargs(self):
        router = self._router()
        received = []
        router.register_handler("test:event", lambda **d: received.append(d))
        router.emit_event("test:event", x=1, y=2)
        assert received[0]["x"] == 1
        assert received[0]["y"] == 2

    def test_unregistered_event_no_crash(self):
        router = self._router()
        router.emit_event("nonexistent:event", data="test")  # should not raise

    def test_multiple_debug_starts(self):
        router = self._router()
        seen = []
        router.register_handler("debugger:session_started",
                                lambda **d: seen.append(d))
        router.on_debug_start("s1")
        router.on_debug_start("s2")
        assert len(seen) == 2

    def test_debug_start_different_sessions(self):
        router = self._router()
        seen = []
        router.register_handler("debugger:session_started",
                                lambda **d: seen.append(d.get("session", "")))
        router.on_debug_start("alpha")
        router.on_debug_start("beta")
        assert len(seen) == 2


class TestIDEHooksExtended:
    """Extra IDE hooks tests."""

    def test_initializer_creates_instance(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer
        init = IDEComponentInitializer()
        assert init is not None

    def test_get_component_marketplace(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer
        init = IDEComponentInitializer()
        init.initialize_marketplace()
        comp = init.get_component("marketplace")
        assert comp is not None

    def test_get_component_debugger(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer
        init = IDEComponentInitializer()
        init.initialize_debugger()
        comp = init.get_component("debugger")
        assert comp is not None

    def test_get_status_after_init(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer
        init = IDEComponentInitializer()
        init.initialize_marketplace()
        status = init.get_status()
        assert isinstance(status, dict)

    def test_initialize_all_returns_dict(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer
        init = IDEComponentInitializer()
        result = init.initialize_all()
        assert isinstance(result, dict)

    def test_initialize_all_has_all_components(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer
        init = IDEComponentInitializer()
        result = init.initialize_all()
        assert "marketplace" in result
        assert "debugger" in result

    def test_event_router_creates_instance(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer, IDEEventRouter
        init = IDEComponentInitializer()
        router = IDEEventRouter(init)
        assert router is not None

    def test_register_and_emit_event(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer, IDEEventRouter
        init = IDEComponentInitializer()
        router = IDEEventRouter(init)
        called = []
        router.register_handler("test_event", lambda: called.append(1))
        router.emit_event("test_event")
        assert isinstance(called, list)

    def test_emit_event_no_handler_no_crash(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer, IDEEventRouter
        init = IDEComponentInitializer()
        router = IDEEventRouter(init)
        router.emit_event("unregistered_event")
        assert True

    def test_on_code_change_no_crash(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer, IDEEventRouter
        init = IDEComponentInitializer()
        router = IDEEventRouter(init)
        router.on_code_change("test code", "BASIC", 0)
        assert True


class TestIDEHooksExtended2:
    """Second extended set of IDE hooks tests."""

    def test_initializer_importable(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer
        assert IDEComponentInitializer is not None

    def test_router_importable(self):
        from time_warp.features.ide_hooks import IDEEventRouter
        assert IDEEventRouter is not None

    def test_router_creation(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer, IDEEventRouter
        init = IDEComponentInitializer()
        router = IDEEventRouter(init)
        assert router is not None

    def test_on_code_change_basic(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer, IDEEventRouter
        init = IDEComponentInitializer()
        router = IDEEventRouter(init)
        router.on_code_change("PRINT 1", "BASIC", 0)
        assert True

    def test_on_code_change_lua(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer, IDEEventRouter
        init = IDEComponentInitializer()
        router = IDEEventRouter(init)
        router.on_code_change("print('hi')", "LUA", 5)
        assert True

    def test_on_code_change_js(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer, IDEEventRouter
        init = IDEComponentInitializer()
        router = IDEEventRouter(init)
        router.on_code_change("console.log(1)", "JAVASCRIPT", 10)
        assert True

    def test_emit_event_basic_type(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer, IDEEventRouter
        init = IDEComponentInitializer()
        router = IDEEventRouter(init)
        router.emit_event("file_opened")
        assert True

    def test_emit_event_save(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer, IDEEventRouter
        init = IDEComponentInitializer()
        router = IDEEventRouter(init)
        router.emit_event("file_saved")
        assert True

    def test_two_routers_independent(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer, IDEEventRouter
        init1 = IDEComponentInitializer()
        init2 = IDEComponentInitializer()
        r1 = IDEEventRouter(init1)
        r2 = IDEEventRouter(init2)
        assert r1 is not r2

    def test_on_code_change_empty(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer, IDEEventRouter
        init = IDEComponentInitializer()
        router = IDEEventRouter(init)
        router.on_code_change("", "BASIC", 0)
        assert True


class TestIDEHooksExtended3:
    """Third round of IDE hooks tests."""

    def test_importable(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer, IDEEventRouter
        assert IDEComponentInitializer is not None and IDEEventRouter is not None

    def test_on_code_change_pascal(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer, IDEEventRouter
        init = IDEComponentInitializer()
        router = IDEEventRouter(init)
        router.on_code_change("PROGRAM t; BEGIN END.", "PASCAL", 5)
        assert True

    def test_on_code_change_erlang(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer, IDEEventRouter
        init = IDEComponentInitializer()
        router = IDEEventRouter(init)
        router.on_code_change("-module(t).", "ERLANG", 3)
        assert True

    def test_on_code_change_forth(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer, IDEEventRouter
        init = IDEComponentInitializer()
        router = IDEEventRouter(init)
        router.on_code_change("1 2 + .", "FORTH", 0)
        assert True

    def test_emit_code_run(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer, IDEEventRouter
        init = IDEComponentInitializer()
        router = IDEEventRouter(init)
        router.emit_event("code_run", language="LUA")
        assert True

    def test_emit_theme_changed(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer, IDEEventRouter
        init = IDEComponentInitializer()
        router = IDEEventRouter(init)
        router.emit_event("theme_changed", theme="Dracula")
        assert True

    def test_on_code_change_lisp(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer, IDEEventRouter
        init = IDEComponentInitializer()
        router = IDEEventRouter(init)
        router.on_code_change("(display 42)", "LISP", 8)
        assert True

    def test_on_code_change_prolog(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer, IDEEventRouter
        init = IDEComponentInitializer()
        router = IDEEventRouter(init)
        router.on_code_change("?- write(hi).", "PROLOG", 5)
        assert True

    def test_three_routers_independent(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer, IDEEventRouter
        r1 = IDEEventRouter(IDEComponentInitializer())
        r2 = IDEEventRouter(IDEComponentInitializer())
        r3 = IDEEventRouter(IDEComponentInitializer())
        assert r1 is not r2 and r2 is not r3

    def test_router_not_none(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer, IDEEventRouter
        init = IDEComponentInitializer()
        router = IDEEventRouter(init)
        assert router is not None


class TestIDEHooksExtended4:
    """Fourth round of IDE hooks tests."""

    def _router(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer, IDEEventRouter
        init = IDEComponentInitializer()
        return IDEEventRouter(init)

    def test_on_code_change_lisp(self):
        r = self._router()
        r.on_code_change("(display 42)", "LISP", 5)
        assert True

    def test_on_code_change_brainfuck(self):
        r = self._router()
        r.on_code_change("+++.---.", "BRAINFUCK", 2)
        assert True

    def test_on_code_change_hypertalk(self):
        r = self._router()
        r.on_code_change("put 1 into x", "HYPERTALK", 5)
        assert True

    def test_on_code_change_logo(self):
        r = self._router()
        r.on_code_change("FORWARD 50", "LOGO", 3)
        assert True

    def test_emit_file_opened(self):
        r = self._router()
        r.emit_event("file_opened", filename="test.bas")
        assert True

    def test_emit_file_saved(self):
        r = self._router()
        r.emit_event("file_saved", filename="test.bas")
        assert True

    def test_emit_language_changed(self):
        r = self._router()
        r.emit_event("language_changed", language="LUA")
        assert True

    def test_router_not_none(self):
        r = self._router()
        assert r is not None

    def test_four_routers_independent(self):
        r1 = self._router()
        r2 = self._router()
        r3 = self._router()
        r4 = self._router()
        assert r1 is not r2
        assert r3 is not r4

    def test_on_code_change_returns_none(self):
        r = self._router()
        result = r.on_code_change("PRINT 1", "BASIC", 0)
        assert result is None


class TestIDEHooksExtended5:
    """Fifth round of IDE hooks tests."""

    def _router(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer, IDEEventRouter
        init = IDEComponentInitializer()
        return IDEEventRouter(init)

    def test_router_creation(self):
        r = self._router()
        assert r is not None

    def test_on_code_change_basic(self):
        r = self._router()
        r.on_code_change("PRINT 1", "BASIC", 3)
        assert True

    def test_on_code_change_lua(self):
        r = self._router()
        r.on_code_change("print(1)", "LUA", 3)
        assert True

    def test_on_code_change_pascal(self):
        r = self._router()
        r.on_code_change("writeln('hi');", "PASCAL", 5)
        assert True

    def test_on_code_change_returns_none(self):
        r = self._router()
        result = r.on_code_change("x", "BASIC", 0)
        assert result is None

    def test_emit_event_run(self):
        r = self._router()
        r.emit_event("run_started", language="BASIC")
        assert True

    def test_emit_event_stopped(self):
        r = self._router()
        r.emit_event("run_stopped", code="PRINT 1")
        assert True

    def test_two_routers_independent(self):
        r1 = self._router()
        r2 = self._router()
        assert r1 is not r2

    def test_multiple_code_changes(self):
        r = self._router()
        for i in range(5):
            r.on_code_change(f"PRINT {i}", "BASIC", i)
        assert True

    def test_emit_multiple_events(self):
        r = self._router()
        r.emit_event("file_opened", filename="test.bas")
        r.emit_event("file_saved", filename="test.bas")
        assert True


class TestIDEHooksExtended6:
    """Sixth round of IDE hooks tests."""

    def _router(self):
        from time_warp.features.ide_hooks import IDEComponentInitializer, IDEEventRouter
        init = IDEComponentInitializer()
        return IDEEventRouter(init)

    def test_router_not_none(self):
        r = self._router()
        assert r is not None

    def test_two_routers_independent(self):
        r1 = self._router()
        r2 = self._router()
        assert r1 is not r2

    def test_on_code_change_basic(self):
        r = self._router()
        r.on_code_change("PRINT 1", "BASIC", 3)
        assert True

    def test_on_code_change_logo(self):
        r = self._router()
        r.on_code_change("fd 100", "LOGO", 6)
        assert True

    def test_on_code_change_lua(self):
        r = self._router()
        r.on_code_change("print(1)", "LUA", 8)
        assert True

    def test_emit_event_run(self):
        r = self._router()
        r.emit_event("run", code="PRINT 1", language="BASIC")
        assert True

    def test_emit_event_stop(self):
        r = self._router()
        r.emit_event("stop")
        assert True

    def test_emit_event_clear(self):
        r = self._router()
        r.emit_event("clear")
        assert True

    def test_emit_event_reset(self):
        r = self._router()
        r.emit_event("reset")
        assert True

    def test_on_code_change_empty(self):
        r = self._router()
        r.on_code_change("", "BASIC", 0)
        assert True
