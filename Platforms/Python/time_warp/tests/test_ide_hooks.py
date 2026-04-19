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
