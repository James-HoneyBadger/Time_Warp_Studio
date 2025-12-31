#!/usr/bin/env python3
"""Integration test for newly implemented features."""

import sys
from pathlib import Path

# Add project to path
project_root = Path(__file__).parent / "Platforms" / "Python"
sys.path.insert(0, str(project_root))

def test_syntax_validator():
    """Test syntax validation feature."""
    print("\n" + "=" * 60)
    print("TEST 1: Real-Time Syntax Validator")
    print("=" * 60)
    
    from time_warp.core.syntax_validator import SyntaxValidator, SeverityLevel
    from time_warp.core.interpreter import Language
    
    validator = SyntaxValidator()
    
    # Test BASIC with unclosed FOR
    code = "10 FOR I = 1 TO 10\n20 PRINT I"
    issues = validator.validate(code, Language.BASIC)
    
    print(f"Code:\n{code}\n")
    print(f"Issues found: {len(issues)}")
    for issue in issues:
        print(f"  Line {issue.line}: {issue.message} (Severity: {issue.severity.value})")
    
    assert len(issues) > 0, "Should detect unclosed FOR loop"
    print("✅ PASSED: Detects unclosed loops")
    
    # Test LOGO with balanced brackets
    code = "REPEAT 5 [FORWARD 100 RIGHT 90]"
    issues = validator.validate(code, Language.LOGO)
    
    assert len(issues) == 0, "Balanced brackets should have no issues"
    print("✅ PASSED: Validates correct LOGO syntax")


def test_project_templates():
    """Test project templates feature."""
    print("\n" + "=" * 60)
    print("TEST 2: Project Templates Library")
    print("=" * 60)
    
    from time_warp.core.project_templates import TemplateLibrary, TemplateCategory
    
    # Get all templates
    all_templates = TemplateLibrary.get_all()
    print(f"Total templates: {len(all_templates)}")
    assert len(all_templates) >= 8, "Should have at least 8 templates"
    print("✅ PASSED: Templates loaded")
    
    # Get by category
    games = TemplateLibrary.get_by_category(TemplateCategory.GAME)
    print(f"Game templates: {len(games)}")
    assert len(games) > 0, "Should have game templates"
    print("✅ PASSED: Category filtering works")
    
    # Search functionality
    results = TemplateLibrary.search("spiral")
    print(f"Search results for 'spiral': {len(results)}")
    assert len(results) > 0, "Should find spiral template"
    print("✅ PASSED: Search functionality works")
    
    # Get by difficulty
    beginner = TemplateLibrary.get_for_beginner()
    print(f"Beginner templates: {len(beginner)}")
    assert len(beginner) > 0, "Should have beginner templates"
    print("✅ PASSED: Difficulty filtering works")


def test_debugger():
    """Test execution timeline debugger."""
    print("\n" + "=" * 60)
    print("TEST 3: Code Execution Timeline Debugger")
    print("=" * 60)
    
    from time_warp.core.debugger import ExecutionTimeline, ExecutionState
    
    timeline = ExecutionTimeline()
    timeline.start_recording()
    
    # Simulate some frames
    timeline.record_frame(
        line=1,
        line_content="X = 5",
        variables={'X': 5},
        stack_depth=1
    )
    
    timeline.record_frame(
        line=2,
        line_content="Y = 10",
        variables={'X': 5, 'Y': 10},
        stack_depth=1
    )
    
    timeline.record_frame(
        line=3,
        line_content="Z = X + Y",
        variables={'X': 5, 'Y': 10, 'Z': 15},
        stack_depth=1
    )
    
    timeline.stop_recording()
    
    print(f"Recorded frames: {len(timeline.frames)}")
    assert len(timeline.frames) == 3, "Should have 3 frames"
    print("✅ PASSED: Frame recording works")
    
    # Test stepping
    timeline.current_frame_index = 0
    timeline.step_forward()
    assert timeline.current_frame_index == 1, "Should move to frame 1"
    print("✅ PASSED: Frame stepping works")
    
    # Test variable history
    history = timeline.get_variable_history('X')
    print(f"Variable X history: {len(history)} snapshots")
    assert len(history) >= 2, "Should have X in multiple frames"
    print("✅ PASSED: Variable history tracking works")
    
    # Test breakpoints
    timeline.set_breakpoint(2)
    assert 2 in timeline.breakpoints, "Should set breakpoint"
    print("✅ PASSED: Breakpoint management works")


def test_language_comparator():
    """Test language comparison feature."""
    print("\n" + "=" * 60)
    print("TEST 4: Multi-Language Comparator")
    print("=" * 60)
    
    from time_warp.core.language_comparator import MultiLanguageComparator
    
    comparator = MultiLanguageComparator()
    
    # Get builtin pairs
    pairs = comparator.get_builtin_pairs()
    print(f"Built-in comparison pairs: {len(pairs)}")
    assert len(pairs) >= 5, "Should have at least 5 built-in pairs"
    print("✅ PASSED: Built-in pairs available")
    
    # Get specific pair
    pair = comparator.get_builtin_pair('hello_world')
    assert pair is not None, "Should find hello_world pair"
    lang1, lang2, code1, code2 = pair
    print(f"Comparing: {lang1} vs {lang2}")
    print("✅ PASSED: Can retrieve specific pair")


def test_asset_library():
    """Test asset library feature."""
    print("\n" + "=" * 60)
    print("TEST 5: Asset Library System")
    print("=" * 60)
    
    from time_warp.core.asset_library import AssetLibrary, AssetType
    
    library = AssetLibrary()
    
    # Get all assets
    all_assets = library.list_all()
    print(f"Total assets: {len(all_assets)}")
    assert len(all_assets) > 0, "Should have assets"
    print("✅ PASSED: Assets loaded")
    
    # Get by type
    sprites = library.get_assets_by_type(AssetType.SPRITE)
    print(f"Sprites: {len(sprites)}")
    assert len(sprites) > 0, "Should have sprite assets"
    print("✅ PASSED: Type-based filtering works")
    
    # Search by tag
    game_assets = library.get_assets_by_tag('game')
    print(f"Game-tagged assets: {len(game_assets)}")
    assert len(game_assets) > 0, "Should have game assets"
    print("✅ PASSED: Tag-based search works")
    
    # Get specific asset
    coin = library.get_asset('coin')
    assert coin is not None, "Should find coin asset"
    assert coin.asset_type == AssetType.SPRITE, "Coin should be a sprite"
    print("✅ PASSED: Asset retrieval works")
    
    # Library info
    info = library.get_library_info()
    print(f"Library: {info.name} v{info.version}")
    print(f"Author: {info.author}")
    print(f"Assets: {info.asset_count}")
    print("✅ PASSED: Library metadata works")


def main():
    """Run all integration tests."""
    print("\n" + "=" * 60)
    print("TIME WARP STUDIO - FEATURE INTEGRATION TESTS")
    print("=" * 60)
    
    try:
        test_syntax_validator()
        test_project_templates()
        test_debugger()
        test_language_comparator()
        test_asset_library()
        
        print("\n" + "=" * 60)
        print("✅ ALL TESTS PASSED!")
        print("=" * 60)
        print("\nNew features are ready for UI integration:")
        print("  1. Real-time syntax validation")
        print("  2. Project templates library")
        print("  3. Code execution timeline debugger")
        print("  4. Multi-language comparator")
        print("  5. Asset library system")
        print("\nSee IMPLEMENTATION_COMPLETE.md for integration guide.")
        
        return 0
        
    except AssertionError as e:
        print(f"\n❌ TEST FAILED: {e}")
        return 1
    except Exception as e:
        print(f"\n❌ ERROR: {e}")
        import traceback
        traceback.print_exc()
        return 1


if __name__ == "__main__":
    sys.exit(main())
