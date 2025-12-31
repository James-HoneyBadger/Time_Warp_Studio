"""Integration tests for Phase 2 extended features (#8, #9, #13-15)."""

import pytest
from pathlib import Path
from Platforms.Python.time_warp.core.executable_exporter import (
    ExecutableExporter, ExportFormat, ExportProfile
)
from Platforms.Python.time_warp.core.ai_assistant import (
    LocalAIAssistant, AssistantMode, CodeLanguage, AssistantSuggestion
)
from Platforms.Python.time_warp.core.learning_analytics import (
    LearningAnalytics, ConceptType, ClassroomAnalytics, ErrorCategory, ErrorPattern
)
from Platforms.Python.time_warp.core.peer_review import (
    PeerReviewManager, ReviewStatus, CommentType, SeverityLevel
)
from Platforms.Python.time_warp.core.accessibility import (
    AccessibilityManager, AccessibilityFeature, ColorBlindType
)


class TestExecutableExporter:
    """Test executable export functionality."""
    
    def test_exporter_initialization(self):
        """Test exporter initializes correctly."""
        exporter = ExecutableExporter()
        assert exporter.temp_dir.exists()
    
    def test_html5_export(self, tmp_path):
        """Test HTML5 export."""
        exporter = ExecutableExporter()
        code = "forward(100)\nright(90)"
        output_file = tmp_path / "program.html"
        
        success, message = exporter.export_html5(
            code,
            output_file,
            title="Test Logo",
            language="LOGO"
        )
        
        assert success
        assert output_file.exists()
        assert "Test Logo" in output_file.read_text()
    
    def test_shell_script_export(self, tmp_path):
        """Test shell script export."""
        exporter = ExecutableExporter()
        code = "PRINT 'Hello World'"
        output_file = tmp_path / "program.sh"
        
        success, message = exporter.export_shell_script(
            code,
            output_file,
            language="BASIC"
        )
        
        assert success
        assert output_file.exists()
        assert output_file.stat().st_mode & 0o111  # Check executable bit
    
    def test_export_presets(self):
        """Test export presets."""
        from Platforms.Python.time_warp.core.executable_exporter import ExportPresets
        
        presets = ExportPresets.list_presets()
        assert 'quick_python' in presets
        assert 'logo_html5' in presets
        
        preset = ExportPresets.get_preset('quick_python')
        assert preset is not None
        assert preset.name == 'Quick Python'


class TestAIAssistant:
    """Test AI assistant functionality."""
    
    def test_assistant_initialization(self):
        """Test assistant initializes."""
        assistant = LocalAIAssistant()
        assert assistant.language == CodeLanguage.BASIC
        assert len(assistant.conversation_history) == 0
    
    def test_explain_error(self):
        """Test error explanation."""
        assistant = LocalAIAssistant()
        result = assistant.explain_error("undefined variable x")
        
        assert isinstance(result, AssistantSuggestion)
        assert result.mode == AssistantMode.EXPLAIN_ERROR
        assert "variable" in result.explanation.lower()
    
    def test_suggest_code(self):
        """Test code suggestion."""
        assistant = LocalAIAssistant()
        assistant.language = CodeLanguage.BASIC
        result = assistant.suggest_code("loops", "BASIC")
        
        assert isinstance(result, AssistantSuggestion)
        assert result.mode == AssistantMode.SUGGEST_CODE
        assert "FOR" in result.code
    
    def test_fix_syntax(self):
        """Test syntax fixing."""
        assistant = LocalAIAssistant()
        code = "FOR i = 1 TO 10\n  PRINT i\n"  # Missing NEXT
        result = assistant.fix_syntax(code)
        
        assert isinstance(result, AssistantSuggestion)
        assert result.mode == AssistantMode.FIX_SYNTAX
    
    def test_explain_concept(self):
        """Test concept explanation."""
        assistant = LocalAIAssistant()
        explanation = assistant.explain_concept("loop")
        
        assert isinstance(explanation, str)
        assert "repeat" in explanation.lower()
    
    def test_chat_interface(self):
        """Test chat interface."""
        assistant = LocalAIAssistant()
        response = assistant.chat("How do I write a loop?")
        
        assert isinstance(response, str)
        assert len(assistant.conversation_history) == 2  # user + assistant


class TestLearningAnalytics:
    """Test learning analytics system."""
    
    def test_analytics_initialization(self):
        """Test analytics initializes."""
        analytics = LearningAnalytics("Student1")
        assert analytics.student_name == "Student1"
        assert len(analytics.concept_mastery) == len(ConceptType)
    
    def test_session_tracking(self):
        """Test session time tracking."""
        analytics = LearningAnalytics()
        analytics.start_session()
        duration = analytics.end_session()
        
        assert duration >= 0
        assert analytics.total_coding_time >= 0
    
    def test_program_recording(self):
        """Test program submission recording."""
        analytics = LearningAnalytics()
        analytics.record_program(
            name="test.bas",
            language="BASIC",
            execution_time=0.5,
            successful=True,
            lines_of_code=10,
            concepts=[ConceptType.LOOPS, ConceptType.VARIABLES]
        )
        
        assert len(analytics.programs) == 1
        assert analytics.programs[0].name == "test.bas"
    
    def test_concept_mastery(self):
        """Test concept mastery tracking."""
        analytics = LearningAnalytics()
        analytics.record_program(
            name="test.bas",
            language="BASIC",
            execution_time=1.0,
            successful=True,
            lines_of_code=5,
            concepts=[ConceptType.LOOPS]
        )
        
        mastery = analytics.concept_mastery[ConceptType.LOOPS]
        assert mastery.attempts > 0
        assert mastery.successes > 0
        assert mastery.confidence > 0
    
    def test_progress_metrics(self):
        """Test progress metrics."""
        analytics = LearningAnalytics()
        analytics.record_program(
            name="test1.bas",
            language="BASIC",
            execution_time=1.0,
            successful=True,
            lines_of_code=10,
            concepts=[ConceptType.LOOPS]
        )
        analytics.record_program(
            name="test2.bas",
            language="BASIC",
            execution_time=2.0,
            successful=False,
            lines_of_code=15,
            concepts=[ConceptType.VARIABLES]
        )
        
        metrics = analytics.get_progress_metrics()
        assert metrics['programs'] == 2
        assert metrics['successful_rate'] == 0.5
        assert metrics['total_lines_written'] == 25
    
    def test_export_report(self, tmp_path):
        """Test report export."""
        analytics = LearningAnalytics("TestStudent")
        report = analytics.export_report()
        
        assert "TestStudent" in report
        assert "PROGRESS SUMMARY" in report
    
    def test_classroom_analytics(self):
        """Test classroom-level analytics."""
        classroom = ClassroomAnalytics("Math Class")
        
        student1 = classroom.add_student("Alice")
        student2 = classroom.add_student("Bob")
        
        student1.record_program(
            name="test.bas",
            language="BASIC",
            execution_time=1.0,
            successful=True,
            lines_of_code=10,
            concepts=[ConceptType.LOOPS]
        )
        
        summary = classroom.get_class_summary()
        assert summary['total_students'] == 2
        assert summary['total_programs'] == 1


class TestPeerReview:
    """Test peer code review system."""
    
    def test_manager_initialization(self):
        """Test review manager initializes."""
        manager = PeerReviewManager()
        assert len(manager.rubrics) > 0
    
    def test_create_review(self):
        """Test creating review session."""
        manager = PeerReviewManager()
        code = "FOR i = 1 TO 10\nPRINT i\nNEXT i"
        
        review = manager.create_review(
            submission_id="test001",
            author="Student1",
            code=code,
            language="BASIC",
            description="Simple loop program"
        )
        
        assert review.submission_id == "test001"
        assert review.author == "Student1"
        assert review.status == ReviewStatus.PENDING
    
    def test_add_comments(self):
        """Test adding comments to review."""
        manager = PeerReviewManager()
        code = "FOR i = 1 TO 10\nPRINT i\nNEXT i"
        review = manager.create_review("test001", "Student1", code, "BASIC")
        
        comment = review.add_comment(
            reviewer="Teacher",
            line_number=1,
            content="Good loop structure!",
            comment_type=CommentType.PRAISE
        )
        
        assert comment.author == "Teacher"
        assert comment.resolved == False
    
    def test_review_feedback(self):
        """Test submitting review feedback."""
        manager = PeerReviewManager()
        code = "PRINT 'Hello'"
        review = manager.create_review("test001", "Student1", code, "BASIC")
        
        scores = {
            'Code Clarity': 20,
            'Correctness': 25,
            'Efficiency': 18,
            'Documentation': 12,
            'Style & Conventions': 13
        }
        
        score = manager.submit_review(
            submission_id="test001",
            reviewer="Teacher",
            summary="Great work!",
            rubric_name="general",
            scores=scores
        )
        
        assert review.status == ReviewStatus.COMPLETED
        assert score > 0
    
    def test_get_pending_reviews(self):
        """Test retrieving pending reviews."""
        manager = PeerReviewManager()
        
        review1 = manager.create_review("001", "Student1", "PRINT 1", "BASIC")
        review2 = manager.create_review("002", "Student2", "PRINT 2", "BASIC")
        
        pending = manager.get_pending_reviews()
        assert len(pending) == 2
    
    def test_export_review(self, tmp_path):
        """Test exporting review."""
        manager = PeerReviewManager()
        code = "FOR i = 1 TO 10\nPRINT i\nNEXT i"
        review = manager.create_review("test001", "Student1", code, "BASIC")
        
        report = manager.export_review("test001")
        assert "test001" in report
        assert "Student1" in report


class TestAccessibility:
    """Test accessibility features."""
    
    def test_manager_initialization(self):
        """Test accessibility manager initializes."""
        manager = AccessibilityManager()
        assert len(manager.enabled_features) == 0
    
    def test_enable_feature(self):
        """Test enabling accessibility feature."""
        manager = AccessibilityManager()
        manager.enable_feature(AccessibilityFeature.HIGH_CONTRAST)
        
        assert AccessibilityFeature.HIGH_CONTRAST in manager.enabled_features
        assert manager.settings.high_contrast_enabled
    
    def test_disable_feature(self):
        """Test disabling accessibility feature."""
        manager = AccessibilityManager()
        manager.enable_feature(AccessibilityFeature.HIGH_CONTRAST)
        manager.disable_feature(AccessibilityFeature.HIGH_CONTRAST)
        
        assert AccessibilityFeature.HIGH_CONTRAST not in manager.enabled_features
    
    def test_magnification(self):
        """Test magnification settings."""
        manager = AccessibilityManager()
        manager.set_magnification(2.0)
        
        assert manager.settings.magnification_level == 2.0
        assert AccessibilityFeature.MAGNIFICATION in manager.enabled_features
    
    def test_font_size(self):
        """Test font size settings."""
        manager = AccessibilityManager()
        manager.set_font_size(1.5)
        
        assert manager.settings.font_size_multiplier == 1.5
    
    def test_line_spacing(self):
        """Test line spacing settings."""
        manager = AccessibilityManager()
        manager.set_line_spacing(1.5)
        
        assert manager.settings.line_spacing_multiplier == 1.5
    
    def test_color_blind_mode(self):
        """Test color blind mode."""
        manager = AccessibilityManager()
        manager.set_color_blind_mode(ColorBlindType.PROTANOPIA)
        
        assert manager.settings.color_blind_mode == ColorBlindType.PROTANOPIA
    
    def test_screen_reader_labels(self):
        """Test ARIA label generation."""
        from Platforms.Python.time_warp.core.accessibility import ScreenReaderSupport
        
        label = ScreenReaderSupport.generate_aria_label('button', 'Run Program')
        assert 'Run Program' in label
    
    def test_keyboard_shortcuts(self):
        """Test keyboard shortcuts."""
        from Platforms.Python.time_warp.core.accessibility import AccessibleKeyboardLayout
        
        shortcuts = AccessibleKeyboardLayout.get_all_shortcuts()
        assert 'run_program' in shortcuts
        assert 'toggle_screen_reader' in shortcuts


class TestIntegration:
    """Integration tests across features."""
    
    def test_features_together(self):
        """Test multiple features working together."""
        # Learning analytics + accessibility
        analytics = LearningAnalytics()
        accessibility = AccessibilityManager()
        
        # Enabled accessibility
        accessibility.enable_feature(AccessibilityFeature.HIGH_CONTRAST)
        
        # Record program
        analytics.record_program(
            name="program.bas",
            language="BASIC",
            execution_time=1.0,
            successful=True,
            lines_of_code=20,
            concepts=[ConceptType.LOOPS, ConceptType.CONDITIONALS]
        )
        
        # Verify both systems work
        assert len(analytics.programs) == 1
        assert AccessibilityFeature.HIGH_CONTRAST in accessibility.enabled_features
    
    def test_review_with_analytics(self):
        """Test peer review integrated with analytics."""
        manager = PeerReviewManager()
        analytics = LearningAnalytics("Student1")
        
        # Create review
        review = manager.create_review(
            "001",
            "Student1",
            "FOR i = 1 TO 10\nPRINT i\nNEXT i",
            "BASIC"
        )
        
        # Record in analytics
        analytics.record_program(
            name="program.bas",
            language="BASIC",
            execution_time=0.5,
            successful=True,
            lines_of_code=3,
            concepts=[ConceptType.LOOPS]
        )
        
        assert len(manager.reviews) == 1
        assert len(analytics.programs) == 1


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
