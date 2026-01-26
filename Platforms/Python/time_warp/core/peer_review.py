"""Peer code review and collaborative feedback system."""

from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from pathlib import Path
from typing import Callable, Dict, List, Optional, Set


class ReviewStatus(Enum):
    """Review status."""

    PENDING = "pending"
    IN_PROGRESS = "in_progress"
    COMPLETED = "completed"
    REJECTED = "rejected"


class CommentType(Enum):
    """Comment types."""

    PRAISE = "praise"
    QUESTION = "question"
    SUGGESTION = "suggestion"
    ISSUE = "issue"
    TODO = "todo"


class SeverityLevel(Enum):
    """Issue severity levels."""

    MINOR = "minor"
    MODERATE = "moderate"
    MAJOR = "major"
    CRITICAL = "critical"


@dataclass
class CodeComment:
    """Comment on specific code lines."""

    author: str
    line_number: int
    content: str
    comment_type: CommentType
    severity: Optional[SeverityLevel] = None
    timestamp: datetime = field(default_factory=datetime.now)
    replies: List["CodeComment"] = field(default_factory=list)
    resolved: bool = False

    def add_reply(self, author: str, content: str):
        """Add reply to comment."""
        reply = CodeComment(
            author=author,
            line_number=self.line_number,
            content=content,
            comment_type=CommentType.QUESTION,
            timestamp=datetime.now(),
        )
        self.replies.append(reply)

    def mark_resolved(self):
        """Mark comment as resolved."""
        self.resolved = True


@dataclass
class ReviewRubric:
    """Grading rubric for reviews."""

    name: str
    criteria: Dict[str, int] = field(default_factory=dict)  # criterion -> max points

    def get_total_points(self) -> int:
        """Get total possible points."""
        return sum(self.criteria.values())

    def add_criterion(self, name: str, max_points: int = 10):
        """Add grading criterion."""
        self.criteria[name] = max_points


@dataclass
class ReviewFeedback:
    """Overall feedback for submission."""

    reviewer: str
    submission_id: str
    status: ReviewStatus
    score: Optional[float] = None
    max_score: Optional[float] = None
    summary: str = ""
    strengths: List[str] = field(default_factory=list)
    areas_for_improvement: List[str] = field(default_factory=list)
    general_comments: str = ""
    timestamp: datetime = field(default_factory=datetime.now)


class CodeReviewSession:
    """Single code review session."""

    def __init__(
        self,
        submission_id: str,
        author: str,
        code: str,
        language: str,
        description: str = "",
    ):
        """Initialize review session."""
        self.submission_id = submission_id
        self.author = author
        self.code = code
        self.language = language
        self.description = description
        self.created_at = datetime.now()

        # Review data
        self.comments: Dict[int, List[CodeComment]] = {}  # line -> comments
        self.feedback: Optional[ReviewFeedback] = None
        self.reviewers: Set[str] = set()

        # Status tracking
        self.status = ReviewStatus.PENDING
        self.rubric: Optional[ReviewRubric] = None

    def add_comment(
        self,
        reviewer: str,
        line_number: int,
        content: str,
        comment_type: CommentType = CommentType.SUGGESTION,
        severity: Optional[SeverityLevel] = None,
    ) -> CodeComment:
        """Add comment to specific line."""
        if line_number not in self.comments:
            self.comments[line_number] = []

        comment = CodeComment(
            author=reviewer,
            line_number=line_number,
            content=content,
            comment_type=comment_type,
            severity=severity,
        )

        self.comments[line_number].append(comment)
        self.reviewers.add(reviewer)

        return comment

    def get_comments_for_line(self, line_number: int) -> List[CodeComment]:
        """Get all comments for a line."""
        return self.comments.get(line_number, [])

    def get_all_comments(self) -> List[CodeComment]:
        """Get all comments."""
        all_comments = []
        for comments in self.comments.values():
            all_comments.extend(comments)
        return all_comments

    def get_issues(self) -> List[CodeComment]:
        """Get only issue-type comments."""
        issues = []
        for comments in self.comments.values():
            for comment in comments:
                if comment.comment_type == CommentType.ISSUE:
                    issues.append(comment)
        return issues

    def add_feedback(
        self,
        reviewer: str,
        summary: str,
        score: Optional[float] = None,
        max_score: Optional[float] = None,
    ):
        """Add overall feedback."""
        self.feedback = ReviewFeedback(
            reviewer=reviewer,
            submission_id=self.submission_id,
            status=ReviewStatus.COMPLETED,
            score=score,
            max_score=max_score,
            summary=summary,
        )
        self.status = ReviewStatus.COMPLETED

    def set_rubric(self, rubric: ReviewRubric):
        """Set grading rubric."""
        self.rubric = rubric

    def grade(self, rubric_scores: Dict[str, int]) -> float:
        """Grade using rubric."""
        if not self.rubric:
            return 0.0

        total = sum(rubric_scores.values())
        max_points = self.rubric.get_total_points()
        percentage = (total / max_points) * 100 if max_points > 0 else 0

        if self.feedback:
            self.feedback.score = percentage
            self.feedback.max_score = 100

        return percentage

    def export_report(self) -> str:
        """Export review as formatted report."""
        report = """
╔════════════════════════════════════════════════════════════╗
║              CODE REVIEW REPORT                             ║
╚════════════════════════════════════════════════════════════╝

Submission: {self.submission_id}
Author: {self.author}
Language: {self.language}
Status: {self.status.value}
Created: {self.created_at.strftime('%Y-%m-%d %H:%M:%S')}

Description:
{self.description}

═══════════════════════════════════════════════════════════
CODE
═══════════════════════════════════════════════════════════

"""

        # Code with line numbers and comments
        for i, line in enumerate(self.code.split("\n"), 1):
            report += f"{i:3d} | {line}\n"
            if i in self.comments:
                for comment in self.comments[i]:
                    icon = "💬"
                    if comment.comment_type == CommentType.PRAISE:
                        icon = "👍"
                    elif comment.comment_type == CommentType.ISSUE:
                        icon = "❌"
                    elif comment.comment_type == CommentType.SUGGESTION:
                        icon = "💡"

                    report += f"    {icon} [{comment.author}] {comment.content}\n"

        # Summary
        report += """

═══════════════════════════════════════════════════════════
SUMMARY
═══════════════════════════════════════════════════════════

Total Comments: {len(self.get_all_comments())}
Issues Found: {len(self.get_issues())}
Reviewers: {len(self.reviewers)}
"""

        if self.feedback:
            report += """
Score: {self.feedback.score:.1f}/{self.feedback.max_score}
Summary: {self.feedback.summary}
"""

        # Comment breakdown
        issues = [
            c for c in self.get_all_comments() if c.comment_type == CommentType.ISSUE
        ]
        suggestions = [
            c
            for c in self.get_all_comments()
            if c.comment_type == CommentType.SUGGESTION
        ]
        questions = [
            c for c in self.get_all_comments() if c.comment_type == CommentType.QUESTION
        ]

        report += f"""

Comment Breakdown:
  Issues: {len(issues)}
  Suggestions: {len(suggestions)}
  Questions: {len(questions)}
"""

        return report


class PeerReviewManager:
    """Manage peer code reviews."""

    def __init__(self):
        """Initialize review manager."""
        self.reviews: Dict[str, CodeReviewSession] = {}
        self.rubrics: Dict[str, ReviewRubric] = {}
        self.callbacks: Dict[str, List[Callable]] = {}

        # Default rubrics
        self._create_default_rubrics()

    def on_event(self, event_name: str, callback: Callable):
        """Register event callback."""
        if event_name not in self.callbacks:
            self.callbacks[event_name] = []
        self.callbacks[event_name].append(callback)

    def _trigger_callbacks(self, event_name: str, **kwargs):
        """Trigger callbacks."""
        if event_name in self.callbacks:
            for callback in self.callbacks[event_name]:
                callback(**kwargs)

    def _create_default_rubrics(self):
        """Create default grading rubrics."""

        # General rubric
        general = ReviewRubric("General Code Quality")
        general.add_criterion("Code Clarity", 25)
        general.add_criterion("Correctness", 25)
        general.add_criterion("Efficiency", 20)
        general.add_criterion("Documentation", 15)
        general.add_criterion("Style & Conventions", 15)
        self.rubrics["general"] = general

        # Game rubric
        game = ReviewRubric("Game Development")
        game.add_criterion("Gameplay", 25)
        game.add_criterion("Graphics", 20)
        game.add_criterion("Controls", 20)
        game.add_criterion("Polish", 15)
        game.add_criterion("Creativity", 20)
        self.rubrics["game"] = game

        # Graphics rubric
        graphics = ReviewRubric("Graphics & Visualization")
        graphics.add_criterion("Visual Appeal", 25)
        graphics.add_criterion("Correctness", 25)
        graphics.add_criterion("Animation", 20)
        graphics.add_criterion("Performance", 15)
        graphics.add_criterion("Code Quality", 15)
        self.rubrics["graphics"] = graphics

    def create_review(
        self,
        submission_id: str,
        author: str,
        code: str,
        language: str,
        description: str = "",
    ) -> CodeReviewSession:
        """Create new review session."""
        review = CodeReviewSession(
            submission_id=submission_id,
            author=author,
            code=code,
            language=language,
            description=description,
        )
        self.reviews[submission_id] = review
        self._trigger_callbacks("review_created", review=review)
        return review

    def get_review(self, submission_id: str) -> Optional[CodeReviewSession]:
        """Get review by ID."""
        return self.reviews.get(submission_id)

    def add_comment(
        self,
        submission_id: str,
        reviewer: str,
        line_number: int,
        content: str,
        comment_type: CommentType = CommentType.SUGGESTION,
        severity: Optional[SeverityLevel] = None,
    ):
        """Add comment to review."""
        review = self.get_review(submission_id)
        if review:
            comment = review.add_comment(
                reviewer=reviewer,
                line_number=line_number,
                content=content,
                comment_type=comment_type,
                severity=severity,
            )
            self._trigger_callbacks("comment_added", review=review, comment=comment)
            return comment
        return None

    def submit_review(
        self,
        submission_id: str,
        reviewer: str,
        summary: str,
        rubric_name: str = "general",
        scores: Optional[Dict[str, int]] = None,
    ) -> float:
        """Submit completed review."""
        review = self.get_review(submission_id)
        if not review:
            return 0.0

        review.add_feedback(reviewer=reviewer, summary=summary)

        if rubric_name in self.rubrics and scores:
            rubric = self.rubrics[rubric_name]
            review.set_rubric(rubric)
            score = review.grade(scores)
            self._trigger_callbacks("review_submitted", review=review, score=score)
            return score

        return 0.0

    def get_reviews_by_author(self, author: str) -> List[CodeReviewSession]:
        """Get all reviews for an author."""
        return [r for r in self.reviews.values() if r.author == author]

    def get_reviews_by_status(self, status: ReviewStatus) -> List[CodeReviewSession]:
        """Get reviews by status."""
        return [r for r in self.reviews.values() if r.status == status]

    def get_pending_reviews(self) -> List[CodeReviewSession]:
        """Get pending reviews."""
        return self.get_reviews_by_status(ReviewStatus.PENDING)

    def get_completed_reviews(self) -> List[CodeReviewSession]:
        """Get completed reviews."""
        return self.get_reviews_by_status(ReviewStatus.COMPLETED)

    def export_review(
        self, submission_id: str, output_path: Optional[Path] = None
    ) -> str:
        """Export review report."""
        review = self.get_review(submission_id)
        if not review:
            return ""

        report = review.export_report()

        if output_path:
            with open(output_path, 'w', encoding='utf-8') as f:
                f.write(report)

        return report

    def export_all_reviews(self, output_dir: Path):
        """Export all reviews to directory."""
        output_dir.mkdir(parents=True, exist_ok=True)

        for submission_id, review in self.reviews.items():
            report = self.export_review(submission_id)
            output_path = output_dir / f"{submission_id}_review.txt"
            with open(output_path, 'w', encoding='utf-8') as f:
                f.write(report)

    def get_summary_statistics(self) -> Dict:
        """Get aggregate review statistics."""
        total_reviews = len(self.reviews)
        completed = len(self.get_completed_reviews())
        pending = len(self.get_pending_reviews())

        total_comments = sum(len(r.get_all_comments()) for r in self.reviews.values())

        total_issues = sum(len(r.get_issues()) for r in self.reviews.values())

        avg_comments = total_comments / total_reviews if total_reviews > 0 else 0
        avg_issues = total_issues / total_reviews if total_reviews > 0 else 0

        return {
            "total_reviews": total_reviews,
            "completed": completed,
            "pending": pending,
            "total_comments": total_comments,
            "avg_comments_per_review": avg_comments,
            "total_issues": total_issues,
            "avg_issues_per_review": avg_issues,
        }


class StructuredReviewTemplate:
    """Template for structured code reviews."""

    @staticmethod
    def get_template(review_type: str = "general") -> str:
        """Get review template."""
        templates = {
            "general": """PEER CODE REVIEW TEMPLATE
================================

## Code Understanding
- [ ] Code purpose is clear
- [ ] Code logic is understandable

## Correctness
- [ ] Code produces correct output
- [ ] Edge cases are handled
- [ ] No obvious bugs found

## Code Quality
- [ ] Variable names are clear
- [ ] Functions are appropriately sized
- [ ] DRY principle is followed

## Performance
- [ ] No obvious performance issues
- [ ] Appropriate algorithms used
- [ ] Resource usage is reasonable

## Documentation
- [ ] Code has helpful comments
- [ ] Complex sections are explained
- [ ] Function purposes are documented

## Style & Convention
- [ ] Follows language conventions
- [ ] Indentation is consistent
- [ ] Naming conventions are followed

## Overall Assessment
Strengths:
[List positive aspects]

Areas for Improvement:
[List suggestions]

Score: __/100
""",
            "game": """GAME REVIEW TEMPLATE
===================

## Gameplay
- [ ] Game mechanics work correctly
- [ ] Win/lose conditions are clear
- [ ] Difficulty is appropriate

## Graphics
- [ ] Graphics are clear and appealing
- [ ] Graphics run smoothly
- [ ] Visual feedback is adequate

## Controls
- [ ] Controls are responsive
- [ ] Controls are intuitive
- [ ] Controls are well-mapped

## Polish
- [ ] No obvious bugs
- [ ] Error handling is present
- [ ] User experience is smooth

## Creativity
- [ ] Shows creative approach
- [ ] Unique features or twists
- [ ] Engaging gameplay

Score: __/100
""",
            "graphics": """GRAPHICS REVIEW TEMPLATE
=======================

## Visual Quality
- [ ] Graphics are visually appealing
- [ ] Colors are well-chosen
- [ ] Composition is balanced

## Technical Implementation
- [ ] Code is efficient
- [ ] Rendering is smooth
- [ ] Memory usage is reasonable

## Animation
- [ ] Animations are smooth
- [ ] Timing is appropriate
- [ ] Transitions are natural

## Code Quality
- [ ] Drawing code is organized
- [ ] No unnecessary complexity
- [ ] Performance is optimized

Score: __/100
""",
        }

        return templates.get(review_type, templates["general"])
