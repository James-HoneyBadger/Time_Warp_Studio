"""
LMS Integration Module - Phase 3 Feature

Integrates Time Warp Studio with popular Learning Management Systems:
- Canvas
- Google Classroom
- Blackboard
- Moodle

Supports grade sync, assignment submission, roster import, and analytics export.
"""

import json
import requests
from typing import Dict, List, Optional, Tuple
from datetime import datetime
from enum import Enum
from dataclasses import dataclass, asdict


class LMSType(Enum):
    """Supported LMS platforms."""
    CANVAS = "canvas"
    GOOGLE_CLASSROOM = "google_classroom"
    BLACKBOARD = "blackboard"
    MOODLE = "moodle"


class AssignmentStatus(Enum):
    """Assignment status."""
    DRAFT = "draft"
    PUBLISHED = "published"
    CLOSED = "closed"
    GRADING = "grading"
    COMPLETED = "completed"


@dataclass
class StudentSubmission:
    """Student assignment submission."""
    student_id: str
    student_name: str
    assignment_id: str
    submitted_at: str
    code: str
    language: str
    score: float
    feedback: str
    status: str  # "submitted", "graded", "returned"


@dataclass
class LMSAssignment:
    """Assignment synced from LMS."""
    id: str
    lms_id: str
    title: str
    description: str
    language: str
    starter_code: str
    due_date: str
    points: int
    rubric: Dict
    status: AssignmentStatus


@dataclass
class Student:
    """Student from LMS."""
    student_id: str
    name: str
    email: str
    username: str


class LMSConnector:
    """Base connector for LMS integration."""
    
    def __init__(self, lms_type: LMSType, base_url: str, api_key: str):
        self.lms_type = lms_type
        self.base_url = base_url
        self.api_key = api_key
        self.session = requests.Session()
        self.session.headers.update({"Authorization": f"Bearer {api_key}"})
    
    def authenticate(self) -> bool:
        """Test connection and authentication."""
        try:
            response = self.session.get(f"{self.base_url}/api/user")
            return response.status_code == 200
        except Exception:
            return False
    
    def get_courses(self) -> List[Dict]:
        """Get list of courses."""
        raise NotImplementedError
    
    def get_students(self, course_id: str) -> List[Student]:
        """Get students in course."""
        raise NotImplementedError
    
    def get_assignments(self, course_id: str) -> List[LMSAssignment]:
        """Get assignments in course."""
        raise NotImplementedError
    
    def submit_grade(self, course_id: str, assignment_id: str,
                    student_id: str, score: float, feedback: str) -> bool:
        """Submit grade to LMS."""
        raise NotImplementedError


class CanvasConnector(LMSConnector):
    """Canvas LMS connector."""
    
    def __init__(self, base_url: str, api_key: str):
        super().__init__(LMSType.CANVAS, base_url, api_key)
    
    def get_courses(self) -> List[Dict]:
        """Get list of courses."""
        try:
            response = self.session.get(f"{self.base_url}/api/v1/courses")
            return response.json() if response.status_code == 200 else []
        except Exception:
            return []
    
    def get_students(self, course_id: str) -> List[Student]:
        """Get students in course."""
        try:
            response = self.session.get(
                f"{self.base_url}/api/v1/courses/{course_id}/users",
                params={"enrollment_type[]": "student"}
            )
            students = []
            for user in response.json():
                students.append(Student(
                    student_id=str(user["id"]),
                    name=user.get("name", ""),
                    email=user.get("email", ""),
                    username=user.get("login_id", "")
                ))
            return students
        except Exception:
            return []
    
    def get_assignments(self, course_id: str) -> List[LMSAssignment]:
        """Get assignments in course."""
        try:
            response = self.session.get(
                f"{self.base_url}/api/v1/courses/{course_id}/assignments"
            )
            assignments = []
            for assign in response.json():
                assignments.append(LMSAssignment(
                    id=f"canvas_{assign['id']}",
                    lms_id=str(assign["id"]),
                    title=assign.get("name", ""),
                    description=assign.get("description", ""),
                    language="BASIC",  # Default, can be customized
                    starter_code="",
                    due_date=assign.get("due_at", ""),
                    points=int(assign.get("points_possible", 0)),
                    rubric={},
                    status=AssignmentStatus.PUBLISHED
                ))
            return assignments
        except Exception:
            return []
    
    def submit_grade(self, course_id: str, assignment_id: str,
                    student_id: str, score: float, feedback: str) -> bool:
        """Submit grade to Canvas."""
        try:
            response = self.session.put(
                f"{self.base_url}/api/v1/courses/{course_id}/assignments/{assignment_id}/submissions/{student_id}",
                json={"submission": {"posted_grade": score}, "comment": {"text_comment": feedback}}
            )
            return response.status_code == 200
        except Exception:
            return False


class GoogleClassroomConnector(LMSConnector):
    """Google Classroom connector."""
    
    def __init__(self, base_url: str, api_key: str):
        super().__init__(LMSType.GOOGLE_CLASSROOM, base_url, api_key)
    
    def get_courses(self) -> List[Dict]:
        """Get list of courses."""
        try:
            response = self.session.get(
                "https://classroom.googleapis.com/v1/courses"
            )
            return response.json().get("courses", []) if response.status_code == 200 else []
        except Exception:
            return []
    
    def get_students(self, course_id: str) -> List[Student]:
        """Get students in course."""
        try:
            response = self.session.get(
                f"https://classroom.googleapis.com/v1/courses/{course_id}/students"
            )
            students = []
            for student_data in response.json().get("students", []):
                profile = student_data.get("profile", {})
                students.append(Student(
                    student_id=student_data["userId"],
                    name=profile.get("name", {}).get("fullName", ""),
                    email=profile.get("emailAddress", ""),
                    username=student_data.get("userId", "")
                ))
            return students
        except Exception:
            return []
    
    def get_assignments(self, course_id: str) -> List[LMSAssignment]:
        """Get assignments in course."""
        try:
            response = self.session.get(
                f"https://classroom.googleapis.com/v1/courses/{course_id}/courseWork"
            )
            assignments = []
            for work in response.json().get("courseWork", []):
                assignments.append(LMSAssignment(
                    id=f"classroom_{work['id']}",
                    lms_id=work["id"],
                    title=work.get("title", ""),
                    description=work.get("description", ""),
                    language="BASIC",
                    starter_code="",
                    due_date=work.get("dueDate", {}).get("year", ""),
                    points=100,  # Default
                    rubric={},
                    status=AssignmentStatus.PUBLISHED
                ))
            return assignments
        except Exception:
            return []
    
    def submit_grade(self, course_id: str, assignment_id: str,
                    student_id: str, score: float, feedback: str) -> bool:
        """Submit grade to Google Classroom."""
        try:
            response = self.session.patch(
                f"https://classroom.googleapis.com/v1/courses/{course_id}/courseWork/{assignment_id}/studentSubmissions/{student_id}",
                json={"assignedGrade": score, "draftGrade": score}
            )
            return response.status_code == 200
        except Exception:
            return False


class LMSIntegration:
    """Main LMS integration manager."""
    
    def __init__(self):
        self.connectors: Dict[str, LMSConnector] = {}
        self.active_lms: Optional[str] = None
        self.synced_courses: Dict[str, Dict] = {}
        self.synced_assignments: Dict[str, LMSAssignment] = {}
        self.student_submissions: List[StudentSubmission] = []
    
    def register_connector(self, name: str, lms_type: LMSType,
                         base_url: str, api_key: str) -> bool:
        """Register an LMS connector."""
        if lms_type == LMSType.CANVAS:
            connector = CanvasConnector(base_url, api_key)
        elif lms_type == LMSType.GOOGLE_CLASSROOM:
            connector = GoogleClassroomConnector(base_url, api_key)
        else:
            return False
        
        if not connector.authenticate():
            return False
        
        self.connectors[name] = connector
        return True
    
    def activate_lms(self, name: str) -> bool:
        """Activate an LMS connector."""
        if name not in self.connectors:
            return False
        self.active_lms = name
        return True
    
    def sync_courses(self) -> List[Dict]:
        """Sync courses from active LMS."""
        if not self.active_lms:
            return []
        
        connector = self.connectors[self.active_lms]
        courses = connector.get_courses()
        
        for course in courses:
            self.synced_courses[course.get("id")] = course
        
        return courses
    
    def sync_assignments(self, course_id: str) -> List[LMSAssignment]:
        """Sync assignments from LMS course."""
        if not self.active_lms:
            return []
        
        connector = self.connectors[self.active_lms]
        assignments = connector.get_assignments(course_id)
        
        for assign in assignments:
            self.synced_assignments[assign.id] = assign
        
        return assignments
    
    def import_roster(self, course_id: str) -> List[Student]:
        """Import student roster from LMS."""
        if not self.active_lms:
            return []
        
        connector = self.connectors[self.active_lms]
        return connector.get_students(course_id)
    
    def submit_grade(self, assignment_id: str, student_id: str,
                    score: float, feedback: str = "") -> bool:
        """Submit grade back to LMS."""
        if not self.active_lms:
            return False
        
        assignment = self.synced_assignments.get(assignment_id)
        if not assignment:
            return False
        
        # Find course for assignment
        course_id = None
        for cid in self.synced_courses:
            if any(a.lms_id == assignment.lms_id for a in [assignment]):
                course_id = cid
                break
        
        if not course_id:
            return False
        
        connector = self.connectors[self.active_lms]
        return connector.submit_grade(course_id, assignment.lms_id,
                                     student_id, score, feedback)
    
    def record_submission(self, assignment_id: str, student_id: str,
                         student_name: str, code: str, language: str,
                         score: float = 0.0) -> StudentSubmission:
        """Record a student submission."""
        submission = StudentSubmission(
            student_id=student_id,
            student_name=student_name,
            assignment_id=assignment_id,
            submitted_at=datetime.now().isoformat(),
            code=code,
            language=language,
            score=score,
            feedback="",
            status="submitted"
        )
        
        self.student_submissions.append(submission)
        return submission
    
    def batch_grade(self, assignment_id: str, grades: Dict[str, float],
                   feedbacks: Dict[str, str]) -> int:
        """Batch grade multiple students."""
        success_count = 0
        
        for student_id, score in grades.items():
            feedback = feedbacks.get(student_id, "")
            if self.submit_grade(assignment_id, student_id, score, feedback):
                success_count += 1
        
        return success_count
    
    def export_analytics(self, course_id: str) -> Dict:
        """Export analytics for course."""
        return {
            "course_id": course_id,
            "synced_at": datetime.now().isoformat(),
            "total_students": len(self.import_roster(course_id)),
            "total_assignments": len([a for a in self.synced_assignments.values() 
                                     if a.lms_id]),
            "submissions": len(self.student_submissions),
            "average_score": (sum(s.score for s in self.student_submissions) / 
                            len(self.student_submissions) 
                            if self.student_submissions else 0.0)
        }
    
    def get_status(self) -> Dict:
        """Get integration status."""
        return {
            "active_lms": self.active_lms,
            "registered_connectors": list(self.connectors.keys()),
            "synced_courses": len(self.synced_courses),
            "synced_assignments": len(self.synced_assignments),
            "total_submissions": len(self.student_submissions),
            "authenticated": bool(self.active_lms and 
                                self.connectors[self.active_lms].authenticate()
                                if self.active_lms else False)
        }
