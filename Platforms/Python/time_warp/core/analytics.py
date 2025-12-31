"""
Time Warp Studio - Analytics & Feature Flags Infrastructure

Provides:
- Event tracking
- User analytics
- Feature flag management
- A/B testing
- Funnel analysis
"""

from typing import Dict, Any, Optional, List
from dataclasses import dataclass, asdict, field
from datetime import datetime
from enum import Enum
from abc import ABC, abstractmethod
import json
import uuid

# ===== ANALYTICS =====

class EventType(Enum):
    """Analytics event types"""
    # User events
    USER_SIGNUP = "user.signup"
    USER_LOGIN = "user.login"
    USER_LOGOUT = "user.logout"
    USER_PROFILE_UPDATE = "user.profile.update"
    
    # Project events
    PROJECT_CREATE = "project.create"
    PROJECT_UPDATE = "project.update"
    PROJECT_DELETE = "project.delete"
    PROJECT_FORK = "project.fork"
    PROJECT_SHARE = "project.share"
    
    # Code execution events
    CODE_EXECUTE = "code.execute"
    CODE_DEBUG = "code.debug"
    CODE_ERROR = "code.error"
    
    # Collaboration events
    COLLABORATION_START = "collaboration.start"
    COLLABORATION_EDIT = "collaboration.edit"
    COLLABORATION_COMMENT = "collaboration.comment"
    
    # Feature usage
    FEATURE_USE = "feature.use"
    FEATURE_ERROR = "feature.error"
    
    # UI events
    BUTTON_CLICK = "ui.button.click"
    MENU_OPEN = "ui.menu.open"
    MODAL_OPEN = "ui.modal.open"
    
    # Performance events
    PERFORMANCE_SLOW = "performance.slow"
    PERFORMANCE_ERROR = "performance.error"

@dataclass
class AnalyticsEvent:
    """Single analytics event"""
    event_type: EventType
    user_id: str
    timestamp: datetime = field(default_factory=datetime.utcnow)
    session_id: str = field(default_factory=lambda: str(uuid.uuid4()))
    properties: Dict[str, Any] = field(default_factory=dict)
    context: Dict[str, Any] = field(default_factory=dict)
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary"""
        return {
            "event_type": self.event_type.value,
            "user_id": self.user_id,
            "timestamp": self.timestamp.isoformat(),
            "session_id": self.session_id,
            "properties": self.properties,
            "context": self.context,
        }

@dataclass
class UserProfile:
    """User analytics profile"""
    user_id: str
    first_seen: datetime
    last_seen: datetime
    total_sessions: int = 0
    total_events: int = 0
    properties: Dict[str, Any] = field(default_factory=dict)
    
    # Engagement metrics
    total_projects: int = 0
    total_executions: int = 0
    total_collaborations: int = 0
    average_session_duration: int = 0  # seconds
    
    # Feature adoption
    features_used: List[str] = field(default_factory=list)
    languages_used: List[str] = field(default_factory=list)

class AnalyticsProvider(ABC):
    """Abstract analytics provider"""
    
    @abstractmethod
    def track_event(self, event: AnalyticsEvent) -> bool:
        """Track an event"""
        pass
    
    @abstractmethod
    def get_user_profile(self, user_id: str) -> Optional[UserProfile]:
        """Get user analytics profile"""
        pass
    
    @abstractmethod
    def get_funnel_data(self, funnel_name: str) -> Dict[str, Any]:
        """Get funnel analysis data"""
        pass
    
    @abstractmethod
    def get_cohort_data(self, cohort_name: str) -> Dict[str, Any]:
        """Get cohort data"""
        pass

class LocalAnalyticsProvider(AnalyticsProvider):
    """Local analytics implementation for testing"""
    
    def __init__(self):
        self.events: List[AnalyticsEvent] = []
        self.user_profiles: Dict[str, UserProfile] = {}
    
    def track_event(self, event: AnalyticsEvent) -> bool:
        """Track event"""
        self.events.append(event)
        self._update_user_profile(event)
        return True
    
    def get_user_profile(self, user_id: str) -> Optional[UserProfile]:
        """Get user profile"""
        return self.user_profiles.get(user_id)
    
    def get_funnel_data(self, funnel_name: str) -> Dict[str, Any]:
        """Get funnel data"""
        # Placeholder implementation
        return {"funnel": funnel_name, "steps": []}
    
    def get_cohort_data(self, cohort_name: str) -> Dict[str, Any]:
        """Get cohort data"""
        # Placeholder implementation
        return {"cohort": cohort_name, "users": 0}
    
    def _update_user_profile(self, event: AnalyticsEvent):
        """Update user profile with event"""
        if event.user_id not in self.user_profiles:
            self.user_profiles[event.user_id] = UserProfile(
                user_id=event.user_id,
                first_seen=event.timestamp,
                last_seen=event.timestamp,
            )
        
        profile = self.user_profiles[event.user_id]
        profile.last_seen = event.timestamp
        profile.total_events += 1

class AnalyticsClient:
    """Analytics client for tracking events"""
    
    def __init__(self, provider: AnalyticsProvider, user_id: str):
        self.provider = provider
        self.user_id = user_id
        self.session_id = str(uuid.uuid4())
        self.context: Dict[str, Any] = {}
    
    def track_event(
        self, 
        event_type: EventType, 
        properties: Dict[str, Any] = None,
        context: Dict[str, Any] = None
    ) -> bool:
        """Track an event"""
        event = AnalyticsEvent(
            event_type=event_type,
            user_id=self.user_id,
            session_id=self.session_id,
            properties=properties or {},
            context={**self.context, **(context or {})}
        )
        return self.provider.track_event(event)
    
    def set_context(self, context: Dict[str, Any]):
        """Set context for all future events"""
        self.context.update(context)
    
    def clear_context(self):
        """Clear context"""
        self.context.clear()

# ===== FEATURE FLAGS =====

class FeatureFlagType(Enum):
    """Feature flag types"""
    BOOLEAN = "boolean"
    PERCENTAGE = "percentage"
    USER_LIST = "user_list"
    RULE_BASED = "rule_based"

@dataclass
class FeatureFlagRule:
    """Rule for feature flag evaluation"""
    attribute: str  # e.g., "user_tier", "country"
    operator: str  # "equals", "contains", "in", "greater_than"
    value: Any  # value to compare
    enabled: bool = True

@dataclass
class FeatureFlag:
    """Feature flag definition"""
    key: str
    name: str
    description: str
    flag_type: FeatureFlagType
    enabled: bool = False
    percentage: int = 0  # 0-100 for PERCENTAGE type
    whitelist_users: List[str] = field(default_factory=list)
    blacklist_users: List[str] = field(default_factory=list)
    rules: List[FeatureFlagRule] = field(default_factory=list)
    created_at: datetime = field(default_factory=datetime.utcnow)
    updated_at: datetime = field(default_factory=datetime.utcnow)

class FeatureFlagProvider(ABC):
    """Abstract feature flag provider"""
    
    @abstractmethod
    def is_enabled(self, flag_key: str, user_id: str = None, context: Dict[str, Any] = None) -> bool:
        """Check if feature flag is enabled"""
        pass
    
    @abstractmethod
    def get_flag(self, flag_key: str) -> Optional[FeatureFlag]:
        """Get feature flag definition"""
        pass
    
    @abstractmethod
    def set_flag(self, flag: FeatureFlag) -> bool:
        """Set/update feature flag"""
        pass

class LocalFeatureFlagProvider(FeatureFlagProvider):
    """Local feature flag implementation"""
    
    def __init__(self):
        self.flags: Dict[str, FeatureFlag] = {}
    
    def is_enabled(
        self, 
        flag_key: str, 
        user_id: str = None, 
        context: Dict[str, Any] = None
    ) -> bool:
        """Check if flag is enabled"""
        flag = self.flags.get(flag_key)
        if not flag:
            return False
        
        if flag.flag_type == FeatureFlagType.BOOLEAN:
            return flag.enabled
        
        elif flag.flag_type == FeatureFlagType.PERCENTAGE:
            if user_id:
                # Consistent hashing for same user
                hash_val = hash(f"{flag_key}:{user_id}") % 100
                return hash_val < flag.percentage
            return False
        
        elif flag.flag_type == FeatureFlagType.USER_LIST:
            if user_id in flag.blacklist_users:
                return False
            return user_id in flag.whitelist_users
        
        elif flag.flag_type == FeatureFlagType.RULE_BASED:
            return self._evaluate_rules(flag.rules, context or {})
        
        return False
    
    def get_flag(self, flag_key: str) -> Optional[FeatureFlag]:
        """Get flag"""
        return self.flags.get(flag_key)
    
    def set_flag(self, flag: FeatureFlag) -> bool:
        """Set flag"""
        flag.updated_at = datetime.utcnow()
        self.flags[flag.key] = flag
        return True
    
    def _evaluate_rules(self, rules: List[FeatureFlagRule], context: Dict[str, Any]) -> bool:
        """Evaluate all rules (AND logic)"""
        for rule in rules:
            if not self._evaluate_rule(rule, context):
                return False
        return True
    
    def _evaluate_rule(self, rule: FeatureFlagRule, context: Dict[str, Any]) -> bool:
        """Evaluate single rule"""
        value = context.get(rule.attribute)
        
        if rule.operator == "equals":
            return value == rule.value
        elif rule.operator == "contains":
            return rule.value in str(value)
        elif rule.operator == "in":
            return value in rule.value
        elif rule.operator == "greater_than":
            return value > rule.value
        elif rule.operator == "less_than":
            return value < rule.value
        
        return False

class FeatureFlagClient:
    """Client for feature flag checks"""
    
    def __init__(self, provider: FeatureFlagProvider, user_id: str = None):
        self.provider = provider
        self.user_id = user_id
        self.context: Dict[str, Any] = {}
    
    def is_enabled(self, flag_key: str) -> bool:
        """Check if feature is enabled"""
        return self.provider.is_enabled(flag_key, self.user_id, self.context)
    
    def set_context(self, context: Dict[str, Any]):
        """Set evaluation context"""
        self.context.update(context)
    
    def get_enabled_features(self, flag_keys: List[str]) -> Dict[str, bool]:
        """Get multiple feature flags at once"""
        return {key: self.is_enabled(key) for key in flag_keys}

# ===== PREDEFINED FEATURE FLAGS =====

FEATURE_FLAGS = {
    "collaborative_editing": FeatureFlag(
        key="collaborative_editing",
        name="Real-time Collaborative Editing",
        description="Enable real-time code editing with multiple users",
        flag_type=FeatureFlagType.PERCENTAGE,
        enabled=True,
        percentage=50,  # 50% rollout
    ),
    "advanced_debugging": FeatureFlag(
        key="advanced_debugging",
        name="Advanced Debugging Tools",
        description="Enable breakpoints, watches, profiling",
        flag_type=FeatureFlagType.PERCENTAGE,
        enabled=True,
        percentage=75,  # 75% rollout
    ),
    "ai_code_assistant": FeatureFlag(
        key="ai_code_assistant",
        name="AI Code Assistant",
        description="AI-powered code suggestions and completions",
        flag_type=FeatureFlagType.PERCENTAGE,
        enabled=False,
        percentage=10,  # 10% early access
    ),
    "performance_profiler": FeatureFlag(
        key="performance_profiler",
        name="Performance Profiler",
        description="Detailed performance analysis and optimization suggestions",
        flag_type=FeatureFlagType.USER_LIST,
        enabled=True,
        whitelist_users=["pro_user_1", "pro_user_2"],
    ),
    "custom_themes": FeatureFlag(
        key="custom_themes",
        name="Custom Color Themes",
        description="Create and save custom editor themes",
        flag_type=FeatureFlagType.BOOLEAN,
        enabled=True,
    ),
    "mobile_app": FeatureFlag(
        key="mobile_app",
        name="Native Mobile App",
        description="iOS and Android native applications",
        flag_type=FeatureFlagType.PERCENTAGE,
        enabled=False,
        percentage=5,  # 5% beta testing
    ),
}

# ===== PREDEFINED FUNNELS =====

FUNNELS = {
    "signup_funnel": {
        "name": "Signup Flow",
        "steps": [
            "visit_landing_page",
            "click_signup",
            "enter_email",
            "verify_email",
            "complete_profile",
            "first_project_create",
        ]
    },
    "code_execution_funnel": {
        "name": "Code Execution",
        "steps": [
            "open_editor",
            "write_code",
            "click_run",
            "code_executes",
            "view_output",
        ]
    },
    "collaboration_funnel": {
        "name": "Collaboration Flow",
        "steps": [
            "create_project",
            "click_share",
            "generate_link",
            "send_to_user",
            "user_accepts",
            "collaborative_edit",
        ]
    },
}

# ===== PREDEFINED COHORTS =====

COHORTS = {
    "power_users": {
        "name": "Power Users",
        "rule": "total_executions > 100 AND total_projects > 5",
        "estimated_size": 2500,
    },
    "daily_active_users": {
        "name": "Daily Active Users",
        "rule": "last_seen TODAY",
        "estimated_size": 15000,
    },
    "beta_testers": {
        "name": "Beta Testers",
        "rule": "beta_opt_in = true",
        "estimated_size": 500,
    },
    "enterprise_users": {
        "name": "Enterprise Users",
        "rule": "subscription_tier = 'enterprise'",
        "estimated_size": 50,
    },
}
