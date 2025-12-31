"""
UI Components for Phase VII-X Integration

Provides Qt-based UI components for:
- Marketplace browser
- Debugger interface
- AI suggestions panel
- Performance monitoring

Compatible with PySide6/PyQt5
"""

from typing import List, Dict, Optional, Callable, Any
from dataclasses import dataclass
import logging

try:
    from PySide6.QtWidgets import (
        QWidget, QVBoxLayout, QHBoxLayout, QTableWidget, QTableWidgetItem,
        QPushButton, QLineEdit, QLabel, QTabWidget, QDockWidget,
        QListWidget, QListWidgetItem, QProgressBar, QComboBox,
        QSpinBox, QCheckBox, QTextEdit, QDialog, QMessageBox,
        QSplitter, QTreeWidget, QTreeWidgetItem, QHeaderView,
        QStatusBar, QToolBar, QMenu, QMenuBar
    )
    from PySide6.QtCore import Qt, Signal, QTimer, QSize, QObject, QThread
    from PySide6.QtGui import QFont, QColor, QIcon, QPixmap, QStandardItem, QStandardItemModel
    from PySide6.QtCharts import QChart, QChartView, QLineSeries
except ImportError:
    # Fallback for PyQt5
    from PyQt5.QtWidgets import (
        QWidget, QVBoxLayout, QHBoxLayout, QTableWidget, QTableWidgetItem,
        QPushButton, QLineEdit, QLabel, QTabWidget, QDockWidget,
        QListWidget, QListWidgetItem, QProgressBar, QComboBox,
        QSpinBox, QCheckBox, QTextEdit, QDialog, QMessageBox,
        QSplitter, QTreeWidget, QTreeWidgetItem, QHeaderView,
        QStatusBar, QToolBar, QMenu, QMenuBar
    )
    from PyQt5.QtCore import Qt, pyqtSignal as Signal, QTimer, QSize, QObject, QThread
    from PyQt5.QtGui import QFont, QColor, QIcon, QPixmap, QStandardItem, QStandardItemModel

# ===== MARKETPLACE UI =====

class MarketplacePanel(QWidget):
    """Marketplace browser panel"""
    
    plugin_installed = Signal(str)  # plugin_id
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self.logger = logging.getLogger(__name__)
        self.marketplace_service = None
        self.init_ui()
    
    def init_ui(self):
        """Initialize UI"""
        layout = QVBoxLayout()
        
        # Search bar
        search_layout = QHBoxLayout()
        search_layout.addWidget(QLabel("Search:"))
        self.search_input = QLineEdit()
        self.search_input.setPlaceholderText("Enter plugin name...")
        search_layout.addWidget(self.search_input)
        
        self.search_button = QPushButton("Search")
        self.search_button.clicked.connect(self.on_search)
        search_layout.addWidget(self.search_button)
        layout.addLayout(search_layout)
        
        # Plugin list table
        self.plugin_table = QTableWidget()
        self.plugin_table.setColumnCount(5)
        self.plugin_table.setHorizontalHeaderLabels([
            "Name", "Version", "Rating", "Downloads", "Action"
        ])
        layout.addWidget(self.plugin_table)
        
        # Details panel
        details_layout = QVBoxLayout()
        details_layout.addWidget(QLabel("Details:"))
        self.details_text = QTextEdit()
        self.details_text.setReadOnly(True)
        self.details_text.setMaximumHeight(150)
        details_layout.addWidget(self.details_text)
        layout.addLayout(details_layout)
        
        self.setLayout(layout)
    
    def set_marketplace_service(self, service):
        """Set marketplace service"""
        self.marketplace_service = service
    
    def on_search(self):
        """Handle search"""
        query = self.search_input.text()
        if not query or not self.marketplace_service:
            return
        
        try:
            results = self.marketplace_service.search_plugins(query)
            self.display_results(results)
        except Exception as e:
            self.logger.error(f"Search failed: {e}")
            QMessageBox.warning(self, "Error", f"Search failed: {e}")
    
    def display_results(self, plugins: List[Dict]):
        """Display search results"""
        self.plugin_table.setRowCount(len(plugins))
        
        for row, plugin in enumerate(plugins):
            # Name
            self.plugin_table.setItem(row, 0, QTableWidgetItem(plugin.get("name", "")))
            
            # Version
            self.plugin_table.setItem(row, 1, QTableWidgetItem(plugin.get("version", "")))
            
            # Rating
            rating = plugin.get("rating", 0)
            self.plugin_table.setItem(row, 2, QTableWidgetItem(f"{rating:.1f}⭐"))
            
            # Downloads
            downloads = plugin.get("downloads", 0)
            self.plugin_table.setItem(row, 3, QTableWidgetItem(f"{downloads:,}"))
            
            # Install button
            install_btn = QPushButton("Install")
            plugin_id = plugin.get("id")
            install_btn.clicked.connect(lambda checked, pid=plugin_id: self.on_install(pid))
            self.plugin_table.setCellWidget(row, 4, install_btn)
    
    def on_install(self, plugin_id: str):
        """Handle plugin installation"""
        self.plugin_installed.emit(plugin_id)

# ===== DEBUGGER UI =====

class DebuggerPanel(QWidget):
    """Debugger control panel"""
    
    breakpoint_created = Signal(str, int, str)  # file, line, condition
    step_into = Signal()
    step_over = Signal()
    step_out = Signal()
    continue_execution = Signal()
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self.logger = logging.getLogger(__name__)
        self.debugger_service = None
        self.init_ui()
    
    def init_ui(self):
        """Initialize UI"""
        layout = QVBoxLayout()
        
        # Control buttons
        control_layout = QHBoxLayout()
        
        self.pause_button = QPushButton("Pause")
        self.pause_button.clicked.connect(self.on_pause)
        control_layout.addWidget(self.pause_button)
        
        self.resume_button = QPushButton("Resume")
        self.resume_button.clicked.connect(self.on_resume)
        control_layout.addWidget(self.resume_button)
        
        self.step_into_button = QPushButton("Step Into")
        self.step_into_button.clicked.connect(self.step_into.emit)
        control_layout.addWidget(self.step_into_button)
        
        self.step_over_button = QPushButton("Step Over")
        self.step_over_button.clicked.connect(self.step_over.emit)
        control_layout.addWidget(self.step_over_button)
        
        self.step_out_button = QPushButton("Step Out")
        self.step_out_button.clicked.connect(self.step_out.emit)
        control_layout.addWidget(self.step_out_button)
        
        layout.addLayout(control_layout)
        
        # Breakpoint management
        bp_layout = QVBoxLayout()
        bp_layout.addWidget(QLabel("Breakpoints:"))
        
        self.breakpoint_table = QTableWidget()
        self.breakpoint_table.setColumnCount(4)
        self.breakpoint_table.setHorizontalHeaderLabels([
            "File", "Line", "Condition", "Remove"
        ])
        self.breakpoint_table.setMaximumHeight(150)
        bp_layout.addWidget(self.breakpoint_table)
        layout.addLayout(bp_layout)
        
        # Watch expressions
        watch_layout = QVBoxLayout()
        watch_layout.addWidget(QLabel("Watch Expressions:"))
        
        watch_input_layout = QHBoxLayout()
        self.watch_input = QLineEdit()
        self.watch_input.setPlaceholderText("Enter expression...")
        watch_input_layout.addWidget(self.watch_input)
        
        self.add_watch_button = QPushButton("Add")
        self.add_watch_button.clicked.connect(self.on_add_watch)
        watch_input_layout.addWidget(self.add_watch_button)
        watch_layout.addLayout(watch_input_layout)
        
        self.watch_table = QTableWidget()
        self.watch_table.setColumnCount(3)
        self.watch_table.setHorizontalHeaderLabels([
            "Expression", "Value", "Type"
        ])
        self.watch_table.setMaximumHeight(150)
        watch_layout.addWidget(self.watch_table)
        layout.addLayout(watch_layout)
        
        # Call stack
        stack_layout = QVBoxLayout()
        stack_layout.addWidget(QLabel("Call Stack:"))
        
        self.call_stack_tree = QTreeWidget()
        self.call_stack_tree.setHeaderLabels(["Function", "File", "Line"])
        self.call_stack_tree.setMaximumHeight(150)
        stack_layout.addWidget(self.call_stack_tree)
        layout.addLayout(stack_layout)
        
        self.setLayout(layout)
    
    def on_pause(self):
        """Pause execution"""
        pass
    
    def on_resume(self):
        """Resume execution"""
        self.continue_execution.emit()
    
    def on_add_watch(self):
        """Add watch expression"""
        expr = self.watch_input.text()
        if expr:
            row = self.watch_table.rowCount()
            self.watch_table.insertRow(row)
            self.watch_table.setItem(row, 0, QTableWidgetItem(expr))
            self.watch_input.clear()

# ===== AI SUGGESTIONS PANEL =====

class AISuggestionsPanel(QWidget):
    """AI intelligence suggestions panel"""
    
    suggestion_accepted = Signal(str)  # suggestion_code
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self.logger = logging.getLogger(__name__)
        self.ai_service = None
        self.init_ui()
    
    def init_ui(self):
        """Initialize UI"""
        layout = QVBoxLayout()
        
        # Tabs for different features
        self.tabs = QTabWidget()
        
        # Completions tab
        self.completions_widget = self.create_completions_tab()
        self.tabs.addTab(self.completions_widget, "Code Completion")
        
        # Bug detection tab
        self.bugs_widget = self.create_bugs_tab()
        self.tabs.addTab(self.bugs_widget, "Bug Detection")
        
        # Code review tab
        self.review_widget = self.create_review_tab()
        self.tabs.addTab(self.review_widget, "Code Review")
        
        # Learning paths tab
        self.learning_widget = self.create_learning_tab()
        self.tabs.addTab(self.learning_widget, "Learning Path")
        
        # Optimization tab
        self.optimization_widget = self.create_optimization_tab()
        self.tabs.addTab(self.optimization_widget, "Optimization")
        
        layout.addWidget(self.tabs)
        self.setLayout(layout)
    
    def create_completions_tab(self) -> QWidget:
        """Create code completions tab"""
        widget = QWidget()
        layout = QVBoxLayout()
        
        layout.addWidget(QLabel("Code Suggestions:"))
        
        self.completions_list = QListWidget()
        layout.addWidget(self.completions_list)
        
        widget.setLayout(layout)
        return widget
    
    def create_bugs_tab(self) -> QWidget:
        """Create bug detection tab"""
        widget = QWidget()
        layout = QVBoxLayout()
        
        layout.addWidget(QLabel("Detected Bugs:"))
        
        self.bugs_table = QTableWidget()
        self.bugs_table.setColumnCount(4)
        self.bugs_table.setHorizontalHeaderLabels([
            "Line", "Severity", "Message", "Fix"
        ])
        layout.addWidget(self.bugs_table)
        
        widget.setLayout(layout)
        return widget
    
    def create_review_tab(self) -> QWidget:
        """Create code review tab"""
        widget = QWidget()
        layout = QVBoxLayout()
        
        layout.addWidget(QLabel("Review Insights:"))
        
        self.review_tree = QTreeWidget()
        self.review_tree.setHeaderLabels(["Category", "Observation", "Recommendation"])
        layout.addWidget(self.review_tree)
        
        widget.setLayout(layout)
        return widget
    
    def create_learning_tab(self) -> QWidget:
        """Create learning path tab"""
        widget = QWidget()
        layout = QVBoxLayout()
        
        layout.addWidget(QLabel("Learning Progress:"))
        
        # Progress bar
        progress_layout = QHBoxLayout()
        progress_layout.addWidget(QLabel("Mastery:"))
        self.mastery_progress = QProgressBar()
        self.mastery_progress.setMaximum(100)
        progress_layout.addWidget(self.mastery_progress)
        layout.addLayout(progress_layout)
        
        # Current lesson
        layout.addWidget(QLabel("Current Lesson:"))
        self.current_lesson_text = QTextEdit()
        self.current_lesson_text.setReadOnly(True)
        self.current_lesson_text.setMaximumHeight(100)
        layout.addWidget(self.current_lesson_text)
        
        # Next lessons
        layout.addWidget(QLabel("Recommended Next:"))
        self.next_lessons_list = QListWidget()
        self.next_lessons_list.setMaximumHeight(100)
        layout.addWidget(self.next_lessons_list)
        
        widget.setLayout(layout)
        return widget
    
    def create_optimization_tab(self) -> QWidget:
        """Create optimization hints tab"""
        widget = QWidget()
        layout = QVBoxLayout()
        
        layout.addWidget(QLabel("Optimization Opportunities:"))
        
        self.optimization_tree = QTreeWidget()
        self.optimization_tree.setHeaderLabels([
            "Category", "Current", "Optimized", "Improvement"
        ])
        layout.addWidget(self.optimization_tree)
        
        widget.setLayout(layout)
        return widget
    
    def set_ai_service(self, service):
        """Set AI service"""
        self.ai_service = service
    
    def update_completions(self, completions: List[Dict]):
        """Update code completions"""
        self.completions_list.clear()
        
        for completion in completions:
            item = QListWidgetItem(completion.get("code", ""))
            item.setToolTip(completion.get("description", ""))
            self.completions_list.addItem(item)
    
    def update_bugs(self, bugs: List[Dict]):
        """Update bug list"""
        self.bugs_table.setRowCount(len(bugs))
        
        for row, bug in enumerate(bugs):
            self.bugs_table.setItem(row, 0, QTableWidgetItem(str(bug.get("line", ""))))
            self.bugs_table.setItem(row, 1, QTableWidgetItem(bug.get("severity", "")))
            self.bugs_table.setItem(row, 2, QTableWidgetItem(bug.get("message", "")))
            self.bugs_table.setItem(row, 3, QTableWidgetItem(bug.get("suggestion", "")))

# ===== PERFORMANCE MONITORING PANEL =====

class PerformanceMonitorPanel(QWidget):
    """Real-time performance monitoring"""
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self.logger = logging.getLogger(__name__)
        self.init_ui()
    
    def init_ui(self):
        """Initialize UI"""
        layout = QVBoxLayout()
        
        # Metrics table
        layout.addWidget(QLabel("Performance Metrics:"))
        
        self.metrics_table = QTableWidget()
        self.metrics_table.setColumnCount(5)
        self.metrics_table.setHorizontalHeaderLabels([
            "Component", "Metric", "Value", "Unit", "Threshold"
        ])
        layout.addWidget(self.metrics_table)
        
        # Health status
        health_layout = QHBoxLayout()
        health_layout.addWidget(QLabel("Overall Status:"))
        self.status_label = QLabel("Healthy")
        self.status_label.setStyleSheet("color: green; font-weight: bold;")
        health_layout.addWidget(self.status_label)
        health_layout.addStretch()
        layout.addLayout(health_layout)
        
        self.setLayout(layout)
    
    def update_metrics(self, metrics: Dict[str, Dict[str, float]]):
        """Update displayed metrics"""
        rows = 0
        for component, component_metrics in metrics.items():
            for metric_name, value in component_metrics.items():
                rows += 1
        
        self.metrics_table.setRowCount(rows)
        
        row = 0
        for component, component_metrics in metrics.items():
            for metric_name, value in component_metrics.items():
                self.metrics_table.setItem(row, 0, QTableWidgetItem(component))
                self.metrics_table.setItem(row, 1, QTableWidgetItem(metric_name))
                self.metrics_table.setItem(row, 2, QTableWidgetItem(f"{value:.2f}"))
                self.metrics_table.setItem(row, 3, QTableWidgetItem("ms/count"))
                row += 1

if __name__ == "__main__":
    import sys
    from PySide6.QtWidgets import QApplication
    
    app = QApplication(sys.argv)
    
    # Test marketplace panel
    marketplace = MarketplacePanel()
    marketplace.show()
    
    sys.exit(app.exec())
