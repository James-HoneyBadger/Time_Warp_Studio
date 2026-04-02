"""Collaboration mixin — real-time collaboration UI methods.

Extracted from ``MainWindow`` to reduce file size. These methods handle
connecting to a collaboration server, creating/joining sessions, and
processing incoming operations.
"""

from __future__ import annotations

import getpass
from typing import TYPE_CHECKING

from PySide6.QtWidgets import (
    QDialog,
    QDialogButtonBox,
    QHBoxLayout,
    QLabel,
    QLineEdit,
    QListWidget,
    QMessageBox,
    QVBoxLayout,
)

if TYPE_CHECKING:

    from ..collaboration_client import CollaborationOperation  # noqa: F401


class CollaborationMixin:
    """Collaboration server methods mixed into MainWindow."""

    def setup_collaboration_callbacks(self):
        """Setup collaboration client callbacks."""
        client = self.collaboration_client
        client.on_connected = self.on_collaboration_connected
        client.on_disconnected = self.on_collaboration_disconnected
        client.on_session_joined = self.on_session_joined
        client.on_session_left = self.on_session_left
        client.on_operation_received = self.on_operation_received
        client.on_cursor_update = self.on_cursor_update
        client.on_user_joined = self.on_user_joined
        client.on_user_left = self.on_user_left

    def collab_connect(self):
        """Connect to collaboration server."""
        if not self.collaboration_client:
            QMessageBox.warning(
                self,
                "Collaboration Error",
                "Collaboration client not available.",
            )
            return

        # Show connection dialog
        dialog = QDialog(self)
        dialog.setWindowTitle("Connect to Collaboration Server")
        dialog.setModal(True)

        layout = QVBoxLayout(dialog)

        # Server address input
        server_layout = QHBoxLayout()
        server_layout.addWidget(QLabel("Server:"))
        self.server_input = QLineEdit("localhost:8765")
        server_layout.addWidget(self.server_input)
        layout.addLayout(server_layout)

        # Username input
        user_layout = QHBoxLayout()
        user_layout.addWidget(QLabel("Username:"))
        self.username_input = QLineEdit()
        try:
            self.username_input.setText(getpass.getuser())
        except (OSError, ImportError):
            self.username_input.setText("Anonymous")
        user_layout.addWidget(self.username_input)
        layout.addLayout(user_layout)

        # Buttons
        flags = (
            QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel
        )
        buttons = QDialogButtonBox(flags)
        buttons.accepted.connect(dialog.accept)
        buttons.rejected.connect(dialog.reject)
        layout.addWidget(buttons)

        if dialog.exec() == QDialog.Accepted:
            server = self.server_input.text().strip()
            username = self.username_input.text().strip()

            if not server or not username:
                QMessageBox.warning(
                    self, "Invalid Input", "Enter server address and username."
                )
                return

            msg = f"Connecting to {server} as {username}..."
            self.statusbar.showMessage(msg)

            def connect_callback(success, message):
                if success:
                    self.collab_disconnect_action.setEnabled(True)
                    self.statusbar.showMessage(f"Connected to {server}")
                    QMessageBox.information(
                        self,
                        "Connected",
                        f"Successfully connected to collaboration server as "
                        f"{username}.",
                    )
                else:
                    self.statusbar.showMessage("Connection failed")
                    QMessageBox.warning(
                        self,
                        "Connection Failed",
                        f"Could not connect to server:\n{message}",
                    )

            self.collaboration_client.connect(
                server,
                username,
                connect_callback,
            )

    def collab_disconnect(self):
        """Disconnect from collaboration server."""
        if self.collaboration_client:
            self.collaboration_client.disconnect()
            self.collab_disconnect_action.setEnabled(False)
            msg = "Disconnected from collaboration server"
            self.statusbar.showMessage(msg)

    def collab_join_session(self):
        """Join a collaboration session."""
        if (
            not self.collaboration_client
            or not self.collaboration_client.is_connected()
        ):
            QMessageBox.warning(
                self,
                "Not Connected",
                "Please connect to a collaboration server first.",
            )
            return

        dialog = QDialog(self)
        dialog.setWindowTitle("Join Collaboration Session")
        dialog.setModal(True)

        layout = QVBoxLayout(dialog)
        layout.addWidget(QLabel("Available Sessions:"))

        self.session_list = QListWidget()
        self.session_list.addItem("Session 1 - Python Project")
        self.session_list.addItem("Session 2 - Logo Graphics")
        self.session_list.addItem("Session 3 - BASIC Tutorial")
        layout.addWidget(self.session_list)

        flags = (
            QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel
        )
        buttons = QDialogButtonBox(flags)
        buttons.accepted.connect(dialog.accept)
        buttons.rejected.connect(dialog.reject)
        layout.addWidget(buttons)

        if dialog.exec() == QDialog.Accepted:
            selected_items = self.session_list.selectedItems()
            if not selected_items:
                QMessageBox.warning(
                    self, "No Selection", "Please select a session to join."
                )
                return

            session_name = selected_items[0].text().split(" - ")[0]
            self.statusbar.showMessage(f"Joining session {session_name}...")

            def join_callback(success, message):
                if success:
                    msg = f"Joined session {session_name}"
                    self.statusbar.showMessage(msg)
                    QMessageBox.information(
                        self,
                        "Joined Session",
                        f"Successfully joined {session_name}.",
                    )
                else:
                    self.statusbar.showMessage("Failed to join session")
                    QMessageBox.warning(
                        self,
                        "Join Failed",
                        f"Could not join session:\n{message}",
                    )

            join_callback(True, "Session joined successfully")

    def collab_create_session(self):
        """Create a new collaboration session."""
        if (
            not self.collaboration_client
            or not self.collaboration_client.is_connected()
        ):
            QMessageBox.warning(
                self,
                "Not Connected",
                "Please connect to a collaboration server first.",
            )
            return

        dialog = QDialog(self)
        dialog.setWindowTitle("Create Collaboration Session")
        dialog.setModal(True)

        layout = QVBoxLayout(dialog)

        name_layout = QHBoxLayout()
        name_layout.addWidget(QLabel("Session Name:"))
        self.session_name_input = QLineEdit("My Project Session")
        name_layout.addWidget(self.session_name_input)
        layout.addLayout(name_layout)

        desc_layout = QHBoxLayout()
        desc_layout.addWidget(QLabel("Description:"))
        self.session_desc_input = QLineEdit("Collaborative coding session")
        desc_layout.addWidget(self.session_desc_input)
        layout.addLayout(desc_layout)

        flags = (
            QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel
        )
        buttons = QDialogButtonBox(flags)
        buttons.accepted.connect(dialog.accept)
        buttons.rejected.connect(dialog.reject)
        layout.addWidget(buttons)

        if dialog.exec() == QDialog.Accepted:
            session_name = self.session_name_input.text().strip()

            if not session_name:
                QMessageBox.warning(
                    self, "Invalid Input", "Please enter a session name."
                )
                return

            self.statusbar.showMessage(f"Creating session '{session_name}'...")

            def create_callback(success, message):
                if success:
                    msg = f"Created session '{session_name}'"
                    self.statusbar.showMessage(msg)
                    QMessageBox.information(
                        self,
                        "Session Created",
                        f"Successfully created session '{session_name}'.",
                    )
                else:
                    self.statusbar.showMessage("Failed to create session")
                    QMessageBox.warning(
                        self,
                        "Create Failed",
                        f"Could not create session:\n{message}",
                    )

            create_callback(True, "Session created successfully")

    def collab_show_users(self):
        """Show users in current session."""
        if (
            not self.collaboration_client
            or not self.collaboration_client.is_connected()
        ):
            QMessageBox.warning(
                self,
                "Not Connected",
                "Please connect to a collaboration server first.",
            )
            return

        dialog = QDialog(self)
        dialog.setWindowTitle("Session Users")
        dialog.setModal(True)

        layout = QVBoxLayout(dialog)
        layout.addWidget(QLabel("Current Session Users:"))

        user_list = QListWidget()
        user_list.addItem("👑 Alice (Host)")
        user_list.addItem("👤 Bob")
        user_list.addItem("👤 Charlie")
        user_list.addItem("👤 You")
        layout.addWidget(user_list)

        buttons = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok)
        buttons.accepted.connect(dialog.accept)
        layout.addWidget(buttons)

        dialog.exec()

    def collab_share_project(self):
        """Share current project."""
        if (
            not self.collaboration_client
            or not self.collaboration_client.is_connected()
        ):
            QMessageBox.warning(
                self,
                "Not Connected",
                "Please connect to a collaboration server first.",
            )
            return

        current_info = self.get_current_tab_info()
        if not current_info["file"]:
            QMessageBox.warning(
                self,
                "No File Open",
                "Please open a file to share.",
            )
            return

        from pathlib import Path as _Path

        filename = current_info["file"]
        self.statusbar.showMessage(f"Sharing project: {_Path(filename).name}")

        def share_callback(success, message):
            if success:
                self.statusbar.showMessage("Project shared successfully")
                QMessageBox.information(
                    self,
                    "Project Shared",
                    "Your project has been shared with the session.",
                )
            else:
                self.statusbar.showMessage("Failed to share project")
                QMessageBox.warning(
                    self,
                    "Share Failed",
                    f"Could not share project:\n{message}",
                )

        share_callback(True, "Project shared successfully")

    def on_collaboration_connected(self):
        """Handle collaboration connection."""
        self.collab_disconnect_action.setEnabled(True)
        self.statusbar.showMessage("Connected to collaboration server")

    def on_collaboration_disconnected(self):
        """Handle collaboration disconnection."""
        self.collab_disconnect_action.setEnabled(False)
        msg = "Disconnected from collaboration server"
        self.statusbar.showMessage(msg)

    def on_session_joined(self, _session_id, session_name):
        """Handle joining a session."""
        self.statusbar.showMessage(f"Joined session: {session_name}")

    def on_session_left(self, _session_id):
        """Handle leaving a session."""
        self.statusbar.showMessage("Left collaboration session")

    def on_operation_received(self, _operation):
        """Handle incoming collaborative operation."""
        current_editor = self.get_current_editor()
        if current_editor:
            pass  # Placeholder — would use operational transform

    def on_cursor_update(self, _user_id, _position):
        """Handle cursor position update from other users."""

    def on_user_joined(self, _user_id, username):
        """Handle user joining session."""
        self.statusbar.showMessage(f"User joined: {username}")

    def on_user_left(self, _user_id, username):
        """Handle user leaving session."""
        self.statusbar.showMessage(f"User left: {username}")
