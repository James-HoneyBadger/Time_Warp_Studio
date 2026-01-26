"""
Operational Transform (OT) Engine for Collaborative Editing
Handles conflict-free concurrent edits across multiple clients
"""

import logging
import uuid
from dataclasses import dataclass
from datetime import datetime
from typing import Any, Dict, List, Tuple

logger = logging.getLogger(__name__)


@dataclass
class Operation:
    """Represents a single edit operation"""

    id: str
    user_id: str
    type: str  # 'insert' or 'delete'
    position: int
    content: str  # For insert: content to add, for delete: content removed
    timestamp: float
    version: int

    def to_dict(self) -> Dict[str, Any]:
        return {
            "id": self.id,
            "userId": self.user_id,
            "type": self.type,
            "position": self.position,
            "content": self.content,
            "timestamp": self.timestamp,
            "version": self.version,
        }


class OperationalTransform:
    """
    Implements OT algorithm for conflict-free collaborative editing

    Key principle: Transform concurrent operations so they have the same effect
    regardless of application order
    """

    def __init__(self, initial_content: str = ""):
        self.content = initial_content
        self.version = 0
        self.operation_history: List[Operation] = []
        self.pending_ops: Dict[str, List[Operation]] = {}  # By user_id

    def apply_operation(
        self, op: Operation, transform_against: List[Operation] = None
    ) -> Tuple[bool, str]:
        """
        Apply an operation, transforming against concurrent operations
        Returns: (success, message)
        """
        try:
            # If we have concurrent operations, transform this one
            if transform_against:
                for concurrent_op in transform_against:
                    op = self._transform_operations(op, concurrent_op)

            # Apply the operation
            if op.type == "insert":
                self.content = (
                    self.content[: op.position]
                    + op.content
                    + self.content[op.position :]
                )
            elif op.type == "delete":
                end_pos = op.position + len(op.content)
                self.content = self.content[: op.position] + self.content[end_pos:]

            self.version += 1
            op.version = self.version
            self.operation_history.append(op)

            logger.info("Applied operation %s from user {op.user_id}", op.id)
            return True, f"Operation applied successfully (v{self.version})"

        except Exception as e:
            logger.error("Error applying operation: %s", e)
            return False, f"Error applying operation: {str(e)}"

    def _transform_operations(self, op_a: Operation, op_b: Operation) -> Operation:
        """
        Transform operation A against operation B
        Used when operations are concurrent

        OT rules:
        - insert vs insert: shift position if B comes before A
        - insert vs delete: adjust position based on deletions
        - delete vs insert: shift position if B comes before A
        - delete vs delete: combine deletions
        """
        if op_a.user_id == op_b.user_id:
            # Operations from same user - use timestamp to order
            if op_a.timestamp > op_b.timestamp:
                return op_a

        # Insert vs Insert
        if op_a.type == "insert" and op_b.type == "insert":
            if op_b.position < op_a.position:
                op_a.position += len(op_b.content)
            elif op_b.position == op_a.position:
                # Both at same position - use user_id for tie-breaking
                if op_b.user_id < op_a.user_id:
                    op_a.position += len(op_b.content)

        # Insert vs Delete
        elif op_a.type == "insert" and op_b.type == "delete":
            if op_b.position < op_a.position:
                # Deletion before insertion
                op_a.position -= min(len(op_b.content), op_a.position - op_b.position)
            elif op_b.position >= op_a.position and op_b.position < op_a.position + len(
                op_a.content
            ):
                # Deletion overlaps with insertion
                pass  # Position stays same

        # Delete vs Insert
        elif op_a.type == "delete" and op_b.type == "insert":
            if op_b.position <= op_a.position:
                # Insertion before deletion
                op_a.position += len(op_b.content)
            elif op_b.position < op_a.position + len(op_a.content):
                # Insertion within deletion range
                op_a.content = (
                    op_a.content[: op_b.position - op_a.position]
                    + op_a.content[op_b.position - op_a.position + len(op_b.content) :]
                )

        # Delete vs Delete
        elif op_a.type == "delete" and op_b.type == "delete":
            if op_b.position < op_a.position:
                op_a.position -= min(len(op_b.content), op_a.position - op_b.position)

        return op_a

    def create_operation(
        self,
        user_id: str,
        op_type: str,
        position: int,
        content: str,
    ) -> Operation:
        """Create a new operation"""
        return Operation(
            id=str(uuid.uuid4()),
            user_id=user_id,
            type=op_type,
            position=position,
            content=content,
            timestamp=datetime.utcnow().timestamp(),
            version=self.version,
        )

    def get_operations_since(self, version: int) -> List[Operation]:
        """Get all operations since a specific version"""
        return [op for op in self.operation_history if op.version > version]

    def get_state(self) -> Dict[str, Any]:
        """Get current document state"""
        return {
            "content": self.content,
            "version": self.version,
            "operationCount": len(self.operation_history),
        }

    def revert_to_version(self, version: int) -> bool:
        """Revert to a previous version (for conflict resolution)"""
        try:
            # Rebuild from history
            self.content = ""
            self.version = 0
            old_history = self.operation_history[:]
            self.operation_history = []

            for op in old_history:
                if op.version <= version:
                    self.apply_operation(op)
                else:
                    break

            logger.info("Reverted to version %s", version)
            return True
        except Exception as e:
            logger.error("Error reverting: %s", e)
            self.operation_history = old_history  # Restore history
            return False

    def detect_conflict(self, op_a: Operation, op_b: Operation) -> Tuple[bool, str]:
        """
        Detect potential conflicts between operations
        Returns: (is_conflict, description)
        """
        # Operations at exact same position by different users
        if (
            op_a.user_id != op_b.user_id
            and op_a.position == op_b.position
            and abs(op_a.timestamp - op_b.timestamp) < 0.1
        ):
            return True, "Concurrent edits at same position"

        # Delete operations that overlap
        if (
            op_a.type == "delete"
            and op_b.type == "delete"
            and op_a.user_id != op_b.user_id
        ):
            overlap_start = max(op_a.position, op_b.position)
            overlap_end = min(
                op_a.position + len(op_a.content),
                op_b.position + len(op_b.content),
            )
            if overlap_end > overlap_start:
                return True, "Overlapping deletes"

        return False, ""

    def merge_operations(self, operations: List[Operation]) -> Operation:
        """
        Merge multiple operations from same user into single operation
        Used for optimization
        """
        if not operations:
            raise ValueError("No operations to merge")

        if len(operations) == 1:
            return operations[0]

        # Sort by timestamp
        sorted_ops = sorted(operations, key=lambda op: op.timestamp)

        # Combine all inserts
        if all(op.type == "insert" for op in sorted_ops):
            content = "".join(op.content for op in sorted_ops)
            return Operation(
                id=str(uuid.uuid4()),
                user_id=sorted_ops[0].user_id,
                type="insert",
                position=sorted_ops[0].position,
                content=content,
                timestamp=sorted_ops[-1].timestamp,
                version=sorted_ops[-1].version,
            )

        # For mixed operations, can't merge meaningfully
        return sorted_ops[-1]
