"""
Data Structures Demo — Python
Demonstrates stacks, queues, linked lists, and binary trees.
"""

# ── Stack ─────────────────────────────────────────────────────────────
class Stack:
    def __init__(self):
        self._data = []

    def push(self, item):
        self._data.append(item)

    def pop(self):
        return self._data.pop()

    def peek(self):
        return self._data[-1]

    def is_empty(self):
        return len(self._data) == 0

    def __len__(self):
        return len(self._data)


# ── Queue ─────────────────────────────────────────────────────────────
class Queue:
    def __init__(self):
        self._data = []

    def enqueue(self, item):
        self._data.append(item)

    def dequeue(self):
        return self._data.pop(0)

    def is_empty(self):
        return len(self._data) == 0

    def __len__(self):
        return len(self._data)


# ── Binary Search Tree ────────────────────────────────────────────────
class BSTNode:
    def __init__(self, value):
        self.value = value
        self.left = None
        self.right = None


class BinarySearchTree:
    def __init__(self):
        self.root = None

    def insert(self, value):
        if self.root is None:
            self.root = BSTNode(value)
        else:
            self._insert(self.root, value)

    def _insert(self, node, value):
        if value < node.value:
            if node.left is None:
                node.left = BSTNode(value)
            else:
                self._insert(node.left, value)
        else:
            if node.right is None:
                node.right = BSTNode(value)
            else:
                self._insert(node.right, value)

    def inorder(self):
        result = []
        self._inorder(self.root, result)
        return result

    def _inorder(self, node, result):
        if node:
            self._inorder(node.left, result)
            result.append(node.value)
            self._inorder(node.right, result)


# ── Demo ──────────────────────────────────────────────────────────────
print("=== Stack Demo ===")
s = Stack()
for v in [10, 20, 30, 40]:
    s.push(v)
    print(f"  Pushed {v}, size={len(s)}")
while not s.is_empty():
    print(f"  Popped {s.pop()}")

print("\n=== Queue Demo ===")
q = Queue()
for v in ["Alice", "Bob", "Carol"]:
    q.enqueue(v)
    print(f"  Enqueued {v}")
while not q.is_empty():
    print(f"  Dequeued {q.dequeue()}")

print("\n=== Binary Search Tree Demo ===")
bst = BinarySearchTree()
values = [50, 30, 70, 20, 40, 60, 80]
for v in values:
    bst.insert(v)
    print(f"  Inserted {v}")
print(f"  Inorder traversal: {bst.inorder()}")
