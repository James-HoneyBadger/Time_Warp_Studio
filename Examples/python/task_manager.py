#!/usr/bin/env python3
"""
╔══════════════════════════════════════════════════════╗
║         TASK MANAGER - Personal Productivity Tool    ║
║  A full-featured command-line task manager with:     ║
║  - Projects, tasks, priorities, due dates            ║
║  - Tags, notes, completion tracking                  ║
║  - Reports, search, filtering                        ║
╚══════════════════════════════════════════════════════╝
"""
from __future__ import annotations
import datetime
import json

# ─── DATA MODEL ─────────────────────────────────────────

class Task:
    """A single task with rich metadata."""

    PRIORITIES = {"high": 3, "medium": 2, "low": 1}
    _id_counter = 0

    def __init__(self, title: str, project: str = "Inbox",
                 priority: str = "medium", due: str | None = None,
                 tags: list[str] | None = None, notes: str = ""):
        Task._id_counter += 1
        self.id = Task._id_counter
        self.title = title
        self.project = project
        self.priority = priority
        self.due = due             # ISO date string "YYYY-MM-DD"
        self.tags = tags or []
        self.notes = notes
        self.done = False
        self.created = datetime.date.today().isoformat()
        self.completed_date: str | None = None

    def complete(self):
        self.done = True
        self.completed_date = datetime.date.today().isoformat()

    def days_until_due(self) -> int | None:
        if not self.due:
            return None
        today = datetime.date.today()
        due = datetime.date.fromisoformat(self.due)
        return (due - today).days

    def priority_int(self) -> int:
        return self.PRIORITIES.get(self.priority, 1)

    def is_overdue(self) -> bool:
        d = self.days_until_due()
        return d is not None and d < 0 and not self.done

    def __repr__(self) -> str:
        status = "✓" if self.done else "○"
        overdue = " ⚠OVERDUE" if self.is_overdue() else ""
        due_str = f" [due {self.due}]" if self.due else ""
        tags_str = f" #{' #'.join(self.tags)}" if self.tags else ""
        return (f"[{self.id:3d}] {status} [{self.priority.upper():6s}] "
                f"{self.title}{due_str}{overdue}{tags_str}"
                f"  ({self.project})")


# ─── TASK MANAGER ────────────────────────────────────────

class TaskManager:
    """Core task management engine."""

    def __init__(self):
        self.tasks: list[Task] = []

    # ── CRUD ──────────────────────────────────────────────

    def add(self, title: str, **kwargs) -> Task:
        t = Task(title, **kwargs)
        self.tasks.append(t)
        return t

    def get(self, task_id: int) -> Task | None:
        for t in self.tasks:
            if t.id == task_id:
                return t
        return None

    def complete(self, task_id: int) -> bool:
        t = self.get(task_id)
        if t:
            t.complete()
            return True
        return False

    def delete(self, task_id: int) -> bool:
        before = len(self.tasks)
        self.tasks = [t for t in self.tasks if t.id != task_id]
        return len(self.tasks) < before

    # ── FILTERING ─────────────────────────────────────────

    def by_project(self, project: str) -> list[Task]:
        return [t for t in self.tasks if t.project == project]

    def by_priority(self, priority: str) -> list[Task]:
        return [t for t in self.tasks if t.priority == priority]

    def by_tag(self, tag: str) -> list[Task]:
        return [t for t in self.tasks if tag in t.tags]

    def pending(self) -> list[Task]:
        return [t for t in self.tasks if not t.done]

    def completed(self) -> list[Task]:
        return [t for t in self.tasks if t.done]

    def overdue(self) -> list[Task]:
        return [t for t in self.tasks if t.is_overdue()]

    def due_soon(self, days: int = 3) -> list[Task]:
        result = []
        for t in self.tasks:
            d = t.days_until_due()
            if d is not None and 0 <= d <= days and not t.done:
                result.append(t)
        return result

    def search(self, query: str) -> list[Task]:
        q = query.lower()
        return [t for t in self.tasks
                if q in t.title.lower() or q in t.notes.lower()
                or any(q in tag for tag in t.tags)]

    # ── SORTING ───────────────────────────────────────────

    def sorted_by_priority(self, tasks: list[Task] | None = None) -> list[Task]:
        src = tasks if tasks is not None else self.tasks
        return sorted(src, key=lambda t: -t.priority_int())

    def sorted_by_due(self, tasks: list[Task] | None = None) -> list[Task]:
        src = tasks if tasks is not None else self.tasks
        return sorted(src, key=lambda t: t.due or "9999-99-99")

    # ── REPORTING ────────────────────────────────────────

    def projects(self) -> list[str]:
        return sorted(set(t.project for t in self.tasks))

    def summary(self) -> dict:
        total = len(self.tasks)
        done = len(self.completed())
        pending = len(self.pending())
        overdue_c = len(self.overdue())
        by_proj: dict[str, int] = {}
        for t in self.pending():
            by_proj[t.project] = by_proj.get(t.project, 0) + 1
        return {
            "total": total,
            "done": done,
            "pending": pending,
            "overdue": overdue_c,
            "completion_rate": f"{done/max(1,total)*100:.1f}%",
            "by_project": by_proj,
        }

    def print_list(self, tasks: list[Task] | None = None, title: str = "TASKS"):
        src = tasks if tasks is not None else self.tasks
        print(f"\n{'═'*60}")
        print(f"  {title} ({len(src)} items)")
        print(f"{'─'*60}")
        if not src:
            print("  (none)")
        for t in self.sorted_by_priority(src):
            print(" ", t)
        print(f"{'═'*60}")

    def print_summary(self):
        s = self.summary()
        print("\n╔══════════════════════════════════════╗")
        print("║          TASK SUMMARY REPORT          ║")
        print("╚══════════════════════════════════════╝")
        print(f"  Total tasks:      {s['total']}")
        print(f"  Completed:        {s['done']}")
        print(f"  Pending:          {s['pending']}")
        print(f"  Overdue:          {s['overdue']}")
        print(f"  Completion rate:  {s['completion_rate']}")
        print()
        print("  BY PROJECT:")
        for proj, count in sorted(s["by_project"].items()):
            bar = "█" * min(count * 3, 30)
            print(f"  {proj:20s} {count:3d}  {bar}")

    def export_json(self) -> str:
        data = []
        for t in self.tasks:
            data.append({
                "id": t.id, "title": t.title, "project": t.project,
                "priority": t.priority, "due": t.due, "tags": t.tags,
                "done": t.done, "created": t.created
            })
        return json.dumps(data, indent=2)


# ─── DEMO ────────────────────────────────────────────────

def main():
    print("╔══════════════════════════════════════════════════╗")
    print("║           PYTHON TASK MANAGER DEMO               ║")
    print("╚══════════════════════════════════════════════════╝")

    mgr = TaskManager()

    # Load demo data
    today = datetime.date.today()
    yesterday = (today - datetime.timedelta(days=1)).isoformat()
    tomorrow = (today + datetime.timedelta(days=1)).isoformat()
    next_week = (today + datetime.timedelta(days=7)).isoformat()

    mgr.add("Fix critical production bug", project="DevOps",
            priority="high", due=yesterday, tags=["urgent", "bug"])
    mgr.add("Write Q4 report", project="Business",
            priority="high", due=tomorrow, tags=["report"])
    mgr.add("Review pull requests", project="DevOps",
            priority="medium", due=tomorrow, tags=["code"])
    mgr.add("Update project documentation", project="DevOps",
            priority="medium", due=next_week, tags=["docs"])
    mgr.add("Schedule team meeting", project="Management",
            priority="medium", tags=["meetings"])
    mgr.add("Order new office supplies", project="Admin",
            priority="low", tags=["admin"])
    mgr.add("Learn Rust programming", project="Personal",
            priority="low", due=next_week, tags=["learning", "coding"])
    mgr.add("Renew SSL certificates", project="DevOps",
            priority="high", due=next_week, tags=["security"])
    mgr.add("Refactor authentication module", project="DevOps",
            priority="medium", tags=["code", "refactor"])
    mgr.add("Monthly backup verification", project="DevOps",
            priority="medium", tags=["maintenance"])

    # Complete some tasks
    mgr.complete(5)
    mgr.complete(6)

    # Display all tasks
    mgr.print_list(title="ALL TASKS")

    # Show overdue
    print("\n⚠️  OVERDUE TASKS:")
    mgr.print_list(mgr.overdue(), "OVERDUE")

    # Show due soon
    print("\n⏰ DUE WITHIN 3 DAYS:")
    mgr.print_list(mgr.due_soon(3), "DUE SOON")

    # By project
    print("\n📁 DEVOPS PROJECT:")
    mgr.print_list(mgr.by_project("DevOps"), "DevOps")

    # Search
    print("\n🔍 SEARCH for 'code':")
    mgr.print_list(mgr.by_tag("code"), "tag:code")

    # Summary
    mgr.print_summary()

    print()
    print("Export to JSON:")
    print(mgr.export_json())


if __name__ == "__main__":
    main()
