"""A small learner portfolio dashboard for the Time Warp Studio lesson dock."""

from dataclasses import dataclass


@dataclass
class Module:
    title: str
    language: str
    minutes: int
    points: int


@dataclass
class Learner:
    name: str
    completed: set[str]
    scores: dict[str, int]
    weekly_minutes: int


modules = [
    Module("Variables and values", "BASIC", 18, 80),
    Module("Branching stories", "PILOT", 22, 92),
    Module("Turtle geometry", "Logo", 28, 86),
    Module("Functions and arrays", "C", 35, 74),
    Module("Structured records", "Pascal", 26, 0),
    Module("Facts and rules", "Prolog", 30, 0),
    Module("Stack thinking", "Forth", 24, 0),
    Module("Tape machine", "Brainfuck", 20, 0),
    Module("Data models", "Python", 32, 0),
]

learner = Learner(
    name="Riley Chen",
    completed={"Variables and values", "Branching stories", "Turtle geometry", "Functions and arrays"},
    scores={
        "Variables and values": 80,
        "Branching stories": 92,
        "Turtle geometry": 86,
        "Functions and arrays": 74,
    },
    weekly_minutes=145,
)


def completion_percent(person: Learner, catalog: list[Module]) -> int:
    return int(100 * len(person.completed) / len(catalog))


def average_score(person: Learner) -> float:
    values = list(person.scores.values())
    return sum(values) / len(values) if values else 0.0


def next_modules(person: Learner, catalog: list[Module]) -> list[Module]:
    return [module for module in catalog if module.title not in person.completed][:3]


def print_progress(person: Learner, catalog: list[Module]) -> None:
    print("=" * 68)
    print("TIME WARP STUDIO | LEARNING PORTFOLIO")
    print("=" * 68)
    print(f"Learner: {person.name}")
    print(f"Weekly study time: {person.weekly_minutes} minutes")
    print(f"Program catalog: {len(catalog)} learning modules across 9 languages")
    print()

    done = len(person.completed)
    percent = completion_percent(person, catalog)
    bar = "#" * (percent // 5) + "." * (20 - percent // 5)
    print(f"PATH PROGRESS  [{bar}] {percent}% ({done}/{len(catalog)})")
    print(f"Checkpoint average: {average_score(person):.1f}%")
    print()

    print("COMPLETED MODULES")
    for module in catalog:
        if module.title in person.completed:
            print(f"  {module.language:10} {module.title:25} {person.scores[module.title]:>3}%")

    print("\nNEXT RECOMMENDATIONS")
    for position, module in enumerate(next_modules(person, catalog), start=1):
        print(f"  {position}. {module.language:10} {module.title:25} {module.minutes:>2} min")

    total_minutes = sum(module.minutes for module in next_modules(person, catalog))
    remaining = len(catalog) - done
    print("\nWEEKLY PLAN")
    print(f"  Remaining modules: {remaining}")
    print(f"  Time for next three: {total_minutes} minutes")
    print(f"  Available study time: {person.weekly_minutes} minutes")
    if person.weekly_minutes >= total_minutes:
        print("  Plan status: the next three modules fit this week's schedule.")
    else:
        print("  Plan status: spread the next three modules across two weeks.")

    language_counts: dict[str, int] = {}
    for module in catalog:
        language_counts[module.language] = language_counts.get(module.language, 0) + 1
    print("\nCATALOG COVERAGE")
    for language, count in language_counts.items():
        print(f"  {language:10} {count} module")
    print("\nPortfolio report complete. Progress is sample data for this demo.")


print_progress(learner, modules)
