// SPDX-License-Identifier: MIT
//
// The lesson plans, rendered from the curriculum as Markdown.
//
// `docs/lessons/plans/` is generated rather than written by hand, and
// `.github/workflows/docs.yml` regenerates it and refuses a diff -- so the
// renderer is gated by 60 committed lesson plans, not by a golden file somebody
// would have to invent.

#pragma once

#include <string>
#include <vector>

#include "timewarp/lessons/curriculum.hpp"

namespace timewarp::lessons {

/// One language's lessons, in teaching order, with the display name to head the
/// document with.
struct PlanGroup {
    std::string language;
    std::vector<const Lesson*> lessons;
};

/// How a checkpoint is checked, in one line: "prints: …", "draws: …", or
/// "reports an error containing: …". Shared with the terminal plan, so the
/// document and the console describe a checkpoint the same way.
[[nodiscard]] std::string describe_expectation(const Checkpoint& checkpoint);

/// The file a language's plan is written to: `chip8.md`, `basic.md`.
[[nodiscard]] std::string plan_filename(lang::Language language);

/// One language's plan, as a Markdown document.
[[nodiscard]] std::string render_plans(const std::string& language_name,
                                       const std::vector<const Lesson*>& lessons);

/// The index that sits beside them, one row per language, in curriculum order.
[[nodiscard]] std::string render_plans_index(const std::vector<PlanGroup>& groups);

/// Groups the curriculum by language, keeping the order each language first
/// appears in.
[[nodiscard]] std::vector<PlanGroup> group_by_language(const std::vector<Lesson>& lessons);

}  // namespace timewarp::lessons
