<!--
Sync Impact Report:
- Version change: none → 1.0.0
- Summary: Initial constitution establishing core principles for code quality, testing, user experience, and performance.
- Added Principles:
  - I. Code Quality: "Good Taste"
  - II. Pragmatic Testing
  - III. Never Break Userspace
  - IV. Efficiency by Design
- Removed Sections: Unused principle and section placeholders from the initial template.
- Templates requiring updates:
  - ⚠ pending: .specify/templates/plan-template.md
  - ⚠ pending: .specify/templates/spec-template.md
  - ⚠ pending: .specify/templates/tasks-template.md
- Follow-up TODOs:
  - TODO(RATIFICATION_DATE): Project started without a formal ratification date.
-->
# org-supertag Constitution

## Core Principles

### I. Code Quality: "Good Taste"
"Bad programmers worry about the code. Good programmers worry about data structures and their relationships."

- **Simplicity is paramount.** If you need more than 3 levels of indentation, your logic is too complex. Fix it.
- **Functions must be small and focused.** Each function should do one thing and do it well.
- **Data structures are the foundation.** Choose the right data structure, and the code will write itself. Avoid unnecessary abstractions that obscure the underlying data.
- **Eliminate special cases.** Good design makes special cases disappear and become normal cases.

### II. Pragmatic Testing
"Talking is cheap. Show me the code. And the tests that prove it works."

- **Tests must be practical.** Focus on tests that catch real-world bugs and prevent regressions. 100% coverage is a vanity metric; focus on testing critical paths.
- **Every bug fix requires a test.** Before you fix a bug, write a test that reproduces it. This ensures the bug is actually fixed and never comes back.
- **Integration over unit tests.** While unit tests have their place, integration tests that mimic how a user interacts with the system are more valuable.

### III. Never Break Userspace
"We don't break userspace. Seriously."

- **Backward compatibility is not optional.** Any change that breaks a user's existing configuration, workflow, or API is a bug. Period.
- **The user is always right.** We serve our users, not the other way around. We don't force them to change for our convenience.
- **Deprecation requires a clear migration path.** If an interface must be changed, provide a long deprecation cycle and a clear, simple path for users to migrate.

### IV. Efficiency by Design
"Show me your flowchart and conceal your tables, and I shall continue to be mystified. Show me your tables, and I won't usually need your flowchart; it'll be obvious."

- **Performance is a feature.** It is not something to be "fixed" later. It must be designed in from the beginning.
- **Choose efficient data structures and algorithms.** This is non-negotiable. An inefficient algorithm with layers of caching is still an inefficient algorithm.
- **Avoid unnecessary overhead.** Every layer of abstraction has a cost. Justify it or remove it.

## Governance

The principles in this Constitution are the supreme law of this project. They override any conflicting team practices, individual preferences, or unwritten rules.

- **Amendments**: Any change to this Constitution requires a formal proposal, a review by the core maintainers, and a clear justification. The impact of the change must be assessed, especially regarding the "Never Break Userspace" principle.
- **Compliance**: All code contributions will be reviewed for compliance with these principles. Non-compliant code will be rejected, with a clear explanation of which principle it violates.

**Version**: 1.0.0 | **Ratified**: TODO(RATIFICATION_DATE): Project started without a formal ratification date. | **Last Amended**: 2025-10-24