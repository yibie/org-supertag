## `org-supertag` Architecture Comparison: New vs Old

### Code Size Comparison

| Version | Lines of Code | Description |
| :--- | :--- | :--- |
| **Old** | ~29,973 | Mixed Emacs Lisp and Python |
| **New** | ~16,691 | Pure Emacs Lisp Implementation |

The new version has approximately **55%** of the old version's code size, retaining and enhancing core functionality while completely eliminating Python dependencies.

### 1. Core Philosophy: From Decentralized to Unified, From Imperative to Data-Driven

While the old architecture was powerful, its design leaned more toward traditional, decentralized imperative models. Each module (such as `node`, `tag`, `db`) maintained its own state and operations, with modules directly calling each other, forming a complex dependency network. Additionally, it relied on an external Python process (`simtag/`) for AI-related functionality, introducing complexity in cross-language communication.

The new architecture represents a complete philosophical evolution, centered on **Data-Centric** and **One-Way Data Flow** principles.

- **Single Source of Truth**: All system states are converged into a global, predictable `supertag--store` hash table, eliminating the root of data inconsistency.
- **Strict Control Flow**: Any data modification must go through the `supertag-transform` function, acting as the sole gateway for data entry into the database. This ensures all modifications are atomic, traceable, and trigger consistent event notifications.
- **Pure Emacs Lisp Implementation**: The new architecture completely removes the Python backend, becoming a purely Emacs Lisp package. This not only simplifies deployment and maintenance but also significantly boosts performance by eliminating EPC communication overhead.

### 2. Architecture Comparison: Key Design Evolution

| Feature | Old Architecture | New Architecture |
| :--- | :--- | :--- |
| **Core Philosophy** | Mixed imperative and object-oriented style. | **Data-Centric & Functional**: Treats data as first-class citizens and operations as transformations of data. |
| **Data Storage** | Two separate hash tables (`--object`, `--link`) storing entities and relationships. | **Single Source of Truth**: A unified, nested hash table (`supertag--store`) storing all application states. |
| **State Management** | Decentralized. States could be directly modified by different modules. | **Centralized & Immutable Style**: All state changes go through the unique `supertag-transform` function, ensuring atomicity and predictability. |
| **Control Flow** | Direct function calls between modules with complex dependencies. | **One-Way Data Flow**: Strictly follows `Action -> Ops -> Transform -> Store -> Notify` process with clear component decoupling. |
| **Modularization** | Function-based division with mixed responsibilities (data, logic, UI) within modules. | **Role-Based Layering**: Clear `core` (data pipeline), `ops` (user intent), `services` (business logic), `ui` (interaction) layers. |
| **External Dependencies** | **Heavy Dependency**: Requires complete Python environment and EPC bridging for communication. | **Lightweight & Native**: Pure Emacs Lisp implementation. AI features integrated through standard Emacs packages like `gptel`. |
| **AI/RAG Implementation** | Implemented in external Python process (`simtag/`) with complex communication. | Natively implemented in Emacs Lisp (`supertag-rag.el`), simplifying the tech stack and boosting performance. |

### Feature Changes

**New Features:**
- `supertag-capture`: Enhanced information capture functionality
- `supertag-automation`: Upgraded behavior automation system (formerly `org-supertag-behavior`)

**Features in Migration:**
- `supertag-completion`: Auto-completion for tags

**Removed Features:**
- Discovery view (`org-supertag-view-discovery`)
- Python backend (`simtag`) and its AI and RAG support

**Improved Features:**
- Tag system: Added tag `extends` method