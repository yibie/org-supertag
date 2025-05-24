# Stevens Assistant Implementation Plan

## 1. Introduction

### Goal
To build a personal AI assistant within the Emacs environment, inspired by Geoffrey Litt's "Stevens" project. This assistant will leverage the `org-supertag` ecosystem to manage information, automate tasks, and provide contextual insights.

### Core Technologies
-   **Memory:** Org Mode files (`stevens-memory.org`) acting as a structured, linkable knowledge base.
-   **Structure & Querying:** `org-supertag` core for tagging, properties, IDs, and finding relevant information within the Org memory.
-   **Automation & External I/O:** `org-supertag-behavior` for defining actions, handling triggers (including scheduled tasks), and interacting with external services (APIs, messaging, email) via Elisp functions or scripts. `org-supertag-behavior-library` will house reusable action functions.
-   **Intelligence & Orchestration:** `org-supertag-ai` for defining and executing multi-step workflows involving LLM interactions, data processing, and conditional logic, capable of calling behaviors.

### Analogy Mapping
-   **Stevens SQLite Table:** Org files with headlines, properties, tags, and IDs.
-   **Stevens Cron Jobs:** `org-supertag-behavior` `:schedule` triggers, potentially augmented by Emacs timers or external cron.
-   **Stevens Importers:** `org-supertag-behavior` actions calling custom Elisp functions or scripts to interact with external APIs (Calendar, Weather) or applications (Email, Telegram).
-   **Stevens LLM Calls:** `org-supertag-ai` `llm` nodes, or direct calls via `org-supertag-behavior--call-ai`.
-   **Stevens Telegram Interface:** Integration with `telega.el` via message handlers triggering behaviors or AI workflows.

## 2. Phase 1: Core Memory Structure

-   **Action:** Design and create the main Org file (`stevens-memory.org`).
-   **Action:** Define standard headline structures for different memory types (Events, Reminders, Notes, Weather Cache, Logs, etc.).
-   **Action:** Establish a consistent set of tags (e.g., `:event:`, `:reminder:`, `:note:`, `:weather:`, `:telegram:`, `:email:`, `:urgent:`, `:processed:`).
-   **Action:** Define standard properties (e.g., `:SOURCE:`, `:TIMESTAMP:`, `:LOCATION:`, `:SENDER:`, `:CREATED:`).
-   **Action:** Ensure all significant entries receive a unique `:ID:`.
-   **Action:** Create basic `org-capture` templates for manually adding reminders and notes.
-   **Expected Outcome:** A well-structured Org file ready to store diverse information.

## 3. Phase 2: Data Ingestion Behaviors

*(Focus on implementing Behaviors triggered by schedules or external events)*

-   **Weather:**
    -   **Action:** Define `@GetWeather` behavior.
    -   **Action:** Implement Elisp function (`my-fetch-weather-data`) in library to call weather API (or use `--execute-script`) and write data to designated `:weather:` node/block.
    -   **Action:** Register behavior with `:schedule` trigger (e.g., hourly during mornings).
-   **Calendar:**
    -   **Action:** Define `@GetCalendar` behavior.
    -   **Action:** Implement Elisp function (`my-fetch-calendar-data`) using API/script to get upcoming events and create/update `:event:calendar:` nodes.
    -   **Action:** Register behavior with `:schedule` trigger (e.g., every few hours).
-   **Telegram Integration (Receiving):**
    -   **Action:** Configure `telega.el` handler.
    -   **Action:** Implement handler function (`my-record-telega-message-action`) to parse incoming messages.
    -   **Action:** Store relevant messages as `:telegram:` nodes (reminders, notes).
    -   **Action:** Add logic to detect commands/questions for Stevens for later processing (Phase 4).
-   **(Optional) Email Integration:**
    -   **Action:** Configure email client hooks (`mu4e`/`notmuch`).
    *   **Action:** Implement email processing function, potentially using `--execute-script` for OCR if needed. Store info in `:email:` nodes.
-   **Expected Outcome:** Automated population of the Org memory with data from external sources.

## 4. Phase 3: Daily Brief Generation & Delivery

*(Focus on scheduled task execution and integrating AI workflow)*

-   **Action:** Define `@DailyBrief` behavior with a `:schedule` trigger (e.g., 7 AM).
-   **Action:** Implement the Elisp trigger function (`my-trigger-daily-brief-workflow`) called by the behavior, responsible for initiating the AI workflow.
-   **Action:** Design the "Daily Brief" AI Workflow in an Org file (start node ID: `BRIEF_START`).
    -   Include a `function` node (`my-gather-brief-context`) to query Org memory for relevant info (today's calendar, reminders, weather).
    -   Include an `llm` node to compose the brief using the gathered context.
    -   Include a `function` node (`my-send-brief-via-telegram`) to send the result.
-   **Action:** Implement the Elisp functions needed by the AI workflow nodes (`my-gather-brief-context`, `my-send-brief-via-telegram`).
-   **Action:** Add `org-supertag-behavior--send-telegram-message` function to the behavior library.
-   **Expected Outcome:** Automated daily brief generated based on Org memory and delivered via Telegram at a scheduled time.

## 5. Phase 4: On-Demand Interaction via Telegram

*(Focus on user-initiated queries and actions)*

-   **Action:** Enhance the Telegram message handler (`my-record-telega-message-action` from Phase 2) to detect user queries/commands directed at "Stevens".
-   **Action:** Upon detecting a query, the handler should trigger a specific "Answer Query" AI workflow (start node ID: `ANSWER_QUERY_START`).
-   **Action:** Design the "Answer Query" AI Workflow in an Org file.
    -   Include a `function` node (`my-find-context-for-query`) to search the Org memory based on the user's query.
    -   Include an `llm` node to formulate an answer using the query and retrieved context.
    -   Include a `function` node (`my-send-answer-via-telegram`) using the library function to send the answer back.
-   **Action:** Implement the Elisp functions needed by this workflow.
-   **Expected Outcome:** Ability to ask "Stevens" questions via Telegram, have it consult its Org memory, and receive an AI-generated answer. Ability to add reminders/notes via Telegram.

## 6. Phase 5: Refinement & Integration

-   **Action:** Thoroughly test each component and workflow.
-   **Action:** Debug issues related to data parsing, API calls, LLM prompt effectiveness, and workflow logic.
-   **Action:** Refine LLM prompts for better consistency and desired output format.
-   **Action:** Implement robust error handling within Elisp functions and potentially within AI workflows (e.g., using conditional nodes).
-   **Action:** Ensure reliable scheduling using Emacs timers or external cron jobs that trigger Emacs batch processes.
-   **Action:** Write user documentation on how to set up and interact with the Emacs Stevens assistant.
-   **Expected Outcome:** A stable, functional, and documented personal assistant running within Emacs.

## 7. Expected Capabilities of Emacs Stevens

-   Maintains a persistent, searchable memory within Org files.
-   Automatically ingests information from Calendar, Weather, Telegram, and potentially Email.
-   Delivers a personalized daily brief via Telegram based on schedule, reminders, and weather.
-   Accepts reminders and notes via Telegram messages.
-   Answers user questions asked via Telegram by consulting its Org memory and using an LLM.
-   Provides a hackable and extensible platform for adding new data sources and capabilities through the `org-supertag` framework. 