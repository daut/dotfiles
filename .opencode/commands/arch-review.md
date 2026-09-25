---
description: Evidence-based architecture review. Read-only. Proposals first, phased tickets on request.
agent: plan
---

Review the architecture: file layout, module boundaries, dependency direction. Do not change code.

**Scope:** $ARGUMENTS
- A path: review only that path.
- Empty: whole repo.

Local code quality (duplication, dead code, comments, tests) is out of scope. It belongs to
`/refactor-plan`.

## Steps

1. **Evidence.** Collect all four signals. If you cannot collect one, say so in the output.
   - **Co-change:** file pairs in different directories that change in the same commits.
     Source: `git log --since=12.months --format=%H --name-only -- <scope>`. Report the pairs and
     directory pairs with the highest co-change ratio.
   - **Dependency graph:** import cycles, lower layers that import higher layers, high fan-in.
     Use the repo's tools if present (`dependency-cruiser`, `madge`, `jdeps`, ArchUnit, `go list`,
     `pydeps`). Else parse the imports. Do not install tools.
   - **Dominant pattern:** the layout the repo uses in most places, and where code breaks from it.
   - **God modules:** files or packages with high fan-in and high churn.

2. **Explore.** Run up to 4 `explore` subagents in parallel, one per signal. Ask for data with
   numbers and paths, not recommendations.

3. **Propose.** Only changes that at least one signal supports.
   - Prefer the repo's own dominant pattern over outside patterns (hexagonal, feature folders, etc.).
   - Do not propose a layout that the code does not already point to.

4. **Defend.** Give each proposal to one fresh `explore` subagent. Tell it to argue for the current
   layout. Keep a proposal only if its evidence beats the defence.

5. **Present.** Summary only. Numbered proposals.
   - Per proposal: a short paragraph, 2-4 sentences. Say what to change, the key evidence, the
     strongest defence and why it loses, and the rough cost (files touched, move-only or not,
     reversible or not). Plain prose. No field lists. No ticket structure.
   - Keep the full details internal: evidence data, migration steps, guard rule, conflict risk.
   - End with **Considered, rejected**: one sentence each, with the reason.
   - Then ask: "Pick numbers to plan, or `details N` to expand."

   Example:
   > **1. Split `shared/` by feature.** 70% of commits that touch `shared/` also touch `orders/`
   > or `billing/`. That means `shared/` holds feature code, not shared code. The defence was that
   > API clients use shared DTOs, but only 3 of the 22 files are DTOs. The move is about 40 files,
   > move-only, and easy to reverse.

6. **Tickets** (only for the numbers the user picks).
   - Tracker: if `.beads/` exists, use `bd`. Else ask: Jira (load the `jira` skill first) or Linear (MCP).
   - One epic per proposal. Under it, one ticket per migration step, in this order:
     1. Move-only commits. No logic changes, so review is trivial and git tracks the renames.
     2. One module per MR. No big-bang reorganization.
     3. A guard rule (`dependency-cruiser`, ArchUnit, or similar) that locks the new boundary.
   - Each ticket: current state, target state, evidence, the step, cost, conflict risk with work
     in progress.
   - Print the epic and ticket IDs. Implementation happens later with `/ship`.
