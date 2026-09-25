---
description: Evidence-based refactoring and deletion analysis. Read-only. Summary first, tickets on request.
agent: plan
---

Analyze the codebase for refactoring and deletion opportunities. Do not change code.

**Scope:** $ARGUMENTS
- A path: analyze only that path.
- `since:<ref|date>`: analyze only files changed since then.
- Empty: whole repo, with hotspots from the last 6 months.

Architecture concerns (file layout, module boundaries, dependency direction) are out of scope.
Mention them in one sentence at the end and suggest `/arch-review`.

## Steps

1. **Evidence.** Collect data before you form opinions.
   - Churn: `git log --since=6.months --format= --name-only -- <scope> | sort | uniq -c | sort -rn`
   - Size: line counts of the files with the most churn.
   - Hotspots: top 10 by churn x size.
   - Test presence for each hotspot.
   - Tools the repo already has for duplication, dead code, or complexity (e.g. `jscpd`, `knip`,
     `ts-prune`, `vulture`, `deadcode`, `depcheck`). Run them if present. Do not install tools.

2. **Explore.** Run up to 4 `explore` subagents in parallel. Give each one its area, the evidence,
   and its checklist. Ask for findings with `file:line` evidence, not opinions.
   - **Hotspots** (one agent per area): duplication and missed reuse of existing helpers,
     logic in the wrong layer, the same thing done in different ways, thin wrapper layers,
     test gaps that block a safe refactor.
   - **Deletion sweep** (whole scope, not ranked by churn, because dead code has zero churn):
     - Code: unused exports, functions, and files. Commented-out code. Feature flags fully rolled
       out. Interfaces with one implementation. Checks for cases that cannot happen. Unused
       dependencies and config.
     - Tests: tests of library behavior, tests of implementation details, mock-only tautologies,
       duplicate tests for the same path, tests of trivial code.
     - Comments: comments that repeat the code, stale comments that contradict the code,
       TODOs older than 6 months with no ticket.
   - **Guards**: deterministic checks missing from CI (duplication, dead code, complexity, lint).

3. **Refute.** Give all candidates to one fresh `explore` subagent. Tell it to try to disprove
   each one. Drop the findings it disproves.
   - Deletions: prove there are no usages. Check reflection, DI wiring, public API consumers,
     dynamic imports, config references.
   - Test deletions: name the test that still covers the same behavior. No name -> drop it.
   - Duplication: check if the copies must differ on purpose.

4. **Rank.** Deletions first, because they are low risk. Then refactors by (churn x pain) / risk.
   Then guards. Batch small findings: one per module per type, not one per instance.

5. **Present.** Summary only. Numbered, grouped under **Deletions**, **Refactors**, **Guards**.
   - Per finding: a short paragraph, 2-4 sentences. Say what is wrong, why it matters, the key
     evidence, and a rough size (S/M/L, max one MR each). Plain prose. No field lists. No ticket
     structure.
   - Keep the full details internal: `file:line` list, safety net, dependencies, out of scope.
   - End with **Not worth it**: one sentence each, with the reason.
   - Then ask: "Pick numbers to ticket, or `details N` to expand."

   Example:
   > **4. Retry logic copied in 4 HTTP clients (M).** `PaymentClient`, `LedgerClient`, and two
   > others each have their own backoff loop, with different max attempts. `http/retry.ts` already
   > does this and is tested. These clients are in the top 3 by churn, so the copies keep drifting
   > apart. Moving them to the shared helper does not change behavior, and the existing client
   > tests cover it.

6. **Tickets** (only for the numbers the user picks).
   - Tracker: if `.beads/` exists, use `bd`. Else ask: Jira (load the `jira` skill first) or Linear (MCP).
   - Write each ticket from the internal details: summary, evidence (`file:line`, numbers), the
     change, behavior-preserving or not, safety net (existing tests, or tests to write first),
     size, dependencies, out of scope.
   - Print the ticket IDs. Implementation happens later with `/ship`.
