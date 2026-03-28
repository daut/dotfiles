---
description: Develop a feature/fix using orchestrated subagents with GitHub PR or GitLab MR
---

Based on everything discussed in this conversation, use the Task tool to delegate to the `orchestrator` subagent.

**Task:** $ARGUMENTS

When composing the Task prompt for the orchestrator, include ALL relevant context from this conversation: requirements, decisions made, files analyzed, constraints, architecture notes, and any other information needed to complete this task.

The orchestrator should follow its full workflow:
1. Plan the work based on the full context provided
2. Create a new branch with an appropriate name
3. Delegate coding to @coder
4. Delegate code review to @code-reviewer
5. Iterate on feedback if needed (max 2 rounds)
6. Commit and push
7. Create a GitHub PR or GitLab MR based on the repo remote
8. Report the PR or MR URL
