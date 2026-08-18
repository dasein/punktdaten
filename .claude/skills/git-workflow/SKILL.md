---
name: git-workflow
description: Use when committing code, creating branches, opening PRs/MRs, syncing with upstream, or finishing work in a git repository. Covers the full lifecycle of working in git — from starting new work (worktrees) to committing, rebasing, publishing (PR/MR), and cleanup. Use this skill whenever the user says "commit", "open pr", "open mr", "create mr", "push", "sync", "rebase", "new branch", "start work on", "finish up", or asks to publish their changes. Also use when the user invokes /commit or /gl.commit.
---

# Git Workflow

Herbert's git workflow operates in two modes depending on whether the work is new or a continuation of existing work. In both modes, the goal is a clean, single-commit branch ready for PR/MR.

## Mode Detection

Determine the mode from context:

- **New work** — user says "start work on", "new branch", "worktree", or is beginning a fresh task with no existing branch. Creates an isolated worktree from latest upstream.
- **Continued work** — user is on a branch that already has changes, or says "commit", "push", "open pr". Works in place on the current branch.

If ambiguous, ask: "Is this new work (I'll create a worktree) or are you continuing on the current branch?"

## Mode 1: New Work (Worktree)

Start isolated work based on the latest upstream.

### Setup

Worktrees are created as **siblings** of the repo root (following the magit convention). This avoids needing gitignore entries and keeps the repo clean.

```bash
git fetch origin

# Sibling worktree: <repo-root>_<slug>
repo_root=$(git rev-parse --show-toplevel)
repo_name=$(basename "$repo_root")
worktree_path="$(dirname "$repo_root")/${repo_name}_<slug>"

git worktree add "$worktree_path" -b <branch-name> origin/main
cd "$worktree_path"
```

**Worktree slug:** short descriptor derived from the task (e.g., `glue-consolidation`, `fix-redis-output`). The resulting directory looks like `cloud-infra-service-resources_glue-consolidation`.

**Branch naming:** infer from context (e.g., `hpfennig/sdpl-1167-description` for Jira tickets, `hpfennig/short-description` otherwise). Match existing conventions visible in `git branch -r`.

### Working

After making changes, commit as a single commit (see Commit Message Format below). To stay current with upstream:

```bash
git fetch origin
git rebase origin/main
```

If there are already local commits, use `git rebase -i origin/main` to squash into one, or amend the existing commit:

```bash
git add <files>
git commit --amend --no-verify
```

The branch should always have exactly one commit ahead of origin/main.

### Cleanup (after PR is merged)

```bash
git worktree remove <worktree-path>  # removes the sibling directory
git branch -D <branch-name>
```

## Mode 2: Continued Work (Current Branch)

Work directly on the current branch — no local branch creation.

### On main/master

When working directly on main, changes are pushed to a remote feature branch without creating a local branch:

```bash
git push origin HEAD:<remote-branch-name>
```

Infer the remote branch name from the commit context (e.g., `hpfennig/sdpl-1167-description`). Match naming conventions from `git branch -r`.

### On a feature branch

Commit in place. Before opening a PR, sync with upstream:

```bash
git fetch origin
git rebase origin/main
```

If multiple commits exist, squash to one:

```bash
git rebase -i origin/main
# squash all commits into the first
```

### Reset for next task

After a PR is merged and you want to reuse the branch:

```bash
git fetch origin
git reset --hard origin/main
```

## Commit Message Format

Use present tense and imperative mood. No signatures, no "Co-Authored-By", no "Generated with Claude Code".

### Subject line

```
JIRA_ID: Subject line (≤72 chars, imperative mood, NO period)
```

Omit `JIRA_ID:` if no Jira ticket is relevant.

### Body

**Trivial changes:** no body.

**All other changes:** a prose paragraph explaining WHY, wrapped at 72 chars. Then bulleted summary grouped by logical concern (not per file). Omit bullets for trivial/obvious changes.

Example:

```
SDPL-1167: Centralize Glue Data Catalog encryption in scs-common

Multiple services independently manage aws_glue_data_catalog_encryption_settings,
an account-level singleton. This causes overwrites and state conflicts.
Centralize the resource in scs-common/account so it is managed once
per account.

- Add Glue Data Catalog encryption settings to scs-common/account
- Add aws_glue_data_catalog_encryption_settings to OPA prohibited
  resource types to prevent other services from creating it
```

### Amending

When amending, write the message describing the FULL commit content (diff against parent), not the incremental changes.

### Executing the commit

```bash
git add <specific-files>
git commit --no-verify -m "$(cat <<'EOF'
<message>
EOF
)"
```

Stage specific files — never `git add -A` or `git add .`.

## Opening a PR/MR

Only when the user explicitly asks (e.g., "open mr", "open pr", "create mr", "push").

### Steps

1. **Detect forge**: check `git remote -v` — `gitlab` → `glab`, `github` → `gh`.

2. **Push**:
   - On a feature branch: `git push -u origin HEAD`
   - On main/master: `git push origin HEAD:<remote-branch-name>`
   - On a worktree branch: `git push -u origin HEAD`

3. **Create the MR/PR** using the commit message:
   - Title: the commit subject line
   - Description: the commit body, merged with any auto-applied template
   - GitLab: `glab mr create --source-branch <branch> --title "..." --description "..." --no-editor`
   - GitHub: `gh pr create --head <branch> --title "..." --body "..."`

4. **Report the URL**.

### Updating an existing MR/PR

1. Read the current description first (`glab mr view` / `gh pr view`).
2. Preserve all template sections (plan-analysis, bot checklists, etc.).
3. Merge new content into the existing template, replacing only the parts you own (e.g., `## Summary`).
