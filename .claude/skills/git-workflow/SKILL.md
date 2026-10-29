---
name: git-workflow
description: Use when committing code, creating branches, opening PRs/MRs, syncing with upstream, or finishing work in a git repository. Covers the full lifecycle of working in git — from starting new work (pool branches or worktrees) to committing, rebasing, publishing (PR/MR), and cleanup. Use this skill whenever the user says "commit", "open pr", "open mr", "create mr", "push", "sync", "rebase", "new branch", "start work on", "finish up", or asks to publish their changes. Also use when the user invokes /commit or /gl.commit.
---

# Git Workflow

Herbert's git workflow always aims for a clean, single-commit branch ready for PR/MR. The only branching decision is how to get a branch to work on — reuse a pool branch, spin up a worktree, or continue where you already are. Everything after that — committing, rebasing, publishing — is the same.

## Step 1: Get a Branch to Work On

- **New work (default)** — reuse an available **pool branch** (below). This is the default for new work; no worktree needed.
- **New work, explicitly requested worktree** — user says "worktree" or it's long-running/parallel work — create an isolated worktree (below).
- **Continued work** — user is already on a branch with changes, or says "commit", "push", "open pr". Skip straight to Step 2.

If ambiguous, ask: "Is this new work, or are you continuing on the current branch?"

### Reusing a pool branch (default for new work)

Herbert keeps a fixed pool of local branch names — `foo`, `bar`, `car`, `star` — reused across unrelated tasks instead of creating (and later deleting) a new local branch each time. A pool branch is **available** if it has no commits ahead of `origin/main` (nothing parked on it):

```bash
git fetch origin
for b in foo bar car star; do
  if ! git show-ref --quiet refs/heads/"$b"; then
    echo "$b: available (doesn't exist yet)"
  elif [ -z "$(git log origin/main..$b --oneline 2>/dev/null)" ]; then
    echo "$b: available (no unpushed commits)"
  else
    echo "$b: occupied"
  fi
done
```

Take the first available branch and reset it to latest upstream:

```bash
git checkout <branch> 2>/dev/null || git checkout -b <branch> origin/main
git reset --hard origin/main
```

If all four are occupied, stop and ask Herbert how to proceed (add another pool name, or fall back to a worktree) rather than guessing.

The local name (`foo`) never matches the real remote branch name, so the first push must set it explicitly (see Step 4).

### Creating a worktree (only when explicitly requested)

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

**Branch naming:** infer from context (e.g., `hpfennig/sdpl-1167-description` for Jira tickets, `hpfennig/short-description` otherwise). Match existing conventions visible in `git branch -r`. Since the worktree branch already has its real name, push with plain `git push -u origin HEAD` (Step 4) — no explicit remote-name mapping needed.

## Step 2: Commit

Default to **one commit per branch**. Only split into multiple commits when Herbert explicitly asks for it — never split proactively.

```bash
git add <specific-files>
git commit --no-verify -m "$(cat <<'EOF'
<message>
EOF
)"
```

Stage specific files — never `git add -A` or `git add .`.

If a commit already exists on the branch (continuing work, or fixing up the last commit), amend instead of adding a new one:

```bash
git add <specific-files>
git commit --amend --no-verify
```

When amending, write the message describing the FULL commit content (diff against parent), not the incremental change.

### Commit message format

Present tense, imperative mood. No signatures, no "Co-Authored-By", no "Generated with Claude Code".

**Subject line:**

```
JIRA_ID: Subject line (≤72 chars, imperative mood, NO period)
```

Omit `JIRA_ID:` if no Jira ticket is relevant.

**Body:**

Trivial changes: no body. All other changes: a prose paragraph explaining WHY, wrapped at 72 chars, then a bulleted summary grouped by logical concern (not per file). Omit bullets for trivial/obvious changes.

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

## Step 3: Sync with Upstream

Before opening a PR, or any time an amend follows a push, sync with upstream:

```bash
git fetch origin
git rebase origin/main
```

The branch should end up with exactly one commit ahead of origin/main. If multiple local commits exist and weren't explicitly requested, squash them:

```bash
git rebase -i origin/main
# squash all commits into the first
```

## Step 4: Open a PR/MR

Only when the user explicitly asks (e.g., "open mr", "open pr", "create mr", "push").

1. **Detect forge**: check `git remote -v` — `gitlab` → `glab`, `github` → `gh`.
2. **Push**:
   - Pool branch (`foo`/`bar`/`car`/`star`) or on main/master: `git push -u origin HEAD:<remote-branch-name>` — the local name is never the real branch name, so it must be set explicitly. The `-u` sets tracking so later pushes (Step 5) don't fall back to pushing `foo` to `origin/foo`. Infer the remote name from context (e.g., `hpfennig/sdpl-1167-description`), matching conventions from `git branch -r`.
   - Worktree branch: `git push -u origin HEAD` (the branch already has its real name).
3. **Create the MR/PR** using the commit message:
   - Title: the commit subject line
   - Description: the commit body, merged with any auto-applied template
   - GitLab: `glab mr create --source-branch <branch> --title "..." --description "..." --no-editor`
   - GitHub: `gh pr create --head <branch> --title "..." --body "..."`
4. **Report the URL**.

### Updating an existing MR/PR description

1. Read the current description first (`glab mr view` / `gh pr view`).
2. Preserve all template sections (plan-analysis, bot checklists, etc.).
3. Merge new content into the existing template, replacing only the parts you own (e.g., `## Summary`).

## Step 5: Address Review Feedback on an Open MR

This is the common loop when context-switching back into a repo to address comments: amend the commit, rebase onto the latest upstream, then force-push.

```bash
git add <specific-files>
git commit --amend --no-verify
git fetch origin
git rebase origin/main
git push --force-with-lease
```

Always rebase onto latest upstream as part of this loop, even if the amend itself doesn't touch conflicting files — the branch should stay current, not just amended. Use `--force-with-lease`, never plain `--force`.

## Cleanup

**Pool branch, after PR is merged:** nothing to do — it's released automatically. The availability check in Step 1 sees no commits ahead of `origin/main` and picks it up again for the next task.

**Worktree, after PR is merged:**

```bash
git worktree remove <worktree-path>  # removes the sibling directory
git branch -D <branch-name>
```
