---
name: hp-plan
description: Use only when Herbert invokes /hp-plan to turn an idea, design or spec into a detailed bite-size implementation plan written to docs/plans/.
argument-hint: ["idea | design | spec"]
disable-model-invocation: true
---

# Plan

## Context

Current brainstorming/planning context, or the design/spec documents Herbert names.

## Your task

Create an implementation plan for the idea, design or spec Herbert passes with this command.

## Instructions

Create a whole coding implementation plan as bite-size tasks for a developer to implement. DRY. YAGNI.

- Assume the developer has zero context for our codebase and questionable taste.
- Assume a skilled developer will implement it, but knows almost nothing about our toolset or problem domain.
- Document everything they need to know: which files to touch for each task, code, testing, docs they might need to check.
- If relevant or requested, how to test it. TDD. Assume the developer doesn't know good test design very well.

Please write out this plan, in full detail, into docs/plans/.
