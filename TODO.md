# GPtel Agent Tools Implementation Task List

## Overview
This document outlines the implementation tasks for creating agent tools using `gptel-make-tool` to enable LLM-based coding assistance in Emacs. The tools will leverage existing libraries (counsel, magit, ivy) and built-in Emacs functions.

When implementing tasks, please follow these rules:

* Write idiomatic code that follows established patterns for writing lisp in emacs
* Keep tool implementations simple and elegant
* Do not introduce unneeded complexity
* When possible, use built in emacs libraries to perform tool operations before running a shell command. Additional libraries available to you are ivy (for buffer operations), magit (git operations), counsel (file lookup).
* If needed, you can write separate helper functions if there is the need for code reuse.
* For agents that take input, focus on simple input parameters (ideally only one) instead of complex arguments.
* For agent output, you can use machine readable efficient outputs where applicable. If not, just return output as is (e.g. reading a file)
* Do not create new files. If necessary to create new files, ask first.

Tasks are ordered by implementation priority, considering dependencies and ease of implementation.

## Preliminary Tasks

### Task 0.1: `test-agent-tool-function`

**Type**: Testing Infrastructure
**Description**: Create function to test agent tool functions directly
**File**: `.emacs.d/lisp/test-agent.el` *(moved from agent-tools.el)*
**Priority**: High (Required for validating each tool implementation)
**Dependencies**: None
**Status**: ✅ COMPLETED *(Simplified: Automatic testing of all tools)*

**Implementation**:

* [x] **SIMPLIFIED**: Single function `test-agent-all-tool-functions` that automatically tests all tools
* [x] Automatically discovers and tests all tools in `gptel--known-tools`
* [x] Tests tools with sample arguments when required
* [x] Reports pass/fail status with error details
* [x] **CLEAN**: Removed complex interactive features, focused on automated testing

**Usage**:

* `M-x test-agent-all-tool-functions` - Test all registered tool functions automatically

**Testing**: 
* Function iterates through `gptel--known-tools` and tests each tool directly
* Provides sample arguments ("test") for tools that require parameters
* Reports results with pass/fail counts

**Code Organization**:
* `agent-tools.el`: Clean tool definitions and registration
* `test-agent.el`: Simplified testing functions
* Tools can be reloaded with `(agent-tools-register-all t)` without restart

### Task 0.2: `test-gptel-integration`

**Type**: Testing Infrastructure
**Description**: Create function to test gptel-send integration with agent tools
**File**: `.emacs.d/lisp/test-agent.el`
**Priority**: High (Required for validating GPtel recognizes and executes tools)
**Dependencies**: Task 0.1
**Status**: ✅ COMPLETED *(Fully Automated: Calls gptel-send directly)*

**Implementation**:

* [x] **AUTOMATED**: Single function `test-agent-gptel-integration` that calls `gptel-send` directly
* [x] Automatically discovers all tools in `gptel--known-tools` 
* [x] Generates structured prompts requesting specific response format from GPtel
* [x] Uses custom callback to capture and analyze responses
* [x] **STRUCTURED PARSING**: Prompts GPtel to respond in format `Success||<input>||<output>` for precise parsing
* [x] Reports whether each tool was successfully called by GPtel with detailed breakdown
* [x] **COMPREHENSIVE**: Tests actual GPtel integration with fallback detection methods

**Usage**:

* `M-x test-agent-gptel-integration` - Test GPtel integration with all tools automatically

**Description**:

The function iterates through `gptel--known-tools`, generates structured test prompts, and calls `gptel-request` with a custom callback to capture the results. It prompts GPtel to respond in a specific format (`Success||<input>||<output>` or `Failed||<reason>||Input_None`) for reliable parsing, then analyzes whether GPtel successfully recognizes the tools and executes them.

Key features:

* Automatic test execution without manual intervention
* **Structured response format** ensures reliable parsing of GPtel outputs
* Custom callback captures tool calls, tool results, and text responses  
* Sequential processing prevents timeout race conditions (60 seconds per tool)
* **Detailed parsing** extracts tool input, output, and success status
* Fallback detection for non-structured responses
* Comprehensive success/failure reporting with clear status indicators

This approach provides true end-to-end testing of the GPtel-agent tool integration and scales automatically as new tools are added to `agent-tools.el`.


## Implementation Tasks (In Order)

### Task 1: `gptel-list-files`

**Type**: Project Context
**Description**: Smart directory listing with filters
**File**: `.emacs.d/lisp/agent-tools.el`
**Priority**: High (Foundation for project exploration)
**Dependencies**: gptel-run-command (for advanced filtering)

**Implementation**:

- [ ] Register with `gptel-make-tool`
- [ ] Support file type filters
- [ ] Add recursive/non-recursive options
- [ ] Exclude common ignore patterns (.git, node_modules, etc.)

### Task 2: `gptel-read-file`

**Type**: File Operations
**Description**: Read file with intelligent context
**File**: `.emacs.d/lisp/agent-tools.el`
**Priority**: High (Foundation tool needed by other agents)
**Dependencies**: None

**Implementation**:

- [ ] Register with `gptel-make-tool`
- [ ] Handle file path validation
- [ ] Add error handling for non-existent files
- [ ] Support both absolute and relative paths

### Task 3: `gptel-run-command`

**Type**: Shell Operations
**Description**: Execute shell commands and return output
**File**: `.emacs.d/lisp/agent-tools.el`
**Priority**: High (Required by many other tools)
**Dependencies**: None

**Implementation**:

- [ ] Register with `gptel-make-tool`
- [ ] Add timeout handling
- [ ] Capture both stdout and stderr
- [ ] Add working directory parameter

### Task 4: `gptel-run-in-project-root`

**Type**: Shell Operations
**Description**: Execute commands in git root directory
**File**: `.emacs.d/lisp/agent-tools.el`
**Priority**: Medium (Builds on gptel-run-command)
**Dependencies**: gptel-run-command

**Implementation**:

- [ ] Register with `gptel-make-tool`
- [ ] Auto-detect git root directory
- [ ] Fallback to current directory if not in git repo
- [ ] Inherit all features from gptel-run-command

### Task 5: `gptel-write-file`

**Type**: File Operations
**Description**: Write content to file (new or overwrite)
**File**: `.emacs.d/lisp/agent-tools.el`
**Priority**: Medium (Essential for code generation)
**Dependencies**: None

**Implementation**:

- [ ] Register with `gptel-make-tool`
- [ ] Create directories if they don't exist
- [ ] Add backup option for existing files
- [ ] Support append mode

### Task 6: `gptel-project-structure`

**Type**: Project Context
**Description**: Get project overview
**File**: `.emacs.d/lisp/agent-tools.el`
**Priority**: Medium (Useful for understanding codebase)
**Dependencies**: gptel-list-files, gptel-run-in-project-root

**Implementation**:

- [ ] Register with `gptel-make-tool`
- [ ] Generate tree-like structure
- [ ] Respect .gitignore patterns
- [ ] Include file counts and sizes

### Task 7: `gptel-get-working-directory-changes`

**Type**: Project Context
**Description**: Get current git status and recent changes
**File**: `.emacs.d/lisp/agent-tools.el`
**Priority**: Medium (Helpful for understanding current work)
**Dependencies**: gptel-run-in-project-root

**Implementation**:

- [ ] Register with `gptel-make-tool`
- [ ] Show git status (staged, unstaged, untracked)
- [ ] Include recent commit information
- [ ] Handle non-git directories gracefully

### Task 8: `gptel-search-ripgrep`

**Type**: Code Discovery & Analysis
**Description**: Search for code patterns using ripgrep
**File**: `.emacs.d/lisp/agent-tools.el`
**Priority**: Low (Advanced feature, requires ripgrep installed)
**Dependencies**: gptel-run-in-project-root

**Implementation**:

- [ ] Register with `gptel-make-tool`
- [ ] Support regex patterns
- [ ] Add file type filtering
- [ ] Include line numbers and context

## Testing & Validation Strategy

After implementing each tool, use the following validation steps to ensure it works correctly with `gptel-send`:

### Basic Tool Registration Test

1. Restart Emacs or evaluate the tool definition
2. Run `M-x describe-variable gptel-directives` to verify the tool is registered
3. Check that the tool appears in the list with correct name and description

### Integration Test with GPtel

1. Open a buffer and run `M-x gptel-send` with a prompt that should trigger the tool
2. Use test prompts like:
   - For `gptel-read-file`: "Read the contents of TODO.md and summarize it"
   - For `gptel-run-command`: "Run the command 'ls -la' and show me the output"
   - For `gptel-list-files`: "List all files in the current directory"
   - For `gptel-project-structure`: "Show me the structure of this project"

### Validation Checklist for Each Tool

- [ ] Tool is registered successfully (appears in `gptel-directives`)
- [ ] Tool function executes without errors when called directly
- [ ] GPtel recognizes the tool from natural language prompts
- [ ] Tool handles edge cases gracefully (missing files, permission errors, etc.)
- [ ] Error messages are informative and helpful
- [ ] Output format is suitable for LLM consumption

### Automated Testing Setup

Consider creating a test file `test-agent-tools.el` with:

```lisp
(defun test-agent-tool (tool-name test-args expected-result-type)
  "Test an agent tool with given arguments."
  (let ((tool-function (plist-get (car (cl-find tool-name gptel-directives 
                                                :key (lambda (x) (plist-get x :name)) 
                                                :test #'string=)) :function)))
    (when tool-function
      (condition-case err
          (let ((result (apply tool-function test-args)))
            (message "✓ %s executed successfully, returned %s" tool-name (type-of result))
            result)
        (error (message "✗ %s failed: %s" tool-name (error-message-string err)))))))
```

## Example Implementation Pattern

```lisp
(gptel-make-tool
 :name "read_buffer"                    ; javascript-style snake_case name
 :function (lambda (buffer)                  ; the function that will run
             (unless (buffer-live-p (get-buffer buffer))
               (error "error: buffer %s is not live." buffer))
             (with-current-buffer  buffer
               (buffer-substring-no-properties (point-min) (point-max))))
 :description "return the contents of an emacs buffer"
 :args (list '(:name "buffer"
               :type string            ; :type value must be a symbol
               :description "the name of the buffer whose contents are to be retrieved"))
 :category "emacs")                     ; An arbitrary label for grouping
```

## Notes

- All tools should leverage existing Emacs libraries and user configuration if possible
- Error handling should be consistent and informative
- Tools should respect current project context (git root, file types, etc.)
- Implementation should follow existing code style and patterns in init.el
