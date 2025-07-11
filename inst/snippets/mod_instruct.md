### Format of proposed changes to code or text files

When you suggest changes to code or text files or completely new files, you MUST use the following format for each modification. Each change must be enclosed in a `!MODIFICATION` block.

#### **Overall Structure**

Each modification block has three parts:
1.  Start and end markers: `!MODIFICATION` and `!END_MODIFICATION`.
2.  A metadata block in **TOML format**. This block ends with a `---` separator line.
3.  A code payload block, which is a standard markdown code fence.

```
!MODIFICATION
# TOML metadata goes here
# ...
---
```language
# New code payload goes here
```
!END_MODIFICATION
```

The metadata block **MUST** contain an `operation` field, which can be either `"complete_file"` or `"modify_lines"`.

---

### **Operation 1: `complete_file`**

Use this to create a new file or to completely rewrite an existing one.

**Required Fields:**
*   `operation = "complete_file"`
*   `file` (string): The relative path to the file.
*   `is_new_file` (boolean): `true` if you are creating a new file, `false` if you are rewriting an existing one.

**Optional Fields:**
*   `description` (string): A brief explanation of the change. Always enclose into triple single quotes `'''...'''`.

#### **Example 1.1: Creating a new file**
!MODIFICATION
operation = "complete_file"
file = "R/new_helpers.R"
is_new_file = true
description = '''Create a new file for helper functions.'''
---
```r
# A new helper function
say_hello <- function(name) {
  paste("Hello,", name)
}
```
!END_MODIFICATION

#### **Example 1.2: Rewriting an existing file**
!MODIFICATION
operation = "complete_file"
file = "README.md"
is_new_file = false
description = '''Rewrite the README to add installation instructions.'''
---
```md
# My Awesome Project

## Installation

Run `remotes::install_github("user/repo")` to install.
```
!END_MODIFICATION

---

### **Operation 2: `modify_lines`**

Use this for all granular changes: inserting, replacing, or deleting lines in an existing file. The behavior depends on the fields you provide.

*   **To Insert:** Provide `insert_at_line`.
*   **To Replace:** Provide `insert_at_line`, `delete_from`, and `delete_to`.
*   **To Delete:** Provide `delete_from` and `delete_to`. The code payload should be empty.

**Required Fields:**
*   `operation = "modify_lines"`
*   `file` (string): The relative path to the file.

**Location Fields (use as needed):**
*   `insert_at_line` (integer): The line number where the new code should be inserted.
*   `delete_from` (integer): The first line number of the block to delete/replace.
*   `delete_to` (integer): The last line number of the block to delete/replace.

**Context & Description Fields (optional but strongly recommended for accuracy):**
*   `description` (string): What are you changing and why? Use `'''...'''`.
*   `context_before` (string): The content of the line just *before* the modification. Use `'''...'''`.
*   `context_delete_first` (string): The content of the first line being deleted/replaced (`delete_from`). Use `'''...'''`.
*   `context_delete_last` (string): The content of the last line being deleted/replaced (`delete_to`). Use `'''...'''`.
*   `context_after` (string): The content of the line just *after* the modification. Use `'''...'''`.

#### **Example 2.1: Inserting new lines**
!MODIFICATION
operation = "modify_lines"
file = "R/utils.R"
insert_at_line = 10
description = '''Add a new function `is_positive()`.'''
context_before = '''}'''
---
```r

#' Check if a number is positive
is_positive <- function(n) {
  n > 0
}
```
!END_MODIFICATION

#### **Example 2.2: Replacing a block of lines**
!MODIFICATION
operation = "modify_lines"
file = "R/f2p.R"
insert_at_line = 55
delete_from = 55
delete_to = 57
description = '''Refactor the `guess_token_num` function for better accuracy.'''
context_before = '''main_prompt'''
context_delete_first = '''guess_token_num = function(txt, bytes_per_token=4) {'''
context_delete_last = '''}'''
---
```r
guess_token_num = function(txt, bytes_per_token=3.8) {
  # A more precise estimation
  ceiling(nchar(txt, type = "bytes") / bytes_per_token)
}
```
!END_MODIFICATION

#### **Example 2.3: Deleting a block of lines**
!MODIFICATION
operation = "modify_lines"
file = "f2p.R"
delete_from = 1
delete_to = 7
description = '''Remove the old, obsolete example function.'''
context_delete_first = '''example = function() {'''
context_delete_last = '''}'''
context_after = '''#' Parse a TOML confifugration file'''
---
```
```
!END_MODIFICATION
