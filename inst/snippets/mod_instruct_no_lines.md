## Format of proposed changes to code or text files

When you suggest changes to code or text files or completely new files, you MUST use the following format for each modification. Each change must be enclosed in a `!MODIFICATION` block.

### Overall Structure

Each modification block has three parts:
1.  Start and end markers: `!MODIFICATION {{what}}` and `!END_MODIFICATION {{what}}`, where {{what}} is just a short reference to what is modified, file name or function name with with file. It will not be parsed but makes it easier for a human to understand blocks.
2.  A metadata block in **TOML format**. This block ends with a `---` separator line.
3.  A code payload block, which is a standard markdown code fence.

```
!MODIFICATION {{what}}
# TOML metadata goes here
# ...
---
```language
# New code payload goes here
```
!END_MODIFICATION {{what}}
```

### Modification Scope

Each modification is of one of the following three scopes:

* `file` (re-)writes a complete file

* `function` (re-)writes a complete function (including comments above)


If more than two functions edits will be performed in the same file, better rewrite the whole file using a `file` scope. For extremely long files also more smaller function scope edits are ok.

Function scope only works for R code files. So for other files, perform edits in file scope.

The metadata block **MUST** contain a `scope` field, which can be `"file"` or `"function"`.

---

### **Scope 1: `file`**

Use this to create a new file or to completely rewrite an existing one.

**Required Fields:**
*   `scope = "file"`
*   `file` (string): The relative path to the file.
*   `is_new_file` (boolean): `true` if you are creating a new file, `false` if you are rewriting an existing one.
*   `description` (string): A brief explanation of the change. Always enclose into triple single quotes `'''...'''`.

#### **Example 1.1: Creating a new file**
!MODIFICATION new_helpers.R
scope = "file"
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
!END_MODIFICATION new_helpers.R

#### **Example 1.2: Rewriting an existing file**
!MODIFICATION README.md
scope = "file"
file = "README.md"
is_new_file = false
description = '''Rewrite the README to add installation instructions.'''
---
```md
# My Awesome Project

## Installation

Run `remotes::install_github("user/repo")` to install.
```
!END_MODIFICATION README.md

---

### **Scope 2: `function`**

Use this to replace an existing function or to insert a new function. The new code payload should contain the complete function, including any preceding comments.

#### **Fields for replacing an existing function:**
*   `scope = "function"`
*   `file` (string): The relative path to the file.
*   `function_name` (string): The name of the function to be replaced.
*   `description` (string): A brief explanation of the change. Always enclose into triple single quotes `'''...'''`.

#### **Fields for inserting a new function:**
*   `scope = "function"`
*   `file` (string): The relative path to the file.
*   `description` (string): A brief explanation of the change. Always enclose into triple single quotes `'''...'''`.
*   **One of** the following fields to specify the insertion point. They are mutually exclusive.
    *   `insert_top = true`: Insert at the top of the file.
    *   `insert_bottom = true`: Insert at the bottom of the file.
    *   `insert_before_fun = "function_name"`: Insert before the specified function.
    *   `insert_after_fun = "function_name"`: Insert after the specified function.


#### **Example 2.1: Replacing an existing function**
!MODIFICATION calculate_sum utils.R
scope = "function"
file = "R/utils.R"
function_name = "calculate_sum"
description = '''Update `calculate_sum` to handle NA values correctly.'''
---
```r
#' Calculate the sum of a vector, ignoring NAs
calculate_sum <- function(vec) {
  sum(vec, na.rm = TRUE)
}
```
!END_MODIFICATION calculate_sum utils.R

#### **Example 2.2: Inserting a new function at the bottom of a file**
!MODIFICATION is_positive in R/utils.R
scope = "function"
file = "R/utils.R"
insert_bottom = true
description = '''Add a new helper function to check for positivity.'''
---
```r

#' Check if a number is positive
is_positive <- function(n) {
  n > 0
}
```
!END_MODIFICATION is_positive in R/utils.R

#### **Example 2.3: Inserting a new function after a specific function**
!MODIFICATION is_negative in R/utils.R
scope = "function"
file = "R/utils.R"
insert_after_fun = "is_positive"
description = '''Add a new helper function `is_negative` after `is_positive`.'''
---
```r

#' Check if a number is negative
is_negative <- function(n) {
  n < 0
}
```
!END_MODIFICATION is_negative in R/utils.R
