This is a comprehensive and well-structured R package that provides a powerful workflow for interacting with LLMs on R projects directly within RStudio.

### Codebase Description

The `files2prompt` R package is a suite of tools for RStudio that facilitates interaction with Large Language Models (LLMs). It consists of two main RStudio add-ins:

1.  **Generate Prompt**: This add-in collects specified project files (code, text, data) based on a TOML configuration file, assembles them into a single, formatted string, and prepares it for an LLM prompt. It supports flexible file inclusion/exclusion using glob patterns, templating for the final prompt structure, and even special handling for data files (e.g., summarizing `.csv` or `.rds` files). The configuration is hierarchically discovered, from the active file, project root, user config directory, to package-level defaults.
2.  **Apply Modifications**: This add-in parses a response from an LLM that contains proposed code changes. It expects the changes to be in a specific `!MODIFICATION` format, which includes TOML metadata (specifying scope, file, description, etc.) and a code payload. It then presents a Shiny gadget UI that allows the user to interactively review each proposed change, see the target code highlighted in the RStudio editor, and decide to apply, skip, or undo the modifications.

The codebase is modular:
*   `f2p.R`: Core logic for prompt generation.
*   `addin.R`: Shiny gadget UI and server logic for the "Generate Prompt" add-in.
*   `mod_parser.R`: Parses the LLM's response to extract modification blocks.
*   `mod_loc.R`: Locates the exact file and line numbers for a given modification. It can find functions in R scripts or match sequences of lines.
*   `mod_apply.R`: Applies the changes using the `rstudioapi`.
*   `mod_addin.R`: Shiny gadget UI and server logic for the "Apply Modifications" add-in.
*   Helper files for code parsing (`code_tools.R`), data file handling (`f2p_data.R`), templating (`tpl.R`), and general utilities (`tools.R`, `mod_tools.R`).

The package is well-structured and leverages powerful R packages like `shiny`, `miniUI`, `rstudioapi`, `RcppTOML`, and `stringi` to create a seamless workflow within the RStudio IDE.

### Proposed Improvements

The current implementation for the "Generate Prompt" add-in defaults to saving the output prompt to a temporary file, which is a sensible default. However, this behavior is hard-coded and not immediately obvious or configurable for the user. Adding an option in the TOML configuration file to specify the output path (`opt_prompt_file`) would improve usability. This allows users to save prompts directly to their project directory for reference or version control.

I will add this `opt_prompt_file` setting to the default TOML configuration files (`f2p_default.toml`, `f2p_r_pkg.toml`, `f2p_simple_r_files.toml`) located in `inst/toml`. The new setting will default to saving in a temporary directory but will include comments explaining how a user can change it to save the prompt in their project's root directory instead.

!MODIFICATION inst/toml/f2p_default.toml
scope = "file"
file = "inst/toml/f2p_default.toml"
is_new_file = false
description = '''Add opt_prompt_file option to the default TOML config, with comments explaining its use.'''
---
```toml
# A simple prompt that includes all R files

# By default, the generated prompt is saved to a temporary file.
# You can change this by uncommenting and editing the line below.
# To save to the project root, use a simple filename like:
# opt_prompt_file = "my_prompt.md"
opt_prompt_file = "<tempdir>/files2prompt.md"

include_files  = '''
*.R
*.r
'''
```
!END_MODIFICATION inst/toml/f2p_default.toml
!MODIFICATION inst/toml/f2p_r_pkg.toml
scope = "file"
file = "inst/toml/f2p_r_pkg.toml"
is_new_file = false
description = '''Add opt_prompt_file option to the R package TOML config, with comments explaining its use.'''
---
```toml
################################################
# Which files go in?
# Typically, you would either specify include_files
# or exclude_files
# We use globs instead of regular expressions
################################################

# Here all included files will be defined
# in the corresponding groups below.
include_files = '''
'''

exclude_files = '''
*f2p_*.toml
'''

# By default, the generated prompt is saved to a temporary file.
# You can change this by uncommenting and editing the line below.
# To save to the project root, use a simple filename like:
# opt_prompt_file = "my_prompt.md"
opt_prompt_file = "<tempdir>/files2prompt.md"


################################################
# Global prompt template
# You can refer to specified variables or
# [[groups]]
# Special variable {files} contains all files
# not yet part of an explicitly included group
################################################
template = '''
Below are relevant files from my R project.

################################################
# R project DESCRIPTION file:
################################################

{{DESCRIPTION}}

################################################
# R code files:
################################################

{{R}}

```md
# files2prompt

> Streamline your R-LLM workflow in RStudio: turn project files into a single prompt, and apply AI-generated code modifications with an interactive reviewer.

`files2prompt` is an R package with two key RStudio add-ins designed to make working with Large Language Models (LLMs) on your R projects easier and more efficient.

1.  **Generate Prompt**: Collect code and text files from your project into a single, well-formatted prompt text, ready to be copied into any LLM.
2.  **Apply Modifications**: Parses a response from an LLM and provides an interactive tool to review and apply the proposed changes directly to your files.

---

## Installation

You can install the package from the [skranz r-universe](https://skranz.r-universe.dev/):
```r
install.packages(
  "files2prompt",
  repos = c("https://skranz.r-universe.dev", getOption("repos"))
)
```

Or install the development version directly from GitHub:
```r
# install.packages("remotes")
remotes::install_github("skranz/files2prompt")
```

---

## The Core Workflow

`files2prompt` provides a seamless, two-part workflow for using an LLM to work on your R project.

### Step 1: Generate a Prompt with the `Files To Prompt` Add-in

This add-in gathers all relevant files from your project and packages them into a single prompt for your LLM.

1.  Open your R project in RStudio.
2.  Go to **Addins ▸ FILES2PROMPT ▸ Files To Prompt**.
3.  The add-in will:
    *   Look for a configuration file (see "Customization" below) or use a smart default.
    *   Read the contents of all specified files.
    *   Assemble them into a single prompt.
    *   Save the prompt to a temporary file (`files2prompt.txt`).
    *   Open this file in RStudio, with all text selected.
    *   Copy the entire prompt to your clipboard (if `{clipr}` is installed).
4.  Paste the prompt into your preferred LLM chat interface.

#### Customizing the Prompt

You can control exactly what goes into the prompt by creating a TOML file in your project root. The add-in will automatically use the first file it finds matching the pattern `f2p*.toml`.

For example, create `f2p_config.toml` in your project's main directory:

```toml
# f2p_config.toml

# A description of your project to start the prompt
template = """
Below are files from my R package project. Please help me with my request.

{{files}}
"""

# Use glob patterns to include files
# Each pattern should be on a new line
include_files = """
R/*.R
README.md
DESCRIPTION
"""

# Use glob patterns to exclude files
exclude_files = """
R/dont_include_this.R
"""
```

### Step 2: Apply Changes with the `Apply Modifications` Add-in

After the LLM provides suggestions, you can use this add-in to apply them safely and interactively.

1.  **Ask your LLM to provide changes in the specific `!MODIFICATION` format.** You can find these formatting instructions in `system.file("prompt_snippets/mod_instruct.md", package="files2prompt")`.
2.  Copy the complete response from the LLM.
3.  Save it as a new file in your project, e.g., `ai_resp.md`.
4.  In RStudio, make sure `ai_resp.md` is your active file, or that it is in your project root.
5.  Go to **Addins ▸ FILES2PROMPT ▸ Apply Modifications**.
6.  A gadget will open in the RStudio viewer pane, showing you the first proposed change.

![Apply Modifications Add-in Screenshot](https://raw.githubusercontent.com/skranz/files2prompt/main/man/figures/apply_modifications_addin.png)

For each proposed modification, you can:
*   **Apply**: Executes the change in the target file.
*   **Skip**: Ignores the current change and moves to the next.
*   **Back**: Moves to the previous modification.
*   **Undo**: Reverts the last applied change.
*   **Cancel**: Stops the process.

The add-in automatically navigates to the relevant file and highlights the code that will be changed, giving you full context for your decision.

#### The AI Modification Format

For the `Apply Modifications` add-in to work, the LLM must format its response using special blocks. Each block starts with `!MODIFICATION` and ends with `!END_MODIFICATION`. Inside is a TOML header with metadata and a standard markdown code block with the new code.

A typical response might look like this:

```
Here are the suggested changes to improve your package.

!MODIFICATION calculate_sum in R/utils.R
# This is a TOML block with metadata
scope = "function"
file = "R/utils.R"
function_name = "calculate_sum"
description = '''Update `calculate_sum` to handle NA values correctly.'''
---
# This is the code payload in a markdown fence
```r
#' Calculate the sum of a vector, ignoring NAs
calculate_sum <- function(vec) {
  sum(vec, na.rm = TRUE)
}
```
!END_MODIFICATION calculate_sum in R/utils.R

I have updated the function as requested.
```

The add-in can handle creating new files, rewriting entire files, replacing specific functions, or even modifying a few lines of code.

---

## Programmatic Usage

You can also use the core prompt-generation function directly in your R scripts:

```r
library(files2prompt)

# Generate a prompt from a TOML spec
prompt <- files2prompt(config_file = "f2p_config.toml")

# Print the prompt to the console
cat(prompt)

# Get a rough estimate of the token count
guess_token_num(prompt)
```

---

## How It Works: The Internals

*   **Prompt Generation (`f2p.R`)**: Parses the TOML specification, uses glob patterns to find files, and stitches them together using a templating system. It can even generate text summaries for data files (like `.csv` or `.rds`).
*   **Modification Parsing (`mod_parser.R`)**: Reads the AI response file (e.g., `ai_resp.md`) and uses regex to extract each `!MODIFICATION` block, separating the TOML metadata from the code payload.
*   **Target Location (`mod_loc.R`)**: For each modification, it determines the exact file and line numbers to edit. It can find a function's location using `utils::getParseData` or find arbitrary line sequences using exact or fuzzy string matching.
*   **Modification Application (`mod_addin.R`, `mod_apply.R`)**: A Shiny gadget (`mod_addin.R`) provides the UI for the review process. When you click "Apply", it uses the `rstudioapi` to directly and safely modify the contents of your source files in the editor (`mod_apply.R`).

---

## License

MIT License. © 2024 Sebastian Kranz.
```


{{mod_instruct}}


#######################################################
# YOUR TASK
#######################################################

Please describe the code base and propose improvements.

If you propose code changes also follow the format described in mod_instruct.md but only use file scope or function scope modifications. Function scope only for a single created / modified function in an R file. Otherwise file scope.


'''

task = '''

#######################################################
# YOUR TASK
#######################################################

Please describe the code base and propose improvements.
'''

###########################################################
# file_template Will be applied to each found file
# and the results will be pasted together
###########################################################

file_template = '''

# FILE: {{filename}}
```
{{filetext}}
```
# END OF FILE: {{filename}}

-----------------------------------------------------------
'''


################################################
# Special handling for R code
################################################
[[R]]
include_files  = '''
R/*.R
R/*.r
'''

[[README]]
include_files = "README.md"
group_template = '''
################################################
# Readme file
################################################
{{files}}
'''
file_template = '''
```md
{{filetext}}
```
'''

[[DESCRIPTION]]
include_files = "DESCRIPTION"
file_template = '''
```
{{filetext}}
```
'''
```
!END_MODIFICATION inst/toml/f2p_r_pkg.toml
!MODIFICATION inst/toml/f2p_simple_r_files.toml
scope = "file"
file = "inst/toml/f2p_simple_r_files.toml"
is_new_file = false
description = '''Add opt_prompt_file option to the simple R files TOML config, with comments explaining its use.'''
---
```toml
# A simple prompt that includes all R files

# By default, the generated prompt is saved to a temporary file.
# You can change this by uncommenting and editing the line below.
# To save to the project root, use a simple filename like:
# opt_prompt_file = "my_prompt.md"
opt_prompt_file = "<tempdir>/files2prompt.md"

include_files  = '''
*.R
*.r
'''
```
!END_MODIFICATION inst/toml/f2p_simple_r_files.toml
