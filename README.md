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
