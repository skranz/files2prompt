# files2prompt

Turn a folder full of code and text files into a single prompt string. Has an RStudio Addin and can be customized using **TOML** specs.

---

## Installation (r‑universe)
```r
install.packages(
  "files2prompt",
  repos = c("https://skranz.r-universe.dev", getOption("repos"))
)
```
Or directly from Github:

```
remotes::install_github("skranz/files2prompt")
```
---

## 1. Usage & RStudio Add‑in (default specs)

1. Open your project in RStudio.
2. **Addins ▸ FILES2PROMPT ▸ Files To Prompt** → the add‑in will
   * build the prompt containing the content of the code files of your R project
     using a default spec or custom spec (see below) 
   * save it as **`files2prompt.txt`** in the project directory (or working directory)
   * open the file in a new tab, select all text, **and**
   * copy it to your clipboard (if the optional **clipr** package is installed).

3. You can then copy & paste it to an LLM interface and possibly manually adapt


## 2. Custom specs

### 2.1 A first custom spec

You can customize the prompt generation and also use the package programmatically. Customiztaion use based on [TOML](https://toml.io/en/) specifications. To get started, you can create a simple config file called **`f2p_my_spec.toml`** in your project root:

```toml
include_files = '''
*.R         
*.Rmd
'''

# E.g. exclude some Rmd that your file convention 
# marks as old versions
exclude_files = '''
*_old.Rmd
'''

```

This will paste every R and Rmd file (unless those marked in exclude_files) into the prompt. If you now call the Addin **Files To Prompt** it will by default use the first file matching the glob `f2p_*.toml` in the project folder as specification to build the prompt. If no R project is open it will search the working directory. 

The root_directory to search for R files is the project directory or working directory, respectively.

### 2.2 Using the package in an R script

The `files2prompt` function is the main interface:

```r
library(files2prompt)
prompt = files2prompt(config_file="f2p_my_spec.toml", root_dir = getwd())

cat(prompt) 
guess_token_num(prompt)
```
---

## 2.3 More complex TOML specs

The TOML specs can be customized further. The `inst/toml` folder contains commented examples. You can customize the generated prompts, have different formats for certain file groups etc. 

## 3 Possible extensions

Perhaps adding possibility to add data set files in certain text representations formats...

© 2025 Sebastian Kranz — MIT License

