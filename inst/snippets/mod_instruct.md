## Format of proposed changes to code or text files

When you suggest changes to code or text files or completely new files, you MUST use the following format for each modification. Each change must be enclosed in a `!MODIFICATION` block.

### Overall Structure

Each modification block has three parts:
1.  Start and end markers: `!MODIFICATION {{what}}` and `!END_MODIFICATION {{what}}`, where {{what}} is just a short reference to what is modified, file name or function name with with file. It will not be parsed but makes it easier for a human to understand blocks.
2.  A metadata block in TOML format. This block ends with a `---` separator line.
3.  A code payload block, which is a standard markdown code fence.
