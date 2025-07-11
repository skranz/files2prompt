mod_loc_file_path = function(mod, base_dir) {
  # Locate the full file path of the file specified in mod
  # store it in mod$file_path

  return(mod)

}

mod_loc_text_pos = function(mod) {
  # an insert without overwriting lines
  # corresponds to mod$line_end = NA
  # while mod$line_start is the line where we insert the text

  if (mod$scope == "file") {
    mod$line_start = NA
    mod$line_end = NA
    mod$edit_special = "all"
  } else if (mod$scope == "function") {
    mod = mod_loc_text_pos_function(mod)
  } else if (mod$scope == "lines") {
    mod = mod_loc_text_pos_lines(mod)
  }

}
