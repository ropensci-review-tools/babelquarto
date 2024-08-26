# Replace logical TRUE and FALSE with character "true" and "false"
# in a nested list
replace_true_false <- function(list) {
  if (is.list(list)) {
    list <- lapply(list, replace_true_false)
  } else if (is.logical(list)) {
    list <- as.character(list)
    list <- gsub("TRUE", "true", list)
    list <- gsub("FALSE", "false", list)
    # Set class of this character vector to 'verbatim'
    # so that yaml::write_yaml() will not add quotation marks
    class(list) <- "verbatim"
  }
  return(list)
}

lang_code_chapter_list <- function(chapters_list, language_code) {
  chapters_list <- gsub(
    "\\.Rmd", sprintf(".%s.Rmd", language_code),
    chapters_list
  )

  gsub(
    "\\.qmd",
    sprintf(".%s.qmd", language_code),
    chapters_list
  )
}
