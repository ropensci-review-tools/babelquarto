#' Alias for yaml::read_yaml with a custom handler
#'
#' @param path Path to yaml file
#' @return A list
#' @examples \dontrun{
#' read_yaml(path)
#' }
#' @dev
read_yaml <- function(path) {
  yaml::read_yaml(path, handlers = list(seq = function(x) x))
}

write_yaml <- function(yaml, path) {
  # Replace TRUE and FALSE with 'true' and 'false'
  # to avoid converting to "yes" and "no"
  yaml <- replace_true_false(yaml)
  yaml::write_yaml(yaml, path)
}

# Replace logical TRUE and FALSE with character "true" and "false"
# in a nested list
replace_true_false <- function(value) {
  if (is.list(value)) {
    value <- lapply(value, replace_true_false)
  } else if (is.logical(value)) {
    value <- tolower(as.character(value))
    # Set class of this character vector to 'verbatim'
    # so that yaml::write_yaml() will not add quotation marks
    class(value) <- "verbatim"
  }
  value
}
