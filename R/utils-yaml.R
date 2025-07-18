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
