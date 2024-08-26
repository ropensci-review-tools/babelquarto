read_yaml <- function(path) {
  yaml::read_yaml(path, handlers = list(seq = function(x) x))
}
