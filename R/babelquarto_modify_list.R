babelquarto_modify_list <- function(original_list, new_list) {
  purrr::imap(original_list, combine_field, new_list)
  browser()
}

combine_field <- function(field, field_name, new_list) {
  if (rlang::is_named(field[["field_name"]])) {
    purrr::imap(field[["field_name"]], new_list[["field_name"]])
    # something
  }
  new_list[["field_name"]] %||% field
}
