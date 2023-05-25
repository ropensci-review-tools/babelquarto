#' Register main language in Quarto config
#'
#' @param main_language Main language code (character, like `"en"`)
#' @inheritParams render_book
#' @return Nothing
#' @export
register_main_language <- function(main_language = "en", book_path = ".") {
  config_path <- file.path(book_path, "_quarto.yml")

  config <- yaml::read_yaml(config_path)
  if (!is.null(config[["babelquarto"]][["mainlanguage"]])) {
    if (config[["babelquarto"]][["mainlanguage"]] != main_language) {
      cli::cli_abort(
        c(
          "Can't register {main_language} as main language.",
          'Main language registered as {config[["babelquarto"]][["mainlanguage"]]}.'
        )
      )
    } else {
      cli::cli_alert_info("Main language already registered.")
      return(invisible())
    }
  }

  config_lines <- brio::read_lines(config_path)
  config_lines <- c(
    config_lines,
    "",
    "babelquarto:",
    sprintf("  mainlanguage: '%s'", main_language),
    sprintf("lang: %s", main_language)
  )
  brio::write_lines(config_lines, path = config_path)
}

#' Register further languages in Quarto config
#'
#' @param further_languages Languages to be registered (character vector)
#' @inheritParams render_book
#'
#' @return Nothing
#' @export
#'
register_further_languages <- function(further_languages, book_path = ".") {

  config_path <- file.path(book_path, "_quarto.yml")

  config <- yaml::read_yaml(config_path)
  if (!is.null(config[["babelquarto"]][["further_languages"]])) {
    if (all(further_languages %in% config[["babelquarto"]][["further_languages"]])) {
      cli::cli_alert_info("All languages already registered.")
      return(invisible())
    }
  }

  if (is.null(config[["babelquarto"]][["mainlanguage"]])) {
    cli::cli_abort(
      c(
        "Can't register further languages before a main language",
        i = "Call {.fun register_main_language} first."
      )
    )
  }

  config_lines <- brio::read_lines(config_path)
  which_main <- which(trimws(config_lines) == "babelquarto:") + 1
  config_lines <- append(
    config_lines,
    c(
      sprintf("  languages: [%s]", toString(sprintf("'%s'", further_languages))),
      purrr::map_chr(further_languages, ~sprintf("title-%s: title in %s", .x, .x)),
      purrr::map_chr(further_languages, ~sprintf("description-%s: description in %s", .x, .x)),
      purrr::map_chr(further_languages, ~sprintf("author-%s: author in %s", .x, .x))
    ),
    after = which_main
  )
  brio::write_lines(config_lines, path = config_path)
}
