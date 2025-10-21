#' Register main language in Quarto config
#'
#' @param main_language Main language code (character, like `"en"`)
#' @inheritParams render_book
#' @return Nothing, called for its side-effect of editing the Quarto configuration file.
#' @examplesIf interactive()
#' parent_dir <- withr::local_tempdir()
#' quarto_multilingual_book(
#'     parent_dir = parent_dir,
#'     project_dir = "blop",
#'     further_languages = c("es", "fr"),
#'     main_language = "en",
#'     register_languages = FALSE
#'   )
#' book_path <- file.path(parent_dir, "blop")
#' register_main_language("en", book_path)
#' # have a look at the config
#' file.edit(file.path(parent_dir, "blop", "_quarto.yml"))
#' @export
register_main_language <- function(main_language = "en", project_path = ".") {
  config_path <- file.path(project_path, "_quarto.yml")

  config <- read_yaml(config_path)
  if (!is.null(config[["babelquarto"]][["mainlanguage"]])) {
    if (config[["babelquarto"]][["mainlanguage"]] != main_language) {
      cli::cli_abort(c(
        "Can't register {main_language} as main language.",
        'Main language registered as {config[["babelquarto"]][["mainlanguage"]]}.' # nolint: line_length_linter
      ))
    } else {
      cli::cli_alert_info("Main language already registered.")
      return(invisible())
    }
  }

  config_lines <- brio::read_lines(config_path)

  # avoid more than one empty lines at the end
  if (!nzchar(utils::tail(config_lines, n = 1))) {
    reps <- rle(config_lines)[["lengths"]]
    how_many_empty <- reps[length(reps)]
    if (how_many_empty > 1) {
      config_lines <- config_lines[
        1:(length(config_lines) - how_many_empty + 1)
      ]
    }
  } else {
    config_lines <- c(config_lines, "")
  }

  mainlang_config <- c(
    "  languagecodes:",
    sprintf("  - name: %s", main_language),
    sprintf('    text: "Version in %s"', main_language),
    sprintf("  mainlanguage: '%s'", main_language)
  )
  no_lang_exists <- !any(grepl("lang:", config_lines, fixed = TRUE))
  if (no_lang_exists) {
    mainlang_config <- c(mainlang_config, sprintf("lang: %s", main_language))
  }
  babelquarto_config_exists <- any(
    grepl("babelquarto:", config_lines, fixed = TRUE)
  )
  if (babelquarto_config_exists) {
    where_langs <- grep("  languagelinks:", config_lines, fixed = TRUE)
    config_lines <- append(config_lines, mainlang_config, after = where_langs)
  } else {
    mainlang_config <- c("babelquarto:", mainlang_config)
    config_lines <- c(config_lines, mainlang_config)
  }
  brio::write_lines(config_lines, path = config_path)

  cli_alert_success(
    "Added configuration for {main_language} to {.path config_path}."
  )

  invisible(config_path)
}

#' Register further languages in Quarto config
#'
#' @param further_languages Languages to be registered (character vector)
#' @inheritParams render_book
#'
#' @return Nothing, called for its side-effect of editing the Quarto configuration file.
#' @examplesIf interactive()
#' parent_dir <- withr::local_tempdir()
#'   quarto_multilingual_book(
#'     parent_dir = parent_dir,
#'     project_dir = "blop",
#'     further_languages = c("es", "fr"),
#'     main_language = "en"
#'   )
#' register_further_languages(
#'   "pt",
#'   project_path = file.path(parent_dir, "blop")
#' )
#' # have a look at the configuration
#' file.edit(file.path(parent_dir, "blop", "_quarto.yml"))
#' @export
#'
register_further_languages <- function(further_languages, project_path = ".") {
  config_path <- file.path(project_path, "_quarto.yml")
  config <- read_yaml(config_path)

  if (is.null(config[["babelquarto"]][["mainlanguage"]])) {
    cli::cli_abort(
      c(
        "Can't register further languages before a main language",
        i = "Call {.fun register_main_language} first."
      )
    )
  }
  languages_config_present <- !is.null(config[["babelquarto"]][["languages"]])
  if (
    languages_config_present &&
      all(further_languages %in% config[["babelquarto"]][["languages"]])
  ) {
    cli_alert_info("All languages already registered.")
    return(invisible())
  }

  config_lines <- brio::read_lines(config_path)
  where_langs <- grep("languages:", config_lines, invert = TRUE, fixed = TRUE)
  config_lines <- config_lines[where_langs]
  which_main <- grep("mainlanguage\\:", trimws(config_lines)) # nolint: fixed_regex_linter
  languages <- sprintf(
    "'%s'",
    union(config[["babelquarto"]][["languages"]], further_languages)
  )

  config_lines <- append(
    config_lines,
    c(
      sprintf("  languages: [%s]", toString(languages)),
      sprintf("title-%s: title in %s", further_languages, further_languages),
      sprintf(
        "description-%s: description in %s",
        further_languages,
        further_languages
      ), # nolint: line_length_linter
      sprintf("author-%s: author in %s", further_languages, further_languages)
    ),
    after = which_main
  )

  which_codes <- which(trimws(config_lines) == "languagecodes:")
  config_lines <- append(
    config_lines,
    unlist(purrr::map(
      further_languages,
      ~ list(
        sprintf("  - name: %s", .x),
        sprintf('    text: "Version in %s"', .x)
      )
    )),
    after = which_codes
  )

  brio::write_lines(config_lines, path = config_path)

  cli_alert_success(
    "Added configuration for use{toString(further_languages)} to {.path config_path}."
  )

  invisible(config_path)
}
