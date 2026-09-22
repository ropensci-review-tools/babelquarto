lang_code_chapter_list <- function(chapters_list, language_code) {
  chapters_list <- gsub(
    "\\.Rmd", # nolint: fixed_regex_linter
    sprintf(".%s.Rmd", language_code),
    chapters_list
  )

  chapters_list <- gsub(
    "\\.qmd", # nolint: fixed_regex_linter
    sprintf(".%s.qmd", language_code),
    chapters_list
  )

  chapters_list <- gsub(
    "\\.ipynb", # nolint: fixed_regex_linter
    sprintf(".%s.ipynb", language_code),
    chapters_list
  )

  return(chapters_list)
}

trim_end <- function(config_lines) {
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

  return(config_lines)
}

# return a existing configuration file based on highest-priority supplied profile
# note: first profile is highest priority, see https://github.com/orgs/quarto-dev/discussions/14612
config_file <- function(proj_path, profile) {
  config_path <-
    profile |>
    purrr::keep(nzchar) |>
    c("") |> # default profile last
    purrr::map(\(p) {
      c("yml", "yaml") |> # quarto config files can take on either extension
        purrr::map(\(ext) {
          f_name <- ifelse(nzchar(p), paste0("_quarto-", p), "_quarto")
          f_path <- fs::path(proj_path, f_name, ext = ext)
          ifelse(file.exists(f_path), f_path, "")
        })
    }) |>
    unlist() |>
    purrr::keep(nzchar)

  if (length(config_path) == 0L) {
    cli::cli_abort(
      "No specified profile ({profile}) or default configuration file (e.g. _quarto.yml) found in {proj_path}"
    ) # nolint: line_length_linter
  }
  config_path[1]
}

read_version_text <- function(config, language_code) {
  languagecodes_config <- config[["babelquarto"]][["languagecodes"]]

  if (is.null(languagecodes_config)) {
    return(default_version_text(language_code))
  }

  languagecodes_config$text[languagecodes_config$name == language_code]
}

default_version_text <- function(language_code) {
  sprintf("Version in %s", toupper(language_code))
}
