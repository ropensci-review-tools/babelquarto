#' Create a starter/example quarto multilingual book
#'
#' @param parent_dir Folder where to create the book folder.
#' @param book_dir Book folder name.
#' @param further_languages Codes for not main languages.
#' @param main_language Code for main languages.
#'
#' @return Nothing, creates the book folder.
#' @export
#'
quarto_multilingual_book <- function(parent_dir,
                                     book_dir,
                                     further_languages = c("es", "fr"),
                                     main_language = "en") {

  # Vanilla book from Quarto CLI ----
  if (parent_dir != getwd()) withr::local_dir(parent_dir)
  quarto_bin <- quarto::quarto_path()
  sys::exec_wait(
    quarto_bin,
    args = c("create-project", book_dir, "--type", "book")
  )

  # Duplicated files for the different languages ----
  qmds <- dir(book_dir, pattern = "\\.qmd", full.names = TRUE)

  create_new_lang_file <- function(qmd_file, language) {
    new_path <- fs::path_ext_set(qmd_file, sprintf(".%s.qmd", language))
    fs::file_copy(qmd_file, new_path)
  }

  purrr::walk(
    further_languages,
    ~ purrr::walk(qmds, create_new_lang_file, language = .x)
  )

  # Config edits ----
  config <- file.path(book_dir, "_quarto.yml")
  config_lines <- brio::read_lines(config)

  ## Remove LaTeX lines ----
  config_lines <- config_lines[1:(which(grepl("pdf:", config_lines)) - 1)]

  ## "Register" languages ----
  config_lines <- c(
    config_lines,
    "",
    "babelquarto:",
    sprintf("  mainlanguage: '%s'", main_language),
    sprintf("  languages: [%s]", toString(sprintf("'%s'", further_languages))),
    sprintf("lang: %s", main_language),
    purrr::map_chr(further_languages, ~sprintf("title-%s: title in %s", .x, .x)),
    purrr::map_chr(further_languages, ~sprintf("description-%s: description in %s", .x, .x)),
    purrr::map_chr(further_languages, ~sprintf("author-%s: author in %s", .x, .x))
  )

  ## Save config
  brio::write_lines(config_lines, path = config)

}
