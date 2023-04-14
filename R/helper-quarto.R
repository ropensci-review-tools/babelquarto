#' Create a starter/example quarto multilingual book
#'
#' @param parent_folder Folder where to create the book folder.
#' @param book_folder Book folder name.
#' @param further_languages Codes for not main languages.
#' @param main_language Code for main languages.
#'
#' @return Nothing, creates the book folder.
#' @export
#'
quarto_multilingual_book <- function(parent_folder,
                                     book_folder,
                                     further_languages = c("es", "fr"),
                                     main_language = "en") {

  # Vanilla book from Quarto CLI ----
  if (parent_folder != getwd()) withr::local_dir(parent_folder)
  quarto_bin <- quarto::quarto_path()
  sys::exec_wait(
    quarto_bin,
    args = c("create-project", book_folder, "--type", "book")
  )

  # Duplicated files for the different languages ----
  qmds <- dir(book_folder, pattern = "\\.qmd", full.names = TRUE)

  create_new_lang_file <- function(qmd_file, language) {
    new_path <- fs::path_ext_set(qmd_file, sprintf(".%s.qmd", language))
    fs::file_copy(qmd_file, new_path)
  }

  purrr::walk(
    further_languages,
    ~ purrr::walk(qmds, create_new_lang_file, language = .x)
  )

  # Config edits ----
  config <- file.path(book_folder, "_quarto.yml")
  config_lines <- brio::read_lines(config)

  ## Remove LaTeX lines ----
  config_lines <- config_lines[1:(which(grepl("pdf:", config_lines)) - 1)]

  ## "Register" languages ----
  config_lines <- c(
    config_lines,
    "",
    "babelquarto:",
    sprintf("  mainlanguage: '%s'", main_language),
    sprintf("  languages: [%s]", toString(sprintf("'%s'", further_languages)))
  )

  ## Save config
  brio::write_lines(config_lines, path = config)

}
