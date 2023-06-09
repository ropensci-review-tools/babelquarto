#' Create a starter/example quarto multilingual book
#'
#' @param parent_dir Folder where to create the book folder.
#' @param book_dir Book folder name.
#' @param further_languages Codes for not main languages.
#' @param main_language Code for main languages.
#' @param register_languages Whether to register languages (logical).
#'
#' @return Nothing, creates the book folder.
#' @export
#'
quarto_multilingual_book <- function(parent_dir,
                                     book_dir,
                                     main_language = "en",
                                     further_languages = c("es", "fr"),
                                     register_languages = TRUE) {

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
  config_path <- file.path(book_dir, "_quarto.yml")
  config_lines <- brio::read_lines(config_path)

  ## Remove LaTeX lines ----
  config_lines <- config_lines[1:(which(grepl("pdf:", config_lines)) - 1)]

  ## Change author ----
  author <- if (nzchar(Sys.getenv("QUARTOBABELAUTHOR"))) {
    Sys.getenv("QUARTOBABELAUTHOR")
  } else {
    whoami::fullname(fallback = "Firstname Lastname")
  }
  config_lines[grepl("author:", config_lines)] <- sprintf('  author: "%s"', author)

  ## Change date ----
  if (nzchar(Sys.getenv("QUARTOBABELDATE"))) {
    config_lines[grepl("date:", config_lines)] <- sprintf('  date: "%s"', Sys.getenv("QUARTOBABELDATE"))
  }

  brio::write_lines(config_lines, path = config_path)

  ## "Register" languages ----
  if (register_languages) {
    register_main_language(main_language, book_path = book_dir)
    register_further_languages(further_languages, book_path = book_dir)
  }

}
