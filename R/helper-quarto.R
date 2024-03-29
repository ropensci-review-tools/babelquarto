#' Create a starter/example quarto multilingual book or website
#'
#' @param parent_dir Folder where to create the project folder.
#' @param project_dir Project (book, webiste) folder name.
#' @param further_languages Codes for not main languages.
#' @param main_language Code for main languages.
#' @param register_languages Whether to register languages (logical).
#' @param site_url Site URL for the book/site-url
#' or website/site-url part of the Quarto configuration.
#'
#' @return The path to the created project.
#' @export
#'
quarto_multilingual_book <- function(parent_dir,
                                     project_dir,
                                     main_language = "en",
                                     further_languages = c("es", "fr"),
                                     register_languages = TRUE,
                                     site_url = "https://example.com") {
  quarto_multilingual_project(
    parent_dir = parent_dir,
    project_dir = project_dir,
    type = "book",
    main_language = main_language,
    further_languages = further_languages,
    register_languages = register_languages,
    site_url = site_url
  )
}

#' @rdname quarto_multilingual_book
#' @export
quarto_multilingual_website <- function(parent_dir,
                                       project_dir,
                                       main_language = "en",
                                       further_languages = c("es", "fr"),
                                       register_languages = TRUE,
                                       site_url = "https://example.com") {
  quarto_multilingual_project(
    parent_dir = parent_dir,
    project_dir = project_dir,
    type = "website",
    main_language = main_language,
    further_languages = further_languages,
    register_languages = register_languages,
    site_url = site_url
  )
}

quarto_multilingual_project <- function(parent_dir,
                                       project_dir,
                                       type = c("book", "website"),
                                       main_language = "en",
                                       further_languages = c("es", "fr"),
                                       register_languages = TRUE,
                                       site_url = "https://example.com") {

  # Vanilla project from Quarto CLI ----
  if (parent_dir != getwd()) withr::local_dir(parent_dir)
  quarto_bin <- quarto::quarto_path()
  sys::exec_wait(
    quarto_bin,
    args = c("create-project", project_dir, "--type", type)
  )

  # Duplicated files for the different languages ----
  qmds <- dir(project_dir, pattern = "\\.qmd", full.names = TRUE)

  create_new_lang_file <- function(qmd_file, language) {
    new_path <- fs::path_ext_set(qmd_file, sprintf(".%s.qmd", language))
    fs::file_copy(qmd_file, new_path)
  }

  purrr::walk(
    further_languages,
    ~ purrr::walk(qmds, create_new_lang_file, language = .x)
  )

  # Config edits ----
  config_path <- file.path(project_dir, "_quarto.yml")
  config_lines <- brio::read_lines(config_path)

  where_project <- which(grepl(sprintf("%s:", type), config_lines))
  config_lines <- append(config_lines, sprintf("  site-url: %s", site_url), after = where_project)

  ## Remove LaTeX lines ----
  if (type == "book") {
    config_lines <- config_lines[1:(which(grepl("pdf:", config_lines)) - 1)]
  }

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
    register_main_language(main_language, project_path = project_dir)
    register_further_languages(further_languages, project_path = project_dir)
  }
  return(file.path(parent_dir, project_dir))
}
