#' Render a Quarto multilingual book
#'
#' @importFrom rlang `%||%`
#'
#' @details babelquarto expects a book folder with
#' each qmd/Rmd present in as many languages as needed,
#' with the same basename but,
#' - once with only `.qmd` as extension for the main language,
#' - once with `.es.qmd` (using the language code) for each other language.
#'
#' You also need to register the language in the configuration file:
#'
#' ```yaml
#' babelquarto:
#'   mainlanguage: 'en'
#'   languages: ['es', 'fr']
#' ```
#'
#' @importFrom rlang `%||%`
#'
#' @param book_path Path where the book source is located
#' @param site_url Base URL of the book.
#'
#' @export
#'
#' @examples
#' directory <- withr::local_tempdir()
#' quarto_multilingual_book(parent_dir = directory, book_dir = "blop")
#' render_book(file.path(directory, "blop"))
#' \dontrun{
#' if (require("servr") && rlang::is_interactive()) {
#'   servr::httw(file.path(directory, "blop", "_book"))
#' }
#' }
#'
render_book <- function(book_path = ".", site_url = NULL) {
  # configuration ----
  config <- file.path(book_path, "_quarto.yml")
  config_contents <- yaml::read_yaml(config)

  if (is.null(site_url)) {
    if (nzchar("BABELQUARTO_TESTS_URL") || !on_ci()) {
      site_url <- site_url %||% config_contents[["book"]][["site-url"]] %||% ""
      site_url <- sub("/$", "", site_url)
    } else {
      # no end slash
      # for deploy previews
      # either root website (Netlify deploys)
      # or something else
      site_url <- Sys.getenv("BABELQUARTO_CI_URL", "")
    }
  }

  output_dir <- config_contents[["project"]][["output-dir"]] %||% "_book"

  language_codes <- config_contents[["babelquarto"]][["languages"]]
  if (is.null(language_codes)) {
    cli::cli_abort("Can't find {.field babelquarto/languages} in {.field _quarto.yml}")
  }
  main_language <- config_contents[["babelquarto"]][["mainlanguage"]]
  if (is.null(main_language)) {
    cli::cli_abort("Can't find {.field babelquarto/mainlanguage} in {.field _quarto.yml}")
  }

  book_output_folder <- file.path(book_path, output_dir)
  if (fs::dir_exists(book_output_folder)) fs::dir_delete(book_output_folder)

  # render book ----
  withr::with_dir(book_path, {
    quarto::quarto_render(as_job = FALSE)
  })

  purrr::walk(
    language_codes,
    render_quarto_lang_book,
    book_path = book_path,
    output_dir = output_dir
  )

  # Add the language switching link to the sidebar ----
  ## For the main language ----
  purrr::walk(
    language_codes,
    ~ purrr::walk(
      fs::dir_ls(book_output_folder, glob = "*.html"),
      add_link,
      main_language = main_language,
      language_code = .x,
      site_url = site_url
    )
  )

  ## For other languages ----
  for (other_lang in language_codes) {
    languages_to_add <- c(main_language, language_codes[language_codes != other_lang])
    purrr::walk(
      languages_to_add,
      ~ purrr::walk(
        fs::dir_ls(file.path(book_output_folder, other_lang), glob = "*.html"),
        add_link,
        main_language = main_language,
        language_code = .x,
        site_url = site_url
        )
    )
  }

}

render_quarto_lang_book <- function(language_code, book_path, output_dir) {

  temporary_directory <- withr::local_tempdir()
  fs::dir_copy(book_path, temporary_directory)
  book_name <- fs::path_file(book_path)

  config <- yaml::read_yaml(file.path(temporary_directory, book_name, "_quarto.yml"))
  config$lang <- language_code
  config[["book"]][["title"]] <- config[[sprintf("title-%s", language_code)]] %||% config[["book"]][["title"]]
  config[["book"]][["author"]] <- config[[sprintf("author-%s", language_code)]] %||% config[["book"]][["author"]]
  config[["book"]][["description"]] <- config[[sprintf("description-%s", language_code)]] %||% config[["book"]][["description"]]

  config$book$chapters <- purrr::map(
    config$book$chapters,
    use_lang_chapter,
    language_code = language_code,
    book_name = book_name,
    directory = temporary_directory
  )

  yaml::write_yaml(config, file.path(temporary_directory, book_name, "_quarto.yml"))

  # fix for Boolean that is yes and should be true
  config_lines <- brio::read_lines(file.path(temporary_directory, book_name, "_quarto.yml"))
  config_lines[grepl("code-link", config_lines)] <- sub("yes", "true", config_lines[grepl("code-link", config_lines)])
  config_lines[grepl("reader-mode", config_lines)] <- sub("yes", "true", config_lines[grepl("reader-mode", config_lines)])
  brio::write_lines(config_lines, file.path(temporary_directory, book_name, "_quarto.yml"))

  # Render language book
  withr::with_dir(file.path(temporary_directory, book_name), {
    quarto::quarto_render(as_job = FALSE)
  })

  # Copy it to local not temporary _book/<language-code>
  fs::dir_copy(
    file.path(temporary_directory, book_name, output_dir),
    file.path(book_path, output_dir, language_code)
  )

}

use_lang_chapter <- function(chapters_list, language_code, book_name, directory) {
  withr::local_dir(file.path(directory, book_name))

    original_chapters_list <- chapters_list

    if (is.list(chapters_list)) {
      # part translation
      chapters_list[["part"]] <- chapters_list[[sprintf("part-%s", language_code)]] %||%
        chapters_list[["part"]]

      # chapters translation

      chapters_list$chapters <- gsub("\\.Rmd", sprintf(".%s.Rmd", language_code), chapters_list$chapters)
      chapters_list$chapters <- gsub("\\.qmd", sprintf(".%s.qmd", language_code), chapters_list$chapters)
      if (any(!fs::file_exists(chapters_list$chapters))) {
        chapters_not_translated <- !fs::file_exists(chapters_list$chapters)
        fs::file_move(
          original_chapters_list$chapters[chapters_not_translated],
          gsub("\\.Rmd", sprintf(".%s.Rmd", language_code) ,
            gsub(
              "\\.qmd", sprintf(".%s.qmd", language_code),
              original_chapters_list$chapters[chapters_not_translated])
            )
        )
      }
    } else {
      chapters_list <- gsub("\\.Rmd", sprintf(".%s.Rmd", language_code), chapters_list)
      chapters_list <- gsub("\\.qmd", sprintf(".%s.qmd", language_code), chapters_list)
      if (!fs::file_exists(file.path(directory, book_name, chapters_list))) {
        fs::file_move(
          original_chapters_list,
          chapters_list
        )
      }
    }

    chapters_list
}

add_link <- function(path, main_language = main_language, language_code, site_url) {
  html <- xml2::read_html(path)

  left_sidebar <- xml2::xml_find_first(html, "//div[@class='sidebar-menu-container']")

  languages_links_div_exists <- (length(xml2::xml_find_first(html, "//div[@id='languages-links']")) > 0)

  if (!languages_links_div_exists) {
    xml2::xml_add_sibling(
      left_sidebar,
      "div",
      class = "sidebar-menu-container",
      id = "languages-links",
      .where = "before"
    )
    xml2::xml_add_child(
      xml2::xml_find_first(html, "//div[@id='languages-links']"),
      "ul",
      class = "list-unstyled mt-1",
      id = "language-links-ul"
    )
  }

  languages_links <- xml2::xml_find_first(html, "//ul[@id='language-links-ul']")

  if (language_code == main_language) {
    new_path <- sub("\\..*\\.html", ".html", basename(path))
    href <- sprintf("%s/%s", site_url, new_path)
  } else {
    base_path <- sub("\\..*\\.html", ".html", basename(path))
    new_path <- fs::path_ext_set(base_path, sprintf(".%s.html", language_code))
    href <- sprintf("%s/%s/%s",site_url, language_code, new_path)
  }

  xml2::xml_add_child(
    languages_links,
    "a",
    sprintf("Version in %s", toupper(language_code)),
    class = "toc-action",
    href = href,
    id = sprintf("language-link-%s", language_code)
  )

  just_added_link <- xml2::xml_find_first(html, sprintf("//a[@id='language-link-%s']", language_code))
  xml2::xml_add_parent(just_added_link, "li", id = sprintf("language-link-li-%s", language_code))

  just_added_link_item <- xml2::xml_find_first(html, sprintf("//li[@id='language-link-li-%s']", language_code))
  xml2::xml_add_child(just_added_link_item, "span", " ", .where = 0)
  xml2::xml_add_child(just_added_link_item, "i", class = "bi bi-globe2", .where = 0)

  xml2::write_html(html, path)
}

# as in testthat
on_ci <- function() {
  isTRUE(as.logical(Sys.getenv("CI", "false")))
}
