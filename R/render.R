#' Render a Quarto multilingual project
#'
#' @importFrom rlang `%||%`
#'
#' @details babelquarto expects a book/website folder with
#' each qmd/Rmd present in as many languages as needed,
#' with the same basename but,
#' - once with only `.qmd` as extension for the main language,
#' - once with `.es.qmd` (using the language code) for each other language.
#'
#' You also need to register the language in the configuration file,
#' see [babelquarto::register_main_language()]
#' and [babelquarto::register_further_languages()]:
#'
#' ```yaml
#' babelquarto:
#'   mainlanguage: 'en'
#'   languages: ['es', 'fr']
#' ```
#'
#' @importFrom rlang `%||%`
#'
#' @param project_path Path where the book/website source is located
#' @param site_url Base URL of the book/website.
#'
#' @export
#'
#' @examples
#' directory <- withr::local_tempdir()
#' quarto_multilingual_book(parent_dir = directory, project_dir = "blop")
#' render_book(file.path(directory, "blop"))
#' \dontrun{
#' if (require("servr") && rlang::is_interactive()) {
#'   servr::httw(file.path(directory, "blop", "_book"))
#' }
#' }
#'
#' @rdname render
render_book <- function(project_path = ".", site_url = NULL) {
  render(path = project_path, site_url = site_url, type = "book")
}
#' @export
#' @rdname render
render_website <- function(project_path = ".", site_url = NULL) {
  render(path = project_path, site_url = site_url, type = "website")
}
render <- function(path = ".", site_url = NULL, type = c("book", "website")) {
  # configuration ----
  config <- file.path(path, "_quarto.yml")
  config_contents <- yaml::read_yaml(config)

  if (is.null(site_url)) {
    if (nzchar(Sys.getenv("BABELQUARTO_TESTS_URL")) || !on_ci()) {
      site_url <- site_url %||% config_contents[[type]][["site-url"]] %||% ""
      site_url <- sub("/$", "", site_url)
    } else {
      # no end slash
      # for deploy previews
      # either root website (Netlify deploys)
      # or something else
      site_url <- Sys.getenv("BABELQUARTO_CI_URL", "")
    }
  }

  output_dir <- config_contents[["project"]][["output-dir"]] %||%
    switch(
      type,
      book = "_book",
      website = "_site"
    )

  language_codes <- config_contents[["babelquarto"]][["languages"]]
  if (is.null(language_codes)) {
    cli::cli_abort("Can't find {.field babelquarto/languages} in {.field _quarto.yml}")
  }
  main_language <- config_contents[["babelquarto"]][["mainlanguage"]]
  if (is.null(main_language)) {
    cli::cli_abort("Can't find {.field babelquarto/mainlanguage} in {.field _quarto.yml}")
  }

  output_folder <- file.path(path, output_dir)
  if (fs::dir_exists(output_folder)) fs::dir_delete(output_folder)

  # render project ----
  temporary_directory <- withr::local_tempdir()
  fs::dir_copy(path, temporary_directory)
  withr::with_dir(file.path(temporary_directory, fs::path_file(path)), {
    fs::file_delete(fs::dir_ls(regexp = "\\...\\.qmd"))
    quarto::quarto_render(as_job = FALSE)
  })
  fs::dir_copy(
    file.path(temporary_directory, fs::path_file(path), output_dir),
    path
  )

  purrr::walk(
    language_codes,
    render_quarto_lang,
    path = path,
    output_dir = output_dir,
    type = type
  )

  # Add the language switching link to the sidebar ----
  ## For the main language ----
  purrr::walk(
    language_codes,
    ~ purrr::walk(
      fs::dir_ls(output_folder, glob = "*.html"),
      add_link,
      main_language = main_language,
      language_code = .x,
      site_url = site_url,
      type = type,
      config = config_contents
    )
  )

  ## For other languages ----
  for (other_lang in language_codes) {
    languages_to_add <- c(main_language, language_codes[language_codes != other_lang])
    purrr::walk(
      languages_to_add,
      ~ purrr::walk(
        fs::dir_ls(file.path(output_folder, other_lang), glob = "*.html"),
        add_link,
        main_language = main_language,
        language_code = .x,
        site_url = site_url,
        type = type,
        config = config_contents
      )
    )
  }

}

render_quarto_lang <- function(language_code, path, output_dir, type) {

  temporary_directory <- withr::local_tempdir()
  fs::dir_copy(path, temporary_directory)
  project_name <- fs::path_file(path)

  config <- yaml::read_yaml(file.path(temporary_directory, project_name, "_quarto.yml"))
  config$lang <- language_code
  config[[type]][["title"]] <- config[[sprintf("title-%s", language_code)]] %||% config[[type]][["title"]]
  config[[type]][["description"]] <- config[[sprintf("description-%s", language_code)]] %||% config[[type]][["description"]]

  # overwrite some fields with language-specific fields
  language_specific_fields <- config[[sprintf("babelquarto-%s", language_code)]]
  if (length(language_specific_fields) > 0) {
    config <- config[names(config) != sprintf("babelquarto-%s", language_code)]
    config <- utils::modifyList(config, language_specific_fields)
  }

  if (type == "book") {
    config[[type]][["author"]] <- config[[sprintf("author-%s", language_code)]] %||% config[[type]][["author"]]
    config[["book"]][["chapters"]] <- purrr::map(
      config[["book"]][["chapters"]],
      use_lang_chapter,
      language_code = language_code,
      book_name = project_name,
      directory = temporary_directory
    )
    yaml::write_yaml(config, file.path(temporary_directory, project_name, "_quarto.yml"))
  }

  if (type == "website") {

    # only keep what's needed
    qmds <- fs::dir_ls(
      file.path(temporary_directory, fs::path_file(path)),
      glob = "*.qmd"
    )
    language_qmds <- qmds[grepl(sprintf("%s.qmd", language_code), qmds)]
    fs::file_delete(qmds[!(qmds %in% language_qmds)])
    for (qmd_path in language_qmds) {
      fs::file_move(
        qmd_path,
        sub(sprintf("%s.qmd", language_code), "qmd", qmd_path)
      )
    }
    # Replace TRUE and FALSE with 'true' and 'false'
    # to avoid converting to "yes" and "no"
    config <- replace_true_false(config)
    
    yaml::write_yaml(config, file.path(temporary_directory, project_name, "_quarto.yml"))
  }

  config_lines <- brio::read_lines(file.path(temporary_directory, project_name, "_quarto.yml"))
  brio::write_lines(config_lines, file.path(temporary_directory, project_name, "_quarto.yml"))

  # Render language book
  withr::with_dir(file.path(temporary_directory, project_name), {
    quarto::quarto_render(as_job = FALSE)
  })

  # Copy it to local not temporary _book/<language-code>
  fs::dir_copy(
    file.path(temporary_directory, project_name, output_dir),
    file.path(path, output_dir, language_code)
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

add_link <- function(path, main_language = main_language, language_code, site_url, type, config) {
  html <- xml2::read_html(path)

  codes <- config[["babelquarto"]][["languagecodes"]]
  current_lang <- purrr::keep(codes, ~.x[["name"]] == language_code)

  version_text <- if (length(current_lang) > 0) {
    current_lang[[1]][["text"]] %||%
      sprintf("Version in %s", toupper(language_code))
  } else {
    sprintf("Version in %s", toupper(language_code))
  }

  if (language_code == main_language) {
    new_path <-  if (type == "book") {
      sub("\\..*\\.html", ".html", basename(path))
    } else {
      basename(path)
    }
    href <- sprintf("%s/%s", site_url, new_path)
  } else {
    base_path <- sub("\\..*\\.html", ".html", basename(path))
    new_path <- if (type == "book") {
      fs::path_ext_set(base_path, sprintf(".%s.html", language_code))
    } else {
      base_path
    }
    href <- sprintf("%s/%s/%s",site_url, language_code, new_path)
  }

  if (type == "book") {

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

    xml2::xml_add_child(
      languages_links,
      "a",
      version_text,
      class = "toc-action",
      href = href,
      id = sprintf("language-link-%s", language_code)
    )

    just_added_link <- xml2::xml_find_first(html, sprintf("//a[@id='language-link-%s']", language_code))
    xml2::xml_add_parent(just_added_link, "li", id = sprintf("language-link-li-%s", language_code))

    just_added_link_item <- xml2::xml_find_first(html, sprintf("//li[@id='language-link-li-%s']", language_code))
    xml2::xml_add_child(just_added_link_item, "span", " ", .where = 0)
    xml2::xml_add_child(just_added_link_item, "i", class = "bi bi-globe2", .where = 0)
  } else {
    navbar <- xml2::xml_find_first(html, "//div[@id='navbarCollapse']")
    xml2::xml_add_child(
      navbar,
      "a",
      style = "text-decoration: none; color:inherit;",
      version_text,
      class = "nav-item",
      href = href,
      id = sprintf("language-link-%s", language_code),
      .where = 0
    )
  }

  xml2::write_html(html, path)
}

# as in testthat
on_ci <- function() {
  isTRUE(as.logical(Sys.getenv("CI", "false")))
}
