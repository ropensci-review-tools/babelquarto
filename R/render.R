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
  config_contents <- yaml::read_yaml(config, handlers = list(seq = function(x) x))

  if (is.null(site_url)) {
    if (nzchar(Sys.getenv("BABELQUARTO_TESTS_URL")) || !on_ci()) {
      site_url <- site_url %||% config_contents[[type]][["site-url"]] %||% ""
    } else {
      # no end slash
      # for deploy previews
      # either root website (Netlify deploys)
      # or something else
      site_url <- Sys.getenv("BABELQUARTO_CI_URL", "")
    }
  }
  site_url <- sub("/$", "", site_url)

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
    fs::file_delete(fs::dir_ls(regexp = "\\...\\.qmd", recurse = TRUE))
    metadata <- list("true")
    names(metadata) <- sprintf("lang-%s", main_language)
    quarto::quarto_render(as_job = FALSE, metadata = metadata)
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

  # we need to recurse but not inside the language folders!
  all_docs <- fs::dir_ls(output_folder, glob = "*.html", recurse = TRUE)
  other_language_docs <- unlist(
    purrr::map(
      language_codes,
      ~fs::dir_ls(file.path(output_folder, .x), glob = "*.html", recurse = TRUE)
    )
  )
  main_language_docs <- setdiff(all_docs, other_language_docs)

  purrr::walk(
    language_codes,
    ~ purrr::walk(
      main_language_docs,
      add_links,
      main_language = main_language,
      language_code = .x,
      site_url = site_url,
      type = type,
      config = config_contents,
      output_folder = output_folder,
      path_language = main_language
    )
  )
  purrr::walk(
      main_language_docs,
      add_cross_links,
      main_language = main_language,
      site_url = site_url,
      config = config_contents,
      output_folder = output_folder,
      path_language = main_language)
  ## For other languages ----
  for (other_lang in language_codes) {
    other_lang_docs <- fs::dir_ls(
      file.path(output_folder, other_lang),
      glob = "*.html", recurse = TRUE
    )
    languages_to_add <- c(main_language, setdiff(language_codes, other_lang))
    purrr::walk(
      languages_to_add,
      ~ purrr::walk(
        other_lang_docs,
        add_links,
        main_language = main_language,
        language_code = .x,
        site_url = site_url,
        type = type,
        config = config_contents,
        output_folder = output_folder,
        path_language = other_lang
      )
    )
    purrr::walk(
      other_lang_docs,
      add_cross_links,
      main_language = main_language,
      site_url = site_url,
      config = config_contents,
      output_folder = output_folder,
      path_language = other_lang
    )
  }

}

render_quarto_lang <- function(language_code, path, output_dir, type) {

  temporary_directory <- withr::local_tempdir()
  fs::dir_copy(path, temporary_directory)
  project_name <- fs::path_file(path)

  config <- yaml::read_yaml(
    file.path(temporary_directory, project_name, "_quarto.yml"),
    handlers = list(seq = function(x) x)
  )
  config$lang <- language_code
  config[[type]][["title"]] <- config[[sprintf("title-%s", language_code)]] %||% config[[type]][["title"]]
  config[[type]][["subtitle"]] <- config[[sprintf("subtitle-%s", language_code)]] %||% config[[type]][["subtitle"]]
  config[[type]][["description"]] <- config[[sprintf("description-%s", language_code)]] %||% config[[type]][["description"]]

  if (type == "book") {
    config[[type]][["author"]] <- config[[sprintf("author-%s", language_code)]] %||% config[[type]][["author"]]

    config[["book"]][["chapters"]] <- purrr::map(
      config[["book"]][["chapters"]],
      use_lang_chapter,
      language_code = language_code,
      book_name = project_name,
      directory = temporary_directory
    )
    config[["book"]][["appendices"]] <- purrr::map(
      config[["book"]][["appendices"]],
      use_lang_chapter,
      language_code = language_code,
      book_name = project_name,
      directory = temporary_directory
    )
    # Replace TRUE and FALSE with 'true' and 'false'
    # to avoid converting to "yes" and "no"
    config <- replace_true_false(config)
    yaml::write_yaml(config, file.path(temporary_directory, project_name, "_quarto.yml"))
  }

  if (type == "website") {

    # only keep what's needed
    qmds <- fs::dir_ls(
      file.path(temporary_directory, fs::path_file(path)),
      glob = "*.qmd",
      recurse = TRUE
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
  metadata <- list("yes")
  names(metadata) <- sprintf("lang-%s", language_code)
  withr::with_dir(file.path(temporary_directory, project_name), {
    quarto::quarto_render(
      as_job = FALSE,
      metadata = metadata
    )
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
        unlist(original_chapters_list$chapters[chapters_not_translated]),
        gsub("\\.Rmd", sprintf(".%s.Rmd", language_code) ,
          gsub(
            "\\.qmd", sprintf(".%s.qmd", language_code),
            original_chapters_list$chapters[chapters_not_translated])
        )
      )
    }

    if (length(chapters_list$chapters) == 1) {
      chapters_list$chapters <- as.list(chapters_list$chapters) # https://github.com/ropensci-review-tools/babelquarto/issues/32
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

add_links <- function(path, main_language = main_language,
                     language_code, site_url, type, config, output_folder,
                     path_language) {
  html <- xml2::read_html(path)

  document_path <- path

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
      sub(
        "\\...\\.html", ".html",
        path_rel(path, output_folder, path_language, main_language)
      )
    } else {
      path_rel(path, output_folder, path_language, main_language)
    }
    href <- sprintf("%s/%s", site_url, new_path)
    no_translated_version <- !fs::file_exists(file.path(output_folder, new_path))
    if (no_translated_version) return()
  } else {
    base_path <- sub(
      "\\..\\.html", ".html",
      path_rel(path, output_folder, path_language, main_language)
    )
    new_path <- if (type == "book") {
      fs::path_ext_set(base_path, sprintf(".%s.html", language_code))
    } else {
      base_path
    }
    href <- sprintf("%s/%s/%s", site_url, language_code, new_path)
    no_translated_version <- !fs::file_exists(file.path(output_folder, language_code, new_path))
    if (no_translated_version) return()
  }

  if (type == "book") {

    sidebar_menu <- xml2::xml_find_first(html, "//div[contains(@class,'sidebar-menu-container')]")

    languages_links <- xml2::xml_find_first(html, "//ul[@id='languages-links']")
    languages_links_div_exists <- (length(languages_links) > 0)

    if (!languages_links_div_exists) {
      xml2::xml_add_sibling(
        sidebar_menu,
        "div",
        class = "dropdown",
        id = "languages-links-parent",
        .where = "before"
      )

      parent <- xml2::xml_find_first(html, "//div[@id='languages-links-parent']")
      xml2::xml_add_child(
        parent,
        "button",
        "",
        class = "btn btn-primary dropdown-toggle",
        type="button",
        `data-bs-toggle` = "dropdown",
        `aria-expanded` = "false",
        id = "languages-button"
      )

      xml2::xml_add_child(
        xml2::xml_find_first(html, "//button[@id='languages-button']"),
        "i",
        class = "bi bi-globe2"
      )

      xml2::xml_add_child(
        parent,
        "ul",
        class = "dropdown-menu",
        id = "languages-links"
      )

      languages_links <- xml2::xml_find_first(html, "//ul[@id='languages-links']")
    }

    xml2::xml_add_child(
      languages_links,
      "a",
      version_text,
      class = "dropdown-item",
      href = href,
      id = sprintf("language-link-%s", language_code)
    )
    xml2::xml_add_parent(
      xml2::xml_find_first(html, sprintf("a[id='%s']", sprintf("language-link-%s", language_code))),
      "li"
    )

  } else {

    languages_links <- xml2::xml_find_first(html, "//ul[@id='languages-links']")
    languages_links_div_exists <- (length(languages_links) > 0)

    if (!languages_links_div_exists) {
      navbar <- xml2::xml_find_first(html, "//div[@id='navbarCollapse']")

      xml2::xml_add_child(
        navbar,
        "div",
        class = "dropdown",
        id = "languages-links-parent",
        .where = 0
      )

      parent <- xml2::xml_find_first(html, "//div[@id='languages-links-parent']")
      xml2::xml_add_child(
        parent,
        "button",
        "",
        class = "btn btn-primary dropdown-toggle",
        type="button",
        `data-bs-toggle` = "dropdown",
        `aria-expanded` = "false",
        id = "languages-button"
      )

      xml2::xml_add_child(
        xml2::xml_find_first(html, "//button[@id='languages-button']"),
        "i",
        class = "bi bi-globe2"
      )

      xml2::xml_add_child(
        parent,
        "ul",
        class = "dropdown-menu",
        id = "languages-links"
      )

      languages_links <- xml2::xml_find_first(html, "//ul[@id='languages-links']")
    }
    xml2::xml_add_child(
      languages_links,
      "a",
      version_text,
      class = "dropdown-item",
      href = href,
      id = sprintf("language-link-%s", language_code),
      .where = 0
    )
    xml2::xml_add_parent(
      xml2::xml_find_first(html, sprintf("//a[@id='language-link-%s']", language_code)),
      "li"
    )
  }

  xml2::write_html(html, document_path)


}

add_cross_links <- function(path,
                            path_language, main_language,
                            config, site_url, output_folder) {
  main_language_href <- if (path_language == main_language) {
    sprintf("%s/%s", site_url, fs::path_rel(path, start = output_folder))
  } else {
    sub(
      sprintf("%s\\.html", path_language), "html",
      sprintf("%s/%s", site_url, fs::path_rel(path, start = file.path(output_folder, path_language)))
    )
  }
  main_language_link <- sprintf(
    '<link rel="alternate" hreflang="%s" href="%s" />',
    main_language,
    main_language_href
  )

  create_other_language_link <- function(lang, main_language_href, site_url) {
    other_language_href <- sub(site_url, paste0(site_url, "/", lang), main_language_href)
    other_language_href <- sub(".html", paste0(".", lang, ".html"), other_language_href)
    sprintf(
      '<link rel="alternate" hreflang="%s" href="%s" />',
      lang,
      other_language_href
    )
  }
  other_language_links <- purrr::map_chr(
    config[["babelquarto"]][["languages"]],
    create_other_language_link,
    main_language_href = main_language_href,
    site_url = site_url
  )
  html_lines <- brio::read_lines(path)
  html_lines <- append(
    html_lines,
    c(main_language_link, other_language_links),
    after = 3
  )
  brio::write_lines(html_lines, path)
}

# as in testthat
on_ci <- function() {
  isTRUE(as.logical(Sys.getenv("CI", "false")))
}

path_rel <- function(path, output_folder, lang, main_language) {
  if (lang == main_language) {
    fs::path_rel(path, start = output_folder)
  } else {
    fs::path_rel(path, start = file.path(output_folder, lang))
  }
}
