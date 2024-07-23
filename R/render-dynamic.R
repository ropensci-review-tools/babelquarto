#' Render a Quarto multilingual project 
#'
#' @importFrom rlang `%||%`
#'
#' @details babelquarto expects a book/website folder with
#' mixed qmd/Rmd present when chapter in language versions are different,
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
#' render_dynamic_book(file.path(directory, "blop"))
#' \dontrun{
#' if (require("servr") && rlang::is_interactive()) {
#'   servr::httw(file.path(directory, "blop", "_book"))
#' }
#' }
#'
#' @rdname render_dynamic
render_dynamic_book <- function(project_path = ".", site_url = NULL) {
  render_dynamic(path = project_path, site_url = site_url, type = "book")
}

#' @export
#' @rdname render_dynamic
render_dynamic_website <- function(project_path = ".", site_url = NULL) {
  render_dynamic(path = project_path, site_url = site_url, type = "website")
}

read_yaml_custom <- function(file) {
  string <- paste(readr::read_lines(file), collapse = "\n")
  yaml::yaml.load(string)
}


render_dynamic <- function(path = ".", site_url = NULL, type = c("book", "website")) {
  # configuration ----
  config <- file.path(path, "_quarto.yml")
  config_contents <- read_yaml_custom(config)
  
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

  #! Modify the _quarto.yml file to remove chapters with language extensions
  project_name <- fs::path_file(path)
  config <- read_yaml_custom(file.path(temporary_directory, project_name, "_quarto.yml"))
  
  if (type == "book") {
    # Remove chapters with language extensions and ensure chapters is a list
    config[["book"]][["chapters"]] <- purrr::map(config[["book"]][["chapters"]], function(chapter) {
      if (is.list(chapter) && !is.null(chapter$chapters)) {
        chapter$chapters <- chapter$chapters[!grepl("\\.[a-z]{2}\\.qmd$", chapter$chapters)]
        return(chapter)
      } else if (is.character(chapter)) {
        if (!grepl("\\.[a-z]{2}\\.qmd$", chapter)) {
          return(chapter)
        }
      }
    })
    
    # Remove any NULL entries in the list
    config[["book"]][["chapters"]] <- purrr::compact(config[["book"]][["chapters"]])
    
    # Ensure all chapters fields are lists of strings
    config[["book"]][["chapters"]] <- lapply(config[["book"]][["chapters"]], function(chapter) {
      if (is.character(chapter)) {
        return(chapter)
      } else if (is.list(chapter) && !is.null(chapter$chapters)) {
        chapter$chapters <- as.list(chapter$chapters)
        return(chapter)
      } else {
        return(chapter)
      }
    })
    
    # Replace TRUE and FALSE with 'true' and 'false'
    config <- replace_true_false(config)
    yaml::write_yaml(config, file.path(temporary_directory, project_name, "_quarto.yml"))
  }
  #! End of modification code

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
    render_quarto_lang_new,
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
      add_link,
      main_language = main_language,
      language_code = .x,
      site_url = site_url,
      type = type,
      config = config_contents,
      output_folder = output_folder
    )
  )
  
  ## For other languages ----
  for (other_lang in language_codes) {
    
    languages_to_add <- c(main_language, setdiff(language_codes, other_lang))
    purrr::walk(
      languages_to_add,
      ~ purrr::walk(
        fs::dir_ls(file.path(output_folder, other_lang),
                   glob = "*.html", recurse = TRUE
        ),
        add_link,
        main_language = main_language,
        language_code = .x,
        site_url = site_url,
        type = type,
        config = config_contents,
        output_folder = output_folder
      )
    )
  }
}


#! Modify the _quarto.yml file to keep only chapters with language extensions
render_quarto_lang_new <- function(language_code, path, output_dir, type) {
  temporary_directory <- withr::local_tempdir()
  fs::dir_copy(path, temporary_directory)
  project_name <- fs::path_file(path)
  
  config <- read_yaml_custom(file.path(temporary_directory, project_name, "_quarto.yml"))
  config$lang <- language_code
  config[[type]][["title"]] <- config[[sprintf("title-%s", language_code)]] %||% config[[type]][["title"]]
  config[[type]][["description"]] <- config[[sprintf("description-%s", language_code)]] %||% config[[type]][["description"]]
  
  if (type == "book") {
    config[[type]][["author"]] <- config[[sprintf("author-%s", language_code)]] %||% config[[type]][["author"]]
    
    #! Ensure index file for the specific language
    lang_index_file <- sprintf("index.%s.qmd", language_code)
    if (fs::file_exists(file.path(temporary_directory, project_name, lang_index_file))) {
      config[["book"]][["chapters"]][[1]] <- lang_index_file
    }
    
    config[["book"]][["chapters"]] <- purrr::map(
      config[["book"]][["chapters"]],
      function(chapter) {
        if (is.list(chapter) && !is.null(chapter$chapters)) {
          chapter$chapters <- chapter$chapters[grepl(paste0("index\\.", language_code, "\\.qmd$|\\.", language_code, "\\.qmd$"), chapter$chapters)]
          if (!is.null(chapter[[sprintf("part-%s", language_code)]])) {
            chapter$part <- chapter[[sprintf("part-%s", language_code)]]
          }
          return(chapter)
        } else if (is.character(chapter)) {
          if (grepl(paste0("index\\.", language_code, "\\.qmd$|\\.", language_code, "\\.qmd$"), chapter)) {
            return(chapter)
          }
        }
      }
    )
    
    config[["book"]][["chapters"]] <- purrr::compact(config[["book"]][["chapters"]])
    config[["book"]][["chapters"]] <- lapply(config[["book"]][["chapters"]], function(chapter) {
      if (is.character(chapter)) {
        return(chapter)
      } else if (is.list(chapter) && !is.null(chapter$chapters)) {
        chapter$chapters <- as.list(chapter$chapters)
        return(chapter)
      } else {
        return(chapter)
      }
    })
    
    config <- replace_true_false(config)
    yaml::write_yaml(config, file.path(temporary_directory, project_name, "_quarto.yml"))
  }
  
  if (type == "website") {
    # only keep what's needed
    qmds <- fs::dir_ls(
      file.path(temporary_directory, fs::path_file(path)),
      glob = "*.qmd"
    )
    language_qmds <- qmds[grepl(sprintf("%s.qmd", language_code), qmds) | grepl(sprintf("index\\.%s.qmd$", language_code), qmds)]
    
    # Check if language-specific index file exists and move it to replace index.qmd
    lang_index_file <- file.path(temporary_directory, fs::path_file(path), sprintf("index.%s.qmd", language_code))
    if (fs::file_exists(lang_index_file)) {
      fs::file_move(lang_index_file, file.path(temporary_directory, fs::path_file(path), "index.qmd"))
    }
    
    fs::file_delete(qmds[!(qmds %in% language_qmds)])
    for (qmd_path in language_qmds) {
      fs::file_move(
        qmd_path,
        sub(sprintf("%s.qmd", language_code), "qmd", qmd_path)
      )
    }
    
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

add_link <- function(path, main_language = main_language,
                     language_code, site_url, type, config, output_folder) {
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
  
  code_in_filename <- unlist(regmatches(path, gregexpr("\\...\\.html", path)))
  code_in_path <- unlist(regmatches(path, gregexpr(
    file.path(output_folder, "..", basename(path)),
    path
  )))
  
  if (length(code_in_filename) > 0) {
    file_lang <- sub("\\.", "", sub("\\.html", "", code_in_filename))
    path <- sub(sprintf("\\.%s\\.html$", file_lang), ".html", path)
  } else {
    if (length(code_in_path) > 0) {
      messy_code <- sub(
        output_folder,
        "",
        sub(basename(path), "", path)
      )
      file_lang <- unlist(
        regmatches(messy_code, gregexpr("[a-zA-Z]+", messy_code))
      )
    } else {
      file_lang <- main_language
    }
  }
  
  if (language_code == main_language) {
    new_path <-  if (type == "book") {
      sub(
        "\\...\\.html", ".html",
        path_rel(path, output_folder, file_lang, main_language)
      )
    } else {
      path_rel(path, output_folder, file_lang, main_language)
    }
    href <- sprintf("%s/%s", site_url, new_path)
  } else {
    base_path <- sub(
      "\\..\\.html", ".html",
      path_rel(path, output_folder, file_lang, main_language)
    )
    new_path <- if (type == "book") {
      fs::path_ext_set(base_path, sprintf(".%s.html", language_code))
    } else {
      base_path
    }
    href <- sprintf("%s/%s/%s", site_url, language_code, new_path)
  }
  
  if (type == "book") {
    
    logo <- xml2::xml_find_first(html, "//div[contains(@class,'sidebar-header')]")
    
    languages_links <- xml2::xml_find_first(html, "//ul[@id='languages-links']")
    languages_links_div_exists <- (length(languages_links) > 0)
    
    if (!languages_links_div_exists) {
      xml2::xml_add_sibling(
        logo,
        "div",
        class = "dropdown",
        id = "languages-links-parent",
        .where = "after"
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
