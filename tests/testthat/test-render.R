test_that("render_book() works", {
  withr::local_envvar(BABELQUARTO_TESTS_URL = "true")

  parent_dir <- withr::local_tempdir()
  project_dir <- "blop"
  quarto_multilingual_book(
    parent_dir = parent_dir,
    project_dir = project_dir,
    further_languages = c("es", "fr"),
    main_language = "en"
  )

  withr::with_dir(parent_dir, render_book(project_dir))
  expect_dir_exists(file.path(parent_dir, project_dir, "_book"))

  index_path <- file.path(parent_dir, project_dir, "_book", "index.html")
  index <- xml2::read_html(index_path)
  spanish_link <- xml2::xml_find_first(index, '//a[@id="language-link-es"]')
  expect_identical(
    xml2::xml_attr(spanish_link, "href"),
    "https://example.com/es/index.es.html"
  )
})

test_that("render_book() works - change link", {
  withr::local_envvar(BABELQUARTO_TESTS_URL = "true")

  parent_dir <- withr::local_tempdir()
  project_dir <- "blop"
  quarto_multilingual_book(
    parent_dir = parent_dir,
    project_dir = project_dir,
    further_languages = c("es", "fr"),
    main_language = "en",
    site_url = "https://ropensci.org"
  )

  withr::with_dir(parent_dir, render_book(project_dir))
  expect_dir_exists(file.path(parent_dir, project_dir, "_book"))

  index_path <- file.path(parent_dir, project_dir, "_book", "index.html")
  index <- xml2::read_html(index_path)
  spanish_link <- xml2::xml_find_first(index, '//a[@id="language-link-es"]')
  expect_identical(
    xml2::xml_attr(spanish_link, "href"),
    "https://ropensci.org/es/index.es.html"
  )
})

test_that("render_book() works -- chapters in folders", {
  withr::local_envvar(BABELQUARTO_TESTS_URL = "true")

  parent_dir <- withr::local_tempdir()
  project_dir <- "blop"
  quarto_multilingual_book(
    parent_dir = parent_dir,
    project_dir = project_dir,
    further_languages = c("es", "fr"),
    main_language = "en"
  )
  qmds <- fs::dir_ls(file.path(parent_dir, project_dir), glob = "*.qmd")
  qmds <- qmds[grep("index", fs::path_file(qmds), invert = TRUE, fixed = TRUE)]
  fs::dir_create(file.path(parent_dir, project_dir, "chapters"))
  purrr::walk(
    qmds, fs::file_move,
    new_path = file.path(parent_dir, project_dir, "chapters")
  )

  config_path <- file.path(parent_dir, project_dir, "_quarto.yml")
  config_lines <- brio::read_lines(config_path)
  config_lines <- gsub("- (.*)\\.qmd", "- chapters/\\1.qmd", config_lines)
  config_lines <- gsub("- chapters/index.qmd", "- index.qmd", config_lines) # nolint: nonportable_path_linter
  brio::write_lines(config_lines, config_path)

  withr::with_dir(parent_dir, render_book(project_dir))
  expect_dir_exists(file.path(parent_dir, project_dir, "_book"))

  references_path <- file.path(
    parent_dir, project_dir,
    "_book", "chapters", "references.html"
  )
  references <- xml2::read_html(references_path)
  spanish_link <- xml2::xml_find_first(references, '//a[@id="language-link-es"]') # nolint: line_length_linter
  expect_identical(
    xml2::xml_attr(spanish_link, "href"),
    "https://example.com/es/chapters/references.es.html"
  )
})

test_that("render_website() works", {
  withr::local_envvar(BABELQUARTO_TESTS_URL = "true")

  parent_dir <- withr::local_tempdir()
  project_dir <- "blop"
  quarto_multilingual_website(
    parent_dir = parent_dir,
    project_dir = project_dir,
    further_languages = c("es", "fr"),
    main_language = "en"
  )

  config_path <- file.path(parent_dir, project_dir, "_quarto.yml")
  config <- brio::read_lines(config_path)
  config[config == '    text: "Version in es"'] <- '    text: "Version en Español"' # nolint: line_length_linter
  brio::write_lines(config, config_path)

  withr::with_dir(parent_dir, render_website(project_dir))
  expect_dir_exists(file.path(parent_dir, project_dir, "_site"))

  index_path <- file.path(parent_dir, project_dir, "_site", "index.html")
  index <- xml2::read_html(index_path)
  spanish_link <- xml2::xml_find_first(index, '//a[@id="language-link-es"]')
  expect_identical(
    xml2::xml_attr(spanish_link, "href"),
    "https://example.com/es/index.html"
  )
  expect_identical(xml2::xml_text(spanish_link), "Version en Español")

  spanish_index_path <- file.path(
    parent_dir, project_dir,
    "_site", "es", "index.html"
  )
  spanish_index <- xml2::read_html(spanish_index_path)
  english_link <- xml2::xml_find_first(
    spanish_index,
    '//a[@id="language-link-en"]'
  )
  expect_identical(
    xml2::xml_attr(english_link, "href"),
    "https://example.com/index.html")
  expect_identical(xml2::xml_text(english_link), "Version in en"
  )

})
test_that("render_book() works -- partial template", {
  withr::local_envvar(BABELQUARTO_TESTS_URL = "true")

  parent_dir <- withr::local_tempdir()
  project_dir <- "blop"
  quarto_multilingual_website(
    parent_dir = parent_dir,
    project_dir = project_dir,
    further_languages = c("es", "fr"),
    main_language = "en"
  )

  fs::file_copy(
    test_path("metadata.html"),
    file.path(parent_dir, project_dir, "metadata.html")
  )

  config_path <- file.path(parent_dir, project_dir, "_quarto.yml")
  config <- brio::read_lines(config_path)
  config[config == '    text: "Version in es"'] <- '    text: "Version en Español"' # nolint: line_length_linter
  config <- append(
    config,
    c("    template-partials:", "      - metadata.html"),
    after = grep("theme: cosmo", config, fixed = TRUE)
  )
  brio::write_lines(config, config_path)

  withr::with_dir(parent_dir, render_website(project_dir))
  expect_dir_exists(file.path(parent_dir, project_dir, "_site"))

  index_path <- file.path(parent_dir, project_dir, "_site", "index.html")
  index <- xml2::read_html(index_path)
  div <- xml2::xml_find_first(
    index,
    '//div[@class="alert alert-info alert-dismissible"]'
  )
  expect_match(xml2::xml_text(div), "Hello")

  spanish_index_path <- file.path(
    parent_dir, project_dir,
    "_site", "es", "index.html"
  )
  spanish_index <- xml2::read_html(spanish_index_path)
  div <- xml2::xml_find_first(
    spanish_index,
    '//div[@class="alert alert-info alert-dismissible"]'
  )
  expect_match(xml2::xml_text(div), "Hola")

  french_index_path <- file.path(
    parent_dir, project_dir, "_site",
    "fr", "index.html"
  )
  french_index <- xml2::read_html(french_index_path)
  div <- xml2::xml_find_first(
    french_index,
    '//div[@class="alert alert-info alert-dismissible"]'
  )
  expect_match(xml2::xml_text(div), "Salut")

})

test_that("render_book() works - appendices", {
  withr::local_envvar(BABELQUARTO_TESTS_URL = "true")

  parent_dir <- withr::local_tempdir()
  project_dir <- "blop"
  quarto_multilingual_book(
    parent_dir = parent_dir,
    project_dir = project_dir,
    further_languages = c("es", "fr"),
    main_language = "en",
    site_url = "https://ropensci.org"
  )

  config_path <- file.path(parent_dir, project_dir, "_quarto.yml")
  config_lines <- brio::read_lines(config_path)
  config_lines <- append(
    config_lines,
    "  appendices:",
    after = which(config_lines == "    - summary.qmd")
  )
  brio::write_lines(config_lines, config_path)

  withr::with_dir(parent_dir, render_book(project_dir))
  expect_dir_exists(file.path(parent_dir, project_dir, "_book"))


})

test_that("render_book() works - chapters", {
  withr::local_envvar(BABELQUARTO_TESTS_URL = "true")

  parent_dir <- withr::local_tempdir()
  project_dir <- "blop"
  quarto_multilingual_book(
    parent_dir = parent_dir,
    project_dir = project_dir,
    further_languages = c("es", "fr"),
    main_language = "en",
    site_url = "https://ropensci.org"
  )

  config_path <- file.path(parent_dir, project_dir, "_quarto.yml")
  config_lines <- brio::read_lines(config_path)
  config_lines <- setdiff(
    config_lines,
    c("    - intro.qmd", "    - references.qmd")
  )
  brio::write_lines(config_lines, config_path)

  fs::file_delete(file.path(parent_dir, project_dir, "summary.es.qmd"))

  withr::with_dir(parent_dir, render_book(project_dir))
  expect_dir_exists(file.path(parent_dir, project_dir, "_book"))


})

test_that("render_book() works - parts and chapters", {
  withr::local_envvar(BABELQUARTO_TESTS_URL = "true")

  parent_dir <- withr::local_tempdir()
  project_dir <- "blop"
  quarto_multilingual_book(
    parent_dir = parent_dir,
    project_dir = project_dir,
    further_languages = c("es", "fr"),
    main_language = "en",
    site_url = "https://ropensci.org"
  )

  config_path <- file.path(parent_dir, project_dir, "_quarto.yml")
  config <- read_yaml(config_path)
  config[["book"]][["chapters"]] <- list(
    "index.qmd",
    list(
      part = "Introduction",
      `part-fr` = "Introduction",
      `part-es` = "Introducción",
      chapters = list(
        "intro.qmd"
      )
    ),
    list(
      part = "Summary",
      `part-fr` = "Résumé",
      `part-es` = "Resumen",
      chapters = list(
        "summary.qmd"
      )
    ),
    "references.qmd"
  )
  yaml::write_yaml(config, config_path)

  withr::with_dir(parent_dir, render_book(project_dir))
  expect_dir_exists(file.path(parent_dir, project_dir, "_book"))

})

test_that("render_website() works - listing", {
  withr::local_envvar(BABELQUARTO_TESTS_URL = "true")

  parent_dir <- withr::local_tempdir()
  project_dir <- "blop"
  quarto_multilingual_website(
    parent_dir = parent_dir,
    project_dir = project_dir,
    further_languages = c("es", "fr"),
    main_language = "en"
  )

  fs::dir_create(file.path(parent_dir, project_dir, "subdir"))
  listing_lines <- c(
    "---",
    'title: "Listing"',
    "listing:",
    '  - contents: ["subdir/*.qmd"]',
    "    type: grid",
    "---"
  )
  brio::write_lines(
    listing_lines,
    file.path(parent_dir, project_dir, "listing.qmd")
  )
  fs::file_copy(
    file.path(parent_dir, project_dir, "about.qmd"),
    file.path(parent_dir, project_dir, "subdir", "about.qmd")
  )
  fs::file_copy(
    file.path(parent_dir, project_dir, "about.fr.qmd"),
    file.path(parent_dir, project_dir, "subdir", "about.fr.qmd")
  )

  withr::with_dir(parent_dir, render_website(project_dir))

  listing_path <- file.path(parent_dir, project_dir, "_site", "listing.html")
  listing <- xml2::read_html(listing_path)
  listing_grid <- xml2::xml_find_first(
    listing,
    "//div[contains(@class, 'quarto-listing-cols-3')]"
  )
  grid_items <- xml2::xml_find_all(
    listing_grid,
    ".//div[contains(@class, 'quarto-grid-item')]"
  )

  expect_length(grid_items, 1L)
})

test_that("render_website() works - clean render for each language", {
  withr::local_envvar(BABELQUARTO_TESTS_URL = "true")

  parent_dir <- withr::local_tempdir()
  project_dir <- "blop"
  quarto_multilingual_website(
    parent_dir = parent_dir,
    project_dir = project_dir,
    further_languages = c("es", "fr"),
    main_language = "en"
  )

  fs::dir_create(file.path(parent_dir, project_dir, "subdir"))
  fs::file_copy(
    file.path(parent_dir, project_dir, "about.qmd"),
    file.path(parent_dir, project_dir, "subdir", "about.qmd")
  )
  fs::file_copy(
    file.path(parent_dir, project_dir, "about.fr.qmd"),
    file.path(parent_dir, project_dir, "subdir", "about.fr.qmd")
  )

  withr::with_dir(parent_dir, render_website(project_dir))
  main_subdir <- file.path(parent_dir, project_dir, "_site", "subdir")
  expect_dir_exists(main_subdir)
  expect_length(fs::dir_ls(main_subdir), 1L)

  french_subdir <- file.path(parent_dir, project_dir, "_site", "fr", "subdir")
  expect_dir_exists(french_subdir)
  expect_length(fs::dir_ls(french_subdir), 1L)
  expect_file_exists(file.path(french_subdir, "about.html"))

  spanish_subdir <- file.path(parent_dir, project_dir, "_site", "es", "subdir")
  expect_dir_absent(spanish_subdir)
})

test_that("render_website() fails when missing sidebar
           and languagelinks is set to sidebar", {
  withr::local_envvar(BABELQUARTO_TESTS_URL = "true")

  parent_dir <- withr::local_tempdir()
  project_dir <- "blop"
  quarto_multilingual_website(
    parent_dir = parent_dir,
    project_dir = project_dir,
    further_languages = c("es", "fr"),
    main_language = "en"
  )

  config_path <- file.path(parent_dir, project_dir, "_quarto.yml")
  config_lines <- brio::read_lines(config_path)
  where_languagelinks <- grep("  languagelinks:", config_lines, fixed = TRUE)
  config_lines[where_languagelinks] <- "  languagelinks: sidebar"
  brio::write_lines(config_lines, config_path)

  expect_error(
    withr::with_dir(parent_dir, render_website(project_dir)),
    regexp = "Can't find website.sidebar in _quarto.yml."
  )
})

test_that("render_book() fails when missing navbar
           and languagelinks is set to navbar", {
  withr::local_envvar(BABELQUARTO_TESTS_URL = "true")

  parent_dir <- withr::local_tempdir()
  project_dir <- "blop"

  quarto_multilingual_book(
    parent_dir = parent_dir,
    project_dir = project_dir,
    further_languages = c("es", "fr"),
    main_language = "en",
    site_url = "https://ropensci.org"
  )

  config_path <- file.path(parent_dir, project_dir, "_quarto.yml")
  config_lines <- brio::read_lines(config_path)
  where_languagelinks <- grep("  languagelinks:", config_lines, fixed = TRUE)
  config_lines[where_languagelinks] <- "  languagelinks: navbar"
  brio::write_lines(config_lines, config_path)

  expect_error(
    withr::with_dir(parent_dir, render_book(project_dir)),
    regexp = "Can't find book.navbar in _quarto.yml."
  )
})

test_that("book with navbar placement has languagelinks in the navbar", {
  withr::local_envvar(BABELQUARTO_TESTS_URL = "true")

  parent_dir <- withr::local_tempdir()
  project_dir <- "blop"

  quarto_multilingual_book(
    parent_dir = parent_dir,
    project_dir = project_dir,
    further_languages = c("es", "fr"),
    main_language = "en",
    site_url = "https://ropensci.org",
    placement = "navbar"
  )

  withr::with_dir(parent_dir, render_book(project_dir))

  index_path <- file.path(parent_dir, project_dir, "_book", "index.html")
  index <- xml2::read_html(index_path)
  language_links <- xml2::xml_find_first(
    index,
    '//div[@id="languages-links-parent"]'
  )
  navbar_li <- xml2::xml_parent(language_links)
  navbar_li_class <- xml2::xml_attr(navbar_li, "class")

  expect_identical(navbar_li_class, "nav-item")
})

test_that("website with sidebar placement has languagelinks in the sidebar", {
  withr::local_envvar(BABELQUARTO_TESTS_URL = "true")

  parent_dir <- withr::local_tempdir()
  project_dir <- "blop"

  quarto_multilingual_website(
    parent_dir = parent_dir,
    project_dir = project_dir,
    further_languages = c("es", "fr"),
    main_language = "en",
    site_url = "https://ropensci.org",
    placement = "sidebar"
  )

  withr::with_dir(parent_dir, render_website(project_dir))

  index_path <- file.path(parent_dir, project_dir, "_site", "index.html")
  index <- xml2::read_html(index_path)
  language_links <- xml2::xml_find_first(
    index,
    '//div[@id="languages-links-parent"]'
  )
  sidebar <- xml2::xml_parent(language_links)
  sidebar_id <- xml2::xml_attr(sidebar, "id")

  expect_identical(sidebar_id, "quarto-sidebar")
})

test_that("render_book() works - all language links are present in sidebar", {

  parent_dir <- withr::local_tempdir()
  project_dir <- "blop"

  quarto_multilingual_book(
    parent_dir = parent_dir,
    project_dir = project_dir,
    further_languages = c("es", "fr"),
    main_language = "en",
    site_url = "https://ropensci.org"
  )

  withr::local_envvar(BABELQUARTO_CI_URL = "https://ropensci.org")
  withr::with_dir(parent_dir, render_book(project_dir))

  index_path <- file.path(parent_dir, project_dir, "_book", "index.html")
  index <- xml2::read_html(index_path)

  language_links <- xml2::xml_find_first(
    index,
    '//div[@id="languages-links-parent"]'
  )
  sidebar <- xml2::xml_parent(language_links)
  sidebar_id <- xml2::xml_attr(sidebar, "id")
  expect_identical(sidebar_id, "quarto-sidebar")

  index_links <- xml2::xml_find_first(index, '//ul[@id="languages-links"]')
  expect_identical(xml2::xml_length(index_links), 2L)

  index_link_fr_href <- xml2::xml_attr(
    xml2::xml_find_first(index, '//a[@id="language-link-fr"]'),
    "href"
  )
  expect_identical(index_link_fr_href, "https://ropensci.org/fr/index.fr.html")

  index_link_es_href <- xml2::xml_attr(
    xml2::xml_find_first(index, '//a[@id="language-link-es"]'),
    "href"
  )
  expect_identical(index_link_es_href, "https://ropensci.org/es/index.es.html")

  index_fr_path <- file.path(
    parent_dir, project_dir,
    "_book", "fr", "index.fr.html"
  )
  index_fr <- xml2::read_html(index_fr_path)
  index_fr_links <- xml2::xml_find_first(
    index_fr,
    '//ul[@id="languages-links"]'
  )
  expect_identical(xml2::xml_length(index_fr_links), 2L)

  index_fr_link_en <- xml2::xml_attr(
    xml2::xml_find_all(index_fr, '//a[@id="language-link-en"]'),
    "href"
  )
  expect_identical(index_fr_link_en, "https://ropensci.org/index.html")

  index_fr_link_es <- xml2::xml_attr(
    xml2::xml_find_all(index_fr, '//a[@id="language-link-es"]'),
    "href"
  )
  expect_identical(index_fr_link_es, "https://ropensci.org/es/index.es.html")

  index_es_path <- file.path(
    parent_dir, project_dir,
    "_book", "es", "index.es.html"
  )
  index_es <- xml2::read_html(index_es_path)
  index_es_links <- xml2::xml_find_first(
    index_es,
    '//ul[@id="languages-links"]'
  )
  expect_identical(xml2::xml_length(index_es_links), 2L)

  index_es_link_en <- xml2::xml_attr(
    xml2::xml_find_all(index_es, '//a[@id="language-link-en"]'),
    "href"
  )
  expect_identical(index_es_link_en, "https://ropensci.org/index.html")

  index_es_link_fr <- xml2::xml_attr(
    xml2::xml_find_all(index_es, '//a[@id="language-link-fr"]'),
    "href"
  )
  expect_identical(index_es_link_fr, "https://ropensci.org/fr/index.fr.html")
})

test_that("render_website() works - all language links are present in navbar", {

  parent_dir <- withr::local_tempdir()
  project_dir <- "blop"

  quarto_multilingual_website(
    parent_dir = parent_dir,
    project_dir = project_dir,
    further_languages = c("es", "fr"),
    main_language = "en",
    site_url = "https://ropensci.org"
  )

  withr::local_envvar(BABELQUARTO_CI_URL = "https://ropensci.org")
  withr::with_dir(parent_dir, render_website(project_dir))

  index_path <- file.path(parent_dir, project_dir, "_site", "index.html")
  index <- xml2::read_html(index_path)

  language_links <- xml2::xml_find_first(
    index,
    '//div[@id="languages-links-parent"]'
  )
  navbar_li <- xml2::xml_parent(language_links)
  navbar_li_class <- xml2::xml_attr(navbar_li, "class")
  expect_identical(navbar_li_class, "nav-item")

  index_links <- xml2::xml_find_first(index, '//ul[@id="languages-links"]')
  expect_identical(xml2::xml_length(index_links), 2L)

  index_link_fr_href <- xml2::xml_attr(
    xml2::xml_find_first(index, '//a[@id="language-link-fr"]'),
    "href"
  )
  expect_identical(index_link_fr_href, "https://ropensci.org/fr/index.html")

  index_link_es_href <- xml2::xml_attr(
    xml2::xml_find_first(index, '//a[@id="language-link-es"]'),
    "href"
  )
  expect_identical(index_link_es_href, "https://ropensci.org/es/index.html")

  index_fr_path <- file.path(parent_dir, project_dir, "_site", "fr", "index.html") # nolint: line_length_linter
  index_fr <- xml2::read_html(index_fr_path)
  index_fr_links <- xml2::xml_find_first(index_fr, '//ul[@id="languages-links"]') # nolint: line_length_linter
  expect_identical(xml2::xml_length(index_fr_links), 2L)

  index_fr_link_en <- xml2::xml_attr(
    xml2::xml_find_all(index_fr, '//a[@id="language-link-en"]'),
    "href"
  )
  expect_identical(index_fr_link_en, "https://ropensci.org/index.html")

  index_fr_link_es <- xml2::xml_attr(
    xml2::xml_find_all(index_fr, '//a[@id="language-link-es"]'),
    "href"
  )
  expect_identical(index_fr_link_es, "https://ropensci.org/es/index.html")

  index_es_path <- file.path(
    parent_dir, project_dir,
    "_site", "es", "index.html"
  )
  index_es <- xml2::read_html(index_es_path)
  index_es_links <- xml2::xml_find_first(index_es, '//ul[@id="languages-links"]') # nolint: line_length_linter
  expect_identical(xml2::xml_length(index_es_links), 2L)

  index_es_link_en <- xml2::xml_attr(
    xml2::xml_find_all(index_es, '//a[@id="language-link-en"]'),
    "href"
  )
  expect_identical(index_es_link_en, "https://ropensci.org/index.html")

  index_es_link_fr <- xml2::xml_attr(
    xml2::xml_find_all(index_es, '//a[@id="language-link-fr"]'),
    "href"
  )
  expect_identical(index_es_link_fr, "https://ropensci.org/fr/index.html")

})

test_that("render_website() works - sidebar in language profile", {
  parent_dir <- withr::local_tempdir()
  project_dir <- "blop"

  quarto_multilingual_website(
    parent_dir = parent_dir,
    project_dir = project_dir,
    further_languages = "fr",
    main_language = "en",
    site_url = "https://ropensci.org"
  )

  config_path <- file.path(parent_dir, project_dir, "_quarto.yml")
  config_lines <- brio::read_lines(config_path)
  where_languagelinks <- grep("  languagelinks:", config_lines, fixed = TRUE)
  config_lines[where_languagelinks] <- "  languagelinks: sidebar"
  brio::write_lines(config_lines, config_path)

  sidebar <- c(
    "website:",
    "  sidebar:",
    "    contents:"
  )

  profile_path_en <- file.path(parent_dir, project_dir, "_quarto-en.yml")
  sidebar_en <- c(
    sidebar,
    "      - text: Home",
    "        href: index.qmd"
  )
  brio::write_lines(sidebar_en, profile_path_en)

  profile_path_fr <- file.path(parent_dir, project_dir, "_quarto-fr.yml")
  sidebar_fr <- c(
    sidebar,
    "      - text: Accueil",
    "        href: index.qmd"
  )
  brio::write_lines(sidebar_fr, profile_path_fr)


  withr::with_dir(parent_dir, render_website(project_dir))
  main <- file.path(parent_dir, project_dir, "_site")
  expect_dir_exists(main)

  index_path <- file.path(main, "index.html")
  index <- xml2::read_html(index_path)
  language_links <- xml2::xml_find_first(
    index,
    '//div[@id="languages-links-parent"]'
  )
  sidebar <- xml2::xml_parent(language_links)
  sidebar_id <- xml2::xml_attr(sidebar, "id")

  expect_identical(sidebar_id, "quarto-sidebar")

  french <- file.path(parent_dir, project_dir, "_site", "fr")
  expect_dir_exists(french)

  index_fr_path <- file.path(french, "index.html")
  index_fr <- xml2::read_html(index_fr_path)
  language_links_fr <- xml2::xml_find_first(
    index_fr,
    '//div[@id="languages-links-parent"]'
  )
  sidebar_fr <- xml2::xml_parent(language_links_fr)
  sidebar_fr_id <- xml2::xml_attr(sidebar_fr, "id")

  expect_identical(sidebar_fr_id, "quarto-sidebar")
})

test_that("render_website() works - quarto freeze for languages is working", {
  parent_dir <- withr::local_tempdir()
  project_dir <- "blop"

  quarto_multilingual_website(
    parent_dir = parent_dir,
    project_dir = project_dir,
    further_languages = "fr",
    main_language = "en",
    site_url = "https://ropensci.org"
  )

  config <- brio::read_lines(file.path(parent_dir, project_dir, "_quarto.yml"))
  config <- c(
    config,
    "",
    "execute:",
    "  freeze: true"
  )
  brio::write_lines(config, file.path(parent_dir, project_dir, "_quarto.yml"))

  index <- brio::read_lines(file.path(parent_dir, project_dir, "index.qmd"))
  index <- c(
    index,
    "",
    "```{r}",
    "print('Hello World!')",
    "```"
  )
  brio::write_lines(index, file.path(parent_dir, project_dir, "index.qmd"))

  index_fr <- brio::read_lines(
    file.path(parent_dir, project_dir, "index.fr.qmd")
  )
  index_fr <- c(
    index_fr,
    "",
    "```{r}",
    "print('Bonjour le monde!')",
    "```"
  )
  brio::write_lines(
    index_fr, file.path(parent_dir, project_dir, "index.fr.qmd")
  )

  # Render with quarto to generate the _freeze folder
  withr::with_dir(
    parent_dir,
    quarto::quarto_render(input = project_dir, as_job = FALSE)
  )

  withr::with_dir(parent_dir, render_website(project_dir))

  index_html <- brio::read_lines(
    file.path(parent_dir, project_dir, "_site", "index.html")
  )
  hello_world_present <- any(grepl(
    "Hello World!",
    index_html,
    fixed = TRUE
  ))
  expect_true(hello_world_present)

  index_html_fr <- brio::read_lines(
    file.path(parent_dir, project_dir, "_site", "fr", "index.html")
  )
  bonjour_monde_present <- any(grepl(
    "Bonjour le monde!",
    index_html_fr,
    fixed = TRUE
  ))
  expect_true(bonjour_monde_present)
})
