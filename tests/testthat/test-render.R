test_that("render_book() works", {
  withr::local_envvar("BABELQUARTO_TESTS_URL" = "true")

  parent_dir <- withr::local_tempdir()
  project_dir <- "blop"
  quarto_multilingual_book(
    parent_dir = parent_dir,
    project_dir = project_dir,
    further_languages = c("es", "fr"),
    main_language = "en"
  )

  withr::with_dir(parent_dir, render_book(project_dir))
  expect_true(fs::dir_exists(file.path(parent_dir, project_dir, "_book")))

  index <- xml2::read_html(file.path(parent_dir, project_dir, "_book", "index.html"))
  spanish_link <- xml2::xml_find_first(index, '//a[@id="language-link-es"]')
  expect_equal(xml2::xml_attr(spanish_link, "href"), "https://example.com/es/index.es.html")
})

test_that("render_book() works - change link", {
  withr::local_envvar("BABELQUARTO_TESTS_URL" = "true")

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
  expect_true(fs::dir_exists(file.path(parent_dir, project_dir, "_book")))

  index <- xml2::read_html(file.path(parent_dir, project_dir, "_book", "index.html"))
  spanish_link <- xml2::xml_find_first(index, '//a[@id="language-link-es"]')
  expect_equal(xml2::xml_attr(spanish_link, "href"), "https://ropensci.org/es/index.es.html")
})

test_that("render_book() works -- chapters in folders", {
  withr::local_envvar("BABELQUARTO_TESTS_URL" = "true")

  parent_dir <- withr::local_tempdir()
  project_dir <- "blop"
  quarto_multilingual_book(
    parent_dir = parent_dir,
    project_dir = project_dir,
    further_languages = c("es", "fr"),
    main_language = "en"
  )
  qmds <- fs::dir_ls(file.path(parent_dir, project_dir), glob = "*.qmd")
  qmds <- qmds[!grepl("index", fs::path_file(qmds))]
  fs::dir_create(file.path(parent_dir, project_dir, "chapters"))
  purrr::walk(
    qmds, fs::file_move,
    new_path = file.path(parent_dir, project_dir, "chapters")
  )

  config_lines <- brio::read_lines(file.path(parent_dir, project_dir, "_quarto.yml"))
  config_lines <- gsub("- (.*)\\.qmd", "- chapters/\\1.qmd", config_lines)
  config_lines <- gsub("- chapters/index.qmd", "- index.qmd", config_lines)
  brio::write_lines(config_lines, file.path(parent_dir, project_dir, "_quarto.yml"))

  withr::with_dir(parent_dir, render_book(project_dir))
  expect_true(fs::dir_exists(file.path(parent_dir, project_dir, "_book")))

  index <- xml2::read_html(file.path(parent_dir, project_dir, "_book", "chapters", "references.html"))
  spanish_link <- xml2::xml_find_first(index, '//a[@id="language-link-es"]')
  expect_equal(xml2::xml_attr(spanish_link, "href"), "https://example.com/es/chapters/references.es.html")
})

test_that("render_website() works", {
  withr::local_envvar("BABELQUARTO_TESTS_URL" = "true")

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
  config[config == '    text: "Version in es"'] <- '    text: "Version en Español"'
  brio::write_lines(config, config_path)

  withr::with_dir(parent_dir, render_website(project_dir))
  expect_true(fs::dir_exists(file.path(parent_dir, project_dir, "_site")))

  index <- xml2::read_html(file.path(parent_dir, project_dir, "_site", "index.html"))
  spanish_link <- xml2::xml_find_first(index, '//a[@id="language-link-es"]')
  expect_equal(xml2::xml_attr(spanish_link, "href"), "https://example.com/es/index.html")
  expect_equal(xml2::xml_text(spanish_link), "Version en Español")

})
test_that("render_book() works -- partial template", {
  withr::local_envvar("BABELQUARTO_TESTS_URL" = "true")

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
  config[config == '    text: "Version in es"'] <- '    text: "Version en Español"'
  config <- append(
    config,
    c("    template-partials:", "      - metadata.html"),
    after = grep("theme: cosmo", config)
  )
  brio::write_lines(config, config_path)

  withr::with_dir(parent_dir, render_website(project_dir))
  expect_true(fs::dir_exists(file.path(parent_dir, project_dir, "_site")))

  index <- xml2::read_html(file.path(parent_dir, project_dir, "_site", "index.html"))
  div <- xml2::xml_find_first(index, '//div[@class="alert alert-info alert-dismissible"]')
  expect_equal(xml2::xml_text(div), "Hello")

  index <- xml2::read_html(file.path(parent_dir, project_dir, "_site", "es", "index.html"))
  div <- xml2::xml_find_first(index, '//div[@class="alert alert-info alert-dismissible"]')
  expect_equal(xml2::xml_text(div), "Hola")

  index <- xml2::read_html(file.path(parent_dir, project_dir, "_site", "fr", "index.html"))
  div <- xml2::xml_find_first(index, '//div[@class="alert alert-info alert-dismissible"]')
  expect_equal(xml2::xml_text(div), "Salut")

})
