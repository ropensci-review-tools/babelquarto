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


test_that("render_website() works -- specific language fields", {
  withr::local_envvar("BABELQUARTO_TESTS_URL" = "true")

  parent_dir <- withr::local_tempdir()
  project_dir <- "blop"
  quarto_multilingual_website(
    parent_dir = parent_dir,
    project_dir = project_dir,
    further_languages = "ja",
    main_language = "en"
  )

  config_path <- file.path(parent_dir, project_dir, "_quarto.yml")
  config <- brio::read_lines(config_path)
  config <- c(
    config,
    "",
    "babelquarto-ja:",
    "  website:",
    "    title: ニッタ ジョエル"
  )
  brio::write_lines(config, config_path)

  withr::with_dir(parent_dir, render_website(project_dir))
  expect_true(fs::dir_exists(file.path(parent_dir, project_dir, "_site")))

  ja_index <- xml2::read_html(file.path(parent_dir, project_dir, "_site", "ja", "index.html"))
  ja_title <- xml2::xml_find_first(ja_index, "//span[@class='navbar-title']")
  expect_equal(xml2::xml_text(ja_title), "ニッタ ジョエル")
})
