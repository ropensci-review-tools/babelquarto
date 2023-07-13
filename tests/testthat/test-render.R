test_that("render_book() works", {
  parent_dir <- withr::local_tempdir()
  book_dir <- "blop"
  quarto_multilingual_book(
    parent_dir = parent_dir,
    book_dir = book_dir,
    further_languages = c("es", "fr"),
    main_language = "en"
  )

  withr::with_dir(parent_dir, render_book(book_dir))
  expect_true(fs::dir_exists(file.path(parent_dir, book_dir, "_book")))

  index <- xml2::read_html(file.path(parent_dir, book_dir, "_book", "index.html"))
  spanish_link <- xml2::xml_find_first(index, '//a[@id="language-link-es"]')
  expect_equal(xml2::xml_attr(spanish_link, "href"), "https://example.com/es/index.es.html")
})

test_that("render_book() works - change link", {
  parent_dir <- withr::local_tempdir()
  book_dir <- "blop"
  quarto_multilingual_book(
    parent_dir = parent_dir,
    book_dir = book_dir,
    further_languages = c("es", "fr"),
    main_language = "en",
    site_url = "https://ropensci.org"
  )

  withr::with_dir(parent_dir, render_book(book_dir))
  expect_true(fs::dir_exists(file.path(parent_dir, book_dir, "_book")))

  index <- xml2::read_html(file.path(parent_dir, book_dir, "_book", "index.html"))
  spanish_link <- xml2::xml_find_first(index, '//a[@id="language-link-es"]')
  expect_equal(xml2::xml_attr(spanish_link, "href"), "https://ropensci.org/es/index.es.html")
})
