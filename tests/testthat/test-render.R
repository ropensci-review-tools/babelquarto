test_that("render_book() works", {
  parent_dir <- withr::local_tempdir()
  book_dir <- "blop"
  quarto_multilingual_book(
    parent_dir = parent_dir,
    book_dir = book_dir,
    further_languages = c("es", "fr"),
    main_language = "en"
  )

  withr::local_dir(parent_dir)
  render_book(book_dir)
  expect_true(fs::dir_exists(file.path(book_dir, "_book")))
})
