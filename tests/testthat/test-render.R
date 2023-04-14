test_that("render_book() works", {
  parent_folder <- withr::local_tempdir()
  book_folder <- "blop"
  quarto_multilingual_book(
    parent_folder = parent_folder,
    book_folder = "blop",
    further_languages = c("es", "fr"),
    main_language = "en"
  )

  withr::local_dir(parent_folder)
  render_book(book_folder)
  expect_true(fs::dir_exists(file.path(book_folder, "_book")))
})
