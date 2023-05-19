test_that("register_main_language() works", {
  parent_dir <- withr::local_tempdir()
  quarto_multilingual_book(
    parent_dir = parent_dir,
    book_dir = "blop",
    further_languages = c("es", "fr"),
    main_language = "en",
    register = FALSE
  )
  book_path <- file.path(parent_dir, "blop")
  register_main_language("en", book_path)
  expect_snapshot_file(file.path(book_path, "_quarto.yml"))

  book_path <- file.path(parent_dir, "blop")
  expect_snapshot(register_main_language("fr", book_path), error = TRUE)

  book_path <- file.path(parent_dir, "blop")
  expect_snapshot(register_main_language("en", book_path))
})
