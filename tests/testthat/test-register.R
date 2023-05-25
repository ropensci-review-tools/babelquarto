test_that("register_main_language() works", {

  withr::local_envvar(QUARTOBABELDATE = "01/01/2023")
  withr::local_envvar(QUARTOBABELAUTHOR = "Firstname Lastname")

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

  expect_snapshot(register_main_language("fr", book_path), error = TRUE)

  expect_snapshot(register_main_language("en", book_path))
})

test_that("register_further_languages() works", {
  withr::local_envvar(QUARTOBABELDATE = "01/01/2023")
  withr::local_envvar(QUARTOBABELAUTHOR = "Firstname Lastname")
  parent_dir <- withr::local_tempdir()
  quarto_multilingual_book(
    parent_dir = parent_dir,
    book_dir = "blop",
    further_languages = c("es", "fr"),
    main_language = "en",
    register = FALSE
  )
  book_path <- file.path(parent_dir, "blop")
  expect_snapshot(
    register_further_languages(c("es", "fr"), book_path),
    error = TRUE
  )

  register_main_language("en", book_path)
  register_further_languages(c("es", "fr"), book_path)
  file.copy(
    file.path(book_path, "_quarto.yml"),
    file.path(book_path, "_quarto2.yml")
  )
  expect_snapshot_file(file.path(book_path, "_quarto2.yml"))

  expect_snapshot(register_further_languages(c("es", "fr"), book_path))
})
