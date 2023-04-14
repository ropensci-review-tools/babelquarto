test_that("quarto_multilingual_book() works", {
  parent_folder <- withr::local_tempdir()
  quarto_multilingual_book(
    parent_folder = parent_folder,
    book_folder = "blop",
    further_languages = c("es", "fr"),
    main_language = "en"
  )
  expect_true(fs::dir_exists(file.path(parent_folder, "blop")))
  expect_setequal(
    fs::dir_ls(file.path(parent_folder, "blop")) |> fs::path_file(),
    c(
      "_quarto.yml", "cover.png", "index.es.qmd", "index.fr.qmd",
      "index.qmd", "intro.es.qmd", "intro.fr.qmd", "intro.qmd",
      "references.bib", "references.es.qmd", "references.fr.qmd",
      "references.qmd", "summary.es.qmd", "summary.fr.qmd", "summary.qmd"
      )
  )
  config <- yaml::read_yaml(file.path(parent_folder, "blop", "_quarto.yml"))
  expect_equal(config[["babelquarto"]][["mainlanguage"]], "en")
  expect_equal(config[["babelquarto"]][["languages"]], c("es", "fr"))
})
