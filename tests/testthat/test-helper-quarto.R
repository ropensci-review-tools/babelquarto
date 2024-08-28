test_that("quarto_multilingual_book() works", {
  parent_dir <- withr::local_tempdir()
  quarto_multilingual_book(
    parent_dir = parent_dir,
    project_dir = "blop",
    further_languages = c("es", "fr"),
    main_language = "en"
  )
  expect_true(fs::dir_exists(file.path(parent_dir, "blop")))
  expect_setequal(
    fs::dir_ls(file.path(parent_dir, "blop")) |> fs::path_file(),
    c(
      "_quarto.yml", "cover.png", "index.es.qmd", "index.fr.qmd",
      "index.qmd", "intro.es.qmd", "intro.fr.qmd", "intro.qmd",
      "references.bib", "references.es.qmd", "references.fr.qmd",
      "references.qmd", "summary.es.qmd", "summary.fr.qmd", "summary.qmd"
      )
  )
  config <- yaml::read_yaml(file.path(parent_dir, "blop", "_quarto.yml"))
  expect_identical(config[["babelquarto"]][["mainlanguage"]], "en")
  expect_identical(config[["babelquarto"]][["languages"]], c("es", "fr"))
})
