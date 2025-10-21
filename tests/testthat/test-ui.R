test_that("cli_alert_success() works", {
  withr::local_options(babelquarto.quiet = TRUE)
  expect_silent(cli_alert_success("hihihi"))

  withr::local_options(babelquarto.quiet = FALSE)
  expect_snapshot(cli_alert_success("hihihi"))
})

test_that("cli_alert_info() works", {
  withr::local_options(babelquarto.quiet = TRUE)
  expect_silent(cli_alert_info("hihihi"))

  withr::local_options(babelquarto.quiet = FALSE)
  expect_snapshot(cli_alert_info("hihihi"))
})
