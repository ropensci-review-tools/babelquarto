expect_snapshot_yaml <- function(path) {
  expect_snapshot_file(path, compare = testthat::compare_file_text)
}
