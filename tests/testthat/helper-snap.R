expect_snapshot_yaml <- function(path) {
  testthat::expect_snapshot_file(path, compare = testthat::compare_file_text)
}
