test_that("Replacing logicals with yes/no works", {
  original_list <- list(
    a = TRUE,
    z = "hey",
    y = 12,
    b = list(
      c = FALSE,
      d = list(
        e = TRUE,
        f = c(FALSE, TRUE)
      )
    )
  )
  replace_list <- list(
    a = "true",
    z = "hey",
    y = 12,
    b = list(
      c = "false",
      d = list(
        e = "true",
        f = c("false", "true")
      )
    )
  )
  class(replace_list$a) <- "verbatim"
  class(replace_list$b$c) <- "verbatim"
  class(replace_list$b$d$e) <- "verbatim"
  class(replace_list$b$d$f) <- "verbatim"

  expect_equal(
    replace_list,
    replace_true_false(original_list)
  )
})
