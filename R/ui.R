cli_alert_success <- function(text, .envir = parent.frame()) {
  if (!getOption("babelquarto.quiet", FALSE)) {
    cli::cli_alert_success(text, .envir = .envir)
  }
}

cli_alert_info <- function(text, .envir = parent.frame()) {
  if (!getOption("babelquarto.quiet", FALSE)) {
    cli::cli_alert_info(text, .envir = .envir)
  }
}
