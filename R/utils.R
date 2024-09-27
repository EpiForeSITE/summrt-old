level_to_probs <- function(level) {
  checkmate::assert_number(level, lower = 0, upper = 1)
  lower <- (1 - level) / 2
  upper <- 1 - lower
  list(lower = lower, upper = upper)
}

probs_to_char <- function(prob, digits = 3L,
                          remove_trailing_zero = TRUE,
                          remove_leading_zero = FALSE) {
  checkmate::assert_number(prob, lower = 0, upper = 1)
  checkmate::assert_int(digits, lower = 0)
  checkmate::assert_logical(remove_trailing_zero, len = 1L)
  checkmate::assert_logical(remove_leading_zero, len = 1L)
  .prob <- sprintf(paste0("%.", digits, "f"), prob)
  if (remove_trailing_zero) .prob <- sub("(0+)$", "", .prob)
  if (remove_leading_zero) .prob <- sub("^0", "", .prob)
  .prob
}
