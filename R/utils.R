level_to_probs <- function(level) {
  checkmate::assert_number(level, lower = 0, upper = 1)
  lower <- (1 - level) / 2
  upper <- 1 - lower
  list(lower = lower, upper = upper)
}

probs_to_char <- function(prob, digits = 3L,
                          remove_trailing_zero = TRUE,
                          add_leading_zero = TRUE) {
  checkmate::assert_number(prob, lower = 0, upper = 1)
  checkmate::assert_int(digits, lower = 0)
  checkmate::assert_logical(remove_trailing_zero, len = 1L)
  checkmate::assert_logical(add_leading_zero, len = 1L)
  .prob <- as.character(round(prob * 10^digits, 0))
  if (nchar(.prob) < digits) {
    .prob <- paste0(paste0(rep("0", digits -  nchar(.prob)), collapse = ""), .prob)
  }
  if (remove_trailing_zero) {
    .prob <- sub("(0+)$", "", .prob)
  }
  .prob <- ifelse(add_leading_zero, paste0("0.", .prob), paste0(".", .prob))
  .prob
}
