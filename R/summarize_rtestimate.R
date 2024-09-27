#' Create a new summary object
#'
#' Creates a new summary object for the `summrt` package while validating the input.
#'
#' @param date Integer vector. vector of index dates.
#' @param median Double vector. vector of median values.
#' @param lb Double vector. vector of lower bounds.
#' @param ub Double vector. vector of upper bounds.
#' @param level Double scalar. the confidence level associated with `lb`/`ub`
#' @param package String. Name of the package.
#' @param notes String. Notes about the summary.
#' @export
#' @return A list of class `summrt_summary`. with the following components:
#' - `estimates`: A tibble with the following columns:
#'   - `date`: Integer vector. vector of index dates.
#'   - `median`: Double vector. vector of median values.
#'   - `lb`: Double vector. vector of lower bounds.
#'   - `ub`: Double vector. vector of upper bounds.
#' - `level`: Double scalar.
#' - `package`: String. Name of the package.
#' - `notes`: String. Notes about the summary.
new_summrt <- function(
    date, median, lb, ub, level, package, notes
) {

  # Asserting the types
  checkmate::assert_integer(date)
  checkmate::assert_double(median)
  checkmate::assert_double(lb)
  checkmate::assert_double(ub)
  checkmate::assert_number(level, lower = 0, upper = 1)
  checkmate::assert_string(package)
  checkmate::assert_string(notes)

  # Checking the length
  len_date <- length(date)
  len_median <- length(median)
  len_lb <- length(lb)
  len_up <- length(ub)
  if (len_date != len_median || len_date != len_lb || len_date != len_up) {
    stop("The length of the date, median, lb, and ub should be the same.")
  }

  structure(
    list(
      estimates = tibble::tibble(
        date = date,
        median = median,
        lb = lb,
        ub = ub
      ),
      level = level,
      package = package,
      notes = notes
    ),
    class = "summrt_summary"
  )
}

#' @export
#' @rdname new_summrt
#' @param x An object of class `summrt_summary`.
#' @param ... Additional arguments passed to methods.
print.summrt_summary <- function(x, ...) {
  cat("Summary of Rt estimation\n")
  cat("Confidence level : ", x$level, "\n")
  cat("Package : ", x$package, "\n")
  if (nchar(x$notes) > 0L) cat("Notes   : ", x$notes, "\n")
  print(x$estimates)
  invisible(x)
}

#' Extract Rt estimation from a model fit
#' @param x Object to extract Rt from.
#' @param level Desired confidence level for the estimate
#' @param ... Additional arguments passed to methods.
#' @param notes String. Optional notes to add to the summary.
#' @export
summarize_rtestimate <- function(x, level = 0.95, ..., notes = "") {
  checkmate::assert_number(level, lower = 0, upper = 1)
  checkmate::assert_string(notes)
  UseMethod("summarize_rtestimate")
}

#' @rdname summarize_rtestimate
#' @importFrom cli cli_abort
#' @export
summarize_rtestimate.default <- function(x, level = 0.95, ..., notes = "") {
  cli::cli_abort("Your Rt method isn't supported yet. You should create a method.")
}

#' @rdname summarize_rtestimate
#' @export
#' @importFrom tibble tibble
#' @param level Confidence level for the confidence interval.
#' @param lambda The Poisson parameter (`cv_poisson_rt`).
summarize_rtestimate.cv_poisson_rt <- function(
    x, level = 0.95, lambda = c("lambda.1se", "lambda.min"), ...,
    notes = "cv_poisson_rt"
) {

  if (!requireNamespace("rtestim", quietly = TRUE)) {
    cli::cli_abort("You must install the {.pkg rtestim} package for this functionality.")
  }

  if (is.character(lambda)) {
    lambda <- x[[match.arg(lambda)]]
  } else {
    checkmate::assert_number(lambda, lower = 0)
  }
  cb <- rtestim::confband(x, lambda = lambda, level = level, ...)

  new_summrt(
    date = as.integer(x$full_fit$x),
    median = cb$fit,
    lb = cb[[2]], # danger
    ub = cb[[3]],
    level = level,
    package = "rtestim",
    notes = notes
  )
}

#' @rdname summarize_rtestimate
#' @importFrom stats median
#' @export
summarize_rtestimate.poisson_rt <- function(x, level = 0.95, lambda = NULL, ..., notes = "poisson_rt") {

  if (!requireNamespace("rtestim", quietly = TRUE)) {
    cli::cli_abort("You must install the {.pkg rtestim} package for this functionality.")
  }

  if (is.null(lambda)) {
    lambda <- 10^stats::median(log10(x$lambda))
  }
  checkmate::assert_number(lambda, lower = 0)
  cb <- rtestim::confband(x, lambda = lambda, level = level, ...)

  new_summrt(
    date = as.integer(x$x),
    median = cb$fit,
    lb = cb[[2]],
    ub = cb[[3]],
    level = level,
    package = "rtestim",
    notes = notes
  )
}

#' @rdname summarize_rtestimate
#' @export
#' @importFrom stats quantile
summarize_rtestimate.epinow <- function(x, level = 0.95, ..., notes = "") {

  if (!requireNamespace("EpiNow2", quietly = TRUE)) {
    cli::cli_abort("You must install the {.pkg EpiNow2} package for this functionality.")
  }

  y_extract <- rstan::extract(x$estimates$fit)$R
  t_max <- max(lubridate::ymd(x$estimates$observations$date), na.rm = TRUE)
  t_min <- min(lubridate::ymd(x$estimates$observations$date), na.rm = TRUE)
  t_length <- as.integer(t_max - t_min)
  t_length_forecast <- ncol(rstan::extract(x$estimates$fit)$R) - nrow(x$estimates$observations)

  probs <- level_to_probs(level)

  return(new_summrt(
    date = c(0:t_length, (t_length + 1):(t_length + t_length_forecast)),
    median = apply(y_extract, 2, stats::quantile, probs = 0.5),
    lb = apply(y_extract, 2, stats::quantile, probs = probs$lower),
    ub = apply(y_extract, 2, stats::quantile, probs = probs$upper),
    level = level,
    package = "EpiNow2",
    notes = notes
  ))

}

#' @export
#' @details The `estimate_R` method is for the `EpiEstim` package. Currently,
#'   only levels in 50%, 90% and 95% confidence levels are allowed.
#' @rdname summarize_rtestimate
summarize_rtestimate.estimate_R <- function(x, level = 0.95, ..., notes = "") {
  if (!requireNamespace("EpiEstim", quietly = TRUE)) {
    cli::cli_abort("You must install the {.pkg EpiEstim} package for this functionality.")
  }
  allowed_levels <- c(0.95, 0.9, 0.5)
  if (min(abs(level - allowed_levels)) > sqrt(.Machine$double.eps)) {
    cli::cli_abort(paste(
      "For {.pkg EpiEstim}, allowable confidence levels are",
      "{.val {allowed_levels}}, not {.val {level}}."
    ))
  }
  probs <- level_to_probs(level)
  probs <- lapply(probs, probs_to_char)
  lb_name <- paste0("Quantile.", probs$lower, "(R)")
  ub_name <- paste0("Quantile.", probs$upper, "(R)")
  new_summrt(
    date    = as.integer((x$R$t_end + x$R$t_start) / 2),
    median  = x$R[["Median(R)"]],
    lb      = x$R[[lb_name]],
    ub      = x$R[[ub_name]],
    level   = level,
    package = "EpiEstim",
    notes   = notes
  )
}

#' @export
#' @details The `Rt` method is for the `EpiLPS` package.
#' @rdname summarize_rtestimate
summarize_rtestimate.Rt <- function(x, level = 0.95, ..., notes = "") {
  if (!requireNamespace("EpiLPS", quietly = TRUE)) {
    cli::cli_abort("You must install the {.pkg EpiLPS} package for this functionality.")
  }

  allowed_levels <- c(0.95, 0.9, 0.5)
  if (min(abs(level - allowed_levels)) > sqrt(.Machine$double.eps)) {
    cli::cli_abort(paste(
      "For {.pkg EpiLPS}, allowable confidence levels are",
      "{.val {allowed_levels}}, not {.val {level}}."
    ))
  }
  probs <- level_to_probs(level)
  probs <- lapply(probs, probs_to_char)
  lb_name <- paste0("Rq", probs$lower)
  ub_name <- paste0("Rq", probs$upper)

  new_summrt(
    date    = x$RLPS$Time,
    median  = x$RLPS$Rq0.50,
    lb      = x$RLPS[[lb_name]],
    ub      = x$RLPS[[ub_name]],
    level   = level,
    package = "EpiLPS",
    notes   = notes
  )
}
