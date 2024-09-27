#' @importFrom tibble as_tibble
#' @export
tibble::as_tibble


#' Coerce summrt_summary object to a tibble
#'
#' @param x A `summrt_summary` object
#' @param .add_pkg_id Logical. Should the `pkg` field be included as a column.
#' @inheritParams tibble::tibble
#'
#' @return A [tibble::tibble()]
#' @export
#'
#' @examples
#' ex <- readRDS(system.file(
#'   "extdata", "EpiEstim_example.rds", package = "summrt"
#' ))
#' summ_ex <- summarize_rtestimate(ex)
#' as_tibble(summ_ex)
#' as_tibble(summ_ex, add_pkg_id = TRUE)
as_tibble.summrt_summary <- function(x, add_pkg_id = FALSE, ...) {
  out <- as_tibble(x$estimates)
  if (add_pkg_id) out$package = x$package
  out
}
