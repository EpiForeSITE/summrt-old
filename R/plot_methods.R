#' @importFrom ggplot2 autoplot .data
#' @export
ggplot2::autoplot


#' Plot a summrt_summary object
#'
#' @param object A `summrt_summary` object
#' @param color A character string giving a color
#' @param add_reference_line Logical. Display a horizontal line at 1.
#' @param ... not used.
#'
#' @return A [ggplot2::ggplot()] object.
#' @export
#'
#' @examples
#' ex <- readRDS(system.file(
#'   "extdata", "EpiEstim_example.rds", package = "summrt"
#' ))
#' summ_ex <- summarize_rtestimate(ex)
#' autoplot(summ_ex)
#' autoplot(summ_ex) + ggplot2::coord_cartesian(ylim = c(0, 2))
autoplot.summrt_summary <- function(
    object, color = "dodgerblue4", add_reference_line = TRUE, ...
) {
  dat <- as_tibble(object)
  level <- ""
  if (!is.null(object$level)) {
    level <- paste0(sprintf("%02.0f", object$level), "% ")
  }
  ylabel <- paste0("Estimated Rt w/ ", level, "confidence band")
  plot_obj <- ggplot2::ggplot(dat, ggplot2::aes(.data$date)) +
    ggplot2::geom_line(ggplot2::aes(y = .data$median), col = color) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lb, ymax = .data$ub),
                         fill = color, alpha = .2) +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(label = object$package) +
    ggplot2::ylab(ylabel)

  if (add_reference_line) {
    plot_obj <- plot_obj + ggplot2::geom_hline(yintercept = 1)
  }
  plot_obj
}
