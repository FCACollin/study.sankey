#' Theme for ggplot2
#'
#' Very sober graphical theme.
#'
#' @param ... \cr Arguments passed to [ggplot2::theme()].
#' @import ggplot2
#' @export
#' @examples
#'
#' library(ggplot2)
#' g <- ggplot(mpg, aes(class)) +
#'   geom_bar()
#' g
#' g + theme_diane()
#'
theme_diane <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(size = 7, color = "#1c1c1c"),
      title = element_text(color = "#555555", face = "bold.italic"),
      axis.text.y = element_blank(),
      axis.title = element_blank(),
      legend.key.width = unit(.75, "lines"),
      legend.title = element_blank(),
      panel.grid.major.x = element_blank()
    ) +
    theme(...)
}
