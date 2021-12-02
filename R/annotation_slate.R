#' Annotation Slate
#'
#' @param margin (`unit`)\cr top, right, bottom, left margins.
#' @param gp (`list`)\cr graphical parameter, see [grid::gpar()].
#' @param page (`grob`)\cr a page as initialised by `clean_slate()`.
#' @param gr (`grob`)\cr a graphical object.
#' @param guide (`logical`)\cr display rectangle delimitating a page area.
#' @param width,height (`numeric` or `unit`).
#' @param left,right,text (`character`)\cr annotation texts.
#' @param str_width (`numeric`)\cr maximal width.
#' @param x (`numeric` or `unit`)\cr horizontal position.
#' @param skipped_lines (`numerix`)\cr number of lines to skip.
#' @param ... arguments passed to other functions.
#' @name slate
#' @examples
#'
#' library(magrittr)
#' library(ggplot2)
#'
#' clean_slate() %>%
#'   add_figure(gr = circleGrob()) %>%
#'   grid.draw()
#'
#' # Default margins are for an A4 page, change it for the plot viewer
#' clean_slate(margin = unit(rep(0.05, 4), units = "npc")) %>%
#'   add_figure(gr = circleGrob()) %>%
#'   grid.draw()
#'
#' # Of course, it works with ggplot objects
#' dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
#' gg <- ggplot(dsamp, aes(carat, price)) +
#'   geom_point(aes(colour = clarity), show.legend = FALSE)
#'
#' clean_slate(margin = unit(rep(0.05, 4), units = "npc")) %>%
#'   add_figure(gr = gg) %>%
#'   grid.draw()
#'
#' # Then add annotation elements, for instance, a title ...
#' clean_slate(margin = unit(rep(0.05, 4), units = "npc")) %>%
#'   add_figure(gr = gg, height = .5) %>%
#'   add_title("Figure 1") %>%
#'   grid.draw()
#'
#' # ... and / or other elements ...
#' clean_slate(margin = unit(rep(0.05, 4), units = "npc")) %>%
#'   add_figure(gr = gg) %>%
#'   add_header(left = "Topic", right = "CONFIDENTIAL") %>%
#'   add_title("Figure 1") %>%
#'   add_note(text = "Figure annotation") %>%
#'   add_footer(left = "Program: dm.utils", right = "Page 1 of 1") %>%
#'   grid.draw()
#'
#' # ... except `clean_slate` and `grid.draw`, the order does not matter
#' clean_slate(margin = unit(rep(0.05, 4), units = "npc")) %>%
#'   add_note(text = "Figure annotation") %>%
#'   add_header(left = "Topic", right = "CONFIDENTIAL") %>%
#'   add_title("Figure 1") %>%
#'   add_figure(gr = gg) %>%
#'   add_footer(left = "Program: dm.utils", right = "Page 1 of 1") %>%
#'   grid.draw()
#'
#' # For several lines of annotation, provide a vector
#' clean_slate(margin = unit(rep(0.05, 4), units = "npc")) %>%
#'   add_figure(gr = gg) %>%
#'   add_header(
#'     left = c("Topic", "Info / PS9999", "Code"),
#'     right = c("CONFIDENTIAL", "Draft")
#'   ) %>%
#'   add_title(text =  c("Figure 1", "title", "Dataset")) %>%
#'   add_note(text = "Figure annotation") %>%
#'   add_footer(left = "Program: dm.utils", right = "Page 1 of 1") %>%
#'   grid.draw()
#'
#' # By default, notes start at 10% of width and span over 80 characters
#' clean_slate(margin = unit(rep(0.05, 4), units = "npc")) %>%
#'   add_figure(gr = gg, width = .5) %>%
#'   add_header(
#'     left = c("Topic", "Info / PS9999", "Code"),
#'     right = c("CONFIDENTIAL", "Draft")
#'   ) %>%
#'   add_title(text = c("Figure 1", "title", "Dataset")) %>%
#'   add_note(
#'     text = c(
#'       "\"There are certain queer times and occasions in this strange mixed
#'       affair we call life when a man takes this whole universe for a vast
#'       practical joke, though the wit thereof he but dimly discerns, and more
#'       than suspects that the joke is at nobody's expense but his own.\"",
#'       "Herman Melville, Moby-Dick"
#'     )
#'   ) %>%
#'   add_footer(left = "Program: dm.utils", right = "Page 1 of 1") %>%
#'   grid.draw()
#'
#' # Check the layout by adding guides
#' clean_slate(margin = unit(rep(0.05, 4), units = "npc")) %>%
#'   add_figure(gr = gg) %>%
#'   add_header(
#'     left = c("Topic", "Info / PS9999", "Code"),
#'     right = c("CONFIDENTIAL", "Draft"),
#'     guide = TRUE
#'   ) %>%
#'   add_title(text = c("Figure 1", "title", "Dataset")) %>%
#'   add_note(
#'     text = c(
#'       "\"There are certain queer times and occasions in this strange mixed
#'       affair we call life when a man takes this whole universe for a vast
#'       practical joke, though the wit thereof he but dimly discerns, and more
#'       than suspects that the joke is at nobody's expense but his own.\"",
#'       "Herman Melville, Moby-Dick"
#'     )
#'   ) %>%
#'   add_footer(left = "Program: dm.utils", right = "Page 1 of 1") %>%
#'   grid.draw()
#'
NULL

#' @describeIn slate start an annotation page.
#' @export
#' @importFrom grid unit gpar grid.newpage gTree viewport grid.layout
#'
clean_slate <- function(
  margin = grid::unit(
    c(t = 2.54, r = 4.2, b = 2.54, l = 2.54),
    units = "cm"
  ),
  gp = grid::gpar(
    lineheight = 1,
    fontfamily = "Courier",
    fontsize = 9
  )) {
  grid::grid.newpage()
  grid::gTree(
    vp = grid::viewport(
      name = "lyt",
      x = margin[4],
      y = margin[3],
      just = c("left", "bottom"),
      width = grid::unit(1, "npc") - margin[4] - margin[2],
      height = grid::unit(1, "npc") - margin[1] - margin[3],
      gp = gp,
      layout = grid::grid.layout(
        nrow = 5,
        ncol = 2,
        heights = grid::unit(
          c(0, 0, 1, 0, 0),
          units = c("lines", "lines", "null", "lines", "lines")
        )
      )
    )
  )
}

#' @describeIn slate add a figure to a clean page.
#' @export
#'
add_figure <- function(page,
                       gr,
                       guide = FALSE,
                       width = 1,
                       height = 1) {
  if (methods::is(gr, "gg")) gr <- ggplot2::ggplotGrob(gr)

  vp <- grid::viewport(
    width = width,
    height = height,
    name = "fig",
    layout.pos.row = 3
  )
  if (guide) page <- grid::addGrob(page, child = grid::rectGrob(vp = vp))

  grid::addGrob(
    gTree = page,
    child = grid::gTree(
      childrenvp = grid::vpTree(
        parent = vp,
        children = grid::vpList(
          grid::viewport(
            name = "plot",
            height = height,
            width = width
          )
        )
      ),
      children = grid::gList(grid::editGrob(gr, vp = "fig::plot"))
    )
  )
}

#' @noRd
#'
add_in_split_row <- function(page, row, left, right, guide = FALSE) {
  nlines <- max(1, length(left), length(right))
  page$vp$layout$heights[[row]] <- nlines
  vp <- list(
    left = grid::viewport(
      name = "lhs", layout.pos.row = row, layout.pos.col = 1
    ),
    right = grid::viewport(
      name = "rhs", layout.pos.row = row, layout.pos.col = 2
    )
  )
  if (guide) {
    page <- grid::addGrob(page, child = grid::rectGrob(vp = vp$left))
    page <- grid::addGrob(page, child = grid::rectGrob(vp = vp$right))
  }

  y_pos <- function(x, n = nlines) grid::unit(n - seq_along(x) + 0.25, "lines")
  page <- grid::addGrob(
    page,
    child = grid::textGrob(
      x = 0, y = y_pos(left), just = c("left", "bottom"),
      label = left, vp = vp$left
    )
  )
  grid::addGrob(
    page,
    child = grid::textGrob(
      x = 1, y = y_pos(right),
      just = c("right", "bottom"),
      label = right, vp = vp$right
    )
  )
}

#' @describeIn slate add a header.
#' @export
#'
add_header <- function(page, left, right, ...) {
  add_in_split_row(page = page, row = 1, left = left, right = right, ...)
}

#' @describeIn slate add a footer.
#' @export
#'
add_footer <- function(page, left, right, ...) {
  add_in_split_row(page = page, row = 5, left = left, right = right, ...)
}

#' @describeIn slate add a title.
#' @export
#'
add_title <- function(page, text, guide = FALSE) {
  page$vp$layout$heights[[2]] <- length(text)
  vp <- grid::viewport(name = "tle", layout.pos.row = 2)
  if (guide) page <- grid::addGrob(page, child = grid::rectGrob(vp = vp))
  grid::addGrob(
    page,
    child = grid::textGrob(
      y = grid::unit(seq_along(text) - 0.25, "lines"),
      just = "top",
      label = rev(text), vp = vp
    )
  )
}

#' @describeIn slate add a note.
#' @export
#'
add_note <- function(page, text,
                     guide = FALSE,
                     str_width = 80,
                     x = 0.1,
                     skipped_lines = c(1, 0)) {
  text <- sapply(text, strwrap, width = str_width)
  text <- unname(text)
  text <- unlist(text)
  page$vp$layout$heights[[4]] <- length(text) + sum(skipped_lines)
  vp <- grid::viewport(name = "note", layout.pos.row = 4)
  if (guide) page <- grid::addGrob(page, child = grid::rectGrob(vp = vp))
  grid::addGrob(
    page,
    child = grid::textGrob(
      x = x,
      y = grid::unit(seq_along(text) + skipped_lines[1] - 0.25, "lines"),
      label = rev(text),
      just = c("left", "top"),
      vp = vp
    )
  )
}
