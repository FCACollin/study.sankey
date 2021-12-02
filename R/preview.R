#' Preview in Viewer Pane
#'
#' Intend to preview files or html page directly into the viewer pane of
#' the RStudio environment (works on unix system).
#'
#' @param ... (`character`)\cr concatenated with [file.path()].
#' @param url_path (`character`)\cr eventually, a url path.
#'
#' @details The file path provided via `...` has priority over `url_path`.
#'   The function is expected to work at least with `.html`, `.pdf`, `.mp4`.
#'
#' @export
#' @examples
#'
#' \dontrun{# Needs an active RStudio session
#'
#' # The input for file path is rather permissive:
#' preview("/opt", "software", "R-3.6.2/doc/manual/R-admin.html")
#' preview("/opt/software/R-3.6.2/doc/manual/R-admin.html")
#'
#' # Works with url:
#' preview(url_path = "https://fcacollin.github.io/Latarnia/portfolio/")
#'
#' # Works with mp4 urls:
#' preview(
#'   url_path = paste0(
#'     "https://www.ucb.com/_up/ucb_com_stories_media/videos/",
#'     "UCB90__Our_history_1080p_25fps_H264-128kbit_AAC.mp4"
#'   )
#' )
#'
#' # Work with pdf:
#' preview("/opt/software/R-3.6.2/doc/NEWS.pdf")
#'
#' }
#' # Should not work if file path points at a directory:
#' testthat::expect_error(
#'   preview(getwd())
#' )
#'
preview <- function(..., url_path = NULL) {

  file_path <- do.call(file.path, list(...))

  if (length(file_path) > 0L) {
    assertthat::assert_that(
      Negate(dir.exists)(file_path),
      file.exists(file_path)
    )
    url_path <- if (Sys.getenv("RSTUDIO_HTTP_REFERER") == "") {
      message(
        paste(
          sapply(
            c(
              "The `preview()` may not be working as intended as the
              environment variable RSTUDIO_HTTP_REFERER is not defined.
              This happens for instance when working in a different environment
              than RStudio or if R was restarted within the RStudio session.",
              "If you want to see the output in the viewer pane,
              start a new RStudio session."
            ),
            function(x) paste(strwrap(x), collapse = "\n")
          ),
          collapse = "\n\n"
        )
      )
      file_path
    } else {
      paste0(
        Sys.getenv("RSTUDIO_HTTP_REFERER"),
        "/file_show?path=", file_path
      )
    }
  }

  viewer <- getOption("viewer")
  viewer <- if (!is.null(viewer))
    viewer
  else
    utils::browseURL
  viewer(url_path)
}
