#' ADaM PASI - Wonderful Wednesdays
#'
#' The Psoriasis Area and Severity Index (PASI) is a continuous measurement
#' of severity, the lower, the better the patient condition. The
#' derived dataset represents patient level follow-up during 8 visits over a
#' year of monitoring.
#'
#' @inheritParams base::set.seed
#' @param n (`numeric`)\cr number of patients to keeps.
#' @source Derived from
#'   Wonderful Wednesdays GitHub data repository (2021-04-14).
#' @return A list with two ADaM datasets:
#' \describe{
#'   \item{ADSL}{subject level dataset}
#'   \item{ADPASI}{PASI records, follows basic data structure (BDS)}
#' }
#' @export
#' @examples
#'
#' adam_ww(2)
#'
adam_ww <- function(n = NULL, seed = 1) {
  y <- utils::read.table(
    file.path(
      "https://raw.githubusercontent.com/VIS-SIG/Wonderful-Wednesdays",
      "master/data/2021/2021-04-14/WWW_SustainedResponse.csv"
    ),
    header = TRUE,
    sep = ",",
    stringsAsFactors = FALSE
  )

  if (!is.null(n)) {
    set.seed(seed)
    y <- y[y$USUBJID %in% sample(y$USUBJID, size = n), ]
  }
  assertthat::assert_that(max(table(y$USUBJID)) == 1)

  adsl <- unique(y[c("USUBJID", "TRT", "BASELINE")])
  adsl$ARMCD <- factor( # nolint - ADaM standard
    adsl$TRT,
    levels = c(
      "COMPARATOR TREATMENT",
      "ACTIVE TREATMENT DOSE 01",
      "ACTIVE TREATMENT DOSE 02"
    ),
    labels = c("ARM A", "ARM B", "ARM C")
  )

  adpasi <- tidyr::gather(
    y[names(y) != "TRT"],
    key = "AVISIT",
    value = "AVAL",
    -"USUBJID"
  )
  adpasi$PARAMCD <- "PASITOT" # nolint - ADaM standard
  adpasi$AVISIT <- ifelse( # nolint - ADaM standard
    adpasi$AVISIT == "BASELINE",
    yes = "WEEK00",
    no = adpasi$AVISIT
  )

  list(
    adsl = adsl,
    adpasi = adpasi
  )
}
