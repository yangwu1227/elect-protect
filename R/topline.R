#' Topline
#'
#' Function for generating toplines.
#'
#' @param var A length-one character vector for the topline variable.
#' @param weight A length-one character vector for the weight variable.
#' @param db A sqlite (`S4`) database connection object.
#'
#' @importFrom dplyr starts_with
#'
#' @return A `datatables` (subclass of `htmlwidget`) object.
#'
#' @export
topline <- function(var, weight = "weight", db) {

  # Branch 1) Multiple selection
  if (var %chin% c(
    "news_source",
    "most_concern",
    "least_concern",
    "least_informed"
  )) {
    # Data
    df <- db |>
      tbl("survey_table") |>
      select(starts_with(var), weight) |>
      collect() |>
      as.data.table()

    # Topline
    topline <- topline_multiselect_internal(df, var, weight)

    # Branch 2) Non-multiple-selection
  } else {
    # Data
    df <- db |>
      tbl("survey_table") |>
      select(var, weight) |>
      collect() |>
      as.data.table()

    # Topline
    topline <- topline_internal(df, var, weight)
  }

  # Return datatable object
  datatable(
    data = topline,
    rownames = FALSE,
    extensions = "Buttons",
    options = list(
      search = list(regex = TRUE, caseInsensitive = TRUE),
      deferRender = TRUE,
      scrollY = 300,
      scroller = TRUE,
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#46bdc6', 'color': 'white', 'font-weight': 'bold'});",
        "}"
      ),
      dom = "Blfrtip",
      buttons = list(
        "copy",
        "print",
        list(
          extend = "collection",
          buttons = list(
            list(extend = "csv", filename = "topline"),
            list(extend = "pdf", filename = "topline")
          ),
          text = "Download"
        )
      ),
      lengthMenu = list(c(-1, 25), c("All", 25))
    )
  ) |>
    formatStyle("Response", fontWeight = "bold") |>
    formatStyle(2:3, `text-align` = "right") |>
    formatString(columns = 3, suffix = " %")
}
