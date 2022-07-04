#' Crosstab
#'
#' Function for generating cross-tabulations.
#'
#' @param var_ind A length-one character vector for the independent variable.
#' @param var_dep A length-one character vector for the dependent variable.
#' @param reverse A length-one character vector indicating whether to reverse the order of the variables.
#' @param weight A length-one character vector for the weight variable.
#' @param db A sqlite (`S4`) database connection object.
#'
#' @importFrom dplyr tbl select collect
#' @importFrom DT JS formatStyle formatString
#'
#' @return A `datatables` (subclass of `htmlwidget`) object.
#'
#' @export
crosstab <- function(var_ind, var_dep, reverse, weight = "weight", db) {

  # To prevent R CMD check notes
  `.` <- NULL

  # Check
  if (var_ind == var_dep) {
    stop("The fields selected cannot be crossed with themselves. Please reselect to deduplicate.", call. = FALSE)
  }

  # Reverse if "specified"
  if (reverse == "Yes") {
    temp <- var_dep
    var_dep <- var_ind
    var_ind <- temp
  }

  # Data
  df <- db |>
    tbl("survey_table") |>
    select(var_ind, var_dep, weight) |>
    collect() |>
    as.data.table()

  # Special handling for converting 'messaging' columns from numeric to character in-place
  df[, c(eval(var_ind), eval(var_dep)) := .(as.character(get(var_ind)), as.character(get(var_dep)))]

  # Cross-tabulation
  xtab <- moe_crosstab_internal(df, var_ind, var_dep, weight)

  # Format names in-place
  setnames(
    xtab,
    old = c(1, 2),
    new = c(
      str_format(str = var_ind),
      str_format(str = var_dep)
    )
  )

  # Return datatable object
  datatable(
    data = xtab,
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
            list(extend = "csv", filename = "two_way_crosstab"),
            list(extend = "pdf", filename = "two_way_crosstab")
          ),
          text = "Download"
        )
      ),
      lengthMenu = list(c(25, -1), c(25, "All"))
    )
  ) |>
    formatStyle(1, fontWeight = "bold") |>
    formatStyle(3:5, `text-align` = "right") |>
    formatString(columns = 3:5, suffix = " %")
}
