#' Plot (Crosstab)
#'
#' Grouped bar chart function for cross-tabulations.
#'
#' @param data A `datatable` object.
#'
#' @importFrom plotly plot_ly config layout
#' @importFrom stringi stri_detect
#' @importFrom stringr str_wrap
#'
#' @return A `plotly` (subclass of `htmlwidget`) object.
#'
#' @export
plot_xtab <- function(data) {
  # Data
  df <- as.data.table(data$x$data)
  cols <- names(df)
  xvar <- cols[[1]]
  yvar <- cols[[3]]
  color <- cols[[2]]
  df[, c(xvar, color, yvar) := .(
    str_wrap(get(xvar), width = 25),
    str_wrap(get(color), width = 25),
    as.double(get(yvar))
  )]
  title <- paste(xvar, sep = " by ", color)

  # To prevent R CMD check notes
  `.` <- `Education Rollup` <- `Democracy Fairness` <- `Party Identification` <- `Age` <- `Ideology` <- `Party` <- NULL

  # Education
  if (xvar == "Education Rollup" | color == "Education Rollup") {
    df[, `Education Rollup` := fcase(
      `Education Rollup` == "bach degree", "b. Bachelor's Degree",
      `Education Rollup` == "less than bach", "c. Less Than Bachelor's Degree",
      `Education Rollup` == "grad", "a. Graduate Degree",
      `Education Rollup` == "Dont know", "d. Don't Know"
    )]
  }
  # Messages
  if (stri_detect(xvar, regex = "Message")) {
    df[, eval(xvar) := fcase(
      get(xvar) == "-999", "a. -999",
      get(xvar) == "0", "b. 0",
      get(xvar) == "1", "c. 1",
      get(xvar) == "2", "d. 2",
      get(xvar) == "3", "e. 3",
      get(xvar) == "4", "f. 4",
      get(xvar) == "5", "g. 5",
      get(xvar) == "6", "h. 6",
      get(xvar) == "7", "i. 7",
      get(xvar) == "8", "j. 8",
      get(xvar) == "9", "k. 9",
      get(xvar) == "10", "l. 10"
    )]
  } else if (stri_detect(color, regex = "Message")) {
    df[, eval(color) := fcase(
      get(color) == "-999", "a. -999",
      get(color) == "0", "b. 0",
      get(color) == "1", "c. 1",
      get(color) == "2", "d. 2",
      get(color) == "3", "e. 3",
      get(color) == "4", "f. 4",
      get(color) == "5", "g. 5",
      get(color) == "6", "h. 6",
      get(color) == "7", "i. 7",
      get(color) == "8", "j. 8",
      get(color) == "9", "k. 9",
      get(color) == "10", "l. 10"
    )]
  }
  # Age
  if (xvar == "Age" | color == "Age") {
    df[, Age := fcase(
      Age == "18-34", "d. 18-34",
      Age == "35-54", "c. 35-54",
      Age == "55-64", "b. 55-64",
      Age == "65+", "a. 65+"
    )]
  }
  # Party
  if (xvar == "Party" | color == "Party") {
    df[, Party := fcase(
      Party == "Republican", "d. Republican",
      Party == "Independent, leaning Democratic", "c. Independent, leaning Republican",
      Party == "Independent, leaning Democratic", "b. Independent, leaning Democratic",
      Party == "Democrat", "a. Democract",
      Party == "Dont know", "e. Don't Know",
      default = NA
    )]
  }
  # Ideology
  if (xvar == "Ideology" | color == "Ideology") {
    df[, Ideology := fcase(
      Ideology == "Very conservative", "d. Very conservative",
      Ideology == "Somewhat conservative", "c. Somewhat conservative",
      Ideology == "Somewhat liberal", "b. Somewhat liberal",
      Ideology == "Very liberal", "a. Very liberal",
      Ideology == "Dont know", "e. Don't Know",
      default = NA
    )]
  }
  # Democracy Fairness
  if (xvar == "Democracy Fairness" | color == "Democracy Fairness") {
    df[, `Democracy Fairness` := fcase(
      `Democracy Fairness` == "Very unfairly", "d. Very unfairly",
      `Democracy Fairness` == "Somewhat unfairly", "c. Somewhat unfairly",
      `Democracy Fairness` == "Somewhat fairly", "b. Somewhat fairly",
      `Democracy Fairness` == "Very fairly", "a. Very fairly",
      `Democracy Fairness` == "DK", "e. Don't Know",
      default = NA
    )]
  }
  # Party Identification
  if (xvar == "Party Identification" | color == "Party Identification") {
    df[, `Party Identification` := fcase(
      `Party Identification` == "Strongly dont identify", "d. Strongly dont identify",
      `Party Identification` == "Somewhat dont identify", "c. Somewhat dont identify",
      `Party Identification` == "Somewhat identify", "b. Somewhat identify",
      `Party Identification` == "Strongly identify", "a. Strongly identify",
      `Party Identification` == "Dont know", "e. Don't Know",
      default = NA
    )]
  }
  # Plotly
  plot_ly(
    data = df,
    x = ~ get(yvar),
    y = ~ get(xvar),
    color = ~ get(color),
    colors = c(
      "#32BDB9", "#036E6B", "#FF7F0E", "#DBDC29", "#6929C4", "#2CA02C",
      "#8DE2DE", "#9F1853", "#E377C2", "#012749", "#D62728"
    ),
    type = "bar",
    orientation = "h",
    text = "%{x}%",
    textposition = "outside",
    texttemplate = "%{x}%",
    hoverinfo = "x+z",
    hovertemplate = "Group: %{fullData.name} <br> Percent: %{x}% <extra></extra>",
    opacity = 0.9,
    textfont = list(
      family = "Open Sans",
      size = 13
    ),
    hoverlabel = list(
      align = "left"
    )
  ) |>
    layout(
      font = list(
        family = "Open Sans",
        size = 13
      ),
      margin = list(t = 60),
      title = title,
      xaxis = list(
        title = "",
        showticklabels = FALSE
      ),
      yaxis = list(
        title = "",
        tickfont = list(
          family = "Open Sans"
        )
      )
    ) |>
    config(
      modeBarButtonsToRemove = list(
        "select", "lasso", "zoomIn", "zoomOut", "resetScale"
      ),
      toImageButtonOptions = list(
        format = "png",
        filename = "crosstab_grouped_bar",
        height = 700,
        width = 900,
        scale = 1
      )
    )
}

#' Plot (Topline)
#'
#' Bar chart for toplines.
#'
#' @param data A `datatable` object.
#' @param var A length-one character vector for the topline variable.
#'
#' @importFrom stringi stri_startswith_fixed
#' @importFrom utils head
#'
#' @return A `plotly` (subclass of `htmlwidget`) object.
#'
#' @export
plot_topline <- function(data, var) {
  # Data, excluding the 'Totals' row
  df <- head(as.data.table(data$x$data), -1)
  df[, Response := str_wrap(Response, width = 25)]
  title <- str_format(str = var)

  # To prevent R CMD check notes
  `Response` <- NULL

  # Plotly
  if (stri_startswith_fixed(str = var, pattern = "message")) {
    plot_ly(
      data = df,
      x = ~Percent,
      y = ~Response,
      type = "bar",
      orientation = "h",
      text = title,
      texttemplate = "%{x}%",
      hovertemplate = "%{text} <br> %{x}% %{y} <extra></extra>",
      opacity = 0.85,
      marker = list(color = c("#46bdc6")),
      textfont = list(
        family = "Open Sans",
        size = 13
      ),
      hoverlabel = list(
        align = "left"
      )
    ) |>
      layout(
        font = list(
          family = "Open Sans",
          size = 13
        ),
        margin = list(t = 60),
        title = title,
        xaxis = list(title = "", showticklabels = FALSE),
        yaxis = list(
          categoryorder = "array",
          categoryarray = df[[1]],
          title = "",
          tickfont = list(
            family = "Open Sans"
          )
        )
      ) |>
      config(modeBarButtonsToRemove = list(
        "select", "lasso", "zoomIn", "zoomOut", "resetScale"
      ))
  } else {
    plot_ly(
      data = df,
      x = ~Percent,
      y = ~Response,
      type = "bar",
      orientation = "h",
      text = title,
      texttemplate = "%{x}%",
      hovertemplate = "%{text} <br> %{x}% %{y} <extra></extra>",
      opacity = 0.85,
      marker = list(color = c("#46bdc6")),
      textfont = list(
        family = "Open Sans",
        size = 13
      ),
      hoverlabel = list(
        align = "left"
      )
    ) |>
      layout(
        font = list(
          family = "Open Sans",
          size = 13
        ),
        margin = list(t = 60),
        title = title,
        xaxis = list(title = "", showticklabels = FALSE),
        yaxis = list(
          categoryorder = "total ascending",
          title = "",
          tickfont = list(
            family = "Open Sans"
          )
        )
      ) |>
      config(
        modeBarButtonsToRemove = list(
          "select", "lasso", "zoomIn", "zoomOut", "resetScale"
        ),
        toImageButtonOptions = list(
          format = "png",
          filename = "topline_bar_chart",
          height = 700,
          width = 900,
          scale = 1
        )
      )
  }
}
