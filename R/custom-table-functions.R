#' Design effect
#'
#' Formula taken from \href{https://www.pewresearch.org/internet/2010/04/27/methodology-85/}{link}.
#'
#' @param weight A double vector of weights.
#'
#' @return A length-one double vector.
#'
#' @keywords internal
design_effect <- function(weight) {
  (length(weight) * sum(weight^2)) / (sum(weight)^2)
}

#' Design effect with margin of error
#'
#' Formula taken from \href{https://www.pewresearch.org/internet/2010/04/27/methodology-85/}{link}.
#'
#' @param percent A double vector of percentages.
#' @param deff A length-one double vector.
#' @param n A double vector of unweighted sample sizes.
#' @param zscore A length-one double vector of zscore.
#'
#' @return A length-one double vector.
#'
#' @keywords internal
moe_design_effect <- function(percent, deff, n, zscore = 1.96) {
  sqrt(deff) * zscore * sqrt((percent * (1 - percent)) / (n - 1)) * 100
}

#' Topline
#'
#' Internal topline function for non-multiple-selection questions.
#'
#' @param df A data frame.
#' @param variable A length-one character vector for the topline variable.
#' @param weight A length-one character vector for the weight variable.
#'
#' @importFrom labelled to_factor
#' @importFrom forcats fct_explicit_na
#' @importFrom dplyr first
#'
#' @return A `data.table` object.
#'
#' @keywords internal
topline_internal <- function(df, variable, weight) {

  # Rename columns to avoid lazy evaluation until data.table version 1.14.3 is released
  setnames(df, c(variable, weight), c("variable", "weight"))

  # To prevent R CMD check notes
  `.` <- valid_total <- Response <- NULL

  topline <- df[, variable := {
    var <- to_factor(variable, sort_levels = levels)
    var <- fct_explicit_na(var)
    .(var)
  }][, valid_total := .(sum(weight[variable != "(Missing)"]))][, .(
    Frequency = round(sum(weight), digits = 0),
    Percent = round((sum(weight) / first(valid_total) * 100), digits = 1)
  ),
  keyby = .(variable)
  ][variable != "(Missing)"]

  # Rename
  setnames(topline, c("variable", "Frequency", "Percent"), c("Response", "Frequency", "Percent"))

  # Add a row of totals (sum down)
  topline <- rbindlist(list(topline, data.table(
    Response = "Total",
    Frequency = sum(topline$Frequency),
    Percent = sum(topline$Percent)
  )))

  topline
}

#' Topline (multiple-selection)
#'
#' Internal topline function for multiple-selection questions.
#'
#' @param df A data frame.
#' @param var A length-one character vector for removing prefixes.
#' @param weight A length-one character vector for the weight variable.
#' @param group A group variable for determining which code path to execute.
#'
#' @return A `data.table` object.
#'
#' @keywords internal
topline_multiselect_internal <- function(df, var, weight, group) {

  # Multiple selection questions
  cols <- setdiff(names(df), weight)
  # Count column frequency (sum of weight vector)
  sum_vec <- multiselect_helper(df, var, weight, group, cols)

  # Topline
  topline <- data.table(
    Response = str_format(str = cols),
    Frequency = round(sum_vec, digits = 0),
    Percent = round(sum_vec / sum(df[[weight]]) * 100, digits = 1)
  )
  # Add a row of totals (sum down)
  topline <- rbindlist(list(
    topline,
    data.table(Response = "Total", Frequency = round(sum(df[[weight]]), digits = 0), Percent = NA)
  ))

  topline
}


#' Helper for handling multiple selection questions
#'
#' @param df A data frame.
#' @param var A length-one character vector for removing prefixes.
#' @param weight A length-one character vector for the weight variable.
#' @param group A group variable for determining which code path to execute.
#' @param cols A character vector of multiple-selection column names.
#'
#' @return A double vector.
#'
#' @keywords internal
multiselect_helper <- function(df, var, weight, group, cols) {
  if (group == "news_source") {

    # Remove 'none above' and 'dislike news' from list
    cols_minus_none_dislike <- cols[1:(length(cols) - 2)]

    sum_list <- lapply(
      X = cols_minus_none_dislike,
      FUN = function(x) {
        # Set 'x' in 'cols' as key
        setkeyv(x = df, cols = x)
        # Subset rows to eliminate "Non-group", then sum the 'weight' column
        sum <- sum(df["My preferred news source"][[weight]])
        setkeyv(x = df, cols = NULL)
        # Return a single double vector
        sum
      }
    )

    # Special handling for 'dislike news'
    setkeyv(x = df, cols = cols[length(cols) - 1])
    sum_dislike <- sum(df["I dislike news"][[weight]])
    setkeyv(x = df, cols = NULL)

    # Special handling for 'none above'
    setkeyv(x = df, cols = cols[length(cols)])
    sum_none <- sum(df["Selected"][[weight]])
    setkeyv(x = df, cols = NULL)

    c(unlist(sum_list), sum_dislike, sum_none)
  } else if (group == "most_concern") {

    # Remove 'none above' from list
    cols_minus_none <- cols[-length(cols)]

    sum_list <- lapply(
      X = cols_minus_none,
      FUN = function(x) {
        # Set 'x' in 'cols' as key
        setkeyv(x = df, cols = x)
        # Subset rows to eliminate "Non-group", then sum the 'weight' column
        sum <- sum(df["Most concerning"][[weight]])
        setkeyv(x = df, cols = NULL)
        # Return a single double vector
        sum
      }
    )

    # Special handling for 'none above'
    setkeyv(x = df, cols = cols[length(cols)])
    sum_vec <- c(unlist(sum_list), sum(df["Selected"][[weight]]))
    setkeyv(x = df, cols = NULL)

    sum_vec
  } else if (group == "least_concern") {

    # Remove 'none above' from list
    cols_minus_none <- cols[-length(cols)]

    sum_list <- lapply(
      X = cols_minus_none,
      FUN = function(x) {
        # Set 'x' in 'cols' as key
        setkeyv(x = df, cols = x)
        # Subset rows to eliminate "Non-group", then sum the 'weight' column
        sum <- sum(df["Least concerning"][[weight]])
        setkeyv(x = df, cols = NULL)
        # Return a single double vector
        sum
      }
    )

    # Special handling for 'none above'
    setkeyv(x = df, cols = cols[length(cols)])
    sum_vec <- c(unlist(sum_list), sum(df["Selected"][[weight]]))
    setkeyv(x = df, cols = NULL)

    sum_vec
  } else if (group == "least_informed") {

    # Remove 'none above' from list
    cols_minus_none <- cols[-length(cols)]

    sum_list <- lapply(
      X = cols_minus_none,
      FUN = function(x) {
        # Set 'x' in 'cols' as key
        setkeyv(x = df, cols = x)
        # Subset rows to eliminate "Non-group", then sum the 'weight' column
        sum <- sum(df["Least informed about"][[weight]])
        setkeyv(x = df, cols = NULL)
        # Return a single double vector
        sum
      }
    )

    # Special handling for 'none above'
    setkeyv(x = df, cols = cols[length(cols)])
    sum_vec <- c(unlist(sum_list), sum(df["Selected"][[weight]]))
    setkeyv(x = df, cols = NULL)

    sum_vec
  }
}

#' Crosstab
#'
#' Internal crosstab function.
#'
#' @param df A data frame.
#' @param x A length-one character vector for the independent variables.
#' @param y A length-one character vector for the dependent variable.
#' @param weight A length-one character vector for the weight variable.
#'
#' @return A `data.table` object.
#'
#' @keywords internal
moe_crosstab_internal <- function(df, x, y, weight) {

  # Rename columns to avoid lazy evaluation until data.table version 1.14.3 is released
  setnames(df, c(x, y, weight), c("x", "y", "weight"))

  # To prevent R CMD check notes
  `.` <- valid_total <- total <- unweighted_n <- Percent <- N <- `..deff` <- `get.1` <- MOE <- `Survey Total Percent` <- observations <- NULL

  # Design effect
  deff <- design_effect(df[, weight])

  # Xtab
  xtab <- df[!is.na(x) & !is.na(y)][, c(x, y) := .(
    to_factor(x),
    to_factor(y)
  )][, c("total", "unweighted_n") := .(
    sum(weight),
    length(weight)
  ), by = x][, .(
    observations = sum(weight),
    N = first(total),
    unweighted_n = first(unweighted_n)
  ),
  keyby = .(x, y)
  ][, Percent := observations / N][, `:=`(MOE = round(
    moe_design_effect(Percent, ..deff, unweighted_n, zscore = 1.96),
    digits = 1
  ))][, `:=`(Percent = round(Percent * 100, digits = 1))][, .(x, y, Percent, MOE)]

  # Lookup table to add "Survey Total Percent" column
  lookup_tbl <- df[, y := {
    var <- to_factor(y, sort_levels = "values")
    var <- fct_explicit_na(var)
    .(var)
  }][, `:=`(valid_total = sum(weight[y != "(Missing)"]))][, .(Percent = round((sum(weight) / first(valid_total) * 100), digits = 1)),
    keyby = .(y)
  ][y != "(Missing)"]
  lookup <- lookup_tbl$Percent
  names(lookup) <- lookup_tbl$y

  xtab[, `Survey Total Percent` := lookup[y]]

  setnames(xtab, c("x", "y"), c(x, y))

  xtab
}
