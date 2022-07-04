#' Interpret (Cross-tabulation)
#'
#' Function for generating template interpretations for the cross-tabulations.
#'
#' @param var_ind A length-one character vector for the independent variable.
#' @param var_dep A length-one character vector for the dependent variable.
#' @param reverse A length-one character vector indicating whether to reverse the order of the variables.
#'
#' @return A `shiny.tag.list` object.
#'
#' @export
interpret_xtab <- function(var_ind, var_dep, reverse) {
  if (reverse == "No") {
    tagList(
      p(
        paste0(
          "Among the respondent population specified by the first field (",
          str_format(var_ind),
          "), the proportion of respondents in each category of the second field (",
          str_format(var_dep),
          "), is given by Percent % plus or minus MOE %; for comparison, the proportion of respondents in each category of the second field from among the entire survey is given by Survey Total Percent %."
        )
      ),
      br(),
      p(
        paste0(
          "Whenever Percent % is greater than Survey Total Percent %, we say that such category of the second field (",
          str_format(var_dep),
          ") is over-indexed relative to the overall survey."
        )
      )
    )
  } else {
    tagList(
      p(
        paste0(
          "Among the respondent population specified by the first field (",
          str_format(var_dep),
          "), the proportion of respondents in each category of the second field (",
          str_format(var_ind),
          "), is given by Percent % plus or minus MOE %; for comparison, the proportion of respondents in each category of the second field from among the entire survey is given by Survey Total Percent %."
        )
      ),
      br(),
      p(
        paste0(
          "Whenever Percent % is greater than Survey Total Percent %, we say that such category of the second field (",
          str_format(var_ind),
          ") is over-indexed relative to the overall survey."
        )
      )
    )
  }
}

#' Interpret (Topline)
#'
#' Function for generating template interpretations for the toplines.
#'
#' @param var A length-one character vector for the topline variable.
#' @param section_var A length-one character vector for the section variable used to determine multiple-selection versus non-multiple-selection questions.
#'
#' @return A `shiny.tag.list` object.
#'
#' @export
interpret_topline <- function(var, section_var) {
  if (section_var == "Multiple-Selection Questions") {
    paste0(
      "'Response' lists the categories of your selected field (",
      str_format(var),
      "); 'Frequency' tallies the count of each category of that field, and 'Percent' computes the proportional breakdowns of these categories in the entire survey.
          For multiple-selection questions, the last row of the table represents the total number of respondents who has seen the question and not just the sum of
      the 'Frequency' column as in the case for non-multiple-selection questions."
    )
  } else {
    paste0(
      "'Response' lists the categories of your selected field (",
      str_format(var),
      "); 'Frequency' tallies the count of each category of that field, and 'Percent' computes the proportional breakdowns of these categories in the entire survey."
    )
  }
}
