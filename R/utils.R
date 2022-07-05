#' Remove Underscore and Convert to Title Case
#'
#' Function for formatting column names for display.
#'
#' @param str A length-one character vector to be formatted.
#'
#' @importFrom stringi stri_trans_totitle stri_replace_all_regex
#'
#' @return A length-one character vector.
#'
#' @export
str_format <- function(str) {
  stri_trans_totitle(stri_replace_all_regex(str, pattern = "_", replacement = " "))
}


#' Function for Accesses Files in the Current Package
#'
#' @param ... Character vectors, specifying subdirectory and file(s) within the package.
#'
#' @export
app_sys <- function(...) {
  system.file(..., package = "ElectProtect")
}


#' Function for computing ranked DataFrame
#'
#' Function to rank multiple-selection columns after grouping by a modeled column.
#'
#' @param df A data frame.
#' @param cols A character vector of multiple selection column names to include in the ranking.
#' @param model_col A length-one character vector for the modeled column.
#' @param multiselect_cat A length-one character vector for the "selected" option for each multiple-selection column.
#'
#' @return A data frame.
#'
#' @keywords internal
rank_multi_select <- function(df, cols, model_col, multiselect_cat) {
  frames <- lapply(
    X = cols,
    FUN = function(col) {
      # Get percentages of 'multiselect_cat' (selection multiple selection option) for each group in 'model_col'
      frame <- df[,
                  {
                    group_weight <- sum(weight)
                    .SD[, .(percent = sum(weight) / group_weight), by = col]
                  },
                  by = model_col
      ][get(col) == multiselect_cat, c(1, 3)]

      # Renmove NA group in 'model_col'
      frame <- na.omit(frame, cols = model_col)
      # Rename
      setnames(x = frame, old = "percent", new = col)
      frame
    }
  )

  # Join based on modeling column
  joined_frame <- Reduce(f = function(frame_x, frame_y) {
    merge(frame_x, frame_y, by = model_col)
  }, x = frames)

  # Transpose
  out <- transpose(joined_frame, keep.names = "rn")
  col_names <- as.character(out[1, ])
  # Set first row as header
  out <- out[2:.N]
  setnames(x = out, old = names(out), new = col_names)

  out
}
