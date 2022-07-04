#' Theme
#'
#' Custom theme function for the dashboard.
#'
#' @importFrom fresh use_theme create_theme bs4dash_color bs4dash_yiq bs4dash_sidebar_light bs4dash_vars bs4dash_status
#'
#' @return A `shiny.tag` object.
#'
#' @export
my_theme <- function() {
  use_theme(
    create_theme(
      # Colors for navbar, sidebar (selected tab) background, text color
      bs4dash_color(
        teal = "#46bdc6"
      ),
      bs4dash_status(
        # Text color for inactive tabPanel in Tabbox uses primary, set to gray
        primary = "#6c757d"
      ),
      # The lightness value that determines when the lightness of color changes from "dark" to "light"
      bs4dash_yiq(
        # Lower values are more sensitive to contrast
        contrasted_threshold = 10,
        # White
        text_dark = "#FFF",
        # Dark
        text_light = "#272c30"
      ),
      bs4dash_vars(
        # The bs4DashBrand function uses "font-weight-light", which has value 300
        # Change to semi-bold
        "font-weight-light" = "500"
      )
    )
  )
}
