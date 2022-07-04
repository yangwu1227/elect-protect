#' Sidebar
#'
#' Function for the dashboard sidebar.
#'
#' @importFrom bs4Dash bs4DashSidebar sidebarUserPanel sidebarHeader menuItem sidebarMenu
#'
#' @return A `shiny.tag` object.
#'
#' @export
sidebar <- function() {
  bs4DashSidebar(

    ############################
    # Customization parameters #
    ############################
    disable = FALSE,
    width = 250,
    skin = "light",
    status = "teal",
    elevation = 4,
    collapsed = FALSE,
    minified = FALSE,
    expandOnHover = TRUE,
    fixed = TRUE,
    id = "sidebar",
    customArea = NULL,

    ##############
    # User panel #
    ##############

    sidebarUserPanel(
      name = "Powered By R Shiny"
    ),
    # Sidebar
    sidebarMenu(
      id = "sidebar",
      flat = FALSE,
      compact = FALSE,
      # Indent child menuItem() after each sidebarHeader()
      childIndent = TRUE,
      sidebarHeader("Survey Result Modules:"),
      menuItem(
        "Topline",
        tabName = "topline",
        icon = icon("chart-bar")
      ),
      menuItem(
        "Two-Way Crosstab",
        tabName = "xtab",
        icon = icon("chart-line")
      ),
      menuItem(
        "Primer",
        tabName = "primer_survey",
        icon = icon("info")
      )
    )
  )
}
