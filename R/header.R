#' Header
#'
#' Function for dashboard header.
#'
#' @importFrom bs4Dash dashboardHeader dashboardBrand
#' @importFrom shiny icon
#'
#' @return A `shiny.tag` object.
#'
#' @export
header <- function() {
  dashboardHeader(
    title = dashboardBrand(
      title = "Election Survey",
      color = "teal",
      href = "https://www.kenwuyang.com/",
      image = "assets/sticker.png",
      opacity = 0.8
    ),
    titleWidth = NULL,
    disable = FALSE,
    .list = NULL,
    skin = "light",
    status = "teal",
    border = TRUE,
    compact = FALSE,
    sidebarIcon = icon("bars"),
    controlbarIcon = icon("th"),
    fixed = TRUE,
    leftUi = NULL,
    rightUi = NULL
  )
}
