#' Body
#'
#' Function for the dashboard body.
#'
#' @importFrom bs4Dash dashboardBody tabItems tabItem box boxLabel boxSidebar updateBoxSidebar tabBox
#' @importFrom shiny fluidRow selectInput tabPanel tabsetPanel actionButton uiOutput
#' @importFrom plotly plotlyOutput
#' @importFrom DT DTOutput
#'
#' @return A `shiny.tag` object.
#'
#' @export
body_ <- function() {
  dashboardBody(

    ################
    # Resize plots #
    ################

    tags$head(
      tags$script(
        "$(function() {
            function resizeBoxContent(trigger, target) {
              $(trigger).on('click', function() {
                setTimeout(function() {
                  var isMaximized = $('html').hasClass('maximized-card');
                    if (isMaximized) {
                      $(target).css('height', '100%');
                    } else {
                      $(target).css('height', '400px');
                    }
                }, 300);
                $(target).trigger('resize');
              });
            }

            setTimeout(function() {
              resizeBoxContent('#topline_plot_box [data-card-widget=\"maximize\"]', '#topline_plot');
              resizeBoxContent('#xtab_plot_box [data-card-widget=\"maximize\"]', '#xtab_plot');
              resizeBoxContent('#topline_box [data-card-widget=\"maximize\"]', '#topline_table');
              resizeBoxContent('#xtab_box [data-card-widget=\"maximize\"]', '#xtab_table');
            }, 500);

          });
          "
      )
    ),

    ######################
    # Apply custom theme #
    ######################

    my_theme(),
    tabItems(

      ###############
      # Topline tab #
      ###############

      tabItem(
        tabName = "topline",

        ########################
        # Table and Text boxes #
        ########################

        fluidRow(

          # Table box ---------------------------------------------------------------

          box(
            title = "Topline",
            # Border color
            status = "teal",
            # Solid box header
            solidHeader = FALSE,
            # Background color of box
            background = NULL,
            # Overall width of a region is 12
            width = 6,
            # Scales automatically with the content
            height = 400,
            # Collapse aka minimize box
            collapsible = TRUE,
            # Do not start as collapsed box
            collapsed = FALSE,
            closable = FALSE,
            # Allow user to maximize box as one single view
            maximizable = TRUE,
            icon = NULL,
            gradient = FALSE,
            boxToolSize = "sm",
            elevation = 2,
            headerBorder = TRUE,
            dropdownMenu = NULL,
            id = "topline_box",
            # Action button for input sidebar
            actionButton(inputId = "show_topline_input", label = "Toggle Input Sidebar"),
            sidebar = boxSidebar(
              id = "topline_input",
              easyClose = FALSE,
              selectInput(
                inputId = "topline_section",
                label = HTML("<b>I want to see...</b>"),
                choices = list(
                  "Experiences & Impressions",
                  "Political Messaging",
                  "Demographics",
                  "Multiple-Selection Questions"
                ),
                selected = NULL
              ),
              selectInput(
                inputId = "topline_var",
                label = HTML("<b>Specifically...</b>"),
                choices = NULL,
                selected = NULL
              )
            ),
            DTOutput(outputId = "topline_table")
          ),

          # Text box ----------------------------------------------------------------

          box(
            title = "Help",
            # Border color
            status = "teal",
            # Solid box header
            solidHeader = FALSE,
            # Background color of box
            background = NULL,
            # Overall width of a region is 12
            width = 6,
            # Scales automatically with the content
            height = 400,
            # Collapse aka minimize box
            collapsible = TRUE,
            # Do not start as collapsed box
            collapsed = FALSE,
            closable = FALSE,
            # Allow user to maximize box as one single view
            maximizable = FALSE,
            icon = NULL,
            gradient = FALSE,
            boxToolSize = "sm",
            elevation = 2,
            headerBorder = TRUE,
            dropdownMenu = NULL,
            id = "topline_text_box",
            uiOutput(
              outputId = "topline_question_interpretation"
            )
          )
        ),

        ####################
        # Topline plot box #
        ####################

        fluidRow(
          box(
            title = "Bar Chart",
            # Border color
            status = "teal",
            # Solid box header
            solidHeader = FALSE,
            # Background color of box
            background = NULL,
            # Overall width of a region is 12
            width = 12,
            # Scales automatically with the content
            height = NULL,
            # Collapse aka minimize box
            collapsible = TRUE,
            # Do not start as collapsed box
            collapsed = FALSE,
            closable = FALSE,
            # Allow user to maximize box as one single view
            maximizable = TRUE,
            icon = NULL,
            gradient = FALSE,
            boxToolSize = "sm",
            elevation = 2,
            headerBorder = TRUE,
            dropdownMenu = NULL,
            id = "topline_plot_box",
            plotlyOutput(outputId = "topline_plot")
          )
        )
      ),

      ###############
      # Two-way tab #
      ###############

      tabItem(
        tabName = "xtab",

        ########################
        # Table and Text boxes #
        ########################

        fluidRow(

          # Table box ---------------------------------------------------------------

          box(
            title = "Cross Tabulation",
            # Border color
            status = "teal",
            # Solid box header
            solidHeader = FALSE,
            # Background color of box
            background = NULL,
            # Overall width of a region is 12
            width = 6,
            # Scales automatically with the content
            height = 400,
            # Collapse aka minimize box
            collapsible = TRUE,
            # Do not start as collapsed box
            collapsed = FALSE,
            closable = FALSE,
            # Allow user to maximize box as one single view
            maximizable = TRUE,
            icon = NULL,
            gradient = FALSE,
            boxToolSize = "sm",
            elevation = 2,
            headerBorder = TRUE,
            dropdownMenu = NULL,
            id = "xtab_box",
            # Action button for input sidebar
            actionButton(inputId = "show_two_way_input", label = "Toggle Input Sidebar"),
            sidebar = boxSidebar(
              id = "two_way_input",
              easyClose = FALSE,
              selectInput(
                inputId = "xtab_section_1",
                label = HTML("<b>I want to see...</b>"),
                choices = list(
                  "Experiences & Impressions",
                  "Political Messaging",
                  "Election Threat (Most Concerning)",
                  "Election Threat (Least Concerning)",
                  "Election Threat (Least Informed)",
                  "Demographics",
                  "News Source"
                ),
                selected = "Demographics"
              ),
              selectInput(
                inputId = "xtab_var1",
                label = HTML("<b>Specifically...</b>"),
                choices = NULL,
                selected = NULL
              ),
              selectInput(
                inputId = "xtab_section_2",
                label = HTML("<b>Broken down by...</b>"),
                choices = list(
                  "Experiences & Impressions",
                  "Political Messaging",
                  "Election Threat (Most Concerning)",
                  "Election Threat (Least Concerning)",
                  "Election Threat (Least Informed)",
                  "Demographics",
                  "News Source"
                ),
                selected = "Experiences & Impressions"
              ),
              selectInput(
                inputId = "xtab_var2",
                label = HTML("<b>Specifically...</b>"),
                choices = NULL,
                selected = NULL
              ),
              selectInput(
                inputId = "reverse",
                label = HTML("<b>Reverse selected fields?</b>"),
                choices = list(
                  "Yes",
                  "No"
                ),
                selected = "No"
              )
            ),
            DTOutput(outputId = "xtab_table")
          ),

          # Text box ----------------------------------------------------------------

          tabBox(
            id = "xtab_text_box",
            title = NULL,
            side = "left",
            type = "tabs",
            elevation = 2,
            width = 6,
            height = 400,
            status = "teal",
            maximizable = FALSE,
            collapsible = TRUE,
            closable = TRUE,
            selected = NULL,
            tabPanel(
              "Questions",
              uiOutput(
                outputId = "xtab_questions"
              )
            ),
            tabPanel(
              "Template Interpretation",
              uiOutput(
                outputId = "xtab_interpretation"
              )
            )
          )
        ),

        #############################
        # Cross tabulation plot box #
        #############################

        fluidRow(
          box(
            title = "Bar Chart",
            # Border color
            status = "teal",
            # Solid box header
            solidHeader = FALSE,
            # Background color of box
            background = NULL,
            # Overall width of a region is 12
            width = 12,
            # Scales automatically with the content
            height = NULL,
            # Collapse aka minimize box
            collapsible = TRUE,
            # Do not start as collapsed box
            collapsed = FALSE,
            closable = FALSE,
            # Allow user to maximize box as one single view
            maximizable = TRUE,
            icon = NULL,
            gradient = FALSE,
            boxToolSize = "sm",
            elevation = 2,
            headerBorder = TRUE,
            dropdownMenu = NULL,
            id = "xtab_plot_box",
            plotlyOutput(outputId = "xtab_plot")
          )
        )
      ),

      ##########
      # Primer #
      ##########

      tabItem(
        tabName = "primer_survey",
        uiOutput(outputId = "primer_text")
      )
    )
  )
}
