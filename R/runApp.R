#' Run application
#'
#' Function to run the shiny application.
#'
#' @param choice_list A `ElectProtect_choices` object instantiated with `ElectProtect::choices`.
#' @param question_list A `ElectProtect_questions` object instantiated with `ElectProtect::questions`.
#' @param db A sqlite (`S4`) database connection object.
#'
#' @import htmltools
#' @importFrom DT datatable renderDT formatStyle
#' @importFrom shiny reactive shinyApp bindCache bindEvent onStop freezeReactiveValue renderUI observe updateSelectInput
#' @importFrom shinyjs runjs
#' @importFrom waiter spin_5
#' @importFrom plotly renderPlotly plot_ly
#' @importFrom bs4Dash dashboardPage
#' @importFrom markdown renderMarkdown
#' @importFrom vctrs field
#'
#' @export
runApp <- function(choice_list, question_list, db) {

  # UI ----------------------------------------------------------------------

  ui <- dashboardPage(
    dark = NULL,
    preloader = list(html = tagList(spin_5(), "Engineering solutions for you..."), color = "#46bdc6"),
    header = header(),
    sidebar = sidebar(),
    body = body_()
  )

  # Server ------------------------------------------------------------------

  server <- function(input, output, session) {

    ###########
    # Topline #
    ###########

    # 1) Topline input sidebar  -----------------------------------------------

    observe({
      updateBoxSidebar("topline_input")
    }) |>
      bindEvent(input$show_topline_input)

    # 2) Reactive expressions and update inputs -------------------------------

    topline_var <- reactive({
      input$topline_section
    })
    observe({
      freezeReactiveValue(input, "topline_var")
      updateSelectInput(inputId = "topline_var", choices = field(choice_list, topline_var()))
    }) |>
      bindEvent(topline_var())

    # 3) Topline --------------------------------------------------------------

    # 3.1) Table reactive expression
    topline_reactive <- reactive({
      topline(var = input$topline_var, weight = "weight", db)
    }) |>
      # The cache key used to determine if a computation has occurred before is 'topline_var'
      bindCache(input$topline_var, cache = "app")

    # 3.2) Table output
    output$topline_table <- renderDT({
      topline_reactive()
    })

    # 3.3) Text output
    output$topline_question_interpretation <- renderUI({
      HTML(
        paste0(
          "<dl><dt>", str_format(str = input$topline_var), "</dt>
          <dd>- ", field(question_list, input$topline_var), " </dd>
          <dt> Template Interpretation </dt>
          <dd>- ", interpret_topline(input$topline_var, topline_var()), " </dd>
        </dl>"
        )
      )
    })

    # 3.4) Topline plot
    output$topline_plot <- renderPlotly({
      plot_topline(data = topline_reactive(), var = input$topline_var)
    })

    ####################
    # Cross-tabulation #
    ####################

    # 1) Two-way input sidebar  ------------------------------------------------

    observe({
      updateBoxSidebar("two_way_input")
    }) |>
      bindEvent(input$show_two_way_input)

    # 2) Reactive expressions and update inputs --------------------------------

    xtab_1 <- reactive({
      input$xtab_section_1
    })
    xtab_2 <- reactive({
      input$xtab_section_2
    })
    observe({
      freezeReactiveValue(input, "xtab_var1")
      updateSelectInput(inputId = "xtab_var1", choices = field(choice_list, xtab_1()))
    }) |>
      bindEvent(xtab_1())
    observe({
      freezeReactiveValue(input, "xtab_var2")
      updateSelectInput(inputId = "xtab_var2", choices = field(choice_list, xtab_2()))
    }) |>
      bindEvent(xtab_2())

    # 3) Cross-tabulation -----------------------------------------------------

    # 3.1) Table reactive expression
    xtab_reactive <- reactive({
      crosstab(var_ind = input$xtab_var1, var_dep = input$xtab_var2, reverse = input$reverse, weight = "weight", db)
    }) |>
      # The cache keys used to determine if a computation has occurred before are 'xtab_var1', 'xtab_var2', and 'reverse'
      bindCache(input$xtab_var1, input$xtab_var2, input$reverse, cache = "app")

    # 3.2) Table output
    output$xtab_table <- renderDT({
      xtab_reactive()
    })

    # 3.3) Text output
    output$xtab_questions <- renderUI({
      HTML(
        paste0(
          "<dl><dt>", str_format(str = input$xtab_var1), "</dt>
          <dd>- ", field(question_list, input$xtab_var1), " </dd>
          <dt>", str_format(str = input$xtab_var2), "</dt>
          <dd>- ", field(question_list, input$xtab_var2), " </dd>
        </dl>"
        )
      )
    })
    output$xtab_interpretation <- renderUI({
      interpret_xtab(var_ind = input$xtab_var1, var_dep = input$xtab_var2, reverse = input$reverse)
    })

    # 3.4) Crosstab plot
    output$xtab_plot <- renderPlotly({
      plot_xtab(data = xtab_reactive())
    })

    ##########
    # Primer #
    ##########

    output$primer_text <- renderUI({
      includeMarkdown(app_sys("assets", "primer.md"))
    })
  }

  shinyApp(ui = ui, server = server)
}
