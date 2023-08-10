# Module UI code.

mod_combine_datasheets_ui <- function(id){
  ns <- NS(id)

  preview_panels = shiny::tabsetPanel(
    shiny::tabPanel(
      title = 'Sheet 1',
      DT::DTOutput(ns('excel_1_DT'))
    ),
    shiny::tabPanel(
      title = 'Sheet 2',
      DT::DTOutput(ns('excel_2_DT'))
    ),
    shiny::tabPanel(
      title = 'Combined Sheet',
      DT::DTOutput(ns('combined_excel_files'))
    )
  )

  tagList(
    h4("Excel Table Joiner"),
    p("This tool allows you to upload two excel files, preview them, and
      perform a join. You can choose between a left-join (keep all rows
      of table one and matching rows from table two), right-join (all rows
      from table 2, matching rows from 1), and a full-join (keep all rows
       from both tables). Select your files to upload by clicking 'Browse'
      under Excel Sheet One or Two"),
    selectInput(ns('join_type'), label = 'Type of Join', choices = c("Left","Right","Full")),
    fluidRow(
      column(width = 6,
             list(
               shiny::fileInput(
                 ns('excel_1'),
                 'Excel Sheet One',
                 width = '80%'
               ),
               uiOutput(ns('excel_1_success'))
               # ),
             )
      ),
      column(width = 6,
             list(
               shiny::fileInput(
                 ns('excel_2'),
                 'Excel Sheet Two',
                 width = '80%'
               ),
               uiOutput(ns('excel_2_success'))
             )
      )
    ),
    preview_panels,
    HTML("<br><br><br>"),
    shiny::downloadLink(ns("downloadData"), "Download Combined Excel File")
  )
}

# Module server code.
mod_combine_datasheets_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    dat_one = reactive({
      if(is.null(input$excel_1)) return(NULL)
      utils_digest_excel(filepath = input$excel_1)
    })

    output$excel_1_DT = DT::renderDT({
      req(!is.null(dat_one()))
      dat_one()
    })

    output$excel_1_success = shiny::renderUI({
      if(!is.null(dat_one())){
        p("Data Uploaded:", shiny::icon('check'))
      } else {
        NULL
      }
    })

    output$excel_2_success = shiny::renderUI({
      if(!is.null(dat_two())){
        p("Data Uploaded:", shiny::icon('check'))
      } else {
        NULL
      }
    })

    dat_two = reactive({
      if(is.null(input$excel_2)) return(NULL)
      utils_digest_excel(filepath = input$excel_2)
    })

    output$excel_2_DT = DT::renderDT({
      req(!is.null(dat_two()))
      dat_two()
    })

    # Initialize a reactive container for the combined table.
    dat_combined = reactive({
      req(!is.null(dat_one()) & !is.null(dat_two()))

      if(input$join_type == 'Left'){
        return(
          dat_one() |>
            dplyr::left_join(dat_two())
        )
      }
      if(input$join_type == 'Right'){
        return(
          dat_one() |>
            dplyr::right_join(dat_two())
        )
      }
      if(input$join_type == 'Full'){
        return(
          dat_one() |>
            dplyr::full_join(dat_two())
        )
      }
    })

    output$combined_excel_files = DT::renderDT({
      dat_combined()
    })

    output$downloadData <- shiny::downloadHandler(
      filename = function() {
        paste("combined_excel_files_", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        openxlsx::write.xlsx(dat_combined(), file)
      }
    )
  })
}