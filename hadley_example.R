ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      map(names(iris), ~ make_ui(iris[[.x]], .x))
    ),
    mainPanel(
      tableOutput("data")
    )
  )
)
server <- function(input, output, session) {
  selected <- reactive({
    each_var <- map(names(iris), ~ filter_var(iris[[.x]], input[[.x]]))
    reduce(each_var, ~ .x & .y)
  })
  
  output$data <- renderTable(head(iris[selected(), ], 12))
}

shinyApp(ui, server)