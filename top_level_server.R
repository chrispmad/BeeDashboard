library(shiny)
library(stringr)
library(purrr)
library(leaflet)
library(dplyr)
library(tidyr)
library(magick)

source('R/mod_combine_datasheets.R')
source('R/mod_combine_datasheets_utils_digest_excel.R')
source('R/utils.R')

# UI Variables
sidebar_width = '250px'

# Javascript
jscode <-
  '
  $(document).on("shiny:connected", function(e) {
  var slideshow_card = $(".slick-list .draggable")
  $("#bee_slideshow_card").css("height", (slideshow_card.height + "px");
});
'

ui <- bslib::page_navbar(
  tags$script(jscode),
  title = 'Bee Dashboard',
  sidebar = bslib::sidebar(
    title = 'Sidebar',
    width = sidebar_width,
    uiOutput('bee_slideshow_ui'),
    bslib::card(
      bslib::card_body(
        slickR::slickROutput('bee_slideshow', width = '100%', height = '200px'),
        padding = 0),
      id = 'bee_slideshow_card',
      min_height = '300px'
      ),
    h5('Filters'),
    HTML("<br><br>"),
    uiOutput('class_sel_ui'),
    uiOutput('order_sel_ui'),
    uiOutput('genus_sel_ui'),
    uiOutput('species_sel_ui'),
    checkboxInput('retain_na','Retain NA values?',value = TRUE),
    uiOutput('list_of_filters')
  ),
  bslib::navset_tab(
    bslib::nav_panel(
      title = 'Bee Map',
      leafletOutput('my_map',
                    height = '600px'),
      DT::DTOutput('bee_table')
    ),
    bslib::nav_panel(
      title =  'Excel Sheet Combiner',
      mod_combine_datasheets_ui('excel_combiner')
    )
  )
)

server <- function(input, output, session) {
  
  # Probably unnecessary: adjust working directory to www/
  ensure_www_wd()
  
  # # Load in our toy bee dataset, left join as 'dat'
  # load('toy_id_dat.rda')
  # load('toy_sample_dat.rda')
  
  dat = qs::qread('bee_db.qs') |> 
    set_names(snakecase::to_snake_case) |> 
    filter(!is.na(lon),
           !is.na(lat))
  
  # What are variable names of dat?
  # Take variable names of dat that aren't sample_date, _id, lat or lon
  filter_var_names = names(dat |> dplyr::select(c(order:genus)))
  
  # Generate UI for filters
  output$class_sel_ui = renderUI({
    selectInput('class_sel','Class',
                choices = unique(dat$class),
                selected = unique(dat$class),
                multiple = T
                )
  })
  output$order_sel_ui = renderUI({
    selectInput('order_sel','Order',
                choices = unique(dat$order),
                selected = unique(dat$order),
                multiple = T
    )
  })
  output$genus_sel_ui = renderUI({
    selectInput('genus_sel','Genus',
                choices = unique(dat$genus),
                selected = unique(dat$genus),
                multiple = T
    )
  })
  output$species_sel_ui = renderUI({
    selectInput('species_sel','Species',
                choices = unique(dat$scientific_name),
                selected = unique(dat$scientific_name),
                multiple = T
    )
  })
    
  # Shiny module for excel combiner tab.
  mod_combine_datasheets_server('excel_combiner')
  
  # Create bee slideshow
  output$bee_slideshow = slickR::renderSlickR({
    self_sizing_slickR(
      file_pattern = '_lr.png',
      container_width = sidebar_width,
      autoplayspeed = 10000
    )
  })
  
  my_leaf_pal = reactive({
    leaflet::colorFactor(
      palette = 'Spectral',
      domain = unique(dat$family)
    )
  })
  
  output$my_map = renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) |> 
      addTiles() |> 
      envreportutils::add_bc_home_button() |> 
      envreportutils::set_bc_view() |> 
      addCircleMarkers(
        col = 'black',
        weight = 1,
        # layerId = 'sample_circles',
        data = dat
      )
  })
  
  # observe({
  #   # if(nrow(dat_f()) > 0){
  #     leafletProxy('my_map') |>
  #       removeControl('legend') |>
  #       # removeShape('sample_circles') |>
  #       addCircleMarkers(
  #         col = 'black',
  #         weight = 1,
  #         # fillOpacity = 0.75,
  #         # fillColor = ~my_leaf_pal()(family),
  #         # label = ~lapply(
  #         #   paste0(
  #         #     'Sample: ',rskm,
  #         #     '<br>Date: ', start_date,
  #         #     '<br>Species: ',species),
  #         #   htmltools::HTML),
  #         layerId = 'sample_circles',
  #         data = dat
  #       ) |>
  #       addLegend(pal = my_leaf_pal(),
  #                 values = dat$family,
  #                 layerId = 'legend')
  #   # }
  # })
  
  # output$bee_table = DT::renderDT({
  #   dat_f()
  # })
}

shinyApp(ui, server)