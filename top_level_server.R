library(shiny)
library(stringr)
library(purrr)
library(leaflet)
library(dplyr)
library(tidyr)
library(magick)
library(sf)

source('R/mod_combine_datasheets.R')
source('R/mod_combine_datasheets_utils_digest_excel.R')
source('R/utils.R')

# UI Variables
sidebar_width = '250px'

# # Javascript
# jscode <-
#   '
#   $(document).on("shiny:connected", function(e) {
#   var slideshow_card = $(".slick-list .draggable")
#   $("#bee_slideshow_card").css("height", (slideshow_card.height + "px");
# });
# '

ui <- bslib::page_navbar(
  # tags$script(jscode),
  shinyjs::useShinyjs(),
  title = 'Bee Dashboard',
  sidebar = bslib::sidebar(
    title = 'Dashboard Controls',
    width = sidebar_width,
    # uiOutput('bee_slideshow_ui'),
    # bslib::card(
    #   bslib::card_body(
    #     slickR::slickROutput('bee_slideshow', width = '100%', height = '200px'),
    #     padding = 0),
    #   id = 'bee_slideshow_card',
    #   min_height = '300px'
    #   ),
    h5("Map Settings"),
    selectInput("pal_var","Variable to Visualize",
                choices = c("Collection Date" = 'my_year',
                            "Trap Type" = 'trap_type',
                            "Order" = 'order',
                            "Genus" = 'genus',
                            "Species" = 'scientific_name'),
                selected = 'Collection Date'),
    h5('Filters'),
    HTML("<br>"),
    textOutput('record_number_test'),
    checkboxInput('all_records','Show All Records',value = TRUE),
    uiOutput('class_sel_ui'),
    uiOutput('order_sel_ui'),
    uiOutput('genus_sel_ui'),
    uiOutput('species_sel_ui')
  ),
  bslib::navset_tab(
    bslib::nav_panel(
      title = 'Bee Map',
      leafletOutput('my_map',
                    height = '600px')
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
           !is.na(lat)) |> 
    mutate(my_year = lubridate::year(start_date))
  
  dat$my_year = factor(dat$my_year, levels = c(
    min(dat$my_year,na.rm=T):max(dat$my_year,na.rm=T)
  ))
  
  # Generate UI for filters
  
  output$order_sel_ui = renderUI({
    if(input$all_records) return(NULL)
    selectInput('order_sel',
                h6('Order'),
                choices = na.omit(unique(dat$order)),
                selected = NULL,
                multiple = T
    )
  })
  
  order_s = reactive({input$order_sel})
  
  genus_options = reactive({
    # No genus options for the selected order? Return just one NA
    if(!is.null(order_s())){
      genus_options = na.omit(unique(dat[dat$order %in% order_s(),]$genus))
    } else {
      genus_options = na.omit(unique(dat$genus))
    }
    return(genus_options)
  })
  
  output$genus_sel_ui = renderUI({
    
    if(input$all_records) return(NULL)

    # No genus options for a selected order? Make text grey.
    the_style = ifelse(length(genus_options())==0,'color:#C4CAE0;text-decoration:line-through;','color:black')
    
    selectInput('genus_sel',
                h6(
                  'Genus', 
                  style = the_style),
                choices = genus_options(),
                selected = NULL,
                multiple = T
    )
  })
  
  genus_s = reactive({input$genus_sel})
  
  species_options = reactive({
    if(!is.null(order_s())){
      if(!is.null(genus_s())){
        species_options = na.omit(dat[dat$order %in% order_s() & dat$genus %in% genus_s(),]$scientific_name)
      } else {
        species_options = na.omit(dat[dat$order %in% order_s(),]$scientific_name)
      }
    } else {
      species_options = na.omit(unique(dat$scientific_name))
    }
  })
  
  output$species_sel_ui = renderUI({
    
    if(input$all_records) return(NULL)
    
    the_style = ifelse(length(species_options())==0,'color:#C4CAE0;text-decoration:line-through;','color:black')
    
    selectInput('species_sel',
                h6(
                  'Species',
                  style = the_style
                ),
                choices = species_options(),
                selected = NULL,
                multiple = T
    )
  })
    
  # Filter dataset based on:
  # 1. checkbox for 'all records'
  # 2. Taxonomic ID
  # 3. checkbox for 'retain NA'
  dat_f = reactive({
    if(input$all_records) return(dat)
  
    # Has the user chosen nothing yet for any dropdown? Return null.
    if(is.null(input$order_sel) & is.null(input$genus_sel) & is.null(input$species_sel)){
      return(dat[0,])
    }

    # Different taxonomic selection situations.
    if(!is.null(input$order_sel) & is.null(input$genus_sel) & is.null(input$species_sel)){
      return(dat |> filter(order %in% input$order_sel))
    }
    if(!is.null(input$order_sel) & !is.null(input$genus_sel) & is.null(input$species_sel)){
      return(dat |> filter(order %in% input$order_sel,
                    genus %in% input$genus_sel))
    }
    if(!is.null(input$order_sel) & is.null(input$genus_sel) & !is.null(input$species_sel)){
      return(dat |> filter(order %in% input$order_sel,
                    scientific_name %in% input$species_sel))
    }
    if(!is.null(input$order_sel) & !is.null(input$genus_sel) & !is.null(input$species_sel)){
      return(dat |> filter(order %in% input$order_sel,
                    genus %in% input$genus_sel,
                    scientific_name %in% input$species_sel))
    }
  })
  
  dat_sf = reactive({
    st_as_sf(dat_f(), coords = c("lon","lat"), crs = 4326)
  })
  
  # What is the most specific ID we have for each row?
  best_id = reactive({
    req(nrow(dat_f()) > 0)
    dat_f() |> 
      mutate(best_id = data.table::fcase(
        !(is.na(scientific_name) & is.na(genus)), paste0(genus,' ',scientific_name),
        (is.na(scientific_name) & is.na(genus)) & !is.na(family), family,
        (is.na(scientific_name) & is.na(genus) & is.na(family)), order
      )) |> 
      dplyr::pull(best_id)
  })
  
  # Test for number of records with filter.
  output$record_number_test = renderText({paste0(nrow(dat_f()),' records selected.')})
  
  # Shiny module for excel combiner tab.
  mod_combine_datasheets_server('excel_combiner')
  
  # # Create bee slideshow
  # output$bee_slideshow = slickR::renderSlickR({
  #   self_sizing_slickR(
  #     file_pattern = '_lr.png',
  #     container_width = sidebar_width,
  #     autoplayspeed = 10000
  #   )
  # })
  
  my_leaf_pal = reactive({
      leaflet::colorFactor(
        palette = 'viridis',
        domain = dat_sf()[[input$pal_var]]
      )
  })
  
  my_labels = reactive({
    lapply(
      paste0(
        'Most specific ID known: ', best_id(),
        '<br>Count: ', dat_f()$n,
        '<br>Date: ', dat_f()$start_date,
        '<br>Trap Type: ', dat_f()$trap_type,
        '<br><strong>Click for more info</strong>'
      ),
      HTML)
  })
  
  # More detailed than the labels above.
  my_popups = reactive({
    lapply(
      paste0(
        'Order: ', dat_f()$order,
        '<br>Family: ',dat_f()$family,
        '<br>Genus: ',dat_f()$genus,
        '<br>Scientific name: ',dat_f()$scientific_name,
        '<br>Count: ', dat_f()$n,
        '<br>Date: ', dat_f()$start_date,
        '<br>Trap Type: ', dat_f()$trap_type
      ),
      HTML)
  })
  
  output$my_map = renderLeaflet({
    leaflet() |> 
      addTiles() |> 
      envreportutils::add_bc_home_button() |> 
      envreportutils::set_bc_view()
  })
  
  observe({
      leafletProxy('my_map') |>
        removeControl('legend') |>
        clearMarkers() |>
        addCircleMarkers(
          col = 'black',
          weight = 1,
          fillColor = my_leaf_pal()(dat_sf()[[input$pal_var]]),
          fillOpacity = 0.75,
          label = my_labels(),
          popup = my_popups(),
          data = dat_sf()
        ) |>
        addLegend(pal = my_leaf_pal(),
                  values = dat_sf()[[input$pal_var]],
                  layerId = 'legend')
  })
  
  # output$bee_table = DT::renderDT({
  #   dat_f()
  # })
}

shinyApp(ui, server)