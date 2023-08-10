remotes::install_local('C:/Users/CMADSEN/Downloads/LocalR/BeeDashboard_0.0.0.9000.tar.gz')

library(BeeDashboard)

BeeDashboard::run_app()

# library(shiny)
# 
# setwd("C:/Users/CMADSEN/Downloads/LocalR/BeeDashboard/")
# 
# source('R/mod_combine_datasheets_utils_digest_excel.R')
# source('R/mod_combine_datasheets.R')
# 
# shinyApp(
#   ui = bslib::page_navbar(
#     title = 'Bee Dashboard',
#     sidebar = bslib::sidebar(
#       title = 'Sidebar',
#       h5("Test"),
#       bg = 'darkgrey',
#       div(
#         slickR::slickROutput('bee_slideshow')
#       )
#     ),
#     bslib::navset_tab(
#       bslib::nav_panel(
#         title = 'Bee Map',
#         div(
#           slickR::slickROutput('bee_slideshow')
#         )
#       ),
#       bslib::nav_panel(
#         title =  'Excel Sheet Combiner',
#         mod_combine_datasheets_ui('excel_combiner')
#       )
#     )
# ),
#  server = function(input, output, session) {
# 
#    # Shiny module for excel combiner tab.
#    mod_combine_datasheets_server('excel_combiner')
# 
#    # Create bee slideshow
#    output$bee_slideshow = slickR::renderSlickR({
#      bee_pictures = list.files(path = 'inst/app/www/',
#                                pattern = '.*lr.*',
#                                full.names = T)
#      slickR::slickR(bee_pictures)
#    })
#  })
