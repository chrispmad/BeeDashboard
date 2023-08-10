# R util functions

# Make sure that our app's wd is '...www/', even when running
# it locally and it's not yet being hosted online.
ensure_www_wd = function(){
  if(!str_detect(getwd(),'www(/)?$')){
    print(paste0('Working directory is this on launch:',getwd()))
    setwd(paste0(getwd(),'/www/'))
    print(paste0('changed working directory to ',getwd()))
  }
}

# Resize a vector of images on disk.
resize_pictures_for_container = function(pictures,container_width){
  # Only accept values for container_width that have numbers and are in pixels.
  if(!stringr::str_detect(container_width,'[0-9]+') | str_detect(container_width,'%')){
    error('Container width must contain numeric values in pixels, e.g. "200px"')
    }

  # Strip away text from the container_width input.
  if(str_detect(container_width,'[!0-9]+')){
    container_width = stringr::str_extract(container_width,'[0-9]+')
  }
  
  purrr::iwalk(
    pictures, ~ {
  
      image_name = str_remove(.x,'.png$')
      new_image_name = paste0(image_name, '_rs.png')
      
      if(file.exists(new_image_name)) file.remove(new_image_name)
      
      magick::image_read(.x) |> 
        magick::image_scale(container_width) |> 
        magick::image_write(new_image_name)
    }
  )
  
  write.csv(data.frame(size = container_width),
            'container_width_recent.csv')
}

# Make a slickR slideshow
self_sizing_slickR = function(file_pattern, container_width, autoplayspeed){
  
  og_pictures = list.files(
    full.names = T,
    pattern = file_pattern
  )
  
  # If the container size is changed from last time we ran this shiny app,
  # or we don't have '_rs' (resized) versions of the photos,
  # use the resize_pictures_for_container function from above.
  if(!file.exists('container_width_recent.csv')){
    resize_pictures_for_container(
      pictures = og_pictures,
      container_width = container_width
    )
  } else {
    if(read.csv('container_width_recent.csv')$size != container_width | length(list.files(pattern = '_rs.png')) == 0){
      resize_pictures_for_container(
        pictures = og_pictures,
        container_width = container_width
      )
    }
  }
  
  pictures_rs = list.files(
    full.names = T,
    pattern = '_rs.png'
  )
  
  slickR::slickR(pictures_rs) + 
    slickR::settings(
      arrows = FALSE,
      autoplay = TRUE,
      autoplaySpeed = autoplayspeed,
      adaptiveHeight = T,
      pauseOnHover = T
    )
}

# Two programmatic UI and filtering functions, from Hadley Wickham.
# URL: https://mastering-shiny.org/action-dynamic.html#dynamic-filter
make_ui <- function(x, var) {
  if (is.numeric(x)) {
    rng <- range(x, na.rm = TRUE)
    sliderInput(var, var, min = rng[1], max = rng[2], value = rng)
  } else if (is.factor(x)) {
    levs <- levels(x)
    selectInput(var, var, choices = levs, selected = levs, multiple = TRUE)
  } else {
    # Not supported
    NULL
  }
}

filter_var <- function(x, val) {
  if (is.numeric(x)) {
    !is.na(x) & x >= val[1] & x <= val[2]
  } else if (is.character(x)) {
    x %in% val
  } else {
    # No control, so don't filter
    TRUE
  }
}

filter_var_include_na <- function(x, val) {
  if (is.numeric(x)) {
    is.na(x) | (x >= val[1] & x <= val[2])
  } else if (is.character(x)) {
    is.na(x) | x %in% val
  } else {
    # No control, so don't filter
    TRUE
  }
}