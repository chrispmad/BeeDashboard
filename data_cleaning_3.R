# Manual data cleaning... third attempt.

# This script cleans a collection of heterogeneously named and organized
# excel sheets of bee collection fieldwork and (museum) specimen identification.

# packages
library(tidyverse)
library(openxlsx)

# Source cleaning functions
source('cleaning_functions.R')

# # Find files in data folder.
# all_files = list.files(path = 'data/', pattern = '[^R]$',full.names = T)
# 
# # Read in files (function detects if they are .csv or .xlsx)
# all_content = lapply(all_files, read_the_file)
# 
# lapply(all_files, )
# 
# excel1 = openxlsx::readWorkbook(all_files[[1]])
# names(excel1)

dat1_sheet1 = read.xlsx('data/MASTER RSKM SPECIMEN DATABASE Jan 21 2022.xlsx', sheet = 'RSKM DATA (2021)')
dat1_sheet2 = read.xlsx('data/MASTER RSKM SPECIMEN DATABASE Jan 21 2022.xlsx', sheet = 'RSKM DATA (pre-2021)')
dat1_sheet3 = read.xlsx('data/MASTER RSKM SPECIMEN DATABASE Jan 21 2022.xlsx', sheet = '2020 Kootenays')
dat1_sheet4 = read.xlsx('data/MASTER RSKM SPECIMEN DATABASE Jan 21 2022.xlsx', sheet = '2020 Manning PP')

# Clean up the first major sheet of the above excel file.
dat1_sheet1 = dat1_sheet1 |> 
  as_tibble() |> 
  mutate(
    Start.Day = coalesce(as.double(End.Day), as.double(Start.Day)),
    End.Month = coalesce(as.character(End.Month), as.character(Start.Month))
  ) |> 
  reframe(
    data_type = 'id',
    id = paste0(Museum.Catalogue, Number),
    id_type = 'rskm',
    trap_type = Trap.Type,
    sex,
    start_date_raw = paste0(Start.Year,'-',Start.Month,'-',Start.Day),
    end_date_raw = paste0(End.Year,'-',End.Month,'-',End.Day),
    start_date = lubridate::ymd(paste0(Start.Year,'-',Start.Month,'-',Start.Day)),
    end_date = lubridate::ymd(paste0(End.Year,'-',End.Month,'-',End.Day)),
    lat = Decimal.Latitude,
    lon = Decimal.Longitude,
    phylum = Phylum,
    class = Class,
    order = Order,
    family = as.character(Family),
    genus = as.character(Genus),
    scientific_name = as.character(Scientific.Name),    
    loc_notes = paste0(Locality.Name,', ',`Notes.(Specific.Location.Name)`)
    )

# Now clean up the second sheet
dat1_sheet2 = dat1_sheet2 |> 
  as_tibble() |> 
  mutate(
    Start.Day = coalesce(as.double(End.Day), as.double(Start.Day)),
    End.Month = coalesce(as.character(End.Month), as.character(Start.Month))
  ) |> 
  reframe(
    data_type = 'id',
    id = paste0(Museum.Catalogue, Number),
    id_type = 'rskm',
    trap_type = Trap.Type,
    sex,
    start_date_raw = paste0(Start.Year,'-',Start.Month,'-',Start.Day),
    end_date_raw = paste0(End.Year,'-',End.Month,'-',End.Day),
    start_date = lubridate::ymd(paste0(Start.Year,'-',Start.Month,'-',Start.Day)),
    end_date = lubridate::ymd(paste0(End.Year,'-',End.Month,'-',End.Day)),
    lat = as.numeric(Decimal.Latitude),
    lon = as.numeric(Decimal.Longitude),
    phylum = Phylum,
    class = Class,
    order = Order,
    family = as.character(Family),
    genus = as.character(Genus),
    scientific_name = as.character(Scientific.Name),
    loc_notes = paste0(Locality.Name,', ',`Notes.(Specific.Location.Name)`)
  )

# Clean up the third sheet.
dat1_sheet3 = dat1_sheet3 |> 
  as_tibble() |> 
  mutate(
    Start.Day = coalesce(as.double(End.Day), as.double(Start.Day)),
    End.Month = coalesce(as.character(End.Month), as.character(Start.Month))
  ) |> 
  reframe(
    data_type = 'id',
    id = paste0(Museum.Catalogue, Number),
    id_type = 'rskm',
    trap_type = Trap.Type,
    sex = as.character(sex),
    start_date_raw = paste0(Start.Year,'-',Start.Month,'-',Start.Day),
    end_date_raw = paste0(End.Year,'-',End.Month,'-',End.Day),
    start_date = lubridate::ymd(paste0(Start.Year,'-',Start.Month,'-',Start.Day)),
    end_date = lubridate::ymd(paste0(End.Year,'-',End.Month,'-',End.Day)),
    lat = as.numeric(Decimal.Latitude),
    lon = as.numeric(Decimal.Longitude),
    phylum = Phylum,
    class = Class,
    order = Order,
    family = as.character(Family),
    genus = as.character(Genus),
    scientific_name = as.character(Scientific.Name),
    loc_notes = paste0(Locality.Name,', ',`Notes.(Specific.Location.Name)`)
  )

# Clean up 4th sheet
dat1_sheet4 = dat1_sheet4 |> 
  as_tibble() |> 
  mutate(
    Start.Day = coalesce(as.double(End.Day), as.double(Start.Day)),
    End.Month = coalesce(as.character(End.Month), as.character(Start.Month))
  ) |> 
  reframe(
    data_type = 'id',
    id = paste0(Museum.Catalogue, Number),
    id_type = 'rskm',
    trap_type = Trap.Type,
    sex = as.character(sex),
    start_date_raw = paste0(Start.Year,'-',Start.Month,'-',Start.Day),
    end_date_raw = paste0(End.Year,'-',End.Month,'-',End.Day),
    start_date = lubridate::ymd(paste0(Start.Year,'-',Start.Month,'-',Start.Day)),
    end_date = lubridate::ymd(paste0(End.Year,'-',End.Month,'-',End.Day)),
    lat = as.numeric(Decimal.Latitude),
    lon = as.numeric(Decimal.Longitude),
    phylum = Phylum,
    class = Class,
    order = Order,
    family = as.character(Family),
    genus = as.character(Genus),
    scientific_name = as.character(Scientific.Name),
    loc_notes = paste0(Locality.Name,', ',`Notes.(Specific.Location.Name)`)
  )


# Bind these guys together.

rskm_dat = dat1_sheet1 |> 
  bind_rows(dat1_sheet2, dat1_sheet3, dat1_sheet4)

rskm_dat = rskm_dat |> 
  filter(!is.na(lat),
         !is.na(lon)) 

rskm_dat = rskm_dat |> 
  distinct()

# rskm_sf = sf::st_as_sf(
#   rskm_dat,
#   coords = c("lon","lat"),
#   crs = 4326
# )


qs::qsave(rskm_dat, file = 'www/bee_db.qs')

# rskm_dat = qs::qread('www/bee_db.qs')

# rskm_dat |> 
#   group_by(lat, lon) |> 
#   summarise()
# dat1 = all_content[[1]] |> 
#   reframe(
#     data_type = 'field',
#     id = jar, 
#     id_type = 'jar',
#     trap_type = 'jar?',
#     start_date = openxlsx::convertToDate(date_set),
#     end_date = openxlsx::convertToDate(date_stop),
#     days_out,
#     lat = as.numeric(latitude), 
#     lon = as.numeric(longitude),
#     loc_notes = location,
#     comments)
# 
# dat2 = all_content[[2]] |> 
#   reframe(
#     data_type = 'id',
#     id = paste0(catalogue_number, number),
#     id_type = 'rskm',
#     trap_type = trap_type,
#     sex,
#     start_date = lubridate::ymd(paste0(start_year,'-',start_month,'-',start_day)),
#     end_date = lubridate::ymd(paste0(end_year,'-',end_month,'-',end_day)),
#     lat = decimal_latitude,
#     lon = decimal_longitude,
#     loc_notes = locality,
#     comments
#   ) 
# 
# # # Skip:
# # all_content[[3]]
# # all_content[[4]]
# dat3 = all_content[[5]] |> 
#   mutate(
#     end_day = coalesce(as.double(end_day_leave_blank_if_this_was_a_one_day_sample), as.double(start_day)),
#     end_month = coalesce(as.character(end_month_leave_blank_if_this_was_a_one_day_sample), as.character(start_month))) |> 
# reframe(
#     data_type = 'field',
#     id = sample,
#     id_type = sample_description,
#     trap_type = collection_method_bumble_bee_plot_bumble_bee_route_wandering_pan_traps,
#     start_date = lubridate::ymd(paste0(year,'-',start_month,'-',start_day)),
#     end_date = lubridate::ymd(paste0(year,'-',end_month,'-',end_day)),
#     lat = latitude_n,
#     lon = longitude_w,
#     loc_notes = locality_description_name_of_regional_park_road_rest_stop_etc_separate_multiple_entries_with_semi_colon
#           )
# 
# dat4 = all_content[[6]] |> 
#   mutate(
#     end_day = coalesce(as.double(end_day_leave_blank_if_this_was_a_one_day_sample), as.double(start_day)),
#     end_month = coalesce(as.character(end_month_leave_blank_if_this_was_a_one_day_sample), as.character(start_month))) |> 
#   reframe(
#     data_type = 'field',
#     id = sample,
#     id_type = sample_description,
#     trap_type = collection_method_bumble_bee_plot_bumble_bee_route_wandering_pan_traps,
#     start_date = lubridate::ymd(paste0(year,'-',start_month,'-',start_day)),
#     end_date = lubridate::ymd(paste0(year,'-',end_month,'-',end_day)),
#     lat = latitude_n,
#     lon = longitude_w,
#     loc_notes = locality_description_name_of_regional_park_road_rest_stop_etc_separate_multiple_entries_with_semi_colon
#   )
# 
# dat5 = all_content[[7]] |> 
#   # mutate(
#   # end_day = coalesce(as.double(end_day_leave_blank_if_this_was_a_one_day_sample), as.double(start_day)),
#   # end_month = coalesce(as.character(end_month_leave_blank_if_this_was_a_one_day_sample), as.character(start_month))) |> 
#   reframe(
#     data_type = 'field',
#     id = sample,
#     id_type = sample_type,
#     trap_type = method,
#     start_date = lubridate::ymd(paste0(year,'-',start_month,'-',start_day)),
#     end_date = lubridate::ymd(paste0(year,'-',end_month,'-',end_day)),
#     lat = latitude_n,
#     lon = longitude_w,
#     loc_notes = paste0(locality_1,', ',locality_2,', ', locality_3),
#     comments = paste0(method,'; ',habitat)
#   )
# 
# dat6 = all_content[[8]] |> 
#   reframe(
#     data_type = 'id',
#     id = stringr::str_extract(num_cat_no_part_3, '[0-9]+'),
#     id_type = 'rskm',
#     trap_type = loc_collection_method,
#     start_date = openxlsx::convertToDate(loc_date_collected_from),
#     end_date = openxlsx::convertToDate(loc_date_collected_to),
#     lat = loc_latitude,
#     lon = loc_longitude,
#     loc_notes = loc_locality_notes
#   )
# 
# all_content[[9]] |> 
#   reframe(
#     data_type = 
#   )
