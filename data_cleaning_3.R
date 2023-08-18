# Manual data cleaning... third attempt.

# This script cleans a collection of heterogeneously named and organized
# excel sheets of bee collection fieldwork and (museum) specimen identification.

# packages
library(tidyverse)
library(openxlsx)
library(sf)

# Source cleaning functions
# source('cleaning_functions.R')

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
  distinct() |> 
  filter(!duplicated(id))

# Simplify the levels for 'trap_type'
rskm_dat = rskm_dat |> 
  mutate(trap_type = str_extract(trap_type, '[ a-zA-Z]*')) |>
  mutate(trap_type = stringr::str_squish(trap_type)) |> 
  mutate(trap_type = str_to_title(trap_type)) |> 
  mutate(trap_type = str_replace_all(trap_type, 'Traps','Trap')) |> 
  mutate(trap_type = str_replace_all(trap_type, 'Hand.*', 'Hand Collected'))

# Replace anything resembling 'sp.' for species ID with NA.
rskm_dat[str_detect(rskm_dat$scientific_name,'.*sp\\..*') & !is.na(rskm_dat$scientific_name),]$scientific_name <- NA

# Remove some seemingly superfluous 
# levels of taxonomic information.
rskm_dat = rskm_dat |> 
  dplyr::select(-phylum,-class)

# Attempting to reduce the number of overlapping, duplicated
# coordinates. Two ideas:
# 1. Spatial match with some polygons to make choropleth. (too intense for shiny app RAM?)
#    After some quick testing, it seems this may be too intense RAM-wise.

# 2. Summarize number of samples of given ID for each coordinate.

rskm_sum = rskm_dat |> 
  dplyr::select(lat,lon,trap_type,start_date,end_date,order:scientific_name) |> 
  group_by_all() |> 
  count() |> 
  ungroup()

qs::qsave(rskm_sum, file = 'www/bee_db.qs')
