# This script cleans a collection of heterogeneously named and organized
# excel sheets of bee collection fieldwork and (museum) specimen identification.

# packages
library(tidyverse)

# Source cleaning functions
source('cleaning_functions.R')

# Find files in data folder.
all_files = list.files(path = 'data/', pattern = '[^R]$',full.names = T)

# Read in files (function detects if they are .csv or .xlsx)
all_content = lapply(all_files, read_the_file)

# all_content[[1]]

# Identify if sheet is fieldwork or specimen ID.
all_content_sorted = lapply(all_content, identify_data_type)

# Initialise lists for field samples and id sheets
field_samples_list = list()
id_sheet_list = list()

# Append data files to either of the two lists depending on the automatically
# detected datatype.
for(element_number in 1:length(all_content_sorted)){
  
  print(element_number)
  
  dat_from_list = all_content_sorted[[element_number]]
  
  dat_from_list = as_tibble(dat_from_list)
  
  if(unique(dat_from_list$datatype) == 'field_samples') field_samples_list[[length(field_samples_list) + 1]] <- dat_from_list
  if(unique(dat_from_list$datatype) == 'id_sheet') id_sheet_list[[length(id_sheet_list) + 1]] <- dat_from_list
}

# names(all_content_sorted[[8]])

source('cleaning_functions.R')

fieldsheet_cleaner(field_samples_list[[2]])

lapply(field_samples_list, fieldsheet_cleaner)

# Bind rows of list elements for field sample list and separately for (museum) id sheets
field_samples_dat = field_samples_list |> 
  bind_rows()

id_sheet_dat = id_sheet_list |> 
  bind_rows()
