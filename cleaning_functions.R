# Read in a file. Detect if it's an excel file or csv.
# If excel, make sure column names are unique and that 
# we skip empty organizational rows.

# Conditional read for excel or csv.
read_the_file = function(file){
  if(stringr::str_detect(file,'xlsx$')) {
    
    dat = openxlsx::read.xlsx(file)
    
    # If the column names are stuff like X2 in the 2nd column, we probs need to
    # skip some rows.
    if(colnames(dat)[2] == 'X2' | colnames(dat)[5] == 'X5'){
      # Get the lowest row number of non-NA value in 2nd column of data.
      data_begins_on_row = min(which(!is.na(dat[,2])))
      
      # there can be excess columns from extraneous note columns etc. Drop those.
      new_column_names = na.omit(unname(unlist(dat[data_begins_on_row,])))
      
      # Drop excess columns.
      dat = dat[,1:length(new_column_names)]
      
      # rewrite column names with cell contents from the row identified in line above.
      colnames(dat) <- new_column_names
      
      dat = dat |> 
        slice((data_begins_on_row+1):nrow(dat))
    }
    
    # If there are repeated column names, add a 'dup' suffix to duplicated colnames.
    if(length(unique(colnames(dat))) != length(colnames(dat))){
      
      the_col_names = names(dat)
      
      repeated_col_names = which(duplicated(the_col_names))
      
      # Reset levels from, e.g., c(8, 10) to c(1, 2)
      duplicate_suffix = as.numeric(as.factor(repeated_col_names))
      
      # Add '_X' as suffix, e.g. if two columns named 'Year', 2nd one will be called 
      # Year_2.
      names(dat)[repeated_col_names] = paste0(names(dat)[repeated_col_names],'_',duplicate_suffix+1)
    }
    
    
  }
  if(stringr::str_detect(file,'csv$')) {
    dat = readr::read_csv(file)
  }
  
  dat = dat |> 
    set_names(snakecase::to_snake_case(names(dat))) |> 
    as_tibble()
}

# Identify if the file is a field data file or something else...
identify_data_type = function(data){
  data$datatype = 'unknown'
  
  if(sum(str_detect(stringr::str_to_lower(colnames(data)), '(lat|latitude)')) >= 1){
    data$datatype = 'field_samples'
  }
  if(sum(str_detect(stringr::str_to_lower(colnames(data)), '(genus|rskm|vial|museum)')) >= 1 & data[1,]$datatype == 'unknown'){
    data$datatype = 'id_sheet'
  }
  return(data)
}


# Complicated cleaning function for data sheets... very ad hoc.
# Some data cleaning steps to ensure we have the same data types for certain 
# column names.
fieldsheet_cleaner = function(dat){

  # browser()
  cols_to_keep = str_detect(names(dat),"(date|year|month|day|longitude|lat|longitude|jar|num_cat_no_part|catalogue_number|number$|sample[_]?[a-zA-Z]*)")
  
  dat_s = dat[,cols_to_keep]
  
  # Clarify which column has the ID, what kind of ID it is...
  if(sum(str_detect(names(dat_s),'^catalogue_number$')) > 0){
    dat_s = dat_s |> 
      dplyr::rename(id = catalogue_number) |> 
      mutate(id_type = 'catalogue')
  }
  if(sum(str_detect(names(dat_s),'rsm_catalogue_number')) > 0){
    dat_s = dat_s |> 
      dplyr::rename(id = rsm_catalogue_number) |> 
      mutate(id_type = 'catalogue')
  }
  if(sum(str_detect(names(dat_s),'jar')) > 0){
    dat_s = dat_s |> 
      dplyr::rename(id = jar) |> 
      mutate(id_type = 'jar')
  }
  if(sum(str_detect(names(dat_s),'sample_description')) > 0){
    dat_s = dat_s |> 
      dplyr::rename(id = sample) |> 
      mutate(id_type = sample_description) |> 
      dplyr::select(-c(sample_number,sample_letter))
  }
  if(sum(str_detect(names(dat_s),'sample_type')) > 0){
    dat_s = dat_s |> 
      dplyr::rename(id = sample) |> 
      mutate(id_type = sample_type) |> 
      dplyr::select(-c(sample_number,sample_letter))
  }
  if(sum(str_detect(names(dat_s),'^number$')) > 0 & sum(str_detect(names(dat_s),'^id$')) == 0){
    dat_s = dat_s |> 
      dplyr::rename(id = number) |> 
      mutate(id_type = 'sample_number')
  }
  
  # Remove prefixes for columns such as 'loc_', if they are present.
  names(dat_s) = str_remove_all(names(dat_s), '^loc_')
  
  # Remove either 'lat' or 'long' with 'determination'
  dat_s = dat_s |> 
    select(-contains('determination'))
  
  # Convert date column(s) to datetime.
  if(sum(str_detect(names(dat_s), '[^verbatim_event_]+date')) > 0){
    # Is the start_date column really a full date? Check the length
    test_col = dat_s |> dplyr::select(contains("[^verbatim_event_]+date")) |> pull(1)
    if(nchar(test_col[1]) >= 3){
      dat_s = dat_s |> 
        mutate(across(contains('date'), \(x) openxlsx::convertToDate(x)))
    }
  }
  
  # Are there date or year/month/day columns present? If so, let's clean those up.
  # Otherwise, skip this chunk.
  if(sum(str_detect(names(dat_s), '.*(date|year|month)+.*')) > 0){
  # If distinct columns for year, month, and day are present in the data,
  # join these together into a single date column (separately for start and end dates 
  # if available)
  if(sum(str_detect(names(dat_s), 'month')) > 0){
    if(sum(str_detect(names(dat_s), '(date_set|start_month|start_date)')) > 0){
      # Start and end date column to produce.
      dat_s = comb_date_columns(dat_s, id = 'start')
      dat_s = comb_date_columns(dat_s, id = 'end')
    } else {
      # No differentiation for start and end dates made. Single date column to produce.
      dat_s = comb_date_columns(dat_s)
    }
  } else {
    # If date columns are already joined, just change the names.
    dat_s = dat_s |> 
      rename_all(gsub, pattern = '.*date[_]?set.*', replacement = 'start_date') |> 
      rename_all(gsub, pattern = '.*date[_]?stop.*', replacement = 'end_date')
  }
  }
  
# If there are columns with 'num_cat_no_part' (as in RSKM datasheets),
# unite all such columns into one called 'rskm_id'
if(sum(str_detect(names(dat_s), 'num_cat_no_part')) > 0){
  dat_s = dat_s |> 
    unite(rskm_full_id, starts_with("num_cat_no_part"), remove = T, sep = "_") |> 
    mutate(rskm_id = str_remove_all(rskm_full_id, '.*_'))
}

dat_s
}

# Function to combine date columns. id parameter allows user to 
# specify if desired date column is a start date, end date, or just a plain
# old date column.
comb_date_columns = function(dat, id = NULL){
  
  if(is.null(id)){
    # Plain old date column.
    dat = dat |> 
      rename_all(gsub, pattern = '.*(date|[d,D]ay).*', replacement = 'day') |> 
      rename_all(gsub, pattern = '.*[m,M]onth.*', replacement = 'month') |> 
      rename_all(gsub, pattern = '.*[y,Y]ear.*', replacement = 'year')
      
    dat$date = lubridate::ymd(paste0(dat$year,'-',dat$month,'-',dat$day))
  }
  if(id == 'start'){
    # Is there only a column named 'year', and no 'start_year'? 
    # Convert 'year' to 'start_year'
    if(sum(str_detect(names(dat), '^year$')) > 0 & sum(str_detect(names(dat), 'start_year')) == 0){
      dat = dat |> 
        dplyr::mutate(start_year = year)
    }
    # Is there one mammoth column called 'collection_start_date_year_month_day' that needs parsing?
    if(sum(str_detect(names(dat), 'collection_start_date_year_month_day') > 0)){
      dat = dat |> 
        mutate(start_year = str_extract(collection_start_date_year_month_day, '^[0-9]{4}'),
               start_month = str_extract(collection_start_date_year_month_day, '[A-Za-z]+'),
               start_day = str_extract(collection_start_date_year_month_day, '[0-9]*$')) |> 
        mutate(start_date = lubridate::ymd(
          paste0(start_year,'-',
                 start_month,'-',
                 start_day)
        )
        ) |> 
        dplyr::select(-c(collection_start_date_year_month_day,start_year,start_month,start_day))
    } else {
      
    # Start date column.
    dat = dat |> 
      rename_all(gsub, pattern = '.*[s,S]tart(_| )?(date|[d,D]ay).*', replacement = 'start_day') |> 
      rename_all(gsub, pattern = '.*[s,S]tart(_| )?[m,M]onth.*', replacement = 'start_month') |> 
      rename_all(gsub, pattern = '.*[s,S]tart(_| )?[y,Y]ear.*', replacement = 'start_year')
    
    dat$start_date = lubridate::ymd(paste0(dat$start_year,'-',dat$start_month,'-',dat$start_day))
    
    dat = dat |> dplyr::select(-c(start_year,start_month,start_day))
    }
  }
  if(id == 'end'){
    # Is there only a column named 'year', and no 'end_year'? 
    # Convert 'year' to 'end_year'
    if(sum(str_detect(names(dat), '^year$')) > 0 & sum(str_detect(names(dat), 'end_year')) == 0){
      dat = dat |> 
        dplyr::mutate(end_year = year)
    }
    # Is there one mammoth column called 'collection_end_date_year_month_day' that needs parsing?
    if(sum(str_detect(names(dat), 'end_date_year_month_day') > 0)){
      dat = dat |> 
        mutate(end_year = str_extract(end_date_year_month_day, '^[0-9]{4}'),
               end_month = str_extract(end_date_year_month_day, '[A-Za-z]+'),
               end_day = str_extract(end_date_year_month_day, '[0-9]*$')) |> 
        mutate(end_date = lubridate::ymd(
          paste0(end_year,'-',
                 end_month,'-',
                 end_day)
        )
        ) |> 
        dplyr::select(-c(end_date_year_month_day,end_year,end_month,end_day))
    } else {
      
      # end date column.
      dat = dat |> 
        rename_all(gsub, pattern = '.*[s,S]tart(_| )?(date|[d,D]ay).*', replacement = 'end_day') |> 
        rename_all(gsub, pattern = '.*[s,S]tart(_| )?[m,M]onth.*', replacement = 'end_month') |> 
        rename_all(gsub, pattern = '.*[s,S]tart(_| )?[y,Y]ear.*', replacement = 'end_year')
      
      dat$end_date = lubridate::ymd(paste0(dat$end_year,'-',dat$end_month,'-',dat$end_day))
      
      dat = dat |> dplyr::select(-c(end_year,end_month,end_day))
    }
  }
  
  if(id %in% c("start","end")){
    # Is there ALSO a column just called 'year', in addition to 'start_year'? 
    # This is the case for some sheets... egads. Drop that column.
    if(sum(str_detect(names(dat), '^year$')) > 0 & sum(str_detect(names(dat), '[start,end]+_year')) > 0){
      dat = dat |> 
        dplyr::select(-year)
    }
  }
  dat
}
