utils_digest_excel = function(filepath){

  if(!stringr::str_detect(filepath$datapath, ".xls(x)?")){
    return(null)
  }

  return(openxlsx::read.xlsx(filepath$datapath))
}
