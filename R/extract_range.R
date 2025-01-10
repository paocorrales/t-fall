extract_range <- function(file_list) {
  
  if (length(file_list) == 0) {
    
    return(data.table::data.table(inidate = NA, enddate = NA))
  }
  
  # tasmax_day_FGOALS-g3_ssp370_r1i1p1f1_gn_21000101-21001231.nc
  unglue::unglue_data(basename(file_list), "{var}_day_{model}_{experiment}_{member}_{grid}_{inidate}-{enddate}.nc") |> 
    data.table::setDT() |> 
    _[, .(inidate = min(inidate),
          enddate = max(enddate))]
  
}