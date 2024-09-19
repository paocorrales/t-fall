
file_list <- Sys.glob("~/BoM_stations/sub-daily/1minute-rainfall_*/HD01D_Data*")

purrr::map(file_list, function(f) {
  
  message(basename(f))
  
  data.table::fread(f,
                    skip = 1, 
                    col.names = c("hd", "station_number", "year", "month", "day", "hour", "minutes",
                                  "year_s", "month_s", "day_s", "hour_s", "minutes_s", "pp", "qc", "period", "rm")) |> 
    _[, .(pp = sum(pp, na.rm = TRUE)), by = .(station_number, year_s, month_s, day_s)] |> 
    fwrite(paste0("~/BoM_stations/daily_pp/", basename(f)))
  
}) 


