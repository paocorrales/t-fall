library(metR)
library(ggplot2)
library(data.table)
library(lubridate)
library(rcdo)

# files <- Sys.glob("data/cmip/tasmax/deltat_day_FGOALS-g3_*_r1i1p1f1_gn_20150101-21001231.nc")
# 
# data <- purrr::map(files, function(f) {
#   
#   meta <- unglue::unglue(basename(f), "deltat_day_{model}_{scenario}_{member}_gn_20150101-21001231.nc")
#   
#   a <- ReadNetCDF(f, vars = "tasmax",
#                   subset = list(lon = c(141, 152), lat = c(-39, -30))) |> 
#     _[, let(scenario = meta[[1]][["scenario"]],
#             model = meta[[1]][["model"]])]
#   
#   
# }) |> 
#   rbindlist()
# 
# data |> 
#   _[, let(front = tasmax <= -10)] |> 
#   _[, .(fronts_year = sum(front, na.rm = TRUE)), by = .(lon, lat, year(time), scenario)] |> 
#   _[lat %between% c(-38, -37) & lon == 144] |> 
#   ggplot(aes(year, fronts_year, color = scenario)) +
#   geom_line() +
#   geom_smooth(method = "lm", se = FALSE)

threshold <- "percentile"
p <- 99 

# ReadNetCDF("/home/565/pc2687/t-fall/data/cmip/100km/deltat_day_AWI-CM-1-1-MR_ssp126_r1i1p1f1_gn_20150101-21001231.nc", 
#            vars = "tasmax", subset = list(time = "1990-10-25")) |> 
#   _[, let(land = MaskLand(lon, lat))] |> 
#   _[, let(tasmax = ifelse(land == TRUE, -10, -4))] |> 
#   _[, let(land = NULL)] |> 
#   metR:::WriteNetCDF(data = _, "~/t-fall/data/cmip/threshold_l10s04_cmip.nc", vars = "tasmax")


# Compare with -10 degrees
#   Sum 1 over years 
#   Ensamble mean --> time series of frequency of events

file_list <- Sys.glob("~/t-fall/data/cmip/100km/deltat_day_*")

# future::plan(future::multisession, workers = 4)
# furrr::future_map(file_list, function(f) {
purrr::map(file_list, function(f) {
  
  meta <- unglue::unglue(basename(f), "deltat_day_{model}_{scenario}_{member}_{grid}_20150101-21001231.nc")
  
  model <- meta[[1]][["model"]]
  member <- meta[[1]][["member"]]
  experiment <- meta[[1]][["scenario"]]
  
  message(paste0("processing ", basename(f)))
  
  outfile <- paste0("~/t-fall/data/cmip/100km/", "deltap99_year_", model, "_", experiment, "_", member, "_20150101-21001231.nc")
  dir.create(dirname(outfile), showWarnings = FALSE, recursive = TRUE)
  
  if (file.exists(outfile)) {
    return(outfile)
  }
  
  if (threshold == "constant") {
    
    cdo_lec(f, c = -10) |>
      cdo_yearsum() |> 
      cdo_execute(outfile, options = "-L")
    
  } else if (threshold == "percentile") {
    
    threshold_file <- Sys.glob(paste0("~/t-fall/data/cmip/percentiles/deltat_", model, "_historical_*_p", p, "_1979-2014.nc"))
    
    if (length(threshold_file) == 0 || !file.exists(threshold_file)) {
      return(paste0("no percentile calcualted for this model ", model))
    }
    
    message(paste0("threshold: ", basename(threshold_file)))
    
    cdo_le(f, threshold_file) |> 
      cdo_yearsum() |> 
      cdo_execute(outfile, options = "-L")
    
  } else {  # land vs sea threshold
     
    cdo_le(f,  "~/t-fall/data/cmip/threshold_l10s04_cmip.nc") |> 
      cdo_yearsum() |> 
      cdo_execute(outfile, options = "-L")
    
  }
  
})
