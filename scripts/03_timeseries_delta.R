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


# Compare with -10 degrees
#   Sum 1 over years 
#   Ensamble mean --> time series of frequency of events

file_list <- Sys.glob("~/t-fall/data/cmip/historical/deltat_day_*")

purrr::map(file_list, function(f) {
  
  meta <- unglue::unglue(basename(f), "deltat_day_{model}_{scenario}_{member}_gn_20150101-21001231.nc")
  
  model <- meta[[1]][["model"]]
  member <- meta[[1]][["member"]]
  experiment <- meta[[1]][["scenario"]]
  
  message(paste0("processing ", basename(f)))
  
  outfile <- paste0("~/t-fall/data/cmip/delta10/", "delta10_year_", model, "_", experiment, "_", member, "_20150101-21001231.nc")
  dir.create(dirname(outfile), showWarnings = FALSE, recursive = TRUE)
  
  if (file.exists(outfile)) {
    return(outfile)
  }
  
  cdo_lec(f, c = -10) |> 
    cdo_yearsum() |> 
    cdo_execute(outfile, options = "-L")
  
})
