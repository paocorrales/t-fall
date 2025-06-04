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
percentiles <- 100 - c(0.1, 0.5,  1, 2, 2.5, 5, 7.5, 10)

# ReadNetCDF("/home/565/pc2687/t-fall/data/cmip/100km/deltat_day_AWI-CM-1-1-MR_ssp126_r1i1p1f1_gn_20150101-21001231.nc", 
#            vars = "tasmax", subset = list(time = "1990-10-25")) |> 
#   _[, let(land = MaskLand(lon, lat))] |> 
#   _[, let(tasmax = ifelse(land == TRUE, -10, -4))] |> 
#   _[, let(land = NULL)] |> 
#   metR:::WriteNetCDF(data = _, "~/t-fall/data/cmip/threshold_l10s04_cmip.nc", vars = "tasmax")


# Compare with -10 degrees
#   Sum 1 over years 
#   Ensamble mean --> time series of frequency of events

file_list <- c(Sys.glob("~/t-fall/data/cmip/100km/deltat_day_*ssp585*"),
               Sys.glob("~/t-fall/data/cmip/100km/deltat_day_*hist*"))

future::plan(future::multisession, workers = 4)
furrr::future_map(percentiles, function(p) {
  purrr::map(file_list, function(f) {
    
    meta <- unglue::unglue(basename(f), "deltat_day_{model}_{scenario}_{member}_{grid}_20150101-21001231.nc")
    
    model <- meta[[1]][["model"]]
    member <- meta[[1]][["member"]]
    experiment <- meta[[1]][["scenario"]]
    
    message(paste0("processing ", basename(f)))
    
    outfile_DJF <- paste0("~/t-fall/data/cmip/100km/", "deltap", p, "_DJF_", model, "_", experiment, "_", member, "_20150101-21001231.nc")
    outfile_JJA <- paste0("~/t-fall/data/cmip/100km/", "deltap", p, "_JJA_", model, "_", experiment, "_", member, "_20150101-21001231.nc")
    dir.create(dirname(outfile_DJF), showWarnings = FALSE, recursive = TRUE)
    
    write(paste0("processing ", basename(outfile_DJF)), file = "~/log", append = TRUE)
    
    if (file.exists(outfile_DJF)) {
      return(outfile_DJF)
    }
    
    if (threshold == "constant") {
      
      cdo_lec(f, c = -10) |>
        cdo_yearsum() |> 
        cdo_execute(outfile, options = "-L")
      
    } else if (threshold == "percentile") {
      
      threshold_file <- Sys.glob(paste0("~/t-fall/data/cmip/percentiles/deltat_", model, "_historical_*_p", p, "_1979-2014_season.nc"))
      
      if (length(threshold_file) == 0 || !file.exists(threshold_file)) {
        return(paste0("no percentile calcualted for this model ", model))
      }
      
      message(paste0("threshold: ", basename(threshold_file)))
      
      th <- cdo_seldate(threshold_file, startdate = "2014-01-01T00:00:00", enddate = "2014-02-28T23:00:00") |> 
        cdo_execute(options = "-L")
      cdo_selseason(f, "DJF") |> 
        cdo_le(th) |>
        cdo_seassum() |>
        cdo_execute(outfile_DJF, options = "-L")
      
      th <- cdo_seldate(threshold_file, startdate = "2014-06-01T00:00:00", enddate = "2014-08-31T23:00:00") |> 
        cdo_execute(options = "-L")
      cdo_selseason(f, "JJA") |> 
        cdo_le(th) |> 
        cdo_seassum() |>
        cdo_execute(outfile_JJA, options = "-L")
      
    } else {  # land vs sea threshold
      
      cdo_le(f,  "~/t-fall/data/cmip/threshold_l10s04_cmip.nc") |> 
        # cdo_yearsum() |> 
        cdo_seassum() |>
        cdo_execute(outfile, options = "-L")
      
    }
    
  })
})
