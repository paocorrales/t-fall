library(data.table)
library(lubridate)
library(rcdo)
purrr::walk(Sys.glob(here::here("R/*")), source)

## Distribution of deltaT
## Calculate percentiles for historical experiments, period 1950-2000
## Compare percentile with experiment and keep those values
## Calculate frequency of each grid point?

file_list <- Sys.glob("~/t-fall/data/cmip/100km/deltat_*historical*")

# future::plan(future::multisession, workers = 4)
# furrr::future_map(file_list[17:28], function(f) {
purrr::map(file_list, function(f) {
  
  meta <- unglue::unglue(basename(f), "deltat_day_{model}_{scenario}_{member}_{grid}_20150101-21001231.nc")
  
  model <- meta[[1]][["model"]]
  member <- meta[[1]][["member"]]
  experiment <- meta[[1]][["scenario"]]
  
  message(paste0("processing ", basename(f)))
  
  outfile <- paste0("~/t-fall/data/cmip/percentiles/", "deltat_", model, "_", experiment, "_", member, "_p99_1979-2014.nc")
  dir.create(dirname(outfile), showWarnings = FALSE, recursive = TRUE)
  
  if (file.exists(outfile)) {
    return(outfile)
  }
  
  period <- cdo_seldate(file_list[1], startdate = "1979-01-01T00:00:00", enddate = "2014-12-31T23:00:00") |>
    cdo_execute(options = "-L")
  
  cdo_timpctl(period, cdo_timmin(period), cdo_timmax(period),  p = 1) |> 
    cdo_execute(outfile, options = "-L")
  
})



file_list <- Sys.glob("~/t-fall/data/cmip/percentiles//deltat_*historical*")

# future::plan(future::multisession, workers = 4)
# furrr::future_map(file_list[17:28], function(f) {
# purrr::map(file_list, function(f) {
#   
#   meta <- unglue::unglue(basename(f), "deltat_{model}_{scenario}_{member}_p90_1950-2000.nc")
#   
#   model <- meta[[1]][["model"]]
#   member <- meta[[1]][["member"]]
#   experiment <- meta[[1]][["scenario"]]
#   
#   message(paste0("processing ", basename(f)))
#   
#   outfile <- paste0("~/t-fall/data/cmip/delta10/", "deltap975_year_", model, "_", experiment, "_", member, "_1979-2014.nc")
#   dir.create(dirname(outfile), showWarnings = FALSE, recursive = TRUE)
#   
#   if (file.exists(outfile)) {
#     return(outfile)
#   }
#   
#   varfile <- Sys.glob(paste0("~/t-fall/data/cmip/100km/deltat_day_" , model, "_", experiment, "_", member, "*"))
#   
#   cdo_le(varfile, f) |> 
#     cdo_setctomiss(c = 0) |> 
#     cdo_mul(varfile) |> 
#     cdo_execute(outfile, options = "-L")
#   
# })


# a |>
#   _[, let(land = MaskLand(lon, lat),
#           region = fcase(lat %between% c(20, 60), "NH",
#                          lat %between% c(-60, -20), "SH",
#                          default = NA))]
# 
# a[!is.na(region)] |>
#   ggplot(aes(tasmax)) +
#   geom_histogram(binwidth = 0.5) +
#   facet_grid(land ~ region)
# 
# a[, .(mean_deltat = mean(tasmax, na.rm = TRUE)), by = .(lon, lat)] |> 
#   ggplot(aes(lon, lat)) +
#   geom_contour_fill(aes(z = mean_deltat)) +
#   geom_contour(aes(z = mean_deltat), breaks = c(-4, -10), color = "orange")
# 
# a[, let(deltat = fcase(land == TRUE, tasmax < -10, land == FALSE, tasmax < -4))] |> 
#   _[, .(freq_deltat = sum(deltat, na.rm = TRUE)), by = .(lon, lat)] |> 
#   ggplot(aes(lon, lat)) +
#   geom_contour_fill(aes(z = freq_deltat)) 
# 
# a[time == as_datetime("2000-01-01 12:00:00")] |> 
#   ggplot(aes(lon, lat)) +
#   geom_contour_fill(aes(z = tasmax))


## Distribution of deltaT for a historical simulation


years <- seq(1961, 2014)
nc <- ncdf4:::nc_open("~/t-fall/data/cmip/extrems/deltat_AWI-CM-1-1-MR_historical_r1i1p1f1_p90_1950-2000.nc")
mask <- ReadNetCDF(nc, vars = "tasmax",
                   subset = list(time = c("2000-01-01"),
                                 lat = list(-60:-20, 20:60))) |> 
  _[, let(land = MaskLand(lon, lat),
          region = fifelse(lat < 0, "SH", "NH"),
          time = NULL,
          tasmax = NULL)] |> 
  setkey(lon, lat)

purrr::map(years, function(y) {
  
  message(y)
  
  # tictoc::tic()
  a <- ReadNetCDF(nc, vars = "tasmax",
                  subset = list(time = c(paste0(y, "-01-01"), paste0(y, "-12-31")),
                                lat = list(-60:-20, 20:60))) |> 
    setkey(lon, lat)
  
  mask[a] |> 
    _[!is.na(tasmax) & !is.na(region)] |> 
    _[, let(deltat = cut_round(tasmax, breaks = seq(-30, 0, 0.5)),
            land = fifelse(land == TRUE, "Land", "Sea"))] |> 
    _[, .N, by = .(deltat, land, region, year(time))]
  
  # tictoc::toc()
}) |> 
  rbindlist() |> 
  fwrite(x = _, file = "~/t-fall/data/cmip/freq_AWI-CM-1-1-MR_historical_r1i1p1f1.csv", append = TRUE)


freq <- fread("~/t-fall/data/cmip/freq_AWI-CM-1-1-MR_historical_r1i1p1f1.csv") |> 
  _[, .(N = sum(N)), by = .(deltat, land, region, year)] |> 
  _[, let(max_freq = max(N)), by = .(region, land)]

freq[year %in% c(1850:2014)] |> 
  ggplot(aes(deltat, N/max_freq)) +
  geom_line(aes(color = year, group = year), linewidth = 0.2) +
  scale_color_viridis_c() +
  facet_grid(land ~ region) +
  theme_minimal()

freq[year <2014] |> 
  _[, .(N = sum(N)), by = .(deltat, land, region, year)] |> 
  ggplot(aes(year, deltat)) +
  geom_contour(aes(z = (N), color = after_stat(level))) +
  scale_color_viridis_c() +
  facet_grid(land ~ region)
