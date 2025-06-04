# uv <- ReadNetCDF("data/cmip/ua/ua_day_BCC-CSM2-MR_ssp585_r1i1p1f1_gn_20150101-21001231.nc",
#                  vars = "ua",
#                  subset = list(time = c("2050-01-01", "2050-01-31"))) |> 
#   _[, let(va = ReadNetCDF("data/cmip/va/va_day_BCC-CSM2-MR_ssp585_r1i1p1f1_gn_20150101-21001231.nc",
#                           vars = "va",
#                           subset = list(time = c("2050-01-01", "2050-01-31")),
#                           out = "vector")[[1]])] |> 
#   _[, let(eke = 0.5*(ua^2 + va^2))]
# 
# 
# uv[, .(eke = mean(eke)), by = .(lon, lat)] |> 
#   ggplot(aes(ConvertLongitude(lon), lat)) +
#   geom_contour_fill(aes(z = eke)) +
#   geom_sf(data = global_map, inherit.aes = FALSE, fill = NA, linewidth = 0.4) +
#   coord_sf(expand = FALSE) 



library(metR)
# library(ggplot2)
library(data.table)
# library(lubridate)
library(rcdo)

	file_list_u <- Sys.glob(here::here("data/cmip/ua/ua_day_*_ssp585_*"))

purrr::map(file_list_u, function(f) {
  # future::plan(future::multisession, workers = 2)
  # furrr::future_map(file_list_u, function(f) {
  
  
  meta <- unglue::unglue(basename(f), "ua_day_{model}_{scenario}_{member}_{grid}_20150101-21001231.nc")
  
  model <- meta[[1]][["model"]]
  member <- meta[[1]][["member"]]
  experiment <- meta[[1]][["scenario"]]
  grid <- meta[[1]][["grid"]]
  
  file_v <- here::here(paste0("data/cmip/va/va_day_", model, "_", experiment, "_", member, "_", grid, "_20150101-21001231.nc"))
  
  if (!file.exists(file_v)) {
    
    message(paste0("No v file for ", model))
    return()
  }
  
  message(paste0("processing ", basename(f)))
  
  outfile <- paste0("~/t-fall/data/cmip/eke/eke_day_", model, "_", experiment, "_", member, ".nc")
  dir.create(dirname(outfile), showWarnings = FALSE, recursive = TRUE)

  # if (file.exists(outfile)) {
  #   return(outfile)
  # }

  # eke <- cdo_add(cdo_sqr(f), cdo_sqr(file_v)) |>
  #   cdo_mulc(0.5) |>
  #   cdo_execute(outfile, options = "-L")
  # 
  # # Year mean
  # cdo_yearmean(eke) |>
  #   cdo_execute(paste0("~/t-fall/data/cmip/eke/eke_year_", model, "_", experiment, "_", member, ".nc"), options = "-L")
  # 
  # # Season mean
  # season_mean <- cdo_seasmean(eke) |>
  #   cdo_execute(paste0("~/t-fall/data/cmip/eke/eke_season_", model, "_", experiment, "_", member, ".nc"), options = "-L")
  
  eke <- paste0("~/t-fall/data/cmip/eke/eke_day_", model, "_", experiment, "_", member, ".nc")
  
  # Trend
  if(experiment != "historical") {

      cdo_trend(eke, equal = "equal=false", 
                ofile1 = paste0("~/t-fall/data/cmip/eke/eke_a_", model, "_", experiment, "_", member, ".nc"), 
                ofile2 = paste0("~/t-fall/data/cmip/eke/eke_b_", model, "_", experiment, "_", member, ".nc")) |> 
      cdo_execute(options = "-L")
    
    # cdo_selseason(eke, "DJF") |> 
    #   cdo_trend(equal = "equal=false", 
    #             ofile1 = paste0("~/t-fall/data/cmip/eke/eke_a-DJF_", model, "_", experiment, "_", member, ".nc"), 
    #             ofile2 = paste0("~/t-fall/data/cmip/eke/eke_b-DJF_", model, "_", experiment, "_", member, ".nc")) |> 
    #   cdo_execute(options = "-L")
    # 
    # cdo_selseason(eke, "JJA") |> 
    #   cdo_trend(equal = "equal=false", 
    #             ofile1 = paste0("~/t-fall/data/cmip/eke/eke_a-JJA_", model, "_", experiment, "_", member, ".nc"), 
    #             ofile2 = paste0("~/t-fall/data/cmip/eke/eke_b-JJA_", model, "_", experiment, "_", member, ".nc")) |> 
    #   cdo_execute(options = "-L")
  }
})
