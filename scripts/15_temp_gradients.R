library(data.table)
library(lubridate)
library(rcdo)
purrr::walk(Sys.glob(here::here("R/*")), source)

models_in_gadi <- c("CMCC-ESM2", "EC-Earth3", "INM-CM4-8", "INM-CM5-0", 
                    "MPI-ESM1-2-HR", "NorESM2-MM", "EC-Earth3-CC", "EC-Earth3-Veg",
                    "EC-Earth3-Veg-LR", "GFDL-CM4")

file_list_cmip <- rbind(readr::read_rds(here::here("data/cmip/cmip_file_list_historical.rds")),
                        readr::read_rds(here::here("data/cmip/cmip_file_list_scenario.rds")))|>
  _[source_id %in% models_in_gadi] |> 
  _[, let(path = paste0("/g/data/oi10/replicas/CMIP6/", activity_drs, "/", institution_id, "/",
                        source_id, "/",
                        experiment_id, "/",
                        member_id, "/day/", 
                        "hfss/*/*/*"))] |> 
  # unique(by = "path") |> 
  _[, let(file_list = list(Sys.glob(path))), by = path] |> 
  _[, let(n_files = length(file_list[[1]])), by = path] |> 
  _[, let(inidate = NULL, enddate = NULL)] |> 
  _[, let(path = fifelse(n_files == 0, paste0("/scratch/gb02/pc2687/CMIP6/", activity_drs, "/", institution_id, "/",
                                              source_id, "/",
                                              experiment_id, "/",
                                              member_id, "/day/", 
                                              "hfss/*/*/*.nc"), path))] |> 
  _[, let(file_list = list(Sys.glob(path))), by = path] |> 
  _[, c("inidate", "enddate") := extract_range(file_list[[1]]), by = path] |> 
  _[, let(inidate = ymd(inidate),
          enddate = ymd(enddate))] |> 
  _[, let( in_gadi = !((inidate >= ymd(18500101) | is.na(inidate)) | (enddate <= ymd(21001231) | is.na(enddate))))]

# Merge and regrid files and calculate deltat

# grid <- "/g/data/oi10/replicas/CMIP6/ScenarioMIP/CAS/FGOALS-g3/ssp370/r1i1p1f1/day/tasmax/gn/v20190820/tasmax_day_FGOALS-g3_ssp370_r1i1p1f1_gn_21000101-21001231.nc"
grid <- "/g/data/oi10/replicas/CMIP6/ScenarioMIP/BCC/BCC-CSM2-MR/ssp245/r1i1p1f1/day/tasmax/gn/v20190318/tasmax_day_BCC-CSM2-MR_ssp245_r1i1p1f1_gn_20150101-20391231.nc"

purrr::map(16:nrow(file_list_cmip), function(m) {
  # future::plan(future::multisession, workers = 4)
  # furrr::future_map(1:nrow(file_list_cmip), function(m) {
  
  model <- file_list_cmip$source_id[m]
  var <- "hfss"
  member <- file_list_cmip$member_id[m]
  experiment <- file_list_cmip$experiment_id[m]
  grid_label <- file_list_cmip$grid_label[m]
  
  files <- file_list_cmip$file_list[[m]]
  
  message(paste0(".........", m, ".........."))
  
  outfile <- paste0("~/t-drop-trends/data/cmip/", var, "/", var, "_day_", model, "_", experiment, "_", member, "_", grid_label, "_20150101-21001231.nc")
  dir.create(dirname(outfile), showWarnings = FALSE, recursive = TRUE)
  
  # if (file.exists(outfile)) {
  #   return(outfile)
  # }
  
  write(paste0("processing ", basename(outfile)), file = "~/log", append = TRUE)
  message(paste0("processing ", basename(outfile)))
  
  if (experiment == "historical") {
    
    file_list <- data.table(path = files, purrr::map_df(files, extract_range)) |> 
      _[, let(grid = basename(dirname(dirname(path))))] |> 
      _[, merge := fifelse(inidate >= 1980 | enddate >= 2000, TRUE, FALSE)] |> 
      _[merge == TRUE & grid == unique(grid)[1]]
    
    temp <- cdo_mergetime(file_list$path) |> 
      cdo_seldate(startdate = "1980-01-01T00:00:00", enddate = "2000-12-31T23:00:00") |> 
      # cdo_sellevel("85000") |>
      # cdo_timmean() |> 
      cdo_remapbil(grid = grid) |> 
      cdo_execute(options = "-L")
    
  } else {
    file_list <- data.table(path = files, purrr::map_df(files, extract_range)) |> 
      _[, let(grid = basename(dirname(dirname(path))))] |> 
      _[, merge := fifelse(inidate >= 2060 | enddate >= 2100 , TRUE, FALSE)] |>
      _[merge == TRUE & grid == unique(grid)[1]]
    
    if (length(file_list$path) == 1) {
      # cdo_mergetime() |> 
      temp <- cdo_seldate(file_list$path, startdate = "2080-01-01T00:00:00", enddate = "2100-12-31T23:00:00") |> 
        # cdo_sellevel("85000") |>
        # cdo_timmean() |> 
        cdo_remapbil(grid = grid) |> 
        cdo_execute(options = "-L") 
    } else {
      temp <- cdo_mergetime(file_list$path) |> 
        cdo_seldate(startdate = "2080-01-01T00:00:00", enddate = "2100-12-31T23:00:00") |> 
        # cdo_sellevel("85000") |>
        # cdo_timmean() |> 
        cdo_remapbil(grid = grid) |> 
        cdo_execute(options = "-L")  
    }
  }
  
  dtemp_dx <- cdo_shiftx(temp, nshift = 1, cyclic = "cyclic") |> 
    cdo_sub(ifile2 = temp) |> 
    cdo_execute(paste0("/scratch/gb02/pc2687/data/cmip/grad/d", var, "-dx_day_", model, "_", experiment, ".nc"), options = "-L")
  
  dtemp_dy <- cdo_shifty(temp, nshift = 1) |> 
    cdo_sub(ifile2 = temp) |> 
    cdo_execute(paste0("/scratch/gb02/pc2687/data/cmip/grad/d", var, "-dy_day_", model, "_", experiment, ".nc"), options = "-L")
  
})


ta <- purrr::map(Sys.glob(here("data/cmip/tas/*hist*.nc")), function(f) {
  
  message(f)
  meta <- unglue::unglue(basename(f), patterns = c("{var}_day_{model}_{scenario}_{member}_{grid}_20150101-21001231.nc"))
  
  file_scenario <- Sys.glob(here(paste0("data/cmip/", meta[[1]][["var"]], "/", 
                                        meta[[1]][["var"]], "_day_", meta[[1]][["model"]], "_ssp585*")))
  
  var <- meta[[1]][["var"]]
  
  ReadNetCDF(f, vars = c(hist = var)) |> 
    _[, let(ssp585 = ReadNetCDF(file_scenario, vars = c(ssp585 = var), out = "vector")[[1]])] |> 
    # ReadNetCDF(f, vars = c(hist = var), subset = list(plev = 85000)) |> 
    #   _[, let(ssp585 = ReadNetCDF(file_scenario, vars = c(ssp585 = var), subset = list(plev = 85000), out = "vector")[[1]])] |> 
    _[, let(model = meta[[1]][["model"]],
            lat = round(lat, digits = 8))] |> 
    _[]
  
}) |> rbindlist() |> 
  _[, let(dt_dy_hist = metR:::.derv(hist, lat)*(180/pi/6371000),
          dt_dy_ssp = metR:::.derv(ssp585, lat)*(180/pi/6371000)), by = .(lon)] |> 
  _[, let(dt_dx_hist = metR:::.derv(hist, lon)*(180/pi/(6371000*cos(lat*pi/180))),
          dt_dx_ssp = metR:::.derv(ssp585, lon)*(180/pi/(6371000*cos(lat*pi/180)))), by = .(lat)]

dt <- purrr::map(Sys.glob(here("data/cmip/eke_mean/*hist*mean.nc")), function(f) {
  
  message(f)
  meta <- unglue::unglue(basename(f), patterns = c("{var}_{model}_{scenario}_mean.nc", "{var}_{model}_{scenario}_mean.nc"))
  
  if (meta[[1]][["var"]] == "eke") {
    var <- "ua"
  } else {
    var <- "tasmax"
  }
  file_scenario <- here(paste0("data/cmip/eke_mean/", meta[[1]][["var"]], "_", meta[[1]][["model"]], "_ssp585_mean.nc"))
  
  ReadNetCDF(f, vars = c(hist = var)) |> 
    _[, let(ssp585 = ReadNetCDF(file_scenario, vars = c(ssp585 = var), out = "vector")[[1]])] |> 
    _[, let(model = meta[[1]][["model"]],
            var = meta[[1]][["var"]],
            change = ssp585 / hist,
            diff = ssp585 - hist,
            lat = round(lat, digits = 8),
            plev = NULL)] |> 
    _[]
  
}) |> rbindlist() |> 
  _[, let(land = MaskLand(lon, lat),
          region = fcase(lat %between% c(25, 65), "NH",
                         lat %between% c(-65, -25), "SH",
                         default = NA))] |> 
  _[, let(land = factor(land, labels = c("Sea", "Land")))]


ta |>
  _[, .(dt_dy_ssp = mean(dt_dy_ssp, na.rm = TRUE),
        dt_dy_hist = mean(dt_dy_hist, na.rm = TRUE)), by = .(lon, lat)] |> 
  # _[plev == 85000] |>
  _[, let(diff = dt_dy_ssp - dt_dy_hist)] |> 
  _[, let(diff = fifelse(lat > 0, -diff, diff))] |>
  ggplot(aes(ConvertLongitude(lon), lat)) +
  geom_contour_fill(aes(z = diff, fill = after_stat(level)),
                    breaks = c(-Inf, seq(-5e-6, 5e-6, 0.5e-6), Inf)) +
  scale_fill_divergent_discretised(guide = guide_colorbar(barheight = 0.5),
                                   labels = function(x) JumpBy(x, 2, fill = "")) +
  geom_sf(data = global_map, fill = NA, inherit.aes = FALSE) +
  ggnewscale::new_scale_fill() +
  geom_contour_fill(data = topo, aes(ConvertLongitude(lon), lat, z = topo), breaks = c(1300, Inf), fill = "grey90") +
  coord_sf(expand = FALSE) +
  # facet_wrap(~plev) +
  labs(x = NULL, y = NULL, fill = "gradT") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        legend.frame = ggplot2::element_rect(color = "black", linewidth = 0.4),
        legend.key.width = grid::unit(1, 'null'))

ta[round(lat, 7) %in% c(24.1120251, 64.4853989, -24.1120251, -64.4853989)] |> 
  dcast(lon + model ~ lat, value.var = c("hist", "ssp585")) |> 
  _[, let(dt_dy.NH = (ssp585_64.48539887 - ssp585_24.1120251 - (hist_64.48539887 - hist_24.1120251))/4452760,
          dt_dy.SH = (`ssp585_-24.1120251` - `ssp585_-64.48539887` - (`hist_-24.1120251` - `hist_-64.48539887`))/4452760)] |> 
  melt(measure.vars = c("dt_dy.NH", "dt_dy.SH")) |> 
  tidyr::separate_wider_delim(variable, delim = ".", names = c("extra", "region")) |> 
  setDT() |> 
  _[, .(lon, model, region, value)] |> 
  _[, .(mean_dt_dy = mean(value)), by = .(model, region)] |> 
  _[, let(mean_dt_dy = fifelse(region == "NH", -mean_dt_dy, mean_dt_dy))] |> 
  _[dt[var == "deltap97.5", .(mean_dt = mean(diff)), by = .(region, model)], on = .NATURAL] |> 
  na.omit() |> 
  ggplot(aes(mean_dt_dy, mean_dt)) +
  geom_point(aes(color = region), size = 2) +
  geom_smooth(aes(colour = region), method = "lm", se = FALSE) +
  scale_color_manual(values = c("cyan4", "orange")) +
  labs(x = latex2exp::TeX("$\\nabla _{y} T$ change"), y = latex2exp::TeX("$\\Delta T$ frequency change (days/year)"),
       color = NULL, shape = NULL) +
  theme_minimal() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.83, 0.08),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "#fbfbfb", color = "#fbfbfb"),
        legend.margin = margin(-3, 0, 0, 0, unit = "mm"),
        panel.background = element_rect(fill = "#fbfbfb", color = NA))


ta |> 
  # _[plev == 85000] |>
  _[, let(land = MaskLand(lon, lat),
          region = fcase(lat %between% c(25, 65), "NH",
                         lat %between% c(-65, -25), "SH",
                         default = NA))] |> 
  _[, let(land = factor(land, labels = c("Sea", "Land")))] |> 
  _[, .(mean_dt_dy = mean(dt_dy_ssp - dt_dy_hist, na.rm = TRUE)), by = .(region, model)] |>
  _[, let(mean_dt_dy = fifelse(region == "NH", -mean_dt_dy, mean_dt_dy))] |> 
  _[dt[var == "deltap97.5", .(mean_dt = mean(diff)), by = .(region, model)], on = .NATURAL] |> 
  na.omit() |> 
  ggplot(aes(mean_dt_dy, mean_dt)) +
  geom_point(aes(color = region), size = 2) +
  geom_smooth(aes(colour = region), method = "lm", se = FALSE) +
  scale_color_manual(values = c("cyan4", "orange")) +
  labs(x = latex2exp::TeX("$\\nabla _{y} T$ change"), y = latex2exp::TeX("$\\Delta T$ frequency change (days/year)"),
       color = NULL, shape = NULL) +
  theme_minimal() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.83, 0.08),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "#fbfbfb", color = "#fbfbfb"),
        legend.margin = margin(-3, 0, 0, 0, unit = "mm"),
        panel.background = element_rect(fill = "#fbfbfb", color = NA))


ta |> 
  _[plev == 85000] |>
  _[, let(land = MaskLand(lon, lat),
          region = fcase(lat %between% c(25, 65), "NH",
                         lat %between% c(-65, -25), "SH",
                         default = NA))] |> 
  _[, let(land = factor(land, labels = c("Sea", "Land")))] |> 
  _[] |> 
  _[, .(mean_dt_dy = mean(dt_dy_ssp - dt_dy_hist, na.rm = TRUE)), by = .(lat, land, model)] |> 
  _[, let(mean_dt_dy = fifelse(lat > 0, -mean_dt_dy, mean_dt_dy))] |> 
  _[dt |> 
      _[(model %in% models_in_gadi) & is.finite(change)] |>
      dcast(lat + lon + model ~ var, value.var = "diff") |> 
      _[, let(land = MaskLand(lon, lat))] |> 
      _[, let(land = factor(land, labels = c("Sea", "Land")))] |> 
      _[, .(mean_dt = mean(deltap97.5-1, na.rm = TRUE)), by = .(lat, land)], on = .NATURAL] |> 
  na.omit() |> 
  ggplot(aes(mean_dt_dy*1e7, lat)) +
  geom_hline(yintercept = 0, color = "grey80") +
  geom_vline(xintercept = 0, color = "grey80") +
  geom_path(aes(group = model), color = "grey70", alpha = 0.7) +
  stat_summary(orientation = "y", geom = "line", aes(color = "mean_dt_dy")) +
  geom_path(aes(x = mean_dt, color = "DeltaT")) +
  # scale_color_manual(values = c("DeltaT" = "red4", "mean_dt_dy" = "black"),
  #                    labels = c(latex2exp::TeX("$\\Delta T$"), "mean_dt_dy")) +
  # geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
  #           fill = "#fbfbfb",
  #           data = annotation,
  #           inherit.aes = FALSE) +
  annotate(geom = "rect", ymin = c(-90, -20, 90), ymax = c(-65, 20, 65), xmin = -Inf, xmax = Inf,
           fill = "grey93", alpha = 0.7) +
  scale_y_latitude(breaks = seq(-80, 80 , 20)) +
  facet_wrap(~land) +
  coord_cartesian(xlim = c(-15, 15))


topo <- rcdo::cdo_topo(grid = grid) |> 
  # cdo_remapbil(grid = grid) |> 
  rcdo::cdo_execute(options = "-f nc") |> 
  ReadNetCDF(vars = "topo") |> 
  _[, let(lat = round(lat, digits = 8),
          lon = round(lon, digits = 3))] |>
  _[, let(dtopo_dy = metR:::.derv(topo, lat)*(180/pi/6371000)), by = .(lon)] |> 
  _[, let(dtopo_dx = metR:::.derv(topo, lon)*(180/pi/(6371000*cos(lat*pi/180)))), by = .(lat)] |> 
  _[, let(dtopo_dx = dtopo_dx/(sqrt(dtopo_dx^2 + dtopo_dy^2)),
          dtopo_dy = dtopo_dy/(sqrt(dtopo_dx^2 + dtopo_dy^2))) ]

ta |> 
  _[, .(dt_dy = mean(dt_dy_ssp - dt_dy_hist, na.rm = TRUE),
        dt_dx = mean(dt_dx_ssp - dt_dx_hist, na.rm = TRUE)), by = .(lon, lat)] |> 
  _[, let(lat = round(lat, digits = 8),
          lon = round(lon, digits = 3))] |>
  _[topo, on = .NATURAL] |> 
  _[, let(prod = dt_dx*dtopo_dy + dt_dy*dtopo_dx)] |> 
  # ggplot(aes(prod)) +
  # geom_histogram()
  ggplot(aes(ConvertLongitude(lon), lat)) +
  geom_contour_fill(aes(z = prod), 
                    breaks = seq(-5e-6, 5e-6, length.out = 40)) +
  scale_fill_divergent() +
  # scale_fill_divergent_discretised(guide = guide_colorbar(barheight = 0.5),
  #                                  labels = function(x) JumpBy(x, 2, fill = "")) +
  geom_sf(data = global_map, fill = NA, inherit.aes = FALSE) +
  coord_sf(expand = FALSE) +
  # facet_wrap(~plev) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        legend.frame = ggplot2::element_rect(color = "black", linewidth = 0.4),
        legend.key.width = grid::unit(1, 'null'))


topo |> 
  ggplot(aes(ConvertLongitude(lon), lat)) +
  # geom_point(aes(topo)) +
  geom_arrow(aes(dx = dtopo_dx, dy = dtopo_dy), skip = 4) +
  scale_mag() +
  # geom_contour_fill(aes(z = dtopo_dy, fill = after_stat(level))) +
  # scale_fill_divergent_discretised(guide = guide_colorbar(barheight = 0.5),
  #                                  labels = function(x) JumpBy(x, 2, fill = "")) +
  geom_sf(data = global_map, fill = NA, inherit.aes = FALSE, colour = "red") +
  coord_sf(expand = FALSE) +
  # facet_wrap(~plev) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        legend.frame = ggplot2::element_rect(color = "black", linewidth = 0.4),
        legend.key.width = grid::unit(1, 'null'))
