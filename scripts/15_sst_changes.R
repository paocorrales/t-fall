library(data.table)
library(lubridate)
library(rcdo)
purrr::walk(Sys.glob(here::here("R/*")), source)

models_in_gadi <- c("CMCC-ESM2", "EC-Earth3", "INM-CM4-8", "INM-CM5-0", 
                    "MPI-ESM1-2-HR", "NorESM2-MM", "EC-Earth3-CC", "EC-Earth3-Veg",
                    "EC-Earth3-Veg-LR", "GFDL-CM4")

file_list_cmip <- readr::read_rds(here::here("data//cmip_file_list_tos.rds"))

# Merge and regrid files and calculate deltat

grid <- "/g/data/oi10/replicas/CMIP6/ScenarioMIP/BCC/BCC-CSM2-MR/ssp245/r1i1p1f1/day/tasmax/gn/v20190318/tasmax_day_BCC-CSM2-MR_ssp245_r1i1p1f1_gn_20150101-20391231.nc"

purrr::map(c(5, 16), function(m) {
  # future::plan(future::multisession, workers = 4)
  # furrr::future_map(1:nrow(file_list_cmip), function(m) {
  
  model <- file_list_cmip$source_id[m]
  var <- "tos"
  member <- file_list_cmip$member_id[m]
  experiment <- file_list_cmip$experiment_id[m]
  grid_label <- file_list_cmip$grid_label[m]
  
  files <- file_list_cmip$file_list[[m]]
  
  message(paste0(".........", m, ".........."))
  
  outfile <- paste0("/g/data/gb02/pc2687/cf/data/cmip/", var, "/", var, "_", model, "_", experiment, "_mean.nc")
  dir.create(dirname(outfile), showWarnings = FALSE, recursive = TRUE)
  
  if (file_list_cmip$n_files[m] == 0) {
    return(outfile)
  }
  
  write(paste0("processing ", basename(outfile)), file = "~/log", append = TRUE)
  message(paste0("processing ", basename(outfile)))
  
  if (experiment == "historical") {
    
    file_list <- data.table(path = files, purrr::map_df(files, extract_range)) |>
      _[, merge := fifelse(inidate >= 1970 | enddate >= 2000, TRUE, FALSE)] |> 
      _[merge == TRUE]
    
    message(file_list$path)
    
    cdo_mergetime(file_list$path) |> 
      cdo_seldate(startdate = "1980-01-01T00:00:00", enddate = "2000-12-31T23:00:00") |> 
      # cdo_sellevel("85000,70000,25000") |> 
      cdo_timmean() |> 
      cdo_remapnn(grid = grid) |> 
      cdo_execute(outfile, options = "-L")
    
  } else {
    file_list <- data.table(path = files, purrr::map_df(files, extract_range)) |> 
      _[, merge := fifelse(inidate >= 2075 | enddate >= 2100, TRUE, FALSE)] |> 
      _[merge == TRUE]
    
    message(file_list$path)
    
    cdo_mergetime(path) |> 
      cdo_seldate(startdate = "2080-01-01T00:00:00", enddate = "2100-12-31T23:00:00") |> 
      # cdo_sellevel("85000,70000,25000") |> 
      cdo_timmean() |> 
      cdo_remapnn(grid = grid) |> 
      cdo_execute(outfile, options = "-L")  
  }
  
})


read_grad <- function(file_list) {

purrr::map(file_list, function(f) {
  
  message(f)
  meta <- unglue::unglue(basename(f), patterns = c("{var}_{model}_historical_mean.nc"))
  
  file_scenario <- Sys.glob(here(paste0("data/", meta[[1]][["var"]], "/", 
                                        meta[[1]][["var"]], "_", meta[[1]][["model"]], "_ssp585*")))
  
  var <- meta[[1]][["var"]]
  
  ReadNetCDF(f, vars = c(hist = var)) |> 
    _[, let(ssp585 = ReadNetCDF(file_scenario, vars = c(ssp585 = var), out = "vector")[[1]])] |> 
    _[, let(model = meta[[1]][["model"]],
            lat = round(lat, digits = 8),
            var = var,
            dt_dy_hist = metR:::.derv(hist, lat)*(180/pi/6371000),
            dt_dy_ssp585 = metR:::.derv(ssp585, lat)*(180/pi/6371000)), by = .(lon)] |> 
    _[, let(cos_lat = cos(lat*180/pi))] |> 
    _[, let(dt_dx_hist = metR:::.derv(hist, lon)*(180/pi/6371000)*cos_lat,
            dt_dx_ssp585 = metR:::.derv(ssp585, lon)*(180/pi/6371000)*cos_lat), by = .(lat)] 
  
}) |> rbindlist() 

}

mask <- tos[var == "tos" & model == "NorESM2-MM", .(lon, lat, hist)] |> 
  _[, let(land = fifelse(!is.na(hist), "Land", "Sea"))] |> 
  _[, .(lon, lat, land)]

tos <- read_grad(Sys.glob(here("data/tos/*hist*mean.nc"))) |> 
  mask[x = _, on = .NATURAL]
tas <- read_grad(Sys.glob(here("data/tas/*hist*mean.nc"))) |> 
  mask[x = _, on = .NATURAL]

dq <- tos[tas, on = c("lon", "lat", "model", "land")] |> 
  _[, let(mod_dtas_hist = sqrt(i.dt_dx_hist^2 + i.dt_dy_hist^2),
          mod_dtas_ssp =  sqrt(i.dt_dx_ssp585^2 + i.dt_dy_ssp585^2))] |> 
  _[, let(dq_dy_hist = (dt_dx_hist*i.dt_dx_hist + dt_dy_hist*i.dt_dy_hist)/mod_dtas_hist,
          dq_dy_ssp = (dt_dx_ssp585*i.dt_dx_ssp585 + dt_dy_ssp585*i.dt_dy_ssp585)/mod_dtas_ssp)] |> 
  _[, let(diff = dq_dy_ssp - dq_dy_hist)]



dq |> 
  # _[vadqr == "tos"] |> 
  _[, .(mean_dq = mean(diff, na.rm = TRUE),
        mean_tos = mean(sqrt(dt_dx_ssp585^2 + dt_dy_ssp585^2) - sqrt(dt_dx_hist^2 + dt_dy_hist^2), na.rm = TRUE)), by = .(lon, lat)] |>
  ggplot(aes(ConvertLongitude(lon), lat)) +
  geom_contour_fill(aes(z = (mean_tos - mean_dq)*10000, fill = after_stat(level)),
                    breaks = c(-Inf, seq(-0.01, 0.01, 0.001), Inf)) +
  scale_fill_divergent_discretised(name = "K/km/year", mid = "#f7f7f7",
                                   guide = guide_colorbar(barheight = 0.5),
                                   labels = function(x) JumpBy(x, 2, fill = "")) +
  geom_sf(data = global_map, inherit.aes = FALSE, fill = "red", linewidth = 0.2) +
  # geom_point(data = sig[is.cross(lon, lat, skip = 1) & abs(sig) >= 9], alpha = 0.3, shape = 4, size = 0.1) +
  # ggnewscale::new_scale_fill() +
  # geom_contour_fill(data = topo, aes(ConvertLongitude(lon), lat, z = topo), breaks = c(1300, Inf), fill = "grey90") +
  coord_sf(expand = FALSE) +
  labs(x = NULL, y = NULL, fill = "K/km/year") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        legend.frame = ggplot2::element_rect(color = "black", linewidth = 0.4),
        legend.key.width = grid::unit(1, 'null'),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        plot.margin = unit(c(0, -0.4 , 0 , 0), units = "cm")) 

# a <- tos[, .(lon, lat, model, var, dt_dy_hist, dt_dy_ssp585, dt_dx_hist, dt_dx_ssp585)] |> 
#   melt(measure.vars = c("dt_dy_hist", "dt_dy_ssp585", "dt_dx_hist", "dt_dx_ssp585"), 
#        id.vars = c("lat", "lon", "model", "var")) |> 
#   _[, let(variable = paste0(tas, "-", variable))] |> 
#   dcast(lon + lat + model ~ variable)

tos |> 
  _[var == "tos"] |> 
  _[, .(mean_tos = mean(diff, na.rm = TRUE)), by = .(lon, lat)] |>
  ggplot(aes(ConvertLongitude(lon), lat)) +
  geom_contour_fill(aes(z = mean_tos*1000, fill = after_stat(level)),
                    breaks = c(-Inf, seq(-0.01, 0.01, 0.001), Inf)) +
  scale_fill_divergent_discretised(name = "K/km/year", mid = "#f7f7f7",
                                   guide = guide_colorbar(barheight = 0.5),
                                   labels = function(x) JumpBy(x, 2, fill = "")) +
  geom_sf(data = global_map, inherit.aes = FALSE, fill = "red", linewidth = 0.2) +
  # geom_point(data = sig[is.cross(lon, lat, skip = 1) & abs(sig) >= 9], alpha = 0.3, shape = 4, size = 0.1) +
  # ggnewscale::new_scale_fill() +
  # geom_contour_fill(data = topo, aes(ConvertLongitude(lon), lat, z = topo), breaks = c(1300, Inf), fill = "grey90") +
  coord_sf(expand = FALSE) +
  labs(x = NULL, y = NULL, fill = "K/km/year") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        legend.frame = ggplot2::element_rect(color = "black", linewidth = 0.4),
        legend.key.width = grid::unit(1, 'null'),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        plot.margin = unit(c(0, -0.4 , 0 , 0), units = "cm")) 

tos |> 
  _[var == "tos" ] |> 
  # _[, .(mean_tos = mean(diff, na.rm = TRUE)), by = .(lon, lat, land)] |> 
  ggplot(aes(ConvertLongitude(lon), lat)) +
  geom_point(aes(color = ssp585), size = 1) +
  facet_wrap(~model) +
  geom_sf(data = global_map, inherit.aes = FALSE, fill = NA, linewidth = 0.2) 




change <- rbind(data, dq |> _[, let(change = NA, 
                                    var = "dq")] |>  _[, .(time, lat, lon, hist, ssp585, model, var, change, diff, land)] ) |> 
  
  dcast(lat + lon + model + land ~ var, value.var = "diff") |> 
  _[, let(tos = fifelse(land == "Land", NA, tos))] |>
  # _[, let(region = fifelse(lat %between% c(25, 65) | lat %between% c(-65, -25), "band", "outside"))] |>
  _[, let(region = fcase(lat %between% c(25, 65), "NH",
                         lat %between% c(-65, -25), "SH",
                         default = "outside"))] |>
  _[, .(mean_eke = weighted.mean(eke, w = cos(lat*pi/180), na.rm = TRUE),
        mean_dt_dy = weighted.mean(tas, w = cos(lat*pi/180), na.rm = TRUE),
        mean_deltat = weighted.mean(deltat, w = cos(lat*pi/180), na.rm = TRUE),
        mean_tos = weighted.mean(tos, w = cos(lat*pi/180), na.rm = TRUE),
        mean_dq = weighted.mean(dq, w = cos(lat*pi/180), na.rm = TRUE)), by = .(land, region, model)] 
  

change |> 
  _[mean_tos < 0.0025/1000 & land != "Land" & region != "outside"] |>
  ggplot(aes(mean_tos*1000, mean_deltat)) +
  geom_point(aes(color = region, shape = land), size = 2) +
  geom_smooth(color = "grey10", linetype = 2, method = "lm", se = FALSE) +
  scale_color_manual(values = c("cyan4", "orange")) +
  labs(x = latex2exp::TeX("$| \\nabla SST|$ change (K/km)"), y = latex2exp::TeX("$\\Delta T$ frequency change (days/year)"),
       color = NULL, shape = NULL) +
  # coord_cartesian(xlim = )
  theme_minimal() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.83, 0.08),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "#fbfbfb", color = "#fbfbfb"),
        legend.margin = margin(-3, 0, 0, 0, unit = "mm"),
        panel.background = element_rect(fill = "#fbfbfb", color = NA))


rbind(data, dq |> _[, let(change = NA, 
                          var = "dq")] |>  _[, .(time, lat, lon, hist, ssp585, model, var, change, diff, land)] ) |> 
  
  dcast(lat + lon + model + land ~ var, value.var = "diff") |> 
  _[!is.na(dq)] |> 
  _[, .(cor_dt_dq = cor(deltat, dq)), by = .(lon, lat)] |> 
  _[] |> 
  ggplot(aes(ConvertLongitude(lon), lat)) +
  geom_contour_fill(aes(z = cor_dt_dq, fill = after_stat(level))) +
  scale_fill_divergent_discretised(name = "K/km/year", mid = "#f7f7f7",
                                   guide = guide_colorbar(barheight = 0.5),
                                   labels = function(x) JumpBy(x, 2, fill = "")) +
  geom_sf(data = global_map, inherit.aes = FALSE, fill = "red", linewidth = 0.2) +
  # geom_point(data = sig[is.cross(lon, lat, skip = 1) & abs(sig) >= 9], alpha = 0.3, shape = 4, size = 0.1) +
  # ggnewscale::new_scale_fill() +
  # geom_contour_fill(data = topo, aes(ConvertLongitude(lon), lat, z = topo), breaks = c(1300, Inf), fill = "grey90") +
  coord_sf(expand = FALSE) +
  labs(x = NULL, y = NULL, fill = "K/km/year") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        legend.frame = ggplot2::element_rect(color = "black", linewidth = 0.4),
        legend.key.width = grid::unit(1, 'null'),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        plot.margin = unit(c(0, -0.4 , 0 , 0), units = "cm")) 


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
