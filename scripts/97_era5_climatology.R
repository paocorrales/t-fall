## ERA5
## Front climatology using deltaT
### Gridded to 100km grid from CMIP6

grid <- "/g/data/oi10/replicas/CMIP6/ScenarioMIP/BCC/BCC-CSM2-MR/ssp245/r1i1p1f1/day/tasmax/gn/v20190318/tasmax_day_BCC-CSM2-MR_ssp245_r1i1p1f1_gn_20150101-20391231.nc"

cdo_remapbil("data/era5/daymax_2t.nc", grid = grid) |> 
  cdo_execute("data/era5/daymax_2t_100km.nc", option = "-L")

cdo_deltat("data/era5/daymax_2t_100km.nc") |> 
  cdo_shifttime(shiftValue = "-1days") |> 
  cdo_execute("data/era5/deltat_100km.nc", options = "-L")

ReadNetCDF("data/era5/deltat_100km.nc", vars = "t2m", subset = list(time = "1990-10-25")) |> 
  _[, let(land = MaskLand(lon, lat))] |> 
  _[, let(t2m = ifelse(land == TRUE, -10, -4))] |> 
  _[, let(land = NULL)] |> 
  metR:::WriteNetCDF(data = _, "~/t-fall/data/era5/threshold_l10s04.nc", vars = "t2m")

# cdo_le("data/era5/deltat_100km.nc", "data/era5/threshold_l10s04.nc") |> 
cdo_lec("data/era5/deltat_100km.nc", c = -10) |>
  cdo_yearsum() |> 
  cdo_execute("data/era5/deltat_l10s10.nc", options = "-L")

cdo_timstd1("data/era5/daymax_2t_100km.nc") |> 
  cdo_execute("data/era5/daymax_sd_100km.nc", options = "-L")

### Analysis

topo <- cdo_topo(grid = "r360x180") |> 
  cdo_execute(options = "-f nc") |> 
  ReadNetCDF()

deltat <- ReadNetCDF(here::here("data/era5/deltat_l10s10.nc"), vars = "t2m") 
# era5 <- ReadNetCDF(here::here("data/era5/deltat_th99_freq_era5.nc"), vars = "t2m")

deltat |> 
  _[, .(mean_freq = mean(t2m, na.rm = TRUE)), by = .(lon, lat)] |> 
  ggplot(aes(ConvertLongitude(lon), lat)) +
  geom_contour_fill(aes(z = mean_freq, fill = after_stat(level)),
                    breaks = c(seq(0.5, 8.5, 1), Inf)) +
  scale_fill_viridis_d(name = "Frequency\n(days/year)", guide = guide_colorsteps(barheight = 0.5), direction = -1) +
  ggnewscale::new_scale_fill() +
  geom_contour_fill(data = topo, aes(ConvertLongitude(lon), lat, z = topo), breaks = c(1300, Inf), fill = "grey90") +
  # geom_hline(yintercept = c(-60, -20, 20, 60)) +
  geom_point(x = 60, y = -45, color = "red", size = 0.5) +

  geom_sf(data = global_map, inherit.aes = FALSE, fill = NA, linewidth = 0.4) +
  coord_sf(expand = FALSE) +
  labs(x = NULL, y = NULL, fill = "deltaT",
       title = "Climatology of temperature drops (deltat < -10)",
       subtitle = "ERA5 1959-2001") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.frame = ggplot2::element_rect(color = "black", linewidth = 0.4),
        legend.key.width = grid::unit(1, 'null'))

sd_deltat <- ReadNetCDF(here::here("data/era5/deltat_sd_100km.nc"), vars = "t2m") 

sd_deltat |> 
  ggplot(aes(ConvertLongitude(lon), lat)) +
  geom_contour_fill(aes(z = t2m, fill = after_stat(level)), binwidth = 1) +
  scale_fill_viridis_d(name = "degrees", guide = guide_colorsteps(barheight = 0.5), direction = -1) +
  geom_sf(data = global_map, inherit.aes = FALSE, fill = NA, linewidth = 0.4) +
  coord_sf(expand = FALSE) +
  labs(x = NULL, y = NULL, fill = "deltaT",
       title = "Standar deviation of temperature drops",
       subtitle = "ERA5 1959-2001") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.frame = ggplot2::element_rect(color = "black", linewidth = 0.4),
        legend.key.width = grid::unit(1, 'null'))
