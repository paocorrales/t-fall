a <- ReadNetCDF("data/cmip/scenarios/deltat_day_ACCESS-CM2_ssp370_r1i1p1f1_gn_20150101-21001231.nc", vars = "tasmax",
           subset = list(list(lat = -38, lon = 145),
                         list(lat = -41, lon = 142)))

a |> 
  _[, let(point = fifelse(lon == 144, "Land", "Sea"))] |> 
  dcast(time ~ point, value.var = "tasmax") |> 
  _[, let(front = fifelse(Land <= -10, TRUE, FALSE))] |> 
  melt( measure.vars = c("Land", "Sea")) |> 
  _[] |> 
  ggplot(aes(value)) +
  geom_histogram(aes(fill = variable), binwidth = 0.5) +
  facet_wrap(~front)

a[year(time) == 2024 & month(time) == 1] |> 
  ggplot(aes(time, tasmax)) +
  geom_line(aes(color = point))

a |> 
  dcast(time ~ point, value.var = "tasmax") |> 
  _[, .(cor(Land, Sea))]
  ggplot(aes(Land, Sea)) +
  geom_point() 

  
### ERA5 climatology
### 
  global_map_pacific <- rnaturalearth::ne_coastline() |> 
    sf::st_shift_longitude()
  
era5 <- ReadNetCDF(here::here("data/deltat_th99_freq_era5.nc"), vars = "t2m")

era5[, .(mean_freq = mean(t2m, na.rm = TRUE)), by = .(lon, lat)] |> 
  ggplot(aes(lon, lat)) +
  geom_contour_fill(aes(z = mean_freq, fill = after_stat(level)),
                    breaks = c(seq(0.5, 8.5, 1), Inf)) +
  geom_contour(aes(z = mean_freq), breaks = 8, color = "red") +
  scale_fill_viridis_d(guide = guide_colorsteps(barheight = 0.5), direction = -1) +
  geom_sf(data = global_map_pacific, inherit.aes = FALSE, fill = NA, linewidth = 0.4) +
  coord_sf(expand = FALSE) +
  labs(x = NULL, y = NULL, fill = "days/year",
       title = "Annual mean temperature drop frequency (deltat <-10 Land & deltat <-4 Sea)",
       subtitle = "ERA5 1959-2001") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.frame = ggplot2::element_rect(color = "black", linewidth = 0.4),
        legend.key.width = grid::unit(1, 'null'))

### Percentiles

p975 <- ReadNetCDF(here::here("data/era5/deltat_p99_era5.nc"), vars = "t2m")

p975 |> 
  # copy() |> 
  # _[, land := MaskLand(lon, lat)] |> 
  # _[, t2m := t2m - mean(t2m), by = .(land)] |> 
  ggplot(aes(ConvertLongitude(lon), lat)) +
  geom_contour_fill(aes(z = t2m, fill = after_stat(level))) +
  geom_contour(aes(z = t2m), breaks = -8, color = "red") +
  geom_hline(yintercept = c(-60, -20, 20, 60)) +
  scale_fill_viridis_d(guide = guide_colorsteps(barheight = 0.5), direction = 1) +
  geom_sf(data = global_map, inherit.aes = FALSE, fill = NA, linewidth = 0.4) +
  coord_sf(expand = FALSE) +
  labs(x = NULL, y = NULL, fill = "deltaT",
       title = "99th percentile for temperature drops (deltat)",
       subtitle = "ERA5 1959-2001") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.frame = ggplot2::element_rect(color = "black", linewidth = 0.4),
        legend.key.width = grid::unit(1, 'null'))


p975 |> 
  copy() |>
  _[, let(land = MaskLand(lon, lat),
          region = fcase(lat %between% c(25, 55), "NH",
                         lat %between% c(-55, -25), "SH",
                         default = NA))] |>
  _[, .(mean = mean(t2m)), by = .(region, land) ]

p975 |> 
  copy() |>
  _[, let(land = MaskLand(lon, lat))] |> 
  _[, let(t2m = ifelse(land == TRUE, -10, -4))] |> 
  _[, let(land = NULL)] |> 
  metR:::WriteNetCDF(data = _, "~/t-fall/data/threshold_p99_era5.nc", vars = "t2m")

  

deltat <- ReadNetCDF(here::here("data/era5/deltat_era5.nc"), vars = "t2m") |> 
  _[, let(land = MaskLand(lon, lat))] |> 
  _[, let(th = ifelse(land == TRUE, -10, -4))] |> 
  _[, let(front = t2m <= th)]

deltat |> 
  _[, let(land = MaskLand(lon, lat))] |> 
  _[, let(th = ifelse(land == TRUE, -10, -4))] |> 
  _[, .(freq = sum(t2m <= th)), by = .(year(time), lon, lat)]

deltat <- ReadNetCDF(here::here("data/era5/deltat_th99_climatology_era5.nc"), vars = "t2m") 
era5 <- ReadNetCDF(here::here("data/era5/deltat_th99_freq_era5.nc"), vars = "t2m")

deltat |> 
  ggplot(aes(ConvertLongitude(lon), lat)) +
  geom_contour_fill(aes(z = t2m, fill = after_stat(level)), breaks = seq(-16, 0, 1)) +
  # geom_contour(aes(z = t2m), breaks = -8, color = "red") +
  geom_hline(yintercept = c(-60, -20, 20, 60)) +
  geom_point(x = 40, y = -40, color = "red") +
  scale_fill_viridis_d(guide = guide_colorsteps(barheight = 0.5), direction = 1) +
  geom_sf(data = global_map, inherit.aes = FALSE, fill = NA, linewidth = 0.4) +
  coord_sf(expand = FALSE) +
  labs(x = NULL, y = NULL, fill = "deltaT",
       title = "99th percentile for temperature drops (deltat)",
       subtitle = "ERA5 1959-2001") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.frame = ggplot2::element_rect(color = "black", linewidth = 0.4),
        legend.key.width = grid::unit(1, 'null'))


era5[, .(mean_freq = mean(t2m, na.rm = TRUE)), by = .(lon, lat)] |> 
  ggplot(aes(ConvertLongitude(lon), lat)) +
  geom_contour_fill(aes(z = mean_freq, fill = after_stat(level)),
                    breaks = c(seq(0.5, 30, 1), Inf)) +
  geom_point(x = 60, y = -45, color = "red") +
  # geom_contour(aes(z = mean_freq), breaks = 8, color = "red") +
  scale_fill_viridis_d(guide = guide_colorsteps(barheight = 0.5), direction = -1) +
  geom_sf(data = global_map, inherit.aes = FALSE, fill = NA, linewidth = 0.4) +
  coord_sf(expand = FALSE) +
  labs(x = NULL, y = NULL, fill = "days/year",
       title = "Annual mean temperature drop frequency (deltat <-10 Land & deltat <-4 Sea)",
       subtitle = "ERA5 1959-2001") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.frame = ggplot2::element_rect(color = "black", linewidth = 0.4),
        legend.key.width = grid::unit(1, 'null'))

era5[lon == 60 & lat %between% c(-45, -43)] |> 
  ggplot(aes(time, t2m)) +
  geom_line()


deltat[lon == 40 & lat %between% c(-40, -39)]

t2m_ori <- ReadNetCDF(here::here("data/era5/daymax_2t_regrided.nc"), vars = "t2m",
                      subset = list(list(lon = 60, lat = -45),
                                    list(lon = 260, lat = -45))) 

t2m_ori |> 
  _[, let(point = fifelse(lon == 60., "Indian", "Pacific"))] |> 
  _[, let(deltat = t2m - shift(t2m, n = 1)), by = point] |> 
  _[, let(front4 = deltat <= -4,
          front5 = deltat <= -5,
          front5.5 = deltat <= -5.5)]

t2m_ori |> 
  ggplot(aes(t2m)) +
  geom_density(aes(color = point, y = after_stat(scaled))) +
  # geom_vline(xintercept = c(-5, -4)) +
  theme_minimal()
  geom_line(aes(color = point))
