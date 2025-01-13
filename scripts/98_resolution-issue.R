file_list <- Sys.glob(here::here(paste0("data/cmip/delta10/delta10_year_", unique(file_list_cmip$source_id), "*")))

# delta10 <- purrr::map(file_list, function(f) {
# 
#   message(basename(f))
# 
#   meta <- unglue::unglue(basename(f), "delta10_year_{model}_{scenario}_{member}_{ini}-{end}.nc")
# 
#   a <- ReadNetCDF(f, vars = "tasmax") |>
#     _[, let(scenario = meta[[1]][["scenario"]],
#             model = meta[[1]][["model"]])] |>
#     _[,  let(time = as_date(time))]
# 
# 
# }) |>
#   rbindlist()
# 
# serie <- delta10[, .(mean_freq = mean(tasmax, na.rm = TRUE)), by = .(year(time), scenario, lat, lon)]
# 
# fwrite(serie, here::here("data/cmip/series/serie_freq_year_delta10_250km.csv"), append = TRUE)
# 
# ensmean <- delta10[, .(mean_freq = mean(tasmax, na.rm = TRUE)), by = .(time, scenario, lat, lon)]
# 
# fwrite(ensmean, here::here("data/cmip/series/delta10_year_enseman_250.csv"), append = TRUE)

clim250 <- fread(here("data/cmip/series/delta10_year_enseman_250km.csv")) |> 
  _[scenario != "historical"] |> 
  _[, .(mean_freq = mean(mean_freq)), by = .(lon, lat, scenario)] |> 
  _[, let(resolution = "250km")]

clim100 <- fread(here("data/cmip/series/delta10_year_enseman_100km.csv")) |> 
  _[scenario != "historical"] |> 
  _[, .(mean_freq = mean(mean_freq)), by = .(lon, lat, scenario)] |> 
  _[, let(resolution = "100km")]

climall <- fread(here("data/cmip/series/delta10_year_enseman.csv")) |> 
  _[scenario != "historical"] |> 
  _[, .(mean_freq = mean(mean_freq)), by = .(lon, lat, scenario)] |> 
  _[, let(resolution = "all models")]


rbind(clim100, clim250, climall) |> 
  _[scenario == "ssp370"] |> 
  ggplot(aes(ConvertLongitude(lon), lat )) +
  geom_contour_fill(aes(z = mean_freq, fill = after_stat(level)),
                    breaks = c(seq(0.5, 8.5, 1), Inf)) +
  # geom_contour(aes(z = mean_freq), breaks = 8, color = "red") +
  scale_fill_viridis_d(guide = guide_colorsteps(barheight = 0.5), direction = -1) +
  geom_sf(data = global_map, inherit.aes = FALSE, fill = NA, linewidth = 0.4) +
  facet_grid(scenario ~ resolution) +
  coord_sf(expand = FALSE) +
  labs(x = NULL, y = NULL, fill = "days/year",
       title = "Annual mean temperature drop frequency (deltat <-10)",
       subtitle = "CMIP models 2015-2100") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.frame = ggplot2::element_rect(color = "black", linewidth = 0.4),
        legend.key.width = grid::unit(1, 'null'))
