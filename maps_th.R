library(metR)
library(data.table)
library(lubridate)
library(ggplot2)

map <- rnaturalearth::ne_states(country = "Australia", returnclass = "sf")


events <- readr::read_rds("events_pprate10.rds") |> 
  _[id != as_date("2006-01-02")]

for (i in seq_len(nrow(events))) {

id_date <- events$id[i]

message(id_date)

date <- with_tz(events$round_time[i], "UTC")

dates <- seq(date - hours(3*6), date + hours(2*6), by = "6 hour")

mslp <- ReadNetCDF(paste0("/scratch/w40/pc2687/cf/msl_era5_oper_sfc_", format(id_date, "%Y-%m-%d"), ".nc"), vars = "msl",
                   subset = list(time = as.list(dates)))

gz <- ReadNetCDF(paste0("/scratch/w40/pc2687/cf/z_era5_oper_pl_", format(id_date, "%Y-%m-%d"), ".nc"), vars = "z",
                 subset = list(time = as.list(dates),
                               level = list(500, 1000))) |>
  dcast(time + latitude + longitude ~ level) |>
  _[, let(thickness = `500`-`1000`)] |>
  _[, .(latitude, time, longitude, thickness)]

# wind <- ReadNetCDF(paste0("/scratch/w40/pc2687/cf/10u_era5_oper_sfc_", format(id_date, "%Y-%m-%d"), ".nc"),
#                    vars = "u10",
#                    subset = list(time = as.list(dates))) |>
#   ReadNetCDF(paste0("/scratch/w40/pc2687/cf/10v_era5_oper_sfc_", format(id_date, "%Y-%m-%d"), ".nc"),
#              vars = "v10",
#              subset = list(time = as.list(dates)))[i = _, on = c("time", "latitude", "longitude")]

pp <- ReadNetCDF(paste0("/scratch/w40/pc2687/cf/tp_era5_oper_sfc_", format(id_date, "%Y-%m-%d"), ".nc"), 
                   vars = "tp",
                   subset = list(time = as.list(dates)))

field <- mslp[gz, on = .NATURAL] |> 
  # _[wind, on = .NATURAL] |>
  _[pp, on = .NATURAL]


field |> 
  _[, let(time = with_tz(time, tzone = "Australia/Melbourne"))] |> 
  ggplot(aes(longitude, latitude)) + 
  geom_sf(data = map, inherit.aes = FALSE, fill = "grey90", linewidth = 0.1) +
  geom_contour_fill(aes(z = tp*1000), breaks = c(seq(1, 25, 1), Inf)) +
  geom_contour2(aes(z = msl/100, label = after_stat(level)), color = "grey10", 
               breaks = seq(900, 1040, 4),
               linewidth = 0.2,
               label_size = 2.5,
               label_color = "grey10") +
  geom_contour2(aes(z = thickness/100, label = after_stat(level)), color = "red4",
                # breaks = seq(900, 1020, 2),
                linewidth = 0.3,
                label_size = 2.5,
                label_color = "red4",
                linetype = 2) +
  # geom_arrow(aes(angle = atan2(dlat(v10), dlon(u10, latitude))*180/pi,
  #                mag = Mag(v10, u10)), skip = 8, pivot = 0.5,
  #            size = 0.3, arrow.angle = 10, arrow.length = 0.4,
  #            color = "grey30") +
  # scale_mag(max_size = 0.5, guide = guide_vector()) +
  scale_fill_viridis_c(option = "G", direction  = -1, guide = guide_colorbar(barwidth = 0.5,
                                                                             barheigh = 25)) +
  guides(mag = guide_legend(override.aes = list(fill = NA))) +
  coord_sf(xlim = c(110, 160), ylim = c(-45, -5)) +
  facet_wrap(~time, ncol = 3) +
  labs(x = NULL, y = NULL, fill =  NULL, mag =  NULL,
       title = format(date, "%d %b %Y"),
       subtitle = "MSLP (black), Wind 10m (m/s), Precip (mm/h)") +
  theme_minimal(base_size = 10) +
  theme(legend.key = element_rect(fill = "white", color = "white"),
        plot.title.position = "plot")

ggsave(paste0("event_", format(date, "%Y-%m-%d"), "_th.png"), width = 15, height = 9, bg = "white")


}
# tp <- ReadNetCDF("/scratch/w40/pc2687/test2.nc") |> 
#   _[, time2 := with_tz(time, "Australia/Melbourne")]
# 
# 
# tp |>
#   _[day(time2) == 25, .(tp = sum(tp, na.rm = TRUE)), by = .(longitude, latitude)] |> 
#   # _[time2 == as_datetime("20091225180000", tz = "Australia/Melbourne")] |> 
#   ggplot(aes(longitude, latitude)) +
#   geom_contour_fill(aes(z = tp*1000)) +
#   scale_fill_viridis_c(option = "G", direction  = -1) +
#   geom_sf(data = map, inherit.aes = FALSE, fill = NA)
#  
