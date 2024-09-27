library(data.table)


stations <- readr::read_fwf("station_list.txt", skip = 4, guess_max = 5000) |> 
  setDT() |> 
  _[, X1 := as.numeric(X1)]

colnames(stations) <- c("site", "name", "lat", "lon", "start_month",
                        "start_year", "end_month", "end_year", "years",
                        "percentage", "aws")

stations[site == 86338]

station_number <- formatC(stations[lon %between% c(144.9816-1, 144.9816+1) & lat %between% c(-37.8255-1, -37.8255+1), site],
                          width = 6, format = "d", flag = "0")


mel <- purrr::map(Sys.glob(paste0("~/BoM_stations/sub-daily/1minute-rainfall_*/HD01D_Data_", station_number, "*")), 
                  function(f) {
                    
                    message(basename(f))
                    fread(f, skip = 1, 
                          col.names = c("hd", "station_number", "year", "month", "day", "hour", "minutes",
                                        "year_s", "month_s", "day_s", "hour_s", "minutes_s", "pp", "qc", "period", "rm")) |> 
                      _[, .(station_number, year, month, day, hour, minutes, pp, qc)] |> 
                      _[pp < 20 & !(qc %in% c("W", "I"))] |> 
                      _[, time_local := make_datetime(year, month, day, hour, minutes, tz = "Australia/Melbourne")] |> 
                      _[, time_local := time_local + hours(15)] |>  #Move day to accumulate between 9 AM and 9 AM
                      _[, let(pp_60 = frollsum(pp, n = 60, align = "center", na.rm = TRUE))] |> 
                      _[, .(pp_60_max = max(pp_60, na.rm = TRUE)), by = .(year(time_local), month(time_local), day(time_local), station_number)]  |> 
                      _[, date_local := make_date(year, month, day)] 
                  }) |> 
  rbindlist() 


readr::write_rds(mel, "daily_pprate.rds")

t_mel <- fread("acorn_sat_v2.5.0_daily_tmax/tmax.086338.daily.csv") |> 
  janitor::clean_names() |> 
  setnames("maximum_temperature_deg_c", "tmax") |> 
  _[, let(site_number = site_number[1],
          site_name = site_name[1])] |>
  _[!is.na(date)] |> 
  _[, site_name := NULL] |> 
  _[, let(delta_t = tmax - shift(tmax, n = 1)), by = site_number] |> 
  _[]


t_mel[mel, on = c("date" = "date_local")] |> 
  _[month %in% c(10, 11, 12, 1, 2, 3)] |> 
  _[, delta_td := cut_round(delta_t, breaks = seq(-30, 25, 2))] |> 
  _[, .(mean_pp = mean(pp_60_max, na.rm = TRUE)), by = .(delta_td)] |> 
  # _[, date := date] |> 
  # _[obs_vic[site_number == 86338], on = c("date")] |> 
  # _[ pp == 0 & pp_60_max != 0 | pp != 0 & pp_60_max == 0]
  ggplot(aes(delta_td, mean_pp)) +
  geom_col()

t_mel[mel, on = c("date" = "date_local")] |> 
  # _[month %in% c(10, 11, 12, 1, 2, 3)] |> 
  ggplot(aes(delta_t, pp_60_max)) +
  geom_point() +
  scale_y_log10() +
  geom_smooth() +
  facet_wrap(~month)


t_mel[mel, on = c("date" = "date_local")] |> 
  _[month %in% c(10, 11, 12, 1, 2, 3)] |>
  _[delta_t <= -10 & pp_60_max >= 10] |> 
  _[, .N, by = .(station_number)] |> 
  stations[i = _, on = c("site" = "station_number")] |> 
  ggplot(aes(lon, lat)) +
  geom_point(aes(color = N, size = N)) +
  scale_color_viridis_c()

t_mel[mel, on = c("date" = "date_local")] |> 
  _[month %in% c(10, 11, 12, 1, 2, 3)] |>
  _[delta_t <= -10 & pp_60_max >= 10] |> 
  unique(by = "date")


obs_vic |> 
  metadata[i = _, on = .NATURAL] |> 
  _[month %in% c(10, 11, 12, 1, 2, 3)] |> 
  _[, delta_td := cut_round(delta_t, breaks = seq(-30, 25, 5))] |> 
  _[, .(mean_pp = mean(pp, na.rm = TRUE)), by = .(station_name, delta_td)] |> 
  ggplot(aes(delta_td, mean_pp)) +
  geom_col() +
  # geom_point(alpha = 0.1, size = 0.5) +
  geom_vline(xintercept = 0, color = "steelblue") +
  geom_vline(xintercept = -10, color = "steelblue", linetype = 2) +
  facet_wrap(~station_name) +
  labs(x = "T fall", y = "Precipitation") +
  theme_minimal()









mel_roll |> 
  ggplot(aes(time, pp_30_max)) +
  geom_line() +
  geom_line(aes(y = pp_60_max), color = "orange")

obs_mel <- obs_vic[site_number == 86338 & year == 2007] |> 
  _[, time := make_date(year, month, day)]

obs_mel[mel_roll, on = .NATURAL] |> 
  # ggplot(aes(time, pp)) +
  ggplot(aes(delta_t, pp_30_max)) +
  geom_point() +
  geom_point(aes(y = pp_60_max), color = "orange")

geom_smooth()
geom_line() +
  geom_line(aes(y = pp_h), color = "orange")


mel |> 
  # _[, .(pp = sum(pp, na.rm = TRUE)), by = .(year(time), month(time), day(time))] |> 
  # _[, time2 := make_date(year, month, day)] |>
  _[year(time) == 2007 & month(time) == 1 & day(time) %in% c(21)] |> 
  ggplot(aes(time, pp)) +
  geom_point() +
  geom_point(data = mel_roll[year(time) == 2007 & month(time) == 1 & day(time) %in% c(21)], color =  "orange")

mel |> 
  _[, .(pp = sum(pp, na.rm = TRUE)), by = .(year(time), month(time), day(time))] |> 
  _[, time2 := make_date(year, month, day)] |>
  _[month(time2) == 11] |> 
  ggplot(aes(time2, pp)) +
  geom_line() +
  geom_line(data = obs_mel |> _[month(time) == 11] , aes(time, pp), color = "red")

mel |> 
  _[, .(pp = sum(pp, na.rm = TRUE)), by = .(year(time), month(time), day(time), hour(time))] |> 
  ggplot(aes(pp)) +
  geom_histogram() +
  scale_x_log10()


mel |> 
  _[, .(pp = sum(pp, na.rm = TRUE)), by = .(year(time), month(time), day(time), hour(time))] |> 
  _[, time := make_date(year, month, day)] |>
  _[, .(max_pp = max(pp, na.rm = TRUE)), by = .(time)] |> 
  ggplot(aes(time, max_pp)) +
  geom_line() +
  geom_line(data = obs_mel, aes(time, delta_t), color = "red")



obs_vic[site_number == 86338 & year == 2007] |> 
  ggplot(aes(make_date(year, month, day), pp)) +
  geom_line() +
  geom_line(data = mel |> 
              _[, time := make_datetime(year, month, day, hour, minutes)], aes(time, pp), color = "red")
