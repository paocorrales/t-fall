library(rcmip6)
library(data.table)
library(lubridate)
purrr::walk(Sys.glob(here::here("R/*")), source)

variables  <- c("tas", "tasmax")

query <- list(
  project     = "CMIP6",
  type        = "Dataset",
  latest      = "true",
  query       = "*",
  activity_id = "ScenarioMIP",
  frequency   = "day",
  variable_id = "tasmax",
  replica     = "false"
)

results <- cmip_search(query)

results_simply <- results |>
  # cmip_filter_replicas() |> 
  cmip_simplify() |>   
  # subset(, select = -full_info) |> 
  _[experiment_id %in% c("ssp370", "ssp126", "ssp245", "ssp585") & table_id == "day"]

models <- results_simply[, .N, by = .(experiment_id, source_id)] |> 
  dcast(source_id ~ experiment_id) |> 
  _[, let(complete = ssp126 + ssp245 + ssp370 + ssp585)] |> 
  _[!is.na(complete), source_id]

a <- results_simply[source_id %in% models] |> 
  _[, c("r", "i", "p", "f") := unglue::unglue_data(member_id, patterns = "r{r}i{i}p{p}f{f}")] |> 
  _[, .SD[r == min(r) & i == min(i) & p == min(p) & f == min(f)], by = .(source_id, experiment_id)] |> 
  _[, let(path = fifelse(stringr::str_detect(source_id, "ACCESS"), 
                         paste0("/g/data/fs38/publications/CMIP6/", activity_drs, "/", institution_id, "/",
                                source_id, "/",
                                experiment_id, "/",
                                member_id, "/day/", 
                                variable_id, "/gn/latest/*"),
                        paste0("/g/data/oi10/replicas/CMIP6/", activity_drs, "/", institution_id, "/",
                        source_id, "/",
                        experiment_id, "/",
                        member_id, "/day/", 
                        variable_id, "/*/*/*")))] |> 
  unique(by = "path") |> 
  _[, let(file_list = list(Sys.glob(path))), by = path] |> 
  _[, let(n_files = length(file_list[[1]])), by = path] |> 
  _[, c("inidate", "enddate") := extract_range(file_list[[1]]), by = path] |> 
  _[, let(inidate = ymd(inidate),
          enddate = ymd(enddate))] |> 
  _[, let( in_gadi = !((inidate > ymd(20150101) | is.na(inidate)) | (enddate < ymd(21001231) | is.na(enddate))))]


models_in_gadi <- a[in_gadi == TRUE, .N, by = .(experiment_id, source_id)] |>
  dcast(source_id ~ experiment_id) |> 
  _[, let(complete = ssp126 + ssp245 + ssp370 + ssp585)] |> 
  _[!is.na(complete), source_id]
  
files <- a[source_id %in% models_in_gadi]

readr::write_rds(files, "cmip_file_list.rds")

query <- list(
  project       = "CMIP6",
  type          = "Dataset",
  latest        = "true",
  replica       = "false",
  query         = "*",
  activity_id   = "CMIP",
  experiment_id = "historical",
  frequency     = "day",
  source_id     = models_in_gadi,
  variable_id   = "tasmax"
)

results <- cmip_search(query)

results_simply <- results |>
  # cmip_filter_replicas() |> 
  cmip_simplify() |> 
  _[, c("r", "i", "p", "f") := unglue::unglue_data(member_id, patterns = "r{r}i{i}p{p}f{f}")] |> 
  _[, .SD[r == min(r) & i == min(i) & p == min(p) & f == min(f)], by = .(source_id, experiment_id)] |> 
  _[, let(path = fifelse(stringr::str_detect(source_id, "ACCESS"), 
                         paste0("/g/data/fs38/publications/CMIP6/", activity_drs, "/", institution_id, "/",
                                source_id, "/",
                                experiment_id, "/",
                                member_id, "/day/", 
                                variable_id, "/gn/latest/*"),
                         paste0("/g/data/oi10/replicas/CMIP6/", activity_drs, "/", institution_id, "/",
                                source_id, "/",
                                experiment_id, "/",
                                member_id, "/day/", 
                                variable_id, "/*/*/*")))] |> 
  unique(by = "path") |> 
  _[, let(file_list = list(Sys.glob(path))), by = path] |> 
  _[, let(n_files = length(file_list[[1]])), by = path] |> 
  _[, c("inidate", "enddate") := extract_range(file_list[[1]]), by = path] |> 
  _[, let(inidate = ymd(inidate),
          enddate = ymd(enddate))] |> 
  _[, let( in_gadi = !((inidate > ymd(18500101) | is.na(inidate)) | (enddate < ymd(20141231) | is.na(enddate))))]

readr::write_rds(results_simply, "cmip_file_list_historical.rds")
