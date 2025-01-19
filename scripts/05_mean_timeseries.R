library(data.table)
library(lubridate)
library(rcdo)
library(here)

scenarios <- c("historical", "ssp126", "ssp245", "ssp370", "ssp585")

purrr::map(scenarios, function(s) {
  
  message(s)
  
  outfile <- here(paste0("data/cmip/series/serie_freq_deltap99_", s, "_100km.nc"))
  
  if (file.exists(outfile)) {
    return(outfile)
  }
  
  file_list <- Sys.glob(here(paste0("data/cmip/100km/deltap99*", s, "*")))
  
  cdo_ensmean(file_list) |> 
    cdo_execute(outfile, options = "-L")
  
})
