library(rcdo)
library(lubridate)
library(data.table)



file_list_ta <- Sys.glob("/scratch/gb02/pc2687/data/cmip/grad/dta-dx*")

purrr::map(file_list_ta, function(fx_ta) {

  message(paste0("Starting ", basename(fx_ta))) 
  meta <- unglue::unglue(basename(fx_ta), "{var}_day_{model}_{experiment}.nc")
  output_q <- paste0("/scratch/gb02/pc2687/data/cmip/grad/dQ-dy_day_", meta[[1]][["model"]], "_", meta[[1]][["experiment"]], ".nc")
  
  if (file.exists(output_q)) {
    
    message(paste0("Skip ", basename(fx_ta)))
    return(output_q)
  }
  
  fy_ta <- paste0("/scratch/gb02/pc2687/data/cmip/grad/dta-dy_day_", meta[[1]][["model"]], "_", meta[[1]][["experiment"]], ".nc")
  fx_hfss <- paste0("/scratch/gb02/pc2687/data/cmip/grad/dhfss-dx_day_", meta[[1]][["model"]], "_", meta[[1]][["experiment"]], ".nc")
  fy_hfss <- paste0("/scratch/gb02/pc2687/data/cmip/grad/dhfss-dy_day_", meta[[1]][["model"]], "_", meta[[1]][["experiment"]], ".nc")
  
  ## Module of temperature gradiente
  output_mod <- paste0("/scratch/gb02/pc2687/data/cmip/grad/modta_day_", meta[[1]][["model"]], "_", meta[[1]][["experiment"]], ".nc")
  
  mod_ta <- cdo_add(cdo_sqr(fx_ta), cdo_sqr(fy_ta)) |> 
    cdo_sqrt() |> 
    cdo_execute(output_mod, options = "-L")
  
  ## surface sentistive flux 
  
  cdo_add(cdo_mul(fx_hfss, fx_ta), cdo_mul(fy_hfss, fy_ta)) |> 
    cdo_div(mod_ta) |> 
    cdo_execute(output_q, options = "-L")
  
})

## Module of temperature gradiente
## 

file_list_q <- Sys.glob("/scratch/gb02/pc2687/data/cmip/grad/dQ-dy*")

purrr::map(file_list_q, function(f) {
  
  message(paste0("Starting ", basename(f))) 
  meta <- unglue::unglue(basename(f), "{var}_day_{model}_{experiment}.nc")
  output_q <- paste0("/g/data/gb02/pc2687/cf/data/cmip/dq/dQ-dy_", meta[[1]][["model"]], "_", meta[[1]][["experiment"]], "_mean.nc")
  
  if (file.exists(output_q)) {
    
    message(paste0("Skip ", basename(fx_ta)))
    return(output_q)
  }
  
  cdo_timmean(f) |> 
    cdo_execute(output_q, options = "-L")
  
})


# Q_r <- ReadNetCDF(fx_hfss, vars = c(dhfss_dx = "hfss"), subset = list(time = "1980-01-01")) |> 
#   _[, let(dhfss_dy = ReadNetCDF(fy_hfss, vars = "hfss", subset = list(time = "1980-01-01"), out = "vector")[[1]],
#           dta_dx = ReadNetCDF(fx_ta, vars = "ta", subset = list(time = "1980-01-01"), out = "vector")[[1]],
#           dta_dy = ReadNetCDF(fy_ta, vars = "ta", subset = list(time = "1980-01-01"), out = "vector")[[1]],
#           mod_ta = ReadNetCDF(a, vars = "ta", subset = list(time = "1980-01-01"), out = "vector")[[1]])]
# 
# Q_r |> 
#   _[, let(q_r = (dhfss_dx * dta_dx + dhfss_dy * dta_dy)/mod_ta)] |> 
#   ggplot(aes(lon, lat)) + 
#   geom_point(aes(color = q_r))
# 
# Q_cdo <- ReadNetCDF(b, vars = c(q_cdo = "hfss"), subset = list(time = "1980-01-01"))
# Q_cdo |> 
#   ggplot(aes(lon, lat)) + 
#   geom_point(aes(color = q_cdo))
# 
# Q_cdo[Q_r, on = .NATURAL] |> 
#   ggplot(aes(lon, lat)) + 
#   geom_point(aes(color = q_cdo - q_r))
