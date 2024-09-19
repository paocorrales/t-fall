# Read precipitation data from VIC stations

library(data.table)
library(janitor)
library(lubridate)

## wilsons Promontory
## 85096 

read_pp <- function() {

wilsons <- fread("data_pp/IDCJAC0009_085096_1800/IDCJAC0009_085096_1800_Data.csv",
                 col.names = c("product_code", "site_number", "year", "month", "day", "pp", "period", "qc")) |> 
  _[, .(site_number, year, month, day, pp)] |> 
  _[, date := make_date(year, month, day)]

## Sale
## 85072 + 85133

sale <- fread("data_pp/IDCJAC0009_085072_1800/IDCJAC0009_085072_1800_Data.csv",
                 col.names = c("product_code", "site_number", "year", "month", "day", "pp", "period", "qc")) |> 
  _[, .(site_number, year, month, day, pp)] |> 
  _[, date := make_date(year, month, day)]

sale2 <- fread("data_pp/IDCJAC0009_085133_1800/IDCJAC0009_085133_1800_Data.csv",
              col.names = c("product_code", "site_number", "year", "month", "day", "pp", "period", "qc")) |> 
  _[, .(site_number, year, month, day, pp)] |> 
  _[, date := make_date(year, month, day)]


sale <- rbind(sale[date >= ymd(19450801)], sale2) |> 
  _[, site_number := 85072]

## Rutherglen Research
## 82039 

rutherglen <- fread("data_pp/IDCJAC0009_082039_1800/IDCJAC0009_082039_1800_Data.csv",
                 col.names = c("product_code", "site_number", "year", "month", "day", "pp", "period", "qc")) |> 
  _[, .(site_number, year, month, day, pp)] |> 
  _[, date := make_date(year, month, day)]

## Orbost
## 84145 + 85133

orbost <- fread("data_pp/IDCJAC0009_084145_1800/IDCJAC0009_084145_1800_Data.csv",
              col.names = c("product_code", "site_number", "year", "month", "day", "pp", "period", "qc")) |> 
  _[, .(site_number, year, month, day, pp)] |> 
  _[, date := make_date(year, month, day)]

orbost2 <- fread("data_pp/IDCJAC0009_084030_1800/IDCJAC0009_084030_1800_Data.csv",
               col.names = c("product_code", "site_number", "year", "month", "day", "pp", "period", "qc")) |> 
  _[, .(site_number, year, month, day, pp)] |> 
  _[, date := make_date(year, month, day)]

orbost <- rbind(orbost, orbost2[date < ymd(20000101)]) |> 
  _[, site_number := 84145]

## Nhill 
## 78015 + 78031

nhill <- fread("data_pp/IDCJAC0009_078015_1800/IDCJAC0009_078015_1800_Data.csv",
                col.names = c("product_code", "site_number", "year", "month", "day", "pp", "period", "qc")) |> 
  _[, .(site_number, year, month, day, pp)] |> 
  _[, date := make_date(year, month, day)]

nhill2 <- fread("data_pp/IDCJAC0009_078031_1800/IDCJAC0009_078031_1800_Data.csv",
                 col.names = c("product_code", "site_number", "year", "month", "day", "pp", "period", "qc")) |> 
  _[, .(site_number, year, month, day, pp)] |> 
  _[, date := make_date(year, month, day)]

nhill <- rbind(nhill, nhill2[date < ymd(20030101)]) |> 
  _[, site_number := 78015]

## Mildura
## 76031 + 76077

mildura <- fread("data_pp/IDCJAC0009_076031_1800/IDCJAC0009_076031_1800_Data.csv",
               col.names = c("product_code", "site_number", "year", "month", "day", "pp", "period", "qc")) |> 
  _[, .(site_number, year, month, day, pp)] |> 
  _[, date := make_date(year, month, day)]

mildura2 <- fread("data_pp/IDCJAC0009_076077_1800/IDCJAC0009_076077_1800_Data.csv",
                col.names = c("product_code", "site_number", "year", "month", "day", "pp", "period", "qc")) |> 
  _[, .(site_number, year, month, day, pp)] |> 
  _[, date := make_date(year, month, day)]

mildura <- rbind(mildura, mildura2[date <= ymd(19451231)]) |> 
  _[, site_number := 76031]

## Melbourne
## 86338 + 86071

melbourne <- fread("data_pp/IDCJAC0009_086338_1800/IDCJAC0009_086338_1800_Data.csv",
                 col.names = c("product_code", "site_number", "year", "month", "day", "pp", "period", "qc")) |> 
  _[, .(site_number, year, month, day, pp)] |> 
  _[, date := make_date(year, month, day)]

melbourne2 <- fread("data_pp/IDCJAC0009_086071_1800/IDCJAC0009_086071_1800_Data.csv",
                  col.names = c("product_code", "site_number", "year", "month", "day", "pp", "period", "qc")) |> 
  _[, .(site_number, year, month, day, pp)] |> 
  _[, date := make_date(year, month, day)]

melbourne <- rbind(melbourne, melbourne2[date <= ymd(20121231)]) |> 
  _[, site_number := 86338]

## Laverton Raaf
## 87031

laverton <- fread("data_pp/IDCJAC0009_087031_1800/IDCJAC0009_087031_1800_Data.csv",
                 col.names = c("product_code", "site_number", "year", "month", "day", "pp", "period", "qc")) |> 
  _[, .(site_number, year, month, day, pp)] |> 
  _[, date := make_date(year, month, day)]

## Kerang
## 80023

kerang <- fread("data_pp/IDCJAC0009_080023_1800/IDCJAC0009_080023_1800_Data.csv",
                  col.names = c("product_code", "site_number", "year", "month", "day", "pp", "period", "qc")) |> 
  _[, .(site_number, year, month, day, pp)] |> 
  _[, date := make_date(year, month, day)]

return(rbind(wilsons, sale, rutherglen, orbost, nhill, mildura, melbourne, laverton, kerang))
}


            