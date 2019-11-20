#Step5 - The Final Merge *Dun dun duuuuun*

#Importing Libraries
rm(list = ls())
library(readxl)
library(tidyverse)
library(doBy)
library(caret)
library(randomForest)
library(data.table)

#Loading Data
load("../anna_data/anna_df_BZA2.Rdata")
load("../Data/stage8_ident.Rdata")
load("../anna_data/anna_nachVerschrottung_updated.Rdata")

#Merging Data
#then aggregating all by lTileID
TV_WBW1.wide.agg <- setDT(df_WBW_length)[, lapply(.SD, mean), by = c("MAT_IDENT","lTileID")]

SGA.agg <- setDT(df_SGA[,c(3,5:6,8:65)])[, lapply(.SD, mean), by = c("MAT_IDENT", "lTileID")]
SGA.VorgHA <- setDT(df_SGA[,c(3,7)])
SGA.VorgHA.agg <- unique(SGA.VorgHA)

SGA.agg <- SGA.agg %>%
  dplyr::left_join(SGA.VorgHA.agg, by=c("MAT_IDENT"))

#Dropping unneeded large table
rm(df_SGA)
rm(SGA.VorgHA)

#Merging
dataSGABZA <- df_BZA %>%
  dplyr::left_join(SGA.agg, by=c("MAT_IDENT", "lTileID")) %>%
  dplyr::select(MAT_IDENT, lTileID, CoilID, POSITION_X, RIEGELLAENGE, VORG_HAUPTAGGREGAT, Class.4, Class.14, Class.15, everything())

df <- dataSGABZA %>%
  dplyr::left_join(TV_WBW1.wide.agg, by=c("MAT_IDENT", "lTileID"))


#Saving DAta
save(df, file="../anna_data/anna_merged_data.Rdata")

#Validating data
rm(list = ls())
load("../anna_data/anna_merged_data.Rdata")

#Mydata
a_df <- df
rm(df)

#Wilehlm Data
load("../Data/stage8_merged_data.Rdata")

anti_join(a_df, df)
#MATCH!!!