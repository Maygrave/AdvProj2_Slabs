#Step3.1 - Reducing data based on identifiable slabs in ident mat

#Importing Libraries
rm(list=ls())
library(readxl)
library(tidyverse)
library(doBy)
library(caret)
library(randomForest)
library(data.table)

#Loading Data
load("../Data/df_BZA.Rdata") 
load("../Data/stage8_ident.Rdata")

#Merge w/ V_COILS_BZA & filter by ident.all.df
df_BZA <- df_BZA %>%
  dplyr::inner_join(ident.all.df, by = c("CoilID" = "SID")) %>%
  dplyr::select(MAT_IDENT, CHARGEN_NR, VORBRAMME, CoilID, lTileID, everything()
  )

#Compute Descriptives
df_BZA_long <- df_BZA %>%
  tidyr::gather(key=length_attr, value=measurement, -c(CHARGEN_NR, VORBRAMME, MAT_IDENT, CoilID, lTileID))
df_BZA_desc <- df_BZA_long %>% 
  dplyr::group_by(length_attr) %>% 
  dplyr::summarise(
    count = n(),
    unique = length(unique(measurement)), 
    na = sum(is.na(measurement)), 
    mean = mean(measurement, na.rm=TRUE), 
    sd = sd(measurement, na.rm = TRUE),
    min = min(measurement, na.rm=TRUE), 
    max=max(measurement, na.rm=TRUE), 
    N=sum(!is.na(measurement))
  )
df_BZA_desc

p <- ggplot(data = df_BZA_long, aes(x = measurement)) + geom_histogram() + facet_wrap(~length_attr, scales = "free") 
p 


#Back to wide
df_BZA <- df_BZA_long %>%
  tidyr::spread(key = length_attr, value = measurement)

#Save lines
save(df_BZA, df_BZA_desc, file="../anna_data/anna_df_BZA2.Rdata")

#Validating data
rm(list=ls())
load("../anna_data/anna_df_BZA2.Rdata")
A_df_BZA <- df_BZA
A_df_BZA_DESC <- df_BZA_desc

rm(df_BZA, df_BZA_desc)

#Load wilhelms data
load("../Data/df_BZA2.Rdata")

anti_join(A_df_BZA, df_BZA)
#good
anti_join(A_df_BZA_DESC, df_BZA_desc)
#good