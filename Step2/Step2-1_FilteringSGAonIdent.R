#Step2 - Identifying newly reduced data set
#Part 2 of step 2

#Importing libraries
rm(list=ls())
library(readxl)
library(tidyverse)
library(doBy)
library(caret)
library(randomForest)
library(data.table)

#Loading the data
load("../anna_data/anna_data_SGA.Rdata")
load("../Data/stage8_ident.Rdata")

#flitering on ident.all.df 
df_SGA <- data_SGA %>%
  dplyr::inner_join(ident.all.df, by = c("CHARGEN_NR", "VORBRAMME"))

#Geting a logical order
df_SGA <- df_SGA %>%
  dplyr::select(CHARGEN_NR, VORBRAMME, MAT_IDENT, SID, RIEGELLAENGE, STRANGNUMMER, VORG_HAUPTAGGREGAT, everything()
  )

#Computing Descriptives
df_SGA_long <- df_SGA %>%
  tidyr::gather(key=length_attr, value=measurement, -c(CHARGEN_NR, VORBRAMME, MAT_IDENT, SID, RIEGELLAENGE, VORG_HAUPTAGGREGAT))

df_SGA_desc <- df_SGA_long %>% 
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
df_SGA_desc

#Viewing dist of vars
p <- ggplot(data = df_SGA_long, aes(x = measurement)) + geom_histogram() + facet_wrap(~length_attr, scales = "free") 
p 
#a few odd dists, worth double checking


#Saving data reduced through filtering
save(df_SGA, df_SGA_desc, file="../anna_data/anna_df_SGA.Rdata")

#Clear data files
rm(list=ls())

#~~Comparing w/ Wilhelm~~#

#load my data
load("../anna_data/anna_df_SGA.Rdata")
anna_df_SGA <- df_SGA
anna_df_SGA_desc <- df_SGA_desc
rm(df_SGA, df_SGA_desc)

#Load Wilhelm's Data
load("../Data/df_SGA.Rdata")

#Compare df_SGA
dplyr::anti_join(df_SGA, anna_df_SGA)
#Both 0 - Data frames match!

#compare df_SGA_desc
dplyr::anti_join(df_SGA_desc, anna_df_SGA_desc)
#Both 0 - Data Frames match!

#All data is accurately replicated!
