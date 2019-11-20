#Step 4 - Adjusting for length and scrapping

#importing libraries
rm(list = ls())
library(readxl)
library(tidyverse)
library(doBy)
library(caret)
library(randomForest)
library(data.table)

#loading data
load("../Data/df_WBW1_length.Rdata")
load("../anna_data/anna_df_SGA_filtered_BZA.Rdata")
load("../Data/stage8_ident.Rdata")

#Summarizing length information on the slab/mat ident level
WBW.summary <- doBy::summaryBy(POSITION_X  ~  MAT_IDENT, data=df_WBW_length,
                               FUN=c(min, median, mean, max, sd, length))
WBW.summary <- WBW.summary %>%
  mutate(
    POSITION_X.range = POSITION_X.max - POSITION_X.min
  )
summary(WBW.summary)

#merging with the SGA summary and computing strech factor for all slabs
WBW_SGA.summary <- WBW.summary %>%
  dplyr::inner_join(ident.all.df, by="MAT_IDENT") %>%
  dplyr::inner_join(SGA.summary, by=c("CHARGEN_NR", "VORBRAMME")) %>%
  mutate(
    stretch_factor = POSITION_X.range/RIEGELLAENGE.range
  )

#Drop those which are cut off due to scrapping(3.0m on both sides)
#First order WBW1 S.T. obvs are increasing order of Mat ident & position
df_WBW_length <- df_WBW_length %>%
  dplyr::filter(MAT_IDENT %in% WBW_SGA.summary$MAT_IDENT)

df_WBW_length2 <- df_WBW_length %>%
  dplyr::select(MAT_IDENT, POSITION_X) %>%
  plyr::arrange(MAT_IDENT, POSITION_X) %>%
  mutate(
    Length.max.slab = rep(WBW_SGA.summary$POSITION_X.max, WBW_SGA.summary$POSITION_X.length)
  ) %>%
  dplyr::filter(
    POSITION_X > 3.0 & POSITION_X < Length.max.slab -3.0
  ) %>%
  dplyr::mutate(
    POSITION_X = POSITION_X - 3.0,
    lTileID = POSITION_X %/% ((Length.max.slab - 6.0)/512)
  )
df_WBW_length <- df_WBW_length2 %>%
  dplyr::left_join(df_WBW_length, by = c("MAT_IDENT", "POSITION_X"))

#Doing the same for the SGA data
df_SGA <- df_SGA %>%
  dplyr::semi_join(WBW_SGA.summary, by=c("CHARGEN_NR", "VORBRAMME")) %>%
  dplyr::mutate(
    sf = rep(WBW_SGA.summary$stretch_factor,WBW_SGA.summary$RIEGELLAENGE.length),
    POSITION_X = RIEGELLAENGE * sf,
    lTile_length = ((RIEGELLAENGE.max.slab - 0.054) * sf - 6.0)/512,
    lTileID = POSITION_X %/% lTile_length
  )%>%
  dplyr::filter(
    POSITION_X > 0.028 * sf + 3.0 & POSITION_X + 3.0 < ((RIEGELLAENGE.max.slab - 0.026)*sf)
  )

#Computing Descriptives
df_SGA_long <- df_SGA %>%
  tidyr::gather(key=length_attr, value=measurement, -c(CHARGEN_NR, VORBRAMME, MAT_IDENT, SID, VORG_HAUPTAGGREGAT, STRANGNUMMER, RIEGELLAENGE))
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

p <- ggplot(data = df_SGA_long, aes(x = measurement)) + geom_histogram() + facet_wrap(~length_attr, scales = "free") 
p

#Saving Data
save(df_SGA, df_SGA_desc, df_WBW_length, file="../anna_data/anna_nachVerschrottung_updated.Rdata")

#Validating Data

rm(list = ls())
load("../anna_data/anna_nachVerschrottung_updated.Rdata")

#My data
A_df_SGA <- df_SGA
A_df_SGA_desc <- df_SGA_desc
A_df_WBW_length <- df_WBW_length
rm(df_SGA, df_SGA_desc, df_WBW_length)

#Wilhelm's Data
load("../Data/stage8_nachVerschrottung_updated.Rdata")

#Validation
anti_join(A_df_SGA, df_SGA)
#Match
anti_join(A_df_SGA_desc, df_SGA_desc)
#Match
anti_join(A_df_WBW_length, df_WBW_length)
#Match
#YAY