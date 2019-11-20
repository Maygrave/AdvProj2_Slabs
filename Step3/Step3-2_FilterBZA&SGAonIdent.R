#Step3.2 - BZA & SGA on Ident

#Importing Libraries
rm(list=ls())
library(readxl)
library(tidyverse)
library(doBy)
library(caret)
library(randomForest)
library(data.table)

#Loading Data
load("../anna_data/anna_df_BZA2.Rdata")
load("../Data/stage8_ident.Rdata")
load("../anna_data/anna_df_SGA.Rdata")

#test Filtering on ident.SGA_BZA
df_BZA <- df_BZA %>%
  dplyr::inner_join(ident.SGA_BZA.df, by = c("CoilID" = "SID", "MAT_IDENT", "CHARGEN_NR", "VORBRAMME"))
#Nothing, Yay

#test filter on ident.SGA_BZA
df_SGA <- df_SGA %>%
  dplyr::inner_join(ident.SGA_BZA.df, by = c("CHARGEN_NR", "VORBRAMME", "MAT_IDENT", "SID"))
#Nothing, Yay

#Aggregate SGA data on slab level to get slab length
SGA_slabs <- summaryBy(RIEGELLAENGE  ~  CHARGEN_NR + VORBRAMME, data=df_SGA,
                       FUN=c(min,max,length))
SGA_slabs <- SGA_slabs %>%
  dplyr::mutate(length = RIEGELLAENGE.max - RIEGELLAENGE.min,
                tilelength = length/512)

#Order S.T. obvs increase first by Mat_Ident and Riegellaenge
df_SGA <- df_SGA %>%
  dplyr::arrange(MAT_IDENT, RIEGELLAENGE)


#Descriptives
SGA.summary <- doBy::summaryBy(RIEGELLAENGE ~  CHARGEN_NR + VORBRAMME, data=df_SGA,
                               FUN=c(min, median, mean, max, sd, length))
SGA.summary <- SGA.summary %>%
  mutate(
    RIEGELLAENGE.range = RIEGELLAENGE.max - RIEGELLAENGE.min
  )
summary(SGA.summary)

#Selecting slabs ending with Vorbramme 2, switch start and end positions
#adjust RIEGELLAENGE 
SGA.summary.VB2 <- SGA.summary %>%
  filter(VORBRAMME %% 2 == 0)

SGA_lengthVB2 <- df_SGA %>%
  filter(VORBRAMME %% 2 == 0) %>%
  mutate(
    RIEGELLAENGE.max.slab = rep(SGA.summary.VB2$RIEGELLAENGE.max,      SGA.summary.VB2$RIEGELLAENGE.length),
    RIEGELLAENGE = RIEGELLAENGE.max.slab - RIEGELLAENGE
  )
summary(SGA.summary.VB2)

#Mergeing the set together
SGA.summary.VB3 <- SGA.summary %>%
  filter(VORBRAMME %% 2 != 0)
SGA_lengthVB3 <- df_SGA %>%
  filter(VORBRAMME %% 2 != 0) %>%
  mutate(
    RIEGELLAENGE.max.slab = rep(SGA.summary.VB3$RIEGELLAENGE.max,       SGA.summary.VB3$RIEGELLAENGE.length)
  )
df_SGA <- SGA_lengthVB2 %>%
  dplyr::bind_rows(SGA_lengthVB3) 

summary(SGA.summary.VB3)

#computing Descriptives
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

#Saving
save(df_SGA, df_SGA_desc, SGA.summary, file="../anna_data/anna_df_SGA_filtered_BZA.Rdata")

#Validating Data
rm(list=ls())
load("../anna_data/anna_df_SGA_filtered_BZA.Rdata")

#My data
a_df_SGA <- df_SGA
a_df_SGA_desc <- df_SGA_desc
a_SGA.summary <- SGA.summary
rm(df_SGA, df_SGA_desc, SGA.summary)

#Wilhelms DAta
load("../Data/df_SGA_filtered_BZA.Rdata")

anti_join(a_df_SGA, df_SGA)
#Match!
anti_join(a_df_SGA_desc, df_SGA_desc)
#Match!
anti_join(a_SGA.summary, SGA.summary)
#Match