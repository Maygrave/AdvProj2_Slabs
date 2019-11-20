#Step 2 - Data Set Reduction - SGA Data set(s)
#Importing libraries

rm(list=ls())
library(readxl)
library(tidyverse)
library(doBy)
library(caret)
library(randomForest)
library(data.table)

#loading data
load("../Data/tks_longitudinal.Rdata")
load("../Data/stage8_ident.Rdata")

#Prepping SGA Data
#Using data file 'SGA_Prozessdaten_Durchsatz'
#Currently 84 var approx 8.838 x 10^5 obs
summary(SGA_Prozessdaten_Durchsatz)

#Changing Date format
#two variables need updated (SEQBEGINN & ZEITSTEMPEL)
data_SGA <- SGA_Prozessdaten_Durchsatz %>%
  dplyr::mutate(
    SEQBEGINN = as.Date(SEQBEGINN, format = "%Y-%m-%d %H:%M:%S"),
    ZEITSTEMPEL = as.Date(ZEITSTEMPEL, format = "%Y-%m-%d %H:%M:%S")
  )

#~~REMOVING VARIABLES~~#

#Dropping redundant variables and time dependant data
#includes:
#SEQBEGINN, ZEITSTEMPEL, SCHMELZE, BRAMMENUMMER, LFD, STRANGAENGE
data_SGA <-dplyr::select(data_SGA, -c("SEQBEGINN", "ZEITSTEMPEL", "SCHMELZE", "BRAMMENNUMMER", "LFD", "STRANGLAENGE"))

#removing variables on request of Dr. Eberle (Email on 27.8.18)
#drop any TSCHL var
data_SGA <- data_SGA %>%
  dplyr::select(-starts_with("TSCHL"))

#Dropping variables as per S. Karrasch
data_SGA <- data_SGA %>%
  dplyr::select(-SCHIEBERFAHRT, -DAUERTEMPERATUR, -GEWICHT)



#Changing TUNDISH_POSITION to numeric
data_SGA <- data_SGA %>%
  dplyr::mutate(
    TUNDISH_POSITION = as.numeric(TUNDISH_POSITION)
  )

#Creating variable lists acorrding to type
#generic data frame list
df <- data_SGA
var.index <- c("CHARGEN_NR", "VORBRAMME")
var.list <- colnames(df)
var.int <- colnames(df[,sapply(df,is.integer)])
var.factor <- colnames(df[,sapply(df,is.factor)])
var.num <- colnames(df[,sapply(df,is.numeric)])

#split above DF acorindg to type
df_int <- df %>%
  dplyr::select(var.index,var.int)
df_factor <- df %>%
  dplyr::select(var.index,var.factor)
df_num <- df %>%
  dplyr::select(var.num, -var.int)
var_num <- colnames(df_num)


#Convert SGA to long format - check data availability & missing patters
data_SGA_long <- data_SGA %>%
  tidyr::gather(key=length_attr, value=measurement, -c(CHARGEN_NR, VORBRAMME, RIEGELLAENGE))

#ERROR MESSAGE - Check if this comes up in Wilhelm's code
#Warning message:
#attributes are not identical across measure variables;
#they will be dropped 
#it does come up in wilhelms too, no worries

data_SGA_desc <- data_SGA_long %>% 
  dplyr::group_by(length_attr) %>% 
  dplyr::summarise(
    count = n(),
    unique = length(unique(measurement)), 
    na = sum(is.na(measurement)), 
    N=sum(!is.na(measurement))
  )
data_SGA_desc

#Dropping consant vars
var.SGA.const <- data_SGA_desc%>%
  dplyr::filter(unique == 1) %>%
  dplyr::select(length_attr) %>%
  unlist()

data_SGA <- data_SGA %>%
  dplyr::select(-var.SGA.const)

#Checking dist of binary vars
var.SGA.bin <- data_SGA_desc%>%
  dplyr::filter(unique == 2) %>%
  dplyr::select(length_attr) %>%
  unlist()

data_SGA_bin_long <- data_SGA_long %>%
  dplyr::filter(length_attr %in% var.SGA.bin) 

p <- ggplot(data = data_SGA_bin_long, aes(x = measurement )) + geom_bar(stat="count") + facet_wrap(~length_attr, scales = "free") 
p 

#Three vars have one of the binary options as value 0
#without a corresponding 1 to indicate the likely use of a binary indicator
#these may therefore be masked missing values
#but cant confirm this here


#Checking dist of integer variables
#Checking them in groups, as there are so many variables
data_SGA_num_long1 <- data_SGA_long %>%
  dplyr::filter(length_attr %in% var_num[1:16]) %>%
  dplyr::mutate(
    measurement = as.numeric(measurement)
  )

p <- ggplot(data = data_SGA_num_long1, aes(x = measurement)) + geom_histogram() + facet_wrap(~length_attr, scales = "free") 
p 
#In this set, although some minor skewing can occasionally be detected
#the majority of these seem to be (generally)following a normal
#excluding TO_FS_SSL, which has one tight and high normal sq spike, and then a few 0 values
#might be masked missings

data_SGA_num_long2 <- data_SGA_long %>%
  dplyr::filter(length_attr %in% var_num[17:32]) %>%
  dplyr::mutate(
    measurement = as.numeric(measurement)
  )

p <- ggplot(data = data_SGA_num_long2, aes(x = measurement)) + geom_histogram() + facet_wrap(~length_attr, scales = "free") 
p 
#one notable bi-modal dist, most other generally normal
#again, two variables showing tight high normals with a few erroneous 0's
#may again be masked mimssing values
#TU_FS_M, TU_LS_M, TU_LS_SSL, TU_LS_SSR

data_SGA_num_long3 <- data_SGA_long %>%
  dplyr::filter(length_attr %in% var_num[33:48]) %>%
  dplyr::mutate(
    measurement = as.numeric(measurement)
  )

p <- ggplot(data = data_SGA_num_long3, aes(x = measurement)) + geom_histogram() + facet_wrap(~length_attr, scales = "free") 
p 
#Notablly more ireggular dists here
#VG_Min produces no histogram
#STOPFENSTELLUNG has two very notable gaps


data_SGA_num_long4 <- data_SGA_long %>%
  dplyr::filter(length_attr %in% var_num[49:65]) %>%
  dplyr::mutate(
    measurement = as.numeric(measurement)
  )

p <- ggplot(data = data_SGA_num_long4, aes(x = measurement)) + geom_histogram() + facet_wrap(~length_attr, scales = "free") 
p 
#almost all vars here show iregular dists
#multiple vars seem to be binary
#some may be some sort of flag, as the 0 value is notable more prominent than the other chosen value
#others seem to be masking missings

#Computing Desripctives
data_SGA_num_long <- data_SGA_long %>%
  dplyr::filter(length_attr %in% var_num) %>%
  dplyr::mutate(
    measurement = as.numeric(measurement)
  )
SGA_num_desc <- data_SGA_num_long %>% 
  dplyr::group_by(length_attr) %>% 
  dplyr::summarise(
    count = n(),
    mean = mean(measurement, na.rm=TRUE), 
    sd = sd(measurement, na.rm = TRUE),
    min = min(measurement, na.rm=TRUE), 
    max=max(measurement, na.rm=TRUE), 
    unique = length(unique(measurement)), 
    na = sum(is.na(measurement)), 
    N=sum(!is.na(measurement))
  )
SGA_num_desc

#Removing constant vars, conditional on 0 being a masked missing (assuming 0 is a masked missing)
var.SGA.num.const <- SGA_num_desc%>%
  dplyr::filter(unique == 2 & min == 0.0) %>% #Checks for binary variables, with a min of 0 
  dplyr::select(length_attr) %>%
  unlist()

data_SGA <- data_SGA %>%
  dplyr::select(-var.SGA.num.const)

#Setting 0's to NA as per S. Karrasch
#note that this effects the variables thought to be masked missings in 
#set 2 and 4
data_SGA <- data_SGA %>%
  dplyr::mutate(
    TO_FS_SSL = replace(TO_FS_SSL, which(TO_FS_SSL == 0L), NA),
    TU_FS_M = replace(TU_FS_M, which(TU_FS_M == 0L), NA),
    TU_LS_SSR = replace(TU_LS_SSR, which(TU_LS_SSR == 0L), NA),
    TU_LS_M = replace(TU_LS_M, which(TU_LS_M == 0L),  NA),
    TU_LS_SSL = replace(TU_LS_SSL, which(TU_LS_SSL == 0L), NA),
    SCHLACKESIGNAL = replace(SCHLACKESIGNAL, which(SCHLACKESIGNAL == 0L),  NA),
    SPI_SIGNAL = replace(SPI_SIGNAL, which(SPI_SIGNAL == 0L),  NA),
    SP_SIGNAL = replace(SP_SIGNAL, which(SP_SIGNAL == 0L),  NA)
  )

#Computing Descriptives
#na.p100 shows the percentage of the obs that are missing
data_SGA_long <- data_SGA %>%
  tidyr::gather(key=length_attr, value=measurement, -c(CHARGEN_NR, VORBRAMME, VORG_HAUPTAGGREGAT, STRANGNUMMER, RIEGELLAENGE))

data_SGA_desc <- data_SGA_long %>% 
  dplyr::group_by(length_attr) %>% 
  dplyr::summarise(
    count = n(),
    unique = length(unique(measurement)), 
    na = sum(is.na(measurement)), 
    mean = mean(measurement, na.rm=TRUE), 
    sd = sd(measurement, na.rm = TRUE),
    min = min(measurement, na.rm=TRUE), 
    max=max(measurement, na.rm=TRUE),
    na.p100 = na/count*100,
    N=sum(!is.na(measurement))
  )
data_SGA_desc
#SPI_SIGNAL is >96% missing
#SP_SIGNAL & SCHLACKESIGNAL both ~30% missing
#all others with known missings no higher than 6%

#Dropping variables with too many missings
var.SGA.na <- data_SGA_desc%>%
  dplyr::filter(na.p100 > 10 ) %>%
  dplyr::select(length_attr) %>%
  unlist()

data_SGA <- data_SGA %>%
  dplyr::select(-var.SGA.na)
#dropped three mentioned above

#Computing Descriptives
data_SGA_long <- data_SGA %>%
  tidyr::gather(key=length_attr, value=measurement, -c(CHARGEN_NR, VORBRAMME, VORG_HAUPTAGGREGAT))

data_SGA_desc <- data_SGA_long %>% 
  dplyr::group_by(length_attr) %>% 
  dplyr::summarise(
    count = n(),
    unique = length(unique(measurement)), 
    na = sum(is.na(measurement)), 
    mean = mean(measurement, na.rm=TRUE), 
    sd = sd(measurement, na.rm = TRUE),
    min = min(measurement, na.rm=TRUE), 
    max=max(measurement, na.rm=TRUE),
    na.p100 = na/count*100,
    N=sum(!is.na(measurement))
  )
data_SGA_desc


save(data_SGA, data_SGA_desc, file="../anna_data/anna_data_SGA.Rdata")

#Clearing all data files
rm(list=ls())

#~~Validating againt Wilhelm's Data~~#

#loading my data
load("../anna_data/anna_data_SGA.Rdata")
anna_data_SGA <- data_SGA
anna_SGA_desc <- data_SGA_desc
rm(data_SGA, data_SGA_desc)

#loading Wilhelm's
load("../Data/data_SGA.Rdata")

#comparison data_SGA
dplyr::anti_join(data_SGA, anna_data_SGA)
#both 0  - data sets match!

#comparison data_SGA_desc
dplyr::anti_join(data_SGA_desc, data_SGA_desc)
#Both 0 - data sets match!!