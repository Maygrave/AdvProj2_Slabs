#Step3 - Prepping the BZA data

#Importing Libraries
rm(list=ls())
library(readxl)
library(tidyverse)
library(doBy)
library(caret)
library(randomForest)
library(data.table)

#loading data
load("../Data/GR_Defects.BZA.Rdata")
V_COILS_BZA <- read_excel("../Data/V_COILS_BZA.xlsx")
load("../Data/stage8_ident.Rdata")

#set seed
set.seed(10101010)

#Prepping BZA Data
#Here we are selecting only relevent variables
#Then ordering data s.t. obs are in increasing order
#first by coilID, the TILEID
GR_Defects_BZA <- GR_Defects_BZA %>%
  dplyr::select(CoilID, TileID, ClassID, Count) %>%
  arrange(CoilID, TileID)

#Converting TileID to length region and width region
#This should preject the tiles on a logitudinal dimension
GR_Defects_BZA <- GR_Defects_BZA %>%
  dplyr::mutate(
    lTileID = TileID %/% 256,
    wTileID = TileID %% 256
    )

defects.width <- GR_Defects_BZA %>%
  dplyr::group_by(wTileID, ClassID) %>%
  dplyr::summarise(
    Count = sum(Count)
  )

defects.length.agg <- GR_Defects_BZA %>%
  dplyr::group_by(lTileID, ClassID) %>%
  dplyr::summarise(
    Count = sum(Count)
  )  

#Error Dist by geometry
defects.position.c4 <- GR_Defects_BZA %>%
  dplyr::filter(ClassID == 4) 
defects.position.c14 <- GR_Defects_BZA %>%
  dplyr::filter(ClassID == 14) 
defects.position.c15 <- GR_Defects_BZA %>%
  dplyr::filter(ClassID == 15) 

#type 4
p <- ggplot(data = defects.position.c4, aes(x = lTileID, y=wTileID, col=Count)) + geom_point(alpha=0.8, size=0.8) + ggtitle("Fehlerverteilung gemäß Oberflächeninspektion (Typ 4)") + labs(x="Längensegment", y="Breitensegment") 
p + scale_colour_gradient(name="Count", low="grey", high="black") + scale_y_reverse()

#type 14
p <- ggplot(data = defects.position.c14, aes(x = lTileID, y=wTileID, col=Count)) + geom_point(alpha=0.1, size=0.8) + ggtitle("Fehlerverteilung Oberflächeninspektion (Typ 14)") + labs(x="Längensegment", y="Anzahl Fehler Typ 14")
p + scale_colour_gradient(name="Count", low="red", high="red4") + scale_y_reverse()

#type 15
p <- ggplot(data = defects.position.c15, aes(x = lTileID, y=wTileID, col=Count)) + geom_point(alpha=0.8, size=0.8) + ggtitle("Fehlerverteilung Oberflächeninspektion (Typ 15)") + labs(x="Längensegment", y="Anzahl Fehler Typ 15")
p + scale_colour_gradient(name="Count", low="lightblue", high="blue") + scale_y_reverse()   

#Quotting Wilhelm's code:
#"Now plot the resulting error counts against widthTile. There are only 758 observations in the data frame, i.e. there are a total of 10 tile IDs that do not have errors for one of three error classes."

defects.width_wide <- defects.width %>%
  tidyr::spread(key= ClassID, value = Count) %>%
  setNames( c("wTileID", "Class.4", "Class.14", "Class.15") ) %>%
  replace_na(list(Class.14=0, Class.15=0))

#type 4
p <- ggplot(data = defects.width_wide, aes(x = wTileID, y=Class.4)) + geom_line(col="grey2", alpha=0.5) + ggtitle("Fehlerverteilung über Breite (Typ 4)") + labs(x="Breitensegment", y="Anzahl Fehler Typ 4")
p

#type 14
p <- ggplot(data = defects.width_wide, aes(x = wTileID, y=Class.14)) + geom_line(col="red", alpha=0.5) + ggtitle("Fehlerverteilung über Breite (Typ 14)") + labs(x="Breitensegment", y="Anzahl Fehler Typ 14")
p

#type 15
p <- ggplot(data = defects.width_wide, aes(x = wTileID, y=Class.15)) + geom_line(col="blue", alpha=0.5) + ggtitle("Fehlerverteilung über Breite (Typ 15)") + labs(x="Breitensegment", y="Anzahl Fehler Typ 15")
p 

###
defects.length_wide <- defects.length.agg %>%
  tidyr::spread(key= ClassID, value = Count) %>%
  setNames( c("lTileID", "Class.4", "Class.14", "Class.15") ) %>%
  replace_na(list(Class.14=0, Class.15=0))

#type 4
p <- ggplot(data = defects.length_wide, aes(x = lTileID, y=Class.4)) + geom_line(col="grey2", alpha=0.5) + ggtitle("Fehlerverteilung über Länge (Typ 4)") + labs(x="Längensegment", y="Anzahl Fehler Typ 4")
p
#type 14
p <- ggplot(data = defects.length_wide, aes(x = lTileID, y=Class.14)) + geom_line(col="red", alpha=0.5) + ggtitle("Fehlerverteilung über Länge (Typ 14)") + labs(x="Längensegment", y="Anzahl Fehler Typ 14")
p
#type 15
p <- ggplot(data = defects.length_wide, aes(x = lTileID, y=Class.15)) + geom_line(col="blue", alpha=0.5) + ggtitle("Fehlerverteilung über Länge (Typ 15)") + labs(x="Längensegment", y="Anzahl Fehler Typ 15")
p  

#Filtering out large error counts, keeping counts <25
defects.position.c4 <- defects.position.c4 %>%
  dplyr::add_count(CoilID, lTileID)
defects.position.c14 <- defects.position.c14 %>%
  dplyr::add_count(CoilID, lTileID)
defects.position.c15 <- defects.position.c15 %>%
  dplyr::add_count(CoilID, lTileID)

run_BZA <- GR_Defects_BZA %>%
  dplyr::add_count(CoilID, lTileID) %>%
  dplyr::filter(n < 25)

#Filtering out Randbefall
BZA_tidy <- run_BZA %>%
  dplyr::filter(wTileID >5 & wTileID < 250)%>%
  dplyr::group_by(CoilID, lTileID, wTileID, ClassID) %>%
  dplyr::summarise(
    Count=sum(Count)
  )  

#New geometric error dist, with improved data
defects.position.c4.tidy <- BZA_tidy %>%
  dplyr::filter(ClassID == 4) 
defects.position.c14.tidy <- BZA_tidy %>%
  dplyr::filter(ClassID == 14) 
defects.position.c15.tidy <- BZA_tidy %>%
  dplyr::filter(ClassID == 15) 

#type 4
p <- ggplot(data = defects.position.c4.tidy, aes(x = lTileID, y=wTileID, col=Count)) + geom_point(alpha=0.8, size=0.8) + ggtitle("Fehlerverteilung gemäß Oberflächeninspektion (Typ 4)") + labs(x="Längensegment", y="Breitensegment") 
p + scale_colour_gradient(name="Count", low="grey", high="black") + scale_y_reverse()
#type 14
p <- ggplot(data = defects.position.c14.tidy, aes(x = lTileID, y=wTileID, col=Count)) + geom_point(alpha=0.1, size=0.8) + ggtitle("Fehlerverteilung Oberflächeninspektion (Typ 14)") + labs(x="Längensegment", y="Anzahl Fehler Typ 14")
p + scale_colour_gradient(name="Count", low="red", high="red4") + scale_y_reverse()
#type 15
p <- ggplot(data = defects.position.c15.tidy, aes(x = lTileID, y=wTileID, col=Count)) + geom_point(alpha=0.8, size=0.8) + ggtitle("Fehlerverteilung Oberflächeninspektion (Typ 15)") + labs(x="Längensegment", y="Anzahl Fehler Typ 15")
p + scale_colour_gradient(name="Count", low="lightblue", high="blue") + scale_y_reverse()

defects.length <- BZA_tidy %>%
  dplyr::group_by(CoilID, lTileID, ClassID) %>%
  dplyr::summarise(
    Count = sum(Count)
  )

#Adding error free tiles between error affected tiles
#selecting for gaps larger than 15
defects.length$nextID <- dplyr::lead(defects.length$lTileID, default =2)

defects <- defects.length %>%
  dplyr::mutate(
    lTileID2 = case_when(nextID - lTileID > 15 ~ lTileID + ceiling((nextID - lTileID)/2))
  )

defects.lTileID2 <- defects %>%
  dplyr::select(CoilID, lTileID, lTileID2) %>%
  tidyr::gather(key=ltile_attr, value=measurement,
                lTileID, lTileID2, -CoilID) %>%
  dplyr::select(CoilID, measurement) %>%
  dplyr::rename(
    lTileID = measurement
  ) %>%
  dplyr::filter(lTileID >= 0 & lTileID <= 511) %>%
    unique() %>%
   drop_na()

#add on longitudinal tile before and after each error tile
tileID.df <- defects.lTileID2 %>%
  dplyr::mutate(
       prelTileID = lTileID - 1,
       postlTileID = lTileID + 1
       ) %>%
  tidyr::gather(key = ltile_attr, value=measurement,
                lTileID, prelTileID,  postlTileID, -CoilID
  ) %>%
  dplyr::select(CoilID, measurement) %>%
  dplyr::rename(
    lTileID = measurement
  ) %>%
  dplyr::filter(lTileID >= 0 & lTileID <= 511) %>%
    unique() 

#Quoting Wilhelm's Code
#"## Generating random tileIDs for error-free coils
#Find ident numbers for error-free coils, i.e. coils which are in V_COILS_BZA but not in GR_Defects_BZA. Generate random elements for lTileId for theses ident numbers."
ident.BZA.df <- V_COILS_BZA %>%
  dplyr::select(SID, SCHMELZ_NR, WB_VORBR_NR)
apply(ident.BZA.df, 2, function(x) {length(unique(x))})
#SID:956, Schmelz_NR:309, WB_VORB_NR:33

slab.ident.BZA_lack.df <- ident.BZA.df %>%
  dplyr::filter(!SID %in% ident.defects$CoilID)
apply(slab.ident.BZA_lack.df, 2, function(x) {length(unique(x))})
#SID:106, Schmelz_NR:74, WB_Vorbr_NR:24

tile.ident.BZA_lack.df <- data.frame(SID = rep(sort(slab.ident.BZA_lack.df$SID), each=40), lTileID = rep(sample(0:511,40),106))

#Merging with previously generated IDs
tile.ident.BZA.df <- tileID.df  %>%
  dplyr::full_join(tile.ident.BZA_lack.df, by=c("CoilID" ="SID", "lTileID"))

#Quotting Wilhelm's Code:
#"Next, we turn the data frame defects into wide format and spread the error counts according to class id. Obtain full list of defect counts for BZA level."
data_BZA <- defects %>%
  dplyr::select(-nextID, -lTileID2)%>%
  tidyr::spread(key = ClassID, value = Count) %>%
  setNames( c("CoilID", "lTileID", "Class.4", "Class.14", "Class.15") ) 

data_BZA$Class.4[is.na(data_BZA$Class.4)] <- 0
data_BZA$Class.14[is.na(data_BZA$Class.14)] <- 0
data_BZA$Class.15[is.na(data_BZA$Class.15)] <- 0
apply(data_BZA, 2, function(x) {length(unique(x))})
#Apply info:
#COLITD:835, lTileID:506, Class.4:29, Class.14:17, Class.15:21

#Merging w/ error free IDs
data_BZA <- tile.ident.BZA.df %>%
  dplyr::full_join(data_BZA, by=c("CoilID", "lTileID"))

data_BZA$Class.4[is.na(data_BZA$Class.4)] <- 0
data_BZA$Class.14[is.na(data_BZA$Class.14)] <- 0
data_BZA$Class.15[is.na(data_BZA$Class.15)] <- 0
apply(data_BZA, 2, function(x) {length(unique(x))})
#Apply Info:
#CoilID:941, lTileID:510, Class.4:29, Class.14:17, Class.15:21

# merging extended GR_Defects_BZA with V_COILS_BZA
df_BZA <- data_BZA %>%
  dplyr::full_join(V_COILS_BZA, by=c("CoilID" = "SID"))

df_BZA$Class.4[is.na(df_BZA$Class.4)] <- 0
df_BZA$Class.14[is.na(df_BZA$Class.14)] <- 0
df_BZA$Class.15[is.na(df_BZA$Class.15)] <- 0
apply(df_BZA, 2, function(x) {length(unique(x))})


#REDUCING THE DATA SET

#dropping prodcution data Raselstein variables
var.wa <- df_BZA %>%
  dplyr::select(starts_with("WA_")) %>%
  names()

df_BZA <- df_BZA %>%
  dplyr::select(-var.wa)

#Deleting redundant indexes
var.index.dup <- df_BZA %>%
  dplyr::select(SCHMELZ_NR, contains("IDENT"), contains("_NR"), contains("BRAMMEN"), contains("UUID"), contains("NAME"), contains("SID")) %>%
  names()

df_BZA <- df_BZA %>%
  dplyr::select(-var.index.dup)

#Drop time stamps
var.ts <- df_BZA %>%
  dplyr::select(contains("ZEITPUNKT"), contains("ZEITSEL"), contains("DATUM"), contains("TIME"), contains("SEQ")) %>%
  names()
df_BZA <- df_BZA %>%
  dplyr::select(-var.ts)

#Remove Irrelevant Variables (Table from Dr. Eberle)
var.irv <- c("basePID", "ImportFlag", "A1", "Weight")
df_BZA <- df_BZA %>%
  dplyr::select(-var.irv)

#Convert to long format and check availability and missing patters
df_BZA_long <- df_BZA %>%
  tidyr::gather(key=length_attr, value=measurement, -c(CoilID, lTileID))

df_BZA_desc <- df_BZA_long %>% 
  dplyr::group_by(length_attr) %>% 
  dplyr::summarise(
    count = n(),
    unique = length(unique(measurement)), 
    na = sum(is.na(measurement)), 
    N=sum(!is.na(measurement))
  )
df_BZA_desc
#Multiple constant vars here

#Dropping constant vars
var.BZA.const <- df_BZA_desc%>%
  dplyr::filter(unique == 1) %>%
  dplyr::select(length_attr) %>%
  unlist() %>%
  as.character()

df_BZA_long <- df_BZA_long %>%
  dplyr::filter(!length_attr %in% var.BZA.const)


#Checking dist of binary variables
var.BZA.bin <- df_BZA_desc%>%
  dplyr::filter(unique <= 3) %>%
  dplyr::select(length_attr) %>%
  unlist()

df_BZA_bin_long <- df_BZA_long %>%
  dplyr::filter(length_attr %in% var.BZA.bin)

p <- ggplot(data = df_BZA_bin_long, aes(x = measurement )) + geom_bar() + facet_wrap(~length_attr, scales = "free") 
p
#Doesn't seem to have any masked missing
#but thats about all that can be seen from here
#VERY few instances of TH_STM_IST that arent in 28VA1

#Back to wide form
df_BZA <- df_BZA_long %>%
  tidyr::spread(key = length_attr, value = measurement)

#Dropping obs not tied to TH_STM_IST$28VA1
df_BZA <- df_BZA %>%
  dplyr::filter(TH_STM_IST == "28VA1")

#Sending most charvars to numeric (Excluding TH_STM_IST and WB_KZ_KANTENRWAERMUNG)
df_BZA <- df_BZA %>%
  dplyr::select(-c(TH_STM_IST, WB_KZ_KANTENERWAERMUNG)) %>%
  dplyr::mutate_if(is.character, as.numeric) %>%
  dplyr::bind_cols(df_BZA[,c("TH_STM_IST", "WB_KZ_KANTENERWAERMUNG")])

#Dropping piece related vars
df_BZA <- df_BZA %>%
  dplyr::select(-starts_with("WB_"), -starts_with("BA_"), -c(TH_STM_IST, WB_KZ_KANTENERWAERMUNG), -Width, -Thick, -Length, -HASPEL_T_1)

#Saving Final Output data
save(list=ls(pattern="defects"), file="../anna_data/anna_defects.Rdata")
save(df_BZA, file="../anna_data/anna_df_BZA.Rdata")

#clearing out data files
rm(list=ls())

#~~VALIDATING DATA~~#

#loading my data
load("../anna_data/anna_df_BZA.Rdata")
load("../anna_data/anna_defects.Rdata")
#renaming
a_defects <- defects
a_defects.length <- defects.length
a_defects.length.agg <- defects.length.agg
a_defects.length_wide <- defects.length_wide
a_defects.lTileID2 <- defects.lTileID2
a_defects.position.c14 <- defects.position.c14
a_defects.position.c14.tidy <- defects.position.c14.tidy
a_defects.position.c15 <- defects.position.c15
a_defects.position.c15.tidy <- defects.position.c15.tidy
a_defects.position.c4 <- defects.position.c4
a_defects.position.c4.tidy <- defects.position.c4.tidy
a_defects.width <- defects.width
a_defects.width_wide <- defects.width_wide
a_df_BZA <- df_BZA
a_ident.defects <- ident.defects
#drop old
rm(defects, defects.length, defects.length.agg, defects.length_wide,
   defects.lTileID2, defects.position.c14, defects.position.c14.tidy,
   defects.position.c15, defects.position.c15.tidy, defects.position.c4,
   defects.position.c4.tidy, defects.width, defects.width_wide, df_BZA, ident.defects)

#Load Wilhelm's Data
load("../Data/df_BZA.Rdata")
load("../Data/stage8_defects.Rdata")

#~~VALIDATING DATA AGAINST WILHELM DATA~~#

#Compare Defects
dplyr::anti_join(defects, a_defects)
# 0 - They match!
rm(a_defects, defects)

#Compare Defects.length
dplyr::anti_join(defects.length, a_defects.length)
#0 - They match!
rm(a_defects.length, defects.length)

#Compare defects.length.agg
dplyr::anti_join(defects.length.agg, a_defects.length.agg)
#0 - They match!
rm(a_defects.length.agg, defects.length.agg)

#Compare defects.length_wide
dplyr::anti_join(defects.length_wide, a_defects.length_wide)
#0 - They match!
rm(a_defects.length_wide, defects.length_wide)

#Compare defects.lTileID2
dplyr::anti_join(defects.lTileID2, a_defects.lTileID2)
#0 - They match!
rm(a_defects.lTileID2 , defects.lTileID2)

#Compare defects.position.c14 
dplyr::anti_join(defects.position.c14, a_defects.position.c14)
#0 - They match!
rm(a_defects.position.c14, defects.position.c14)

#Compare defects.position.c14.tidy
dplyr::anti_join(defects.position.c14.tidy, a_defects.position.c14.tidy)
#0 - They match!
rm(a_defects.position.c14.tidy, defects.position.c14.tidy)

#Compare defects.position.c15 
dplyr::anti_join(defects.position.c15, a_defects.position.c15 )
#0 - They match!
rm(a_defects.position.c15, defects.position.c15)

#Compare defects.position.c15.tidy
dplyr::anti_join(defects.position.c15.tidy, a_defects.position.c15.tidy)
#0 - They match!
rm(a_defects.position.c15.tidy, defects.position.c15.tidy)

#Compare defects.position.c4 
dplyr::anti_join(defects.position.c4, a_defects.position.c4)
#Both 0 - They match!
rm(a_defects.position.c4, defects.position.c4)

#Compare defects.position.c4.tidy
dplyr::anti_join(defects.position.c4.tidy, a_defects.position.c4.tidy)
#0 - They match!
rm(a_defects.position.c4.tidy, defects.position.c4.tidy)

#Compare defects.width
dplyr::anti_join(defects.width, a_defects.width)
#0 - They match!
rm(a_defects.width, defects.width)

#Compare defects.width_wide
dplyr::anti_join(defects.width_wide, a_defects.width_wide)
#0 - They match!
rm(a_defects.width_wide , defects.width_wide)

#Compare df_BZA
dplyr::anti_join(df_BZA, a_df_BZA)
#Double check
#These do not match. 
#There are the same number of obvs, but they are 3848 that do not match
#Doesn't even match to what the Wilhelm code just outputs alone
rm(a_df_BZA , df_BZA)

#Compare ident.defects
dplyr::anti_join(ident.defects, a_ident.defects)
#0 - They match!
rm(a_ident.defects , ident.defects)

#NOTE:
#All data sets match excluding df_BZA
#The number of obvs is the same, but there are aprox
#3700-3800 obs that differ from 
#the data saved by wilhelm which can be found on next cloud
#the data that can be produced by running Wilhelm's original code
#and the data produced by my code
#The data wilhelm has saved
#The data wilhelm has saved differes by the exact same amount of obs
#as my output data does
#when compared to the data procuded by his code
#but these are not all the same (about 300 same)

#Will use Wilhelm df_BZA