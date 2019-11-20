#Step 1
#Preping data

#Importing Libraries
rm(list=ls())
library(readxl)
library(tidyverse)
library(doBy)
library(caret)
library(randomForest)
library(data.table)

#Importing Data
load("../Data/DataMining_28VA1_JU_20180507.RData")
load("../Data/tks_longitudinal.Rdata")


#Preparing the ident matrix for slab ids
#linking BZA, WBW1, & SGA 
ident.SGA.df <- Tera_T_ARBEITSGANG_DURCHSATZ %>%
  dplyr::select(MAT_IDENT, CHARGEN_NR, VORBRAMME)
apply(ident.SGA.df, 2, function(x) {length(unique(x))})
#Mat_Ident:995,Charge_NR:320,Vorbramme:33

ident.SGA2.df <- SGA_Prozessdaten_Durchsatz %>%
  dplyr::select(CHARGEN_NR, VORBRAMME) %>%
  dplyr::inner_join(ident.SGA.df, by = c("CHARGEN_NR", "VORBRAMME"))%>%
  dplyr::add_count(CHARGEN_NR, VORBRAMME) %>%
  dplyr::add_count(MAT_IDENT)
length(unique(ident.SGA2.df$MAT_IDENT))#640
#length(c(unique(ident.SGA2.df$CHARGEN_NR, ident.SGA2.df$VORBRAMME)))#640
#There remains 640 uniquely identifiable M/C/V's

ident.BZA.df <- V_COILS_BZA %>%
  dplyr::select(SID, SCHMELZ_NR, WB_VORBR_NR)
apply(ident.BZA.df, 2, function(x) {length(unique(x))})
#SID:663,SCHMELZ_Nr:249,WB_VORBR_NR:23

ident.wbw1.df <- Tera_V_MESSWERT_EINZEL_WBW1 %>%
  dplyr::select(MAT_IDENT) %>%
  unique()
apply(ident.wbw1.df, 2, function(x) {length(unique(x))})
#Mat_Ident:968

#This is in the tks_ data set
ident.wbw2.df <- TV_WBW1.long %>%
  dplyr::select(MAT_IDENT) %>%
  unique()
apply(ident.wbw2.df, 2, function(x) {length(unique(x))})

ident.wbw3.df <- Tera_V_SPUR_WBW1 %>%
  dplyr::select(MAT_IDENT) %>%
  unique()
apply(ident.wbw3.df, 2, function(x) {length(unique(x))})

ident.wbw.df <- ident.wbw1.df %>%
  dplyr::inner_join(ident.wbw3.df, by ="MAT_IDENT") %>%
  dplyr::inner_join(ident.wbw2.df, by ="MAT_IDENT") 
apply(ident.wbw.df, 2, function(x) {length(unique(x))})
#Mat_Ident:968

ident.SGA_BZA.df <- ident.SGA.df %>%
  dplyr::inner_join(ident.BZA.df, by= (c("CHARGEN_NR" = "SCHMELZ_NR", "VORBRAMME"  = "WB_VORBR_NR")))
apply(ident.SGA_BZA.df, 2, function(x) {length(unique(x))})
#Mat_Ident:661,Chargen_NR:249,VorBramme:23,SID:661

ident.all.df <- ident.wbw.df %>%
  dplyr::inner_join(ident.SGA_BZA.df, by ="MAT_IDENT")
apply(ident.all.df, 2, function(x) {length(unique(x))})
#Mat_Ident:657,Chargen_NR:249,Vorbramme:22,SID:657

save(list=ls(pattern = "ident."), file="../anna_data/Anna_ident.Rdata")

#Reviewing differences in Wilhelm output data, and output data produced here
#clear data
rm(list=ls())

#load Wilhelm's output
load("../Data/stage8_ident.Rdata")

#change variable name > This is the Wilhelm Data
AW_Ident <- ident.all.df
AW_ident.BZA <- ident.BZA.df
AW_ident.defects <- ident.defects
AW_ident.SGA_BZA <- ident.SGA_BZA.df
AW_ident.SGA <- ident.SGA.df
AW_ident.wbw <- ident.wbw.df
AW_ident.wbw1 <- ident.wbw1.df
AW_ident.wbw2 <- ident.wbw2.df
AW_ident.wbw3 <- ident.wbw3.df
AW_slab.ident.BZA_lack <- slab.ident.BZA_lack.df
AW_tile.ident.BZA <- tile.ident.BZA.df
rm(ident.all.df, ident.BZA.df, ident.defects, ident.SGA_BZA.df, 
  ident.SGA.df, ident.wbw.df, tile.ident.BZA.df, ident.wbw1.df, 
  ident.wbw2.df, slab.ident.BZA_lack.df, ident.wbw3.df)

#Loading in the data I produced
load("../anna_data/Anna_ident.Rdata")

#This code produces one additional file (ident.SGA2.df)
#and is missing three files from the data produced by wilhelm
#ident.defects
#slab.ident.BZA_lack
#tile.ident.BZA.df

#checking
anti_join(AW_Ident, ident.all.df)#good
anti_join(AW_ident.BZA, ident.BZA.df)#good
#no defects file in my data
anti_join(AW_ident.SGA_BZA, ident.SGA_BZA.df)#good
anti_join(AW_ident.SGA, ident.SGA.df)#good
anti_join(AW_ident.wbw, ident.wbw.df)#good
anti_join(AW_ident.wbw1, ident.wbw1.df)#good
anti_join(AW_ident.wbw2, ident.wbw2.df)#good
anti_join(AW_ident.wbw3, ident.wbw3.df)#good

#That which exists matches to Wilhelms output

#NOTE:
#Three data frames are created and included in the ident data created by wilhelm 
#ident.defects
#slab.ident.BZA_lack
#tile.ident.BZA.df
#Im not sure what files they were originally created from
#So I'll be using wilhelms data in the rest of the reproduction
