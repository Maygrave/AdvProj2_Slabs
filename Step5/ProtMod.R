#Load Libraries
rm(list=ls())
library(readxl)
library(ggplot2)
library(plyr)
library(data.table)
library(tidyverse)
library(randomForest) #for random forests
library(caret) # for CV folds and data splitting
library(GGally)
library(MASS)
library(car)
library(party)

#Loading data
load("../anna_data/anna_merged_data.Rdata")

var.index <- c("MAT_IDENT", "lTileID")
var.num <- colnames(df[,sapply(df,is.numeric)])
var.factor <- c("VORG_HAUPTAGGREGAT.x", "BPW_ERZEUGUNG", "FLAEMMGRAD_IST", "TAUCHAUSGUSS")

#Defining index vars and reordering
var.index <- c("MAT_IDENT", "lTileID", "CoilID")
df <- df %>%
  dplyr::select(var.index, everything())

#Variable Reduction
df <- df %>%
  dplyr::select(-c(sf, RIEGELLAENGE.max.slab, lTile_length))

#Send to long format, review missing patterns
df_long <- df %>%
  tidyr::gather(key=length_attr, value=measurement, -c(MAT_IDENT, lTileID, CoilID))

df_desc <- df_long %>% 
  dplyr::group_by(length_attr) %>% 
  dplyr::summarise(
    count = n(),
    unique = length(unique(measurement)), 
    na = sum(is.na(measurement)), 
    N=sum(!is.na(measurement))
  )
df_desc

#Drop constant vars
var.all.const <- df_desc%>%
  dplyr::filter(unique == 1) %>%
  dplyr::select(length_attr) %>%
  unlist() %>%
  as.character()

df_long <- df_long %>%
  dplyr::filter(!length_attr %in% var.all.const)
df <- df %>%
  dplyr::select(-var.all.const)

#var lists by type
var.list <- colnames(df)
var.int <- colnames(df[,sapply(df,is.integer)])
var.factor <- colnames(df[,sapply(df,is.factor)])
var.num <- colnames(df[,sapply(df,is.numeric)])

#splitting DF by type
df_factor <- df %>%
  dplyr::select(var.index,var.factor)
df_num <- df %>%
  dplyr::select(var.num, -var.int)

#Exploration of Numeric Vars
df_num_long <- df_num %>%
  gather(key=slab_attr, value=measurement)

df_num_desc <- df_num_long %>% 
  dplyr::group_by(slab_attr) %>% 
  dplyr::summarise(
    count = n(),
    mean = mean(measurement, na.rm=TRUE), 
    sd = sd(measurement, na.rm = TRUE),
    min = min(measurement, na.rm=TRUE), 
    max=max(measurement, na.rm=TRUE), 
    unique = length(unique(measurement)), 
    na = sum(is.na(measurement)), 
    N=sum(!is.na(measurement)),
    max_freq = max(table(measurement)),
    min_freq = min(table(measurement)),
    freq_ratio = (max_freq-min_freq)/unique
  )
df_num_desc

#Dropping variables with no variation
var.sd0 <- df_num_desc %>%
  dplyr::filter(sd==0) %>%
  dplyr::select(slab_attr) %>%
  unlist() %>%
  as.character()

df_num_long <- df_num_long %>%
  dplyr::filter(!slab_attr %in% var.sd0)

df_num <- df_num %>%
  dplyr::select(-var.sd0)

#Cormat of numeric vars
cormat <- round(cor(df_num, use="pairwise.complete.obs"),4)

#For all pairs which are dully, absolutely correlatted, we keep only one
corONE <- function(x) {
    if (is.matrix(x)) {
      cor1.df <- data.frame(which(abs(x)==1, arr.in=TRUE))
      setDT(cor1.df, keep.rownames = TRUE)[]
      cor1.list <- cor1.df$rn[which(cor1.df$row > cor1.df$col, arr.in=TRUE)]
      grx <- glob2rx("*.*")
      duplicate.list <- grepl(grx,cor1.list, perl=TRUE)
      cor1.list <- cor1.list[!duplicate.list]
    } else {
      print("no matrix!")
    }
}
cor1.list <- corONE(cormat)
write.table(cor1.list, file="anna_Length_ListofVariableswithCor1.txt", sep="\t")

#Drop corresponding columns
f_num <- df_num %>%
  dplyr::select(-cor1.list)

#Computing Descriptives
df_num_long <- df_num %>%
  gather(key=slab_attr, value=measurement) 

df_num_desc2 <- df_num_long %>% 
  dplyr::group_by(slab_attr) %>% 
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
df_num_desc2

#For each pair with absolute correlation >0.95, only keep the first
cormat <- round(cor(df_num, use="pairwise.complete.obs"),4)

filter.cor <- function(x, eps) {
    if (is.matrix(x)) {
      cor.df <- data.frame(which(abs(x) > eps, arr.in=TRUE))
      setDT(cor.df, keep.rownames = TRUE)[]
      cor.df$cor <- x[which(abs(x) > eps, arr.in=TRUE)]
      cor.df <- cor.df[which(cor.df$row > cor.df$col, arr.in=TRUE)]
      cor.df$cn <- colnames(x[, cor.df$col])
      cor.list <- cor.df$rn
      grx <- glob2rx("*.*")
      duplicate.list <- grepl(grx,cor.list, perl=TRUE)
      cor.list <- cor.list[!duplicate.list]
      cor.df$rn <- sub(pattern = "(.*)\\..*$", replacement = "\\1", cor.df$rn)
      corList <- list(CorMat = cor.df, cor.list = cor.list)
      return(corList)
    } else {
      print("no matrix!")
    }
}


df_numList <- filter.cor(cormat, eps=0.95)
df_num2 <- df_numList$CorMat
cor.list <- df_numList$cor.list
cor.list <- cor.list[!cor.list %in% c("CoilID")]
cor.list <- c(cor.list, "POSITION_X.y")

write.table(cor.list, file="anna_length_ListofVariableswithCorLT095.txt", sep="\t")

#descriptives on excluded vars
df_cor2 <- df_num2 %>% 
  dplyr::group_by(rn) %>% 
  dplyr::summarise(
    count = n(),
    runique = length(unique(rn)),
    unique = length(unique(cn)), 
    cor_var = toString(cn),
    na = sum(is.na(cn)), 
    N=sum(!is.na(cn))
  )
df_cor2

#dropping corresponding columns
df_num <- df_num %>%
  dplyr::select(-cor.list)

#New descriptives
df_num_long <- df_num %>%
  gather(key=slab_attr, value=measurement) 

df_num_desc3 <- df_num_long %>% 
  dplyr::group_by(slab_attr) %>% 
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
df_num_desc3

#Reduce accordingly
df <- df %>%
  dplyr::select(-cor1.list, -cor.list, -var.sd0) %>%
  dplyr::rename(
    POSITION_X = POSITION_X.x
  )

#Dropping redundancies and piece related vars
df <- df %>%
  dplyr::select(-CHARGEN_NR, -VORBRAMME, -Length.max.slab) 

#Log transform of error counts
df1 <- df%>%
  dplyr::mutate(lnClass.4 = log(Class.4 + 1), lnClass.14 = log(Class.14 + 1), 
                lnClass.15 = log(Class.15 + 1))

#Modeling the log error counts
linmodlnC4.pred <- lm(lnClass.4~.- CoilID - MAT_IDENT - lTileID - Class.4 - Class.14 - Class.15 - lnClass.14 - lnClass.15, data =df1)
summary(linmodlnC4.pred)
anova(linmodlnC4.pred)

#Modeling class 4 error counts with tree
index <- createDataPartition(df1$lnClass.4, p=0.75, list=FALSE)
trainSet <- df1[ index,]
testSet <- df1[-index,]

outcomeName<-'lnClass.4'
predictors<- colnames(trainSet)
predictors <- predictors[!predictors %in% c("CoilID", "MAT_IDENT", "lTileID", "lnClass.4", "Class.4", "Class.14", "Class.15", "lnClass.14", "lnClass.15")]
lnC4.pred <- formula(paste("lnClass.4 ~ ", paste(predictors, collapse= " + ")))
output.tree <- ctree(lnC4.pred, data = trainSet)
png("anna_tks_tree10.png", res=80, height=800, width=1600) 
plot(output.tree)
dev.off()
print(output.tree)
plot(output.tree, gp = gpar(fontsize = 6),     # font size changed to 6
  inner_panel=node_inner,
  ip_args=list(
       abbreviate = TRUE, 
       id = FALSE)
  )

#Modeling Class 4 error with RF
df1$C4 <- with(df1, Class.4>0)
df1$C4<-factor(df1$C4, levels=c(FALSE,TRUE), labels=c("no.error", "error"))

prop.table(table(df1$C4))

index <- createDataPartition(df1$C4, p=0.75, list=FALSE)
trainSet <- df1[ index,]
testSet <- df1[-index,]

C4.pred <- formula(paste("C4 ~ ", paste(predictors, collapse= " + ")))
output.tree <- ctree(C4.pred, data = trainSet)
png("anna_tks_tree20.png", res=80, height=800, width=1600) 
plot(output.tree)
dev.off()
print(output.tree)
plot(output.tree, gp = gpar(fontsize = 6),     # font size changed to 6
  inner_panel=node_inner,
  ip_args=list(
       abbreviate = TRUE, 
       id = FALSE)
  )

trainSet2 <- trainSet[sample(1:nrow(trainSet), 1000,
  	replace=FALSE),]
output.forest <- cforest(C4.pred, data = trainSet2)
var.imp.c4 <- varimp(output.forest)
png("anna_tks_tree20varimp.png", res=80, height=800, width=1600) 
barchart(tail(sort(var.imp.c4), 40), xlab="Variable Importance", main="Variable Importance Fehlertyp 4")
dev.off()

#Modeling class 14 with tree
df1$C14 <- with(df1, Class.14>0)
df1$C14<-factor(df1$C14, levels=c(FALSE,TRUE), labels=c("no.error", "error"))

prop.table(table(df1$C14))

index <- createDataPartition(df1$C14, p=0.75, list=FALSE)
trainSet <- df1[ index,]
testSet <- df1[-index,]

outcomeName<-'C14'
predictors<- colnames(trainSet)
predictors <- predictors[!predictors %in% c("CoilID", "MAT_IDENT", "lTileID", "lnClass.4", "Class.4", "Class.14", "Class.15", "lnClass.14", "lnClass.15", "C4", "C14")]
C14.pred <- formula(paste("C14 ~ ", paste(predictors, collapse= " + ")))
output.tree.c14 <- ctree(C14.pred, data = trainSet)
png("anna_tks_tree_c14_2.png", res=80, height=800, width=1600) 
plot(output.tree.c14)
dev.off()
print(output.tree.c14)
plot(output.tree.c14, gp = gpar(fontsize = 6),     # font size changed to 6
  inner_panel=node_inner,
  ip_args=list(
       abbreviate = TRUE, 
       id = FALSE)
  )

#Modeling Class 14 with RF
trainSet2 <- trainSet[sample(1:nrow(trainSet), 10000,
  	replace=FALSE),]
output.rf.c14 <- cforest(C14.pred, data = trainSet2)
var.imp.c14 <- varimp(output.rf.c14)
png("anna_tks_rfc14varimp.png", res=80, height=800, width=1600) 
barchart(tail(sort(var.imp.c14), 40), xlab="Variable Importance", main="Variable Importance Fehlertyp 14")
dev.off()

#Modeling class 15 with tree
df1$C15 <- with(df1, Class.15>0)
df1$C15<-factor(df1$C15, levels=c(FALSE,TRUE), labels=c("no.error", "error"))

prop.table(table(df1$C15))

index <- createDataPartition(df1$C15, p=0.75, list=FALSE)
trainSet <- df1[ index,]
testSet <- df1[-index,]

outcomeName<-'C15'
predictors <- predictors[!predictors %in% c("CoilID", "MAT_IDENT", "lTileID", "lnClass.4", "Class.4", "Class.14", "Class.15", "lnClass.14", "lnClass.15", "C4", "C14", "C15")]

C15.pred <- formula(paste("C15 ~ ", paste(predictors, collapse= " + ")))
output.tree.c15 <- ctree(C15.pred, data = trainSet)
png("anna_tks_tree_c15_2.png", res=80, height=800, width=1600) 
plot(output.tree.c15)
dev.off()
print(output.tree.c15)
plot(output.tree.c15, gp = gpar(fontsize = 6),     # font size changed to 6
  inner_panel=node_inner,
  ip_args=list(
       abbreviate = TRUE, 
       id = FALSE)
  )

#Modeling Class 15 with RF
trainSet2 <- trainSet[sample(1:nrow(trainSet), 10000,
  	replace=FALSE),]
output.rf.c15 <- cforest(C15.pred, data = trainSet2)
var.imp.c15 <- varimp(output.rf.c15)
png("anna_tks_rfc15varimp.png", res=80, height=800, width=1600) 
barchart(tail(sort(var.imp.c15), 40), xlab="Variable Importance", main="Variable Importance Fehlertyp 15")
dev.off()
