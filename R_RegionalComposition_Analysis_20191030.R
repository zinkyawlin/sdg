Sys.setenv(JAVA_HOME='C:\\Program Files (x86)\\Java\\jre1.8.0_211')  # for 32-bit version

library(dplyr)
library(reshape2)
library(RODBC)
library(openxlsx)
library(compareDF)


Geo <- openxlsx::read.xlsx("C:/Users/Owner/OneDrive - United Nations/SDG_ReferenceFiles/SDG_ReferenceArea_20191105_AM.xlsx", sheet = "REF_AREA_DISAGGREGATED") #Not needed for this exercise
Geo <- openxlsx::read.xlsx("C:/Users/Zin.Lin/OneDrive - United Nations/SDG_ReferenceFiles/SDG_ReferenceArea_20191105_AM.xlsx", sheet = "REF_AREA_DISAGGREGATED") #Not needed for this exercise


setwd("C:/Users/Zin.Lin/OneDrive - United Nations/SDG_Database/10_RegionalGroupingsAnalysis/Analysis")
setwd("C:/Users/Owner/OneDrive - United Nations/SDG_Database/10_RegionalGroupingsAnalysis/Analysis")

# Getting variables for rows
GeoVar <- data.frame(names(Geo)) %>%
  .[48:158,]

#Gettting variables for columns
GeoM49 <- data.frame(GeoVar) %>%
  .[c(1:26,35,36,37,38,39),]

#matrix h
h <- matrix(0,ncol = 31,nrow = 111)
h <- data.frame(h)
colnames(h) <- GeoM49 
rownames(h) <- GeoVar


Tx <- matrix(0,ncol = 31,nrow = 111)
Tx <- data.frame(Tx)
colnames(Tx) <- GeoM49 
rownames(Tx) <- GeoVar


TxSame <- matrix(0,ncol = 31,nrow = 111)
TxSame <- data.frame(TxSame)
colnames(TxSame) <- GeoM49 
rownames(TxSame) <- GeoVar

Pct_T <- matrix(0,ncol = 31,nrow = 111)
Pct_T <- data.frame(Pct_T)
colnames(Pct_T) <- GeoM49 
rownames(Pct_T) <- GeoVar

for (j in GeoM49){
  for (i in GeoVar){
    Temp_j <- Geo %>% select(M49,NAME_E,j) %>% filter(.,.[,3]==1)
    Temp_i <- Geo %>% select(M49,NAME_E,i) %>% filter(.,.[,3]==1)
    Temp <- merge(Temp_j,Temp_i,by = c("M49","NAME_E"),all = TRUE)
    Temp_Same <- Temp %>% filter(.,.[,3]==1&.[,4]==1)
    if(length(Temp_Same$M49)==0 ){
      text <- paste0("-")
      Num <- paste0("-")
      NumSame <- paste0("-")
      Pct <- 0} else { 
        if(length(Temp_Same$M49)==length(Temp$M49)){
          InM49 <- filter(Temp,Temp[,3]==1)
          OnlyInM49 <- filter(InM49,is.na(InM49[,4]))
          InM49_LessOther <- paste0(data.frame(OnlyInM49$NAME_E))
          InOther <- filter(Temp,Temp[,4]==1)
          OnlyInOther <- filter(InOther,is.na(InOther[,3]))
          InOther_LessM49 <- paste0(data.frame(OnlyInOther$NAME_E))
          text <- paste0("SAME")
          Num <- paste0("(",length(InOther$M49),",",length(InM49$M49),"), (",length(Temp_Same$M49),")")
          NumSame <- paste0("SAME")
          Pct <- 100} else 
          {  
            InM49 <- filter(Temp,Temp[,3]==1)
            OnlyInM49 <- filter(InM49,is.na(InM49[,4]))
            InM49_LessOther <- paste0(data.frame(OnlyInM49$NAME_E))
            InOther <- filter(Temp,Temp[,4]==1)
            OnlyInOther <- filter(InOther,is.na(InOther[,3]))
            InOther_LessM49 <- paste0(data.frame(OnlyInOther$NAME_E))
            text <- paste0("#M49LessOther - ",InM49_LessOther,". ","#InOtherLessM49 - ",InOther_LessM49)
            Num <- paste0("(",length(InOther$M49),",",length(InM49$M49),"), (",length(Temp_Same$M49),")")
            NumSame <- paste0(data.frame(Temp_Same$NAME_E))
            Pct <- 100*(length(Temp_Same$M49)/length(Temp$M49))
          }
      }
    
    h[i,j] <- Num
    Tx[i,j] <- text
    TxSame[i,j] <- NumSame
    Pct_T[i,j] <- Pct
  } 
}
write.csv(h,"Regional_Analysis_Num_20191105.csv",row.names = TRUE)
write.csv(Tx,"Regional_Analysis_TextDiff_20191105.csv",row.names = TRUE)
write.csv(TxSame,"Regional_Analysis_TextSame_20191105.csv",row.names = TRUE)
write.csv(Pct_T,"Regional_Analysis_PctDiff_20191105.csv", row.names = TRUE)










