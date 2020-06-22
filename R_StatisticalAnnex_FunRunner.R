Sys.setenv(JAVA_HOME='C:\\Program Files (x86)\\Java\\jre1.8.0_211')  # for 32-bit version

library(dplyr)
library(openxlsx)
library(ggplot2)
library(rmarkdown)
library(pivottabler)

setwd("C:/Users/Owner/OneDrive - United Nations/SDG_Database")
source("./15. RScripts/R_StatisticalAnnex_Source_20200409.R")

  
  setwd("C:/Users/Zin.Lin/OneDrive - United Nations/SDG_Database") #. Work Computer
  setwd("C:/Users/Owner/OneDrive - United Nations/SDG_Database") #. Home Computer
  #. Data file - saved in the folder
  load("./2. Statistical Annex/2020/0_WorkingFiles_ZL/2020_Q2_AllData_After_20200426.RData")

  getRecentData("2020_Q2_AllData_After_20200414.RData")
  getGeo()
  getDim()
  prepData(df1)
  
  temp <- df11 %>%
    filter(.,Goal == 8, IndicatorCode %!in% c("C200205","C170602"))
  
  
  temp <- df11 %>%
    filter(.,IndicatorCode %in% c("C150501"))
           #TimePeriod %in% c("2000","2005","2010","2015","2018","2019"))

  temp <- df11 %>%
    filter(.,TableID %in% c("C040202-1"))
  
  t <- temp %>%
    filter(.,GeoAreaName == "World", TimePeriod == 2019)
  write.csv(t,"5.6.2_WorldData.csv",row.names = FALSE)
  
      for (j in unique(temp$IndicatorCode)) {
        wb <- createWorkbook(creator = "Zin Lin")
        df12 <- temp %>% filter(.,IndicatorCode == j)
          for (i in unique(df12$TableID)) {
              getTypeSpecificData(i,df12,2) #. Change the decimal points here
              if(dim(df111)[1]==0) next else {
                if(dim(distinct(df111,TableType))[1]>1) {
                  print(paste0("Data table contains multile table type. Reveiw data of ",
                               unique(df111$IndicatorCode)[1]," and TableID: ", i))
                  break
                    } else {
                  if(df111$TableType[1] == 1) {getTableType1()} else {
                    if(df111$TableType[1] == 2) {getTableType2()} else {
                      if(df111$TableType[1] == 3) {getTableType3()} else {
                        if(df111$TableType[1] == 4) {getTableType1()
                          }}}}
                  
                    }
                }
    
          }
        filename <- paste0("./2. Statistical Annex/2020/0_WorkingFiles_ZL/0_ExcelFiles/",j,".xlsx")
        saveWorkbook(wb, file=filename, overwrite = TRUE)
    
      }
  
  write.csv(temp,file = "./2. Statistical Annex/2020/0_WorkingFiles_ZL/0_ExcelFiles/test.csv", row.names = FALSE)
  
  #. Type1 : With the upper/lower bounds
  df111 <- df111 %>%
    mutate(.,UpperBound = round3(.$UpperBound,2),
           LowerBound = round3(.$LowerBound,2),
           Value_Annex = paste(sprintf("%.2f",round3(Value,2))," (",sprintf("%.2f",LowerBound),"-",sprintf("%.2f",UpperBound),")",sep = ""))
  wb <- createWorkbook(creator = "Zin Lin")
  getTableType1()
  filename <- paste0("./2. Statistical Annex/2020/0_WorkingFiles_ZL/0_ExcelFiles/",i,".xlsx")
  saveWorkbook(wb, file=filename, overwrite = TRUE)
  
  #. Type 3: With the upper/lower bounds
  df111 <- df111 %>%
    mutate(.,UpperBound = round3(.$UpperBound,1),
           LowerBound = round3(.$LowerBound,1),
           Value = paste(round3(Value,1)," (",LowerBound,"-",UpperBound,")",sep = ""))
  wb <- createWorkbook(creator = "Zin Lin")
  getTableType3()
  filename <- paste0("./2. Statistical Annex/2020/0_WorkingFiles_ZL/0_ExcelFiles/",i,".xlsx")
  saveWorkbook(wb, file=filename, overwrite = TRUE)
  
  #. Without the footnotes
  df111 <- df111 %>%
    mutate(.,Value_Annex = paste(round3(Value,2),sep = ""))
  wb <- createWorkbook(creator = "Zin Lin")
  getTableType1()
  filename <- paste0("./2. Statistical Annex/2020/0_WorkingFiles_ZL/0_ExcelFiles/",i,".xlsx")
  saveWorkbook(wb, file=filename, overwrite = TRUE)
  
  
  #. Getting source information of all seriesCode
  getAllSource <- df1 %>%
    select(.,Goal,Target,Indicator,SeriesCode,SeriesDescription,Source) %>%
    distinct(.)
  write.csv(getAllSource,file = "./2. Statistical Annex/2020/0_WorkingFiles_ZL/0_ExcelFiles/allSource.csv", row.names = FALSE)
  shell.exec(file = "C:/Users/Owner/OneDrive - United Nations/SDG_Database/2. Statistical Annex/2020/0_WorkingFiles_ZL/0_ExcelFiles/allSource.csv")
    
  #. Downloading the list of data series with data for the Annex
  RegSeries <- df11 %>% select(.,Indicator,IndicatorCode,SeriesCode) %>% unique(.)
  write.csv(RegSeries,"SeriesWithRegionalData.csv",row.names = FALSE)
    