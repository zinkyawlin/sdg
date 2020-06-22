#The following code prepares the data for modelling
Sys.setenv(JAVA_HOME='C:\\Program Files (x86)\\Java\\jre1.8.0_181')  # for 32-bit version


#set working directory
setwd("C:/Users/Zin.Lin/OneDrive - United Nations/SDG_Database/0.2_Data Modelling")


############## downloading Data ###############
#. Downloading data from the 
  sqlname_strpro <<- paste0("select * from dbo.ObservationDnZinternal where ReleaseName = '2018.Q4.G.01'")
  library(RODBC)
  odbcChannel <<- odbcConnect("SDGDB51")
  all_mydata <<- sqlQuery(odbcChannel,sqlname_strpro,as.is = TRUE) #it shouldn't take much to load.


#. SDGLab Data
  odbcChannel <- odbcConnect("SDGDB53")
  sqlname_labdata <- paste0("select * from dbo.DatasetsComparison")
  df <- sqlQuery(odbcChannel,sqlname_labdata,as.is = TRUE) #it shouldn't take much to load.
  odbcCloseAll()  


############## Data Model - SDG Database (SDGCR from 51 Server) ###############

library(dplyr)
library(magrittr)
library(RODBC)
#. load("C:/Users/Zin.Lin/OneDrive - United Nations/SDG_Database/99. Database Copies/all_mydata_ObservationDN_20191219.RData")

drop <- c("ID","ReleaseStatus","ReleaseName","SeriesObservationCount","Freq","GeoAreaCode","GeoAreaName","TimePeriod","Value","ValueType","Time_Detail","Source","ObservationID","UpperBound","LowerBound","TimeCoverage","BasePeriod","FootNote")

t0 <- df %>%
  .[,!(names(.) %in% drop)] %>%
  distinct(.) %>% 
  group_by(SeriesID) %>% 
  summarise_each(funs(toString(unique(.),collaspe = ";")))
openxlsx::write.xlsx(t0,"SDGDB_Model_20200414.xlsx")

#. Pulling Series information from SQL 51
  sqlname_series <- paste("select * from dbo.Series")
  odbcChannel <<- odbcConnect("SDGDB51")
  series <- sqlQuery(odbcChannel,sqlname_series,as.is = TRUE) %>% 
    .[colnames(.) %in% c("ID","SDMXCode","SDMXDescription")] 
  
t1 <- left_join(t0,series,by = c("SeriesID"="ID")) 

write.csv(t1,file = paste0("SDGData_Model_Series_",format(Sys.Date(),"%Y%m%d"),".csv"),row.names = FALSE)


############## Data Model - SDG Database (SDGCR from 51 Server) ###############


#load("C:/Users/Zin.Lin/OneDrive - United Nations/SDG_Database/99. Database Copies/all_mydata_ObservationDN_20191219.RData")

droplab <- c("ID","ReleaseStatus","ReleaseName","SeriesObservationCount","Freq","GeoAreaCode","GeoAreaName","TimePeriod","Value_Global","Value_Country","ValueType","Time_Detail","Source","ObservationID","UpperBound","LowerBound","TimeCoverage","BasePeriod","FootNote","Difference","Source_Global","Source_National","Footnote_Global","Footnote_National")

t0 <- df %>%
  .[,!(names(.) %in% droplab)] %>%
  distinct(.) %>% 
  group_by(SeriesID) %>% 
  summarise_each(funs(toString(unique(.),collaspe = ";")))
openxlsx::write.xlsx(t0,"SDGLab_Model_20200414.xlsx")




