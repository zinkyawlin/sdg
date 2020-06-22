
#The following script produces the data templates for SDMX


Sys.setenv(JAVA_HOME='C:\\Program Files (x86)\\Java\\jre1.8.0_211')  # for 32-bit version
#Work
setwd("C:/Users/Zin.Lin/OneDrive - United Nations/SDG_Database/12. SDMX/3. SDMX Data Templates")
#Home
setwd("C:/Users/Owner/OneDrive - United Nations/SDG_Database/12. SDMX/3. SDMX Data Templates")


library(dplyr)
library(reshape2)
library(RODBC)
library(openxlsx)
odbcChannel <<- odbcConnect("SDGDB51")
AllDataSQL <<- paste0("select * from dbo.ObservationDn_SDMX")
mydf_New <<- sqlQuery(odbcChannel,AllDataSQL,as.is = TRUE) 
save(mydf_New,file = "SDMX_ObservationDN_20191219.RData")
load("SDMX_ObservationDN_20191219.RData")
mydf <- mydf_New

########## filtering data by release info ################
#. f_mydata <- dplyr::filter(mydf,ReleaseName=="2019.Q3.G.01")




#. The following pulls the dimension values information from 51 server.
sqlname_dimval <<- paste("select * from dbo.DimensionValue")
library(RODBC)
odbcChannel <<- odbcConnect("SDGDB51")
dim_values <<- sqlQuery(odbcChannel,sqlname_dimval,as.is = TRUE) #it shouldn't take much to load.

#the following gives you dimension
sqlname_dimnames <<- paste("select * from dbo.Dimension")
library(RODBC)
odbcChannel <<- odbcConnect("SDGDB51")
dim_names <<- sqlQuery(odbcChannel,sqlname_dimnames,as.is = TRUE) #it shouldn't take much to load.

dim_info <- dplyr::left_join(dim_values,dim_names,by=c("DimensionID"="ID"))
colnames(dim_info)[6] <- "SDMX_DimensionValue"
colnames(dim_info)[9] <- "Dimension_Name"
colnames(dim_info)[10] <- "SDMX_Dimension"
dim_infofinal <- dplyr::select(dim_info,ID,Code,Description,DimensionID,Dimension_Name,SDMX_Dimension,SDMX_DimensionValue)
save(dim_infofinal,file = "dim_infofinal.RData")
load(file = "dim_infofinal.RData")
#write.csv(dim_infofinal,"dim_info_20181217.csv")


#. Adding the regional information
GeoInfo <<- xlsx::read.xlsx("C:/Users/Zin.Lin/OneDrive - United Nations/SDG_ReferenceFiles/ReferenceArea_20190128.xlsx",sheetName="SDG_Geos") #create a table containing series used in country profiles

#. The following prepares mapping file including DB Codes, SDMX Codes and Agency Bloc numbers.
#. Work
DB_DSD <- openxlsx::read.xlsx("C:/Users/Zin.Lin/OneDrive - United Nations/SDG_Database/0.2_Data Modelling/SDGData_Modeling.xlsx",sheet = 1) #create a table containing series used in country profiles
#. Home
DB_DSD <- openxlsx::read.xlsx("C:/Users/Owner/OneDrive - United Nations/SDG_Database/0.2_Data Modelling/SDGData_Modeling.xlsx",sheet = 1) #create a table containing series used in country profiles

#Work
UNIT_MAP <- openxlsx::read.xlsx("C:/Users/Zin.Lin/OneDrive - United Nations/SDG_Database/0.2_Data Modelling/DB_SDMX_UNIT_Mappings_20191212.xlsx",sheet = 1) #create a table containing unit mappings (DB vs SDMX wtih unit mult) used in country profiles
#Home
UNIT_MAP <- openxlsx::read.xlsx("C:/Users/Owner/OneDrive - United Nations/SDG_Database/0.2_Data Modelling/DB_SDMX_UNIT_Mappings_20191212.xlsx",sheet = 1) #create a table containing unit mappings (DB vs SDMX wtih unit mult) used in country profiles


Agency_DBSeries <- DB_DSD[colnames(DB_DSD) %in% c("DB_SeriesCode","Abloc")] %>% unique(.)

UNIT_MAP_1 <- UNIT_MAP %>%
  filter(.,IsDSDCode == 1) %>%
  select(.,DEC_SDMX,SDMX_Code,UNIT_MULT_UPDATE) %>%
  unique(.)

  #. Pulling Series information from SQL 51
  sqlname_series <- paste("select * from dbo.Series")
  odbcChannel <<- odbcConnect("SDGDB51")
  series <- sqlQuery(odbcChannel,sqlname_series,as.is = TRUE) %>% 
    .[colnames(.) %in% c("ID","Code","SDMXCode","isDSDSeries")] %>%
    filter(.,isDSDSeries == 1)
  save(series,file = "series.RData")
  write.csv(series,"51_series_20191213.csv")
  load(file = "series.RData")
  
  Mappings <- left_join(Agency_DBSeries,series,by = c("DB_SeriesCode" = "Code")) %>% 
    unique(.) %>%
    filter(.,!is.na(ID))

    #. testing to see if codes matches.
    SDMXSeries <- mydf %>%
      select(.,SeriesCode,SeriesDesc) %>%
      unique(.)
    SDMXSeriesMapped <- left_join(SDMXSeries,Mappings,by = c("SeriesCode" = "SDMXCode"))
    Comparison <- full_join(SDMXSeries,SDMXSeriesMapped,by = c("SeriesCode" = "SeriesCode"))
    write.csv(Comparison,"Comparison.csv")
    
  
############## Removing the columns that are not needed ###############
  '%!in%' <- Negate('%in%')
    
            #. test for unit measure
            test_unit <- mydf_1 %>%
              select(.,UNIT_MEASURE) %>%
              unique(.) %>%
              full_join(.,UNIT_MAP_1,by = c("UNIT_MEASURE"="DEC_SDMX")) %>%
              filter(.,is.na(SDMX_Code)) %>%
              print(.)
  
  mydf_0 <- mydf %>%
    left_join(.,UNIT_MAP_1,by = c("UNIT_MEASURE"="DEC_SDMX"))
    
                #. test again to see if there is any NA Unit
              test_mydf_0 <- mydf_0 %>%
                filter(.,is.na(SDMX_Code)) %>%
                print(.) 
              test_mydf_0_2 <- mydf_0 %>%
                select(.,UNIT_MEASURE,SDMX_Code,UNIT_MULT_UPDATE) %>%
                unique(.)
                

              
  mydf_1 <- mydf_0[,colnames(mydf_0) %!in% c("ID","ValueType","ObservationID","Data_Last_Update_Date","GEO_INFO_TYPE","UNIT_MEASURE","UNIT_MULT","FREQ","REPORTING_TYPE","Goal","Target","OBS_STATUS")] %>%
    left_join(.,Mappings,by = c("SeriesCode"="SDMXCode"))
  

  
  save(mydf_1,file = "SDMXDataForTemplates_20191218.RData")
  
  omit <- c("DB_SeriesCode","Abloc","ID","isDSDSeries","")
  
  #. Renaming the columns to SDMX var name
  names(mydf_1)[names(mydf_1) == "TimePeriod"] <- "TIME_PERIOD"
  names(mydf_1)[names(mydf_1) == "Time_Detail"] <- "TIME_DETAIL"
  names(mydf_1)[names(mydf_1) == "SeriesCode"] <- "SERIES"
  names(mydf_1)[names(mydf_1) == "GeoAreaCode"] <- "REF_AREA"
  names(mydf_1)[names(mydf_1) == "Source"] <- "SOURCE_DETAIL"
  names(mydf_1)[names(mydf_1) == "FootNote"] <- "COMMENT_OBS"
  names(mydf_1)[names(mydf_1) == "TimeCoverage"] <- "TIME_COVERAGE"
  names(mydf_1)[names(mydf_1) == "Value"] <- "OBS_VALUE"
  names(mydf_1)[names(mydf_1) == "UpperBound"] <- "UPPER_BOUND"
  names(mydf_1)[names(mydf_1) == "LowerBound"] <- "LOWER_BOUND"
  names(mydf_1)[names(mydf_1) == "BasePeriod"] <- "BASE_PER"
  names(mydf_1)[names(mydf_1) == "SDMX_Code"] <- "UNIT_MEASURE"
  names(mydf_1)[names(mydf_1) == "UNIT_MULT_UPDATE"] <- "UNIT_MULT"

# # # to rename the columns according to the DSD
  #. unlink("SDMX_Templates", recursive = TRUE)
  #. dir.create("SDMX_Templates")
  
  for (m in unique(mydf_1$Abloc)){
    tname <-  paste0("SDMXTemplates_",m)
    dir.create(tname)
  }
  
  
for (i in unique(mydf_1$SERIES)) {
  s <- dplyr::filter(mydf_1,SERIES == i)
  if(length(s[,1])<1) next
  #. the following deals with naming multi-purpose indicators.
  if(length(unique(s$Indicator))>1) {
    t1 <- data.frame(unique(s$Indicator))
    t2 <- t1 %>% summarise_each(funs(toString(unique(.),collaspe = ";")))
    s$Indicator  <- t2$unique.s.Indicator.[1]
    s <- unique(s)}
    
    #. Preparing file names.
      filename <<- paste("./SDMXTemplates_",unique(s$Abloc)[1],"/",unique(s$Abloc)[1],"-",unique(s$Indicator)[1],"-",unique(s$SERIES)[1],"-",length(s$Indicator),".xlsx",sep = "") #. filename for xlsx file.
      filenameR <<- paste("./SDMX_Templates/",unique(s$Abloc)[1],"-",unique(s$Indicator)[1],"-",unique(s$SERIES)[1],".RData",sep = "") #. filename for RData file.
        #. removing the columns no longer needed.
      
    #. Preparing the file which includes removing columns with (1) NAs and (2) only _T
      s_1 <- s[,colSums(is.na(s)) <nrow(s)] %>% #removing columns with NAs
      .[,colSums(. == "_T",na.rm = TRUE)<nrow(.)] %>%
      .[,colnames(.) %!in% omit] #removing columns with only _T
      
    #. Preparing the parameter sheet.
      #. Creating an matrix.
      Parameters <- matrix(0,ncol = 7, nrow = 24)
      Parameters <- data.frame(Parameters)
      colnames(Parameters) <- c("Element", "Type", "PosType", "Position", "", "DataStart", paste0(LETTERS[grep("OBS_VALUE",colnames(s_1))],2) )
      
      #. The following is the initial set up of parameter matrix. This will be replaced with outputs from the loop directly below.
      #. 1. This list needs to be updated whenever there is a change in DSD-Dimension list (or) the attribute list.
      Parameters [1,] <- 	c("FREQ", "DIM", "FIX", "A", NA, "NumColumns",1)
      Parameters [2,] <- 	c("REPORTING_TYPE", "DIM", "FIX", "G", NA, NA,NA)
      Parameters [3,] <- 	c("SERIES", "DIM", "COLUMN", "C", NA, NA,NA)
      Parameters [4,] <- 	c("REF_AREA", "DIM", "COLUMN", "F", NA, NA,NA)
      Parameters [5,] <- 	c("TIME_PERIOD", "DIM", "COLUMN", "G", NA, NA,NA)
      Parameters [6,] <- 	c("SEX", "DIM", "COLUMN", "K", NA, NA,NA)
      Parameters [7,] <- 	c("AGE", "DIM", "COLUMN", "I", NA, NA,NA)
      Parameters [8,] <- 	c("URBANISATION", "DIM", "FIX", "_T", NA, NA,NA)
      Parameters [9,] <- 	c("INCOME_WEALTH_QUANTILE", "DIM", "FIX", "_T", NA, NA,NA)
      Parameters [10,] <- 	c("EDUCATION_LEV", "DIM", "FIX", "_T", NA, NA,NA)
      Parameters [11,] <- 	c("OCCUPATION", "DIM", "FIX", "_T", NA, NA,NA)
      Parameters [12,] <- 	c("CUST_BREAKDOWN", "DIM", "FIX", "_T", NA, NA,NA)
      Parameters [13,] <- 	c("COMPOSITE_BREAKDOWN", "DIM", "FIX", "_T", NA, NA,NA)
      Parameters [14,] <- 	c("DISABILITY_STATUS", "DIM", "FIX", "_T", NA, NA,NA)
      Parameters [15,] <- 	c("PRODUCT", "DIM", "FIX", "_T", NA, NA,NA)
      Parameters [16,] <- 	c("ACTIVITY", "DIM", "FIX", "_T", NA, NA,NA)
      Parameters [17,] <- 	c("NATURE", "ATT", "FIX", "_X", NA, NA,NA)
      Parameters [18,] <- 	c("SOURCE_DETAIL", "ATT", "COLUMN", "M", NA, NA,NA)
      Parameters [19,] <- 	c("COMMENT_OBS", "ATT", "SKIP", "", NA, NA,NA)
      Parameters [20,] <- 	c("UNIT_MEASURE", "ATT", "COLUMN", "D", NA, NA,NA)
      Parameters [21,] <- 	c("UNIT_MULT", "ATT", "FIX", "0", NA, NA,NA)
      Parameters [22,] <- 	c("TIME_DETAIL", "ATT", "COLUMN", "G", NA, NA,NA)
      Parameters [23,] <- 	c("UPPER_BOUND", "ATT", "SKIP", "", NA, NA,NA)
      Parameters [24,] <- 	c("LOWER_BOUND", "ATT", "SKIP", "", NA, NA,NA)
      
      #. If a dim (if it is a dim) is not available in template, then "PosType" is FIX, "Position" is _T
      for (h in Parameters[,1]){
        if(h %in% colnames(s_1)){
          Parameters[grep(h,Parameters[,1]),3] <- "COLUMN"
          Parameters[grep(h,Parameters[,1]),4] <- LETTERS[grep(h,colnames(s_1))]} else {
            if(h %in% Parameters[which(Parameters$Type == "ATT"),][,1]){
            Parameters[grep(h,Parameters[,1]),3] <- "SKIP"
            Parameters[grep(h,Parameters[,1]),4] <- ""} else {
              for (h in Parameters[which(Parameters$Type == "DIM"),][,1]){
                if(h == "FREQ") {
                  Parameters[grep(h,Parameters[,1]),3] <- "FIX"
                  Parameters[grep(h,Parameters[,1]),4] <- "A"} else {
                    if(h == "REPORTING_TYPE"){
                      Parameters[grep(h,Parameters[,1]),3] <- "FIX"
                      Parameters[grep(h,Parameters[,1]),4] <- "G"} else {
                        Parameters[grep(h,Parameters[,1]),3] <- "FIX"
                        Parameters[grep(h,Parameters[,1]),4] <- "_T"} 
              }
            }
          }
        }
      }

  #. Preparing for Excel Output
  wb <- createWorkbook(filename)
  addWorksheet(wb,"Data")
  writeData(wb, sheet = "Data", s_1, colNames=TRUE, rowNames=FALSE, keepNA=FALSE)
  addWorksheet(wb,"Parameters")
  writeData(wb,sheet = "Parameters", Parameters, colNames=TRUE, rowNames=FALSE, keepNA=FALSE)
  saveWorkbook(wb,filename, overwrite = TRUE)
  #save(s_rev, file = filenameR)
 
}

#Main loop ends.


#. The following produces the mapping between the DB Series code vs. DSD Series code.
  
  #. Obtaining the agency bloc code and series mapping
  DB_DSD <- openxlsx::read.xlsx("C:/Users/Zin.Lin/OneDrive - United Nations/SDG_Database/0.2_Data Modelling/SDGData_Modeling.xlsx",sheet = 1) #create a table containing series used in country profiles
  #. Home
  DB_DSD <- openxlsx::read.xlsx("C:/Users/Owner/OneDrive - United Nations/SDG_Database/0.2_Data Modelling/SDGData_Modeling.xlsx",sheet = 1) #create a table containing series used in country profiles
  
  Agency_DBSeries <- DB_DSD[colnames(DB_DSD) %in% c("DB_SeriesCode","Abloc","IndicatorCode")] %>% 
    unique(.) %>%
    filter(.,!is.na(Abloc))
  
  map_sr <- mydf_1 %>%
    select(.,SERIES,DB_SeriesCode,Abloc) %>%
    unique(.) %>%
    full_join(.,Agency_DBSeries,by = c("DB_SeriesCode"="DB_SeriesCode","Abloc" = "Abloc")) %>%
    filter(.,!is.na(SERIES))

  #. load("all_mydata_ObservationDN_20191219.RData")
  #. mydf_DB <- mydf
  
  mydf_DB_1 <- mydf_DB %>%
    select(.,SeriesCode,isDSDSeries) %>%
    distinct(.)

  map_sr_1 <- map_sr %>%
    full_join(.,mydf_DB_1,by = c("DB_SeriesCode"="SeriesCode"))

