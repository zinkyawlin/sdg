# what this script does:
# 1) it creates whole database backup in csv
# 2) it creates backup excel files by SeriesID (with footnotes attached


#Work
setwd("C:/Users/Zin.Lin/OneDrive - United Nations/SDG_Database/0.1_Data Request Preparations/Excel_Templates")

#home
setwd("C:/Users/Owner/OneDrive - United Nations/SDG_Database/0.1_Data Request Preparations/Excel_Templates")

library(RODBC)
library(dplyr)
library(openxlsx)
library(stringr)
library(testit)
    #. Downloading data from SQL Sever 51.
    odbcChannel <- odbcConnect("SDGDB51")
    sqlname_strpro <- paste0("select * from dbo.ObservationDn")
    mydf <- sqlQuery(odbcChannel,sqlname_strpro,as.is = TRUE) #it shouldn't take much to load.
    save(mydf, file = "all_mydata_ObservationDN_20191219.RData")
    load("C:/Users/Zin.Lin/OneDrive - United Nations/SDG_Database/SDG_Database/99. Database Copies/all_mydata_ObservationDN_20191219.RData")


    #function for changing data type to numeric
    num = function(x) {
      require(testit)
      if(has_warning(as.numeric(as.character(x)))){x}else
      {as.numeric(as.character(x))}
    } #change the data type to numeric
    
    mydf[,"Value"] <- num(mydf$Value) # converting the value to numeric
    mydf$Value[is.na(mydf$Value)] <- "NA" # making sure that NA are displayed in the Excel template.
    
    #. Series Mapping between SeriesCode (DB) and SeriesID
    series_map <- mydf %>%
      select(.,SeriesCode,SeriesID) %>%
      unique(.)
    
    

    #. the following table gives dimensions and dimension values
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
      load("dim_infofinal.RData")
      write.csv(dim_infofinal,"dim_info_20191218.csv")
      
    #. Obtaining the geo information
      GeoInfo <- openxlsx::read.xlsx("C:/Users/Owner/OneDrive - United Nations/SDG_Database/0. SDG_Reference_Files/SDG_ReferenceArea_Final.xlsx",sheet = 2) #create a table containing series used in country profiles
      SDG_GEO <- GeoInfo %>%
        select(.,M49,Ref_Area_Type) %>%
        unique(.)
      
    #. Obtaining the agency bloc code and series mapping
      DB_DSD <- openxlsx::read.xlsx("C:/Users/Zin.Lin/OneDrive - United Nations/SDG_Database/0.2_Data Modelling/SDGData_Modeling.xlsx",sheet = 1) #create a table containing series used in country profiles
      #. Home
      DB_DSD <- openxlsx::read.xlsx("C:/Users/Owner/OneDrive - United Nations/SDG_Database/0.2_Data Modelling/SDGData_Modeling.xlsx",sheet = 1) #create a table containing series used in country profiles
      
      Agency_DBSeries <- DB_DSD[colnames(DB_DSD) %in% c("DB_SeriesCode","Abloc")] %>% 
        unique(.) %>%
        full_join(.,series_map,by = c("DB_SeriesCode"="SeriesCode")) %>%
        filter(.,!is.na(Abloc) & !is.na(SeriesID))
      
    #. Preparing data
      '%!in%' <- Negate('%in%')
      mydf_xls <- dplyr::filter(mydf,ReleaseName=="2019.Q4.G.01")
      omit <- c("ID","ReleaseStatus","ReleaseName","SeriesCode","isDSDSeries","SeriesObservationCount","ValueType","TimeCoverage","BasePeriod","ObservationID","Freq","Goal","Target")
      mydf_xls_1 <- mydf_xls[,colnames(mydf_xls) %!in% omit] %>%
        left_join(.,Agency_DBSeries,by = c("SeriesID"="SeriesID"))

      save(mydf_xls_1,file = "DataForTemplates_20191212.RData")
      #load("DataForTemplates_20181217.RData")

      #. Creating folders for each data agency bloc
      for (n in unique(mydf_1$Abloc)){
        mname <-  paste0("C:/Users/Owner/OneDrive - United Nations/SDG_Database/0.1_Data Request Preparations/DataRequestDocs_",n)
        dir.create(mname)
        setwd(mname)
        sname <-  paste0("SDMXTemplates_",n)
        ename <- paste0("ExcelTemplates_",n)
        dir.create(sname)
        dir.create(ename)
        
      }
      
      
      for (i in unique(mydf_xls_1$SeriesID)) {
        s <- mydf_xls_1 %>% 
          filter(.,SeriesID == i)
        if(length(unique(s$Indicator))>1) 
            { t1 <- data.frame(unique(s$Indicator))
              t2 <- t1 %>% summarise_each(funs(toString(unique(.),collaspe = ";")))
              s$Indicator  <- t2$unique.s.Indicator.[1]
              s <- unique(s)}
        
        #. Preparing file names.
        wd <- paste0("C:/Users/Owner/OneDrive - United Nations/SDG_Database/0.1_Data Request Preparations/DataRequestDocs_")
        filename <<- paste(wd,unique(s$Abloc)[1],"./ExcelTemplates_",unique(s$Abloc)[1],"/",unique(s$Abloc)[1],"-",unique(s$Indicator)[1],"-",unique(s$SeriesID)[1],"-",unique(s$DB_SeriesCode)[1],"-",length(s$Indicator),".xlsx",sep = "") #. filename for xlsx file.
        filenameR <<- paste(unique(s$Abloc)[1],"-",unique(s$Indicator)[1],"-",unique(s$SeriesID)[1],".RData",sep = "") #. filename for RData file.
        
        s <- s[,colnames(s) %!in% c("Abloc")]
        names(s)[names(s) == "DB_SeriesCode"] <- "SeriesCode"
        
        wb <- createWorkbook(filename)
        if(all(is.na(s$Nature))){s$Nature[is.na(s$Nature)] <- ""} #replacing NAs from nature column with blanks 
        if(all(is.na(s$Source))) {s$Source[is.na(s$Source)] <- ""} # replacing NAs from Source column with blanks
        s_rev <- s[,colSums(is.na(s))<nrow(s)] #Removing blank columns

        #. Preparing for Excel Output
        wb <- createWorkbook(filename)
        addWorksheet(wb, "Data")
        openxlsx::writeData(wb, sheet = "Data", s_rev, colNames=TRUE, rowNames=FALSE, keepNA=FALSE)
        
        X <- paste0("Units")
        Unit <- unique(s$Units)
        for (h in colnames(s_rev))
          { for (k in unique(dim_infofinal$Dimension_Name))
              {
              if(h==k) 
                { DimName <- ifelse(nchar(k)>30,print(str_trunc(k,30)),print(k)) #. just right-truncating text (>31char) for Excel sheet name. 
                  DimData <- dplyr::filter(dim_infofinal,Dimension_Name==k)
                  if(k==X) {DimData <- filter(DimData,Code == Unit)}
                  DimData <- DimData[,c(2:3)]
                  addWorksheet(wb,DimName)
                  openxlsx::writeData(wb, sheet= DimName, DimData, colNames=TRUE, rowNames=FALSE, keepNA=FALSE)
              }
          }
        }
        saveWorkbook(wb,filename, overwrite = TRUE)
      }





