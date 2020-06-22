#. Downloading SDGCR Tables to RData File

Sys.setenv(JAVA_HOME='C:\\Program Files (x86)\\Java\\jre1.8.0_211')  # for 32-bit version

library(dplyr)
library(reshape2)
library(RODBC)
library(openxlsx)
library(compareDF)
    



    
    library(RODBC)
    odbcChannel <<- odbcConnect("SDGDB51")
    #. Downloading User Statistics
    sqlname_userstat <<- paste("select * from dbo.BI_User")
    user_51 <- sqlQuery(odbcChannel,sqlname_userstat,as.is = TRUE) #it shouldn't take much to load.
    save(download,file = "C:/Users/Zin.Lin/OneDrive - United Nations/SDG_Database/0.3_SDGCR_Tables/dim_names.RData")
    write.csv(user,"C:/Users/Zin.Lin/OneDrive - United Nations/SDG_Database/0.3_SDGCR_Tables/users_53.csv",row.names = FALSE)
    
    #. Download Statistics
    sqlname_dlstat <<- paste("select * from dbo.BI_Download")
    download_51 <- sqlQuery(odbcChannel,sqlname_dlstat,as.is = TRUE) #it shouldn't take much to load.
    save(download,file = "C:/Users/Zin.Lin/OneDrive - United Nations/SDG_Database/0.3_SDGCR_Tables/dim_names.RData")
    write.csv(download,"C:/Users/Zin.Lin/OneDrive - United Nations/SDG_Database/0.3_SDGCR_Tables/downloads_53.csv",row.names = FALSE)
    
    user <- rbind(user_51,user_53) %>%
      unique(.)
    write.csv(user,"C:/Users/Zin.Lin/OneDrive - United Nations/SDG_Database/0.3_SDGCR_Tables/user.csv",row.names = FALSE)
    
    download <- rbind(download_51,download_53) %>%
      unique(.)
    write.csv(download,"C:/Users/Zin.Lin/OneDrive - United Nations/SDG_Database/0.3_SDGCR_Tables/downloads.csv",row.names = FALSE)


    odbcChannel <<- odbcConnect("SDGDB51")
    #. Dimension Names
    sqlname_dimnames <<- paste("select * from dbo.Dimension")
    dim_names <<- sqlQuery(odbcChannel,sqlname_dimnames,as.is = TRUE) #it shouldn't take much to load.
    save(dim_names,file = "C:/Users/Zin.Lin/OneDrive - United Nations/SDG_Database/0.3_SDGCR_Tables/dim_names.RData")
    write.csv(dim_names,"C:/Users/Zin.Lin/OneDrive - United Nations/SDG_Database/0.3_SDGCR_Tables/dim_names.csv",row.names = FALSE)
    
    #. Dimension Value Names
    sqlname_dimval <<- paste("select * from dbo.DimensionValue")
    dim_val <<- sqlQuery(odbcChannel,sqlname_dimval,as.is = TRUE) #it shouldn't take much to load.
    save(dim_val,file = "C:/Users/Zin.Lin/OneDrive - United Nations/SDG_Database/0.3_SDGCR_Tables/dim_val.RData")
    
    #. Geography Table
    sqlname_geo <<- paste("select * from dbo.Geography")
    geo <<- sqlQuery(odbcChannel,sqlname_geo,as.is = TRUE) #it shouldn't take much to load.
    save(geo,file = "C:/Users/Zin.Lin/OneDrive - United Nations/SDG_Database/0.3_SDGCR_Tables/geo.RData")
    write.csv(geo,"C:/Users/Zin.Lin/OneDrive - United Nations/SDG_Database/0.3_SDGCR_Tables/geo.csv",row.names = FALSE)
    
    #. ObservationDN
    sqlname_strpro <- paste0("select * from dbo.ObservationDn")
    mydf <- sqlQuery(odbcChannel,sqlname_strpro,as.is = TRUE) #it shouldn't take much to load.
    save(mydf, file = "C:/Users/Zin.Lin/OneDrive - United Nations/SDG_Database/0.3_SDGCR_Tables/all_mydata_ObservationDN_20191219.RData")
    
    #. Indicator vs. series
    library(RODBC)
    odbcChannel <<- odbcConnect("SDGDB51")
    #. indicator table
    sqlname_ind <<- paste("select * from dbo.Indicator")
    ind <- sqlQuery(odbcChannel,sqlname_ind,as.is = TRUE) 
    
    #. target table
    sqlname_tg <<- paste("select * from dbo.Target")
    tg <- sqlQuery(odbcChannel,sqlname_tg,as.is = TRUE) 
    tg <- tg[,c("ID","Code","Description")]
    
    #. series table
    sqlname_sr <<- paste("select * from dbo.Series")
    sr <- sqlQuery(odbcChannel,sqlname_sr,as.is = TRUE) 
    tg <- tg[,c("ID","Code","Description")]
    
    #. targetIndicator table
    sqlname_tgind <<- paste("select * from dbo.TargetIndicator")
    tgind <- sqlQuery(odbcChannel,sqlname_tgind,as.is = TRUE) 
    tgind <- tgind[,c("TargetID","IndicatorID","RefCode")]
    
    #. targetIndicator table
    sqlname_indsr <<- paste("select * from dbo.IndicatorSeries")
    indsr <- sqlQuery(odbcChannel,sqlname_indsr,as.is = TRUE) 
    indsr <- indsr[,c("IndicatorID","SeriesID")]
    
    #. SeriesVersion table
    odbcChannel <<- odbcConnect("SDGDB51")
    sqlname_srver <<- paste("select * from dbo.SeriesVersion")
    srver <- sqlQuery(odbcChannel,sqlname_srver,as.is = TRUE) 
    srver <- srver[,c("SeriesID","VersionID","observationsCount")] %>%
      filter(.,VersionID == 15)
    write.csv(srver,"C:/Users/Zin.Lin/OneDrive - United Nations/SDG_Database/0.3_SDGCR_Tables/SeriesVersion15.csv",row.names = FALSE)
    odbcCloseAll()
    
    #. Target-Indicator-Series Table
    tgindsr <- indsr %>%
      full_join(.,tgind,by = "IndicatorID") %>%
      left_join(.,ind, by = c("IndicatorID" = "ID")) %>% 
      full_join(.,sr, by = c("SeriesID" = "ID"))
write.csv(tgindsr,"C:/Users/Zin.Lin/OneDrive - United Nations/SDG_Database/0.3_SDGCR_Tables/tg-ind-sr.csv",row.names = FALSE)  
    odbcCloseAll()
    
    #. Release - Series Version Table
    odbcChannel <<- odbcConnect("SDGDB51")
    sqlname_rlsrver <<- paste("select * from dbo.ReleaseSeriesVersion")
    rlsrver <- sqlQuery(odbcChannel,sqlname_rlsrver,as.is = TRUE) 
    rlsrver <- rlsrver[,c("SeriesID","VersionID","ReleaseID")] %>%
      filter(.,ReleaseID == 13)
    write.csv(rlsrver,"C:/Users/Zin.Lin/OneDrive - United Nations/SDG_Database/0.3_SDGCR_Tables/ReleaseSeriesVersion15.csv",row.names = FALSE)
    
    #. GeoGroups - GeoGraphies Version Table
    odbcChannel <<- odbcConnect("SDGDB51")
    sqlname_geo <<- paste("select * from dbo.Geography")
    geography <- sqlQuery(odbcChannel,sqlname_geo,as.is = TRUE) 
    
    sqlname_geogrp <<- paste("select * from dbo.GeoGroups")
    geogroups <- sqlQuery(odbcChannel,sqlname_geogrp,as.is = TRUE) 
    
    sqlname_geogeogrp <<- paste("select * from dbo.GeographiesGeoGroups")
    geo_geogroups <- sqlQuery(odbcChannel,sqlname_geogeogrp,as.is = TRUE) 
    
    geo_geogroups_1 <- geo_geogroups %>%
      full_join(.,geography[,c("ID","M49","Name","inDSD")],by = c("GeographyID" = "ID")) %>%
      full_join(.,geogroups[,c("ID","Description")], by = c("GeoGroupID" = "ID"))
    write.(geo_geogroups_1,"C:/Users/Zin.Lin/OneDrive - United Nations/SDG_Database/0.3_SDGCR_Tables/Geography_GeoGroup.csv",row.names = FALSE)
    wb <- createWorkbook(creator = Sys.getenv("USERNAME"))
    addWorksheet(wb, "geo_geogroups")
    addWorksheet(wb, "geogroups")
    addWorksheet(wb, "geo_geogroups_1")
    addWorksheet(wb, "geography")
    writeData(wb,sheet = "geo_geogroups", geo_geogroups)
    writeData(wb,sheet = "geogroups", geogroups)
    writeData(wb,sheet = "geography", geography)
    writeData(wb,sheet = "geo_geogroups_1", geo_geogroups_1)
    saveWorkbook(wb, file=paste0("C:/Users/Zin.Lin/OneDrive - United Nations/SDG_Database/0.3_SDGCR_Tables/Geography_GeoGroup_",Sys.Date(),".xlsx"), overwrite = TRUE)    
    
 
