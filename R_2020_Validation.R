
Sys.setenv(JAVA_HOME='C:\\Program Files (x86)\\Java\\jre1.8.0_211')  # for 32-bit version

library(dplyr)
library(reshape2)
library(RODBC)
library(openxlsx)
library(tidyr)
library(pivottabler)
library(lubridate)
library(htmlTable)
library(ggplot2)
library(RColorBrewer)

#. 2020 update.
  #. Only SQL table or Excel downloads from Processing System will be used in this year's processing.

#set working directory
#setwd("C:/Users/Zin.Lin/OneDrive - United Nations/SDG_Database")
setwd("C:/Users/Owner/OneDrive - United Nations/SDG_Database")

################### Run these functions first ####################

    #function for changing data type to numeric
#function for changing data type to numeric
        num = function(x) {
          require(testit)
          if(has_warning(as.numeric(as.character(x)))){x}else
          {as.numeric(as.character(x))}
        } #change the data type to numeric
        
        # function for displaying elements from a list
        display_function <- function(x){
            t <- paste0("(")
            y <- na.omit(unique(x))
          for (i in 1:length(y)){
            t <- paste0(t,y[i],ifelse(i==max(length(y)),")",","))
        
          }
            return(t)
        }
        
        #Function for "rounding in commerce" - "kaufmännische Rundung"
        round2 = function(x, n) {
          posneg = sign(x)
          z = abs(x)*10^n
          z = z + 0.5
          z = trunc(z)
          z = z/10^n
          z*posneg
        }
        
  
  ########################################################
    '%!in%' <- Negate('%in%')  
    #. And loading the Geo Area Info
      #. The Excel file contains all ref area in use for SDG monitoring - and is regularly updated.
    
    getGeo = function(){
    GeoInfo <<- openxlsx::read.xlsx("./0. SDG_Reference_Files/SDG_ReferenceArea_20191105_AM.xlsx",sheet = 1) #create a table containing series used in country profiles
    SDG_GEO <<- GeoInfo %>%
      select(.,M49,Ref_Area_Type,Annex.Sequence) %>%
      unique(.)} #. Getting geo information.
    
    #. Loading Dimension list and Dimension Values List
      
    getDim = function(){
      require(testit)
      require(RODBC)
      odbcChannel <- odbcConnect("SDGDB51")  
      sqlname_dv <- paste0("select Code, Description from dbo.DimensionValue")
      if(has_error(sqlQuery(odbcChannel,sqlname_dv,as.is = TRUE), silent = interactive())){
        load("./0.3_SDGCR_Tables/dim_names.RData")
        Dims <<- dim_names
        colnames(Dims)[2] <- "DimensionNames"
        DimsF <<- Dims %>%
          filter(.,isAttribute == 0,DimensionNames %!in% c("Freq","Reporting Type"))
      } else {
        DimData <<- sqlQuery(odbcChannel,sqlname_dv,as.is = TRUE) #it shouldn't take much to load.  
        colnames(DimData)[2] <- "DimensionValueName"
        
        
        sqlname_dim <- paste0("select * from dbo.Dimension")
        Dims <- sqlQuery(odbcChannel,sqlname_dim,as.is = TRUE) #it shouldn't take much to load.  
        colnames(Dims)[2] <- "DimensionNames"
        DimsF <<- Dims %>%
          filter(.,isAttribute == 0,DimensionNames %!in% c("Freq","Reporting Type"))}
      }

    #. function to load Excel files downloaded from the Processing System.
    getDataExcel = function(previous,current){
      paste0("to develop this feature when the Excel download option becomes available.")
    }
    
    
    #. function to load data from the SQL download.
      #. Description of how to prepare these RData file.
        #. 1. Run stored procedures in SDGCR51 on 51 server.
        #. 2. Pull the ObservationDN table to R and save the file as a RData file in ./9. Database Updates + History + Archives/Archive folder.
    
    getDataSQL = function(previous,current){
      p_filename = paste0("./9. Database Updates + History + Archives/Archive/",previous)
      c_filename = paste0("./9. Database Updates + History + Archives/Archive/",current)
        #. function loading data into a set dataset name.
        loadRData <- function(fileName){
          #loads an RData file, and returns it
          load(fileName)
          get(ls()[ls() != "fileName"])
        }
        
        df0 <<- loadRData(p_filename)
        df1 <<- loadRData(c_filename)
        
                #. preparing (and filtering by series) the data from previous version.
                #df00 <- df0 %>%
                  #filter(.,SeriesCode == "SI_POV_EMP1") %>%
                  #left_join(.,SDG_GEO,by = c("GeoAreaCode" = "M49"))
                  
                #. preparing new data.
                #df11 <- df1 %>% #. This is a data set of 
                  #filter(.,SeriesCode == "SI_POV_EMP1") %>%
                  #left_join(.,SDG_GEO,by = c("GeoAreaCode" = "M49"))
            
    }    
    #getDataSQL("2019_Q4_AllData_After_20191231.RData","2020_Q1_AllData_After_20200313.RData")
    
    #. function for creating old and new data files by series. SDG Geo input is required.
          sfilter = function(x) { #. x for SeriesCode
            df00 <<- df0 %>% filter(.,SeriesCode == x) %>% left_join(.,SDG_GEO,by = c("GeoAreaCode" = "M49"))
            df11 <<- df1 %>% filter(.,SeriesCode == x) %>% left_join(.,SDG_GEO,by = c("GeoAreaCode" = "M49"))
          }
          
      #sfilter("SI_POV_EMP1")
    
  #. Functions for Dimension Identifier Code      
          dimid <- function(.){
            paste0(ifelse(is.na(.),"",.))
          }
          
          dimid2 <- function(x){
            tempdim <- paste0("")
            for (i in na.omit(match(DimsF[,2],names(x)))) {
              print(names(x)[i])
              tempdim <- paste0(tempdim,dimid(x[,i]),sep = "")
              #print(tempdim)
            }
            return(tempdim)
          }
      
  #. Function to check whether data are avilable. if not, provide the message.
          s_status = function(x) {
            if(nrow(x)==0)
            {print(paste0("No data. Validation terminated."))} else
            {   if(identical(x,df00)) {
              df000 <<- df00 %>%
                ungroup() %>%
                .[,colSums(is.na(.))<nrow(.)] %>%
                mutate(.,Dimidentifier = dimid2(x)) %>%
                mutate(.,R_ID = paste0(.$GeoAreaCode,.$TimePeriod,.$Dimidentifier,sep = "")) %>%
                mutate(.,Annex.Sequence = ifelse(c("Annex.Sequence") %in% names(.),Annex.Sequence,NA))%>%
                mutate(.,Nature = ifelse(c("Nature") %in% names(.),Nature,NA))
              
            } else {
                if(identical(x,df11)) {
                  df111 <<- df11 %>%
                    ungroup() %>%
                    .[,colSums(is.na(.))<nrow(.)] %>%
                    mutate(.,Dimidentifier = dimid2(x)) %>%
                    mutate(.,R_ID = paste0(.$GeoAreaCode,.$TimePeriod,.$Dimidentifier,sep = "")) %>%
                    mutate(.,Annex.Sequence = ifelse(c("Annex.Sequence") %in% names(.),Annex.Sequence,NA)) %>%
                    mutate(.,Nature = ifelse(c("Nature") %in% names(.),Nature,NA)) %>%
                    ungroup()
                }
              }
            }
          }
      
      #s_status(df11)
      #s_status(df00)
      
      

      
 #.Removing some of the columns. 
          
      #. (1) renmaing the column names of old dataset. (2) Trimming down the dataset for analysis.
        s_renamed = function(x){
          keep <<- c("Ref_Area_Type","SeriesCode","GeoAreaCode","GeoAreaName","TimePeriod","R_ID")   
          dropx <<- c("ID","Goal","Target","Indicator","Freq","Time_Detail","Annex.Sequence","Dimidentifier","SeriesObservationCount","ValueType","ObservationID","SeriesDescription")
          
          if(identical(x,df000)) {
            df001 <<- df000 %>%
              ungroup() %>%
              .[,(names(.) %!in% dropx)] %>%
              rename_at(vars(setdiff(names(.),keep)), funs(paste0(.,"_OLD")))
          } else {
            if(identical(x,df111)) {
              df112 <<- df111 %>%
                ungroup() %>%
                .[,(names(.) %!in% dropx)]
            }
          }
        }
      #s_renamed(df000)
      #s_renamed(df111)
      
 #. Find out about the data availability.
      #. Record count (total, by global, regional and country, by year)
      #. Country count (type of area)
      #. 
      dataval = function(x) {
        if(identical(x,df000)){
          #. if clause starts
          #. Old data - Data Availability: data point count, by ref. area type.
          aval_dpc_0 <<- x %>%
            select(.,GeoAreaCode,TimePeriod,Ref_Area_Type) %>%
            group_by(Ref_Area_Type) %>%
            tally() %>%
            mutate(.,dpt_count = num(n)) %>%
            select(.,Ref_Area_Type,dpt_count) %>%
            ungroup()
          
          #. Old data - Data Availability: Unique ref area count by ref area type
          aval_fac_0 <<- x %>%
            select(.,GeoAreaCode,Ref_Area_Type) %>%
            distinct(.) %>%
            group_by(Ref_Area_Type) %>%
            tally() %>%
            mutate(.,GeoArea_count = num(n)) %>%
            select(.,Ref_Area_Type,GeoArea_count)%>%
            ungroup()
          
          #. Old data - Data Availability: data point count by time period and ref area type
          aval_tpc_0 <<- x %>%
            select(.,TimePeriod,Ref_Area_Type) %>%
            group_by(Ref_Area_Type,TimePeriod) %>%
            tally() %>%
            plyr::rename(.,c("n" = "dpt_count_OLD")) %>%
            ungroup()
              #. Wide format.
              aval_tpc_0w <<- x %>%
                select(.,TimePeriod,Ref_Area_Type) %>%
                group_by(Ref_Area_Type,TimePeriod) %>%
                tally() %>%
                spread(.,TimePeriod,n) %>%
                ungroup()
          
          #. Old data - Data Availability: count count by time period and ref area type.
          aval_tpga_0 <<- x %>%
            select(.,TimePeriod,Ref_Area_Type,GeoAreaCode) %>%
            group_by(Ref_Area_Type,TimePeriod) %>%
            distinct() %>%
            tally() %>% 
            plyr::rename(.,c("n" = "RefArea_count_OLD")) %>%
            ungroup()
         
              #. Wide format
              aval_tpga_0w <<- x %>%
                select(.,TimePeriod,Ref_Area_Type,GeoAreaCode) %>%
                group_by(Ref_Area_Type,TimePeriod) %>%
                distinct() %>%
                tally() %>%
                spread(.,TimePeriod,n) %>%
                ungroup()
          #. if clause ends.
        } else {
          if(identical(x,df111)) {
          #. if clause starts.
            #. New data - Data Availability: Data point count by ref. area type.
            aval_dpc_1 <<- x %>%
              select(.,GeoAreaCode,TimePeriod,Ref_Area_Type) %>%
              group_by(Ref_Area_Type) %>%
              tally() %>%
              mutate(.,dpt_count = num(n)) %>%
              select(.,Ref_Area_Type,dpt_count) %>%
              ungroup()
            
            #. New data - Data Availability: Ref area count by ref area type
            aval_fac_1 <<- x %>%
              select(.,GeoAreaCode,Ref_Area_Type) %>%
              distinct(.) %>%
              group_by(Ref_Area_Type) %>%
              tally() %>%
              mutate(.,GeoArea_count = num(n)) %>%
              select(.,Ref_Area_Type,GeoArea_count) %>%
              ungroup()
            
            #. New data - Data Availability: data point count by time period and ref area type
            aval_tpc_1 <<- x %>%
              select(.,TimePeriod,Ref_Area_Type) %>%
              group_by(Ref_Area_Type,TimePeriod) %>%
              tally() %>%
              plyr::rename(.,c("n" = "dpt_count")) %>%
              ungroup()
                #. Wide format
                aval_tpc_1w <<- x %>%
                  select(.,TimePeriod,Ref_Area_Type) %>%
                  group_by(Ref_Area_Type,TimePeriod) %>%
                  tally() %>%
                  spread(.,TimePeriod,n)
            
            #. New data - Data Availability: country count by time period and ref area type.
            aval_tpga_1 <<- x %>%
              select(.,TimePeriod,Ref_Area_Type,GeoAreaCode) %>%
              group_by(Ref_Area_Type,TimePeriod) %>%
              distinct() %>%
              tally() %>%
              plyr::rename(.,c("n" = "country_count")) %>%
              ungroup() 
                #. Wide format.
                aval_tpga_1w <<- x %>%
                  select(.,TimePeriod,Ref_Area_Type,GeoAreaCode) %>%
                  group_by(Ref_Area_Type,TimePeriod) %>%
                  distinct() %>%
                  tally() %>%
                  spread(.,TimePeriod,n)
          #. if clause ends.
          }
        }
        
      }
      
      #dataval(df000)
      #dataval(df111)
      
      validation = function(df001,df112) { #. First input is the prepared data file of previous (OLD) version, second input is the current (most recent) data file.
        #. Joing two data files.
          dfJ <<- full_join(df001,df112,by = c("SeriesCode","GeoAreaCode","GeoAreaName","TimePeriod","R_ID","Ref_Area_Type"))
        
        #. preparing the value column for the analysis.
          dfJ <<- dfJ %>%
            mutate(.,Value = num(Value),Value_OLD = num(Value_OLD))
      
        #. Comparing the datasets.
          #. value differences - absolute and relative. (data frame)
          dfJ_vcompare <<- dfJ %>%
            select(.,R_ID,Ref_Area_Type,GeoAreaCode,GeoAreaName, TimePeriod, Value, Value_OLD) %>%
            filter(.,is.numeric(Value),is.numeric(Value_OLD))
          
          if(nrow(dfJ_vcompare) == 0) {"No numeric data. Skipping validation"} else {
                  
                  dfJ_vcompare <<- dfJ_vcompare %>%
                    mutate(.,vdiff_abs = Value - Value_OLD, 
                           vdiff_rel = round2((Value - Value_OLD)*100/Value_OLD,0), 
                           vdiff_display = ifelse(is.na(Value),"...",paste0(Value," (",vdiff_rel,"%)"))) %>%
                    mutate(.,added = ifelse(!is.na(Value)&is.na(Value_OLD),1,0),deleted = ifelse(is.na(Value)&!is.na(Value_OLD),1,0)) %>%
                    ungroup()
                  
                 dfJ_delta <<- dfJ_vcompare %>%
                    filter(.,vdiff_rel!=0) %>%
                    .[,names(.) %!in% c("Annex.Sequence")] %>%
                    arrange(.,desc(vdiff_rel))
                  addWorksheet(wb, "dpt_revised")
                  writeData(wb,"dpt_revised",dfJ_delta)
                  
                  
                  
                  #. Value stock comparison. (what is the relative tolerance level)
                  #dfJ_stk <<- dfJ_vcompare %>%
                    #select(.,Ref_Area_Type,Value,Value_OLD,vdiff_abs) %>%
                    #group_by(Ref_Area_Type) %>%
                    #summarise_each(.,funs(sum(.,na.rm = TRUE))) %>%
                    #mutate(.,stkdiff_abs = Value - Value_OLD, stkdiff_rel = round2((Value-Value_OLD)*100/Value_OLD,2))%>%
                    #ungroup()
                  
                  #. Nature differences - show the change (data frame)  
                  dfJ_ncompare <<- dfJ %>%
                    select(.,R_ID,Ref_Area_Type,GeoAreaCode,GeoAreaName,TimePeriod, Nature, Nature_OLD) %>%
                    mutate(.,ndiff = ifelse(Nature == Nature_OLD,0,1),ndiff_delta = ifelse(Nature == Nature_OLD,"...",paste0(Nature_OLD,"-to-",Nature))) %>%
                    ungroup()
                  NatureMessage <<- if(sum(dfJ_ncompare$ndiff,na.rm = TRUE)>0) {
                    paste0("Some natures are different. Review the nature column.") 
                  } else {paste0("Natures are the same.")}
                  
                  nature_diff <<- dfJ_ncompare %>%
                    filter(.,ndiff==1) %>%
                    ungroup()
                  addWorksheet(wb, "Nature_revised")
                  writeData(wb,"Nature_revised",nature_diff)
                  
                  #. Unit - show if different.  
                  dfJ_unitc <<- dfJ %>%
                    select(.,R_ID,Ref_Area_Type,GeoAreaCode,GeoAreaName,TimePeriod, Units, Units_OLD) %>%
                    mutate(.,unitdiff = ifelse(Units == Units_OLD,0,1),unitdiff_delta = ifelse(Units == Units_OLD,"...",paste0(Units_OLD,"-to-",Units))) %>%
                    ungroup()
                  UnitMessage <<- if(sum(dfJ_unitc$unitdiff,na.rm = TRUE)>0) {
                    paste0("Some units are different. Review the units column.") 
                  } else {paste0("Units are the same.")}
                  
                  unit_diff <<- dfJ_unitc %>%
                    filter(.,unitdiff==1) %>%
                    ungroup()
                  addWorksheet(wb, "Unit_revised")
                  writeData(wb,"Unit_revised",unit_diff)
                  
                  #. Following analyses the new and deleted data.
                    #. Newly added data.
                    new_df <<- dfJ_vcompare %>% filter(.,is.na(Value_OLD))
                    
                    if(nrow(new_df) == 0) {print(paste0("No new data in this dataset."))} else {
                      #. Country list, country count, datapoint count and data point distribution by year.
                      #. Country list
                      new_df_clist <<- new_df %>% 
                        select(.,GeoAreaCode,GeoAreaName,Ref_Area_Type,TimePeriod) %>% 
                        group_by(Ref_Area_Type,GeoAreaName,GeoAreaCode) %>% 
                        distinct() %>%
                        summarise_each(funs(toString(unique(.),collaspe = ";"))) %>%
                        ungroup() %>%
                        spread(.,Ref_Area_Type,TimePeriod) 
                        #. Excel file, sheet name should be "New Data: Dist. by Ref. Area"
                      
                      #. Data point Count and Distribution
                      new_df_dptc <<- new_df %>%
                        select(.,GeoAreaCode,GeoAreaName,Ref_Area_Type,TimePeriod) %>% 
                        group_by(Ref_Area_Type,TimePeriod) %>% 
                        tally() %>%
                        ungroup() %>%
                        spread(.,TimePeriod,n)
                      
                      #. Country Count
                      new_df_cc <<- new_df %>%
                        select(.,GeoAreaCode,Ref_Area_Type) %>% 
                        group_by(Ref_Area_Type) %>% 
                        distinct() %>%
                        tally() %>%
                        ungroup()
                    } #.Ending the if clause.
                        
                    #. Deleted data.
                    deleted_df <<- dfJ_vcompare %>% filter(.,is.na(Value))
        
                    if(nrow(deleted_df) == 0) {del_df <<- print(paste0("No deleted data in this dataset."))} else {
                      #. Country list, country count, datapoint count and data point distribution by year.
                      #. Country list and years deleted.
                      dropx2 <<- c("ID","Goal","Target","Indicator","ReleaseStatus","ReleaseName","SeriesID","Freq","Time_Detail","SeriesObservationCount",
                                  "ValueType","ObservationID","SeriesDescription","Annex.Sequence","Dimidentifier","R_ID")
                      del_df <<- deleted_df %>%
                        select(.,R_ID) %>% 
                        distinct(.) %>%
                        left_join(.,df000,by = c("R_ID")) %>%
                        .[,names(.) %!in% dropx2]
                      addWorksheet(wb, "dpt_deleted")
                      writeData(wb,"dpt_deleted",del_df)
                      
                      deleted_df_clist <<- deleted_df %>% 
                        select(.,GeoAreaCode,GeoAreaName,Ref_Area_Type,TimePeriod) %>% 
                        group_by(Ref_Area_Type,GeoAreaName,GeoAreaCode) %>% 
                        distinct() %>%
                        summarise_each(funs(toString(unique(.),collaspe = ";"))) %>%
                        ungroup() %>%
                        spread(.,Ref_Area_Type,TimePeriod) 
                      #. Excel file, sheet name should be "Deleted Data: Dist. by Ref. Area"
                      
                      #. Data point Count and Distribution of deleted dataset.
                      deleted_df_dptc <<- deleted_df %>%
                        select(.,GeoAreaCode,GeoAreaName,Ref_Area_Type,TimePeriod) %>% 
                        group_by(GeoAreaName,Ref_Area_Type,TimePeriod) %>% 
                        tally() %>%
                        ungroup() %>%
                        spread(.,TimePeriod,n)
                      
                      #. Country Count of deleted dataset.
                      deleted_df_cc <<- deleted_df %>%
                        select(.,GeoAreaCode,GeoAreaName,Ref_Area_Type) %>% 
                        group_by(Ref_Area_Type,GeoAreaName) %>% 
                        distinct() %>%
                        tally() %>%
                        ungroup()
                    } #.Ending the if clause.
          
          }
       }
          
      
    #validation(df001,df112)
    
    #. Visualization module starts here.
      #. 1.Current vs. Previous data availability (Use: aval_dpc_0 and aval_dpc_1)
        #. a.1.Data point count - by Ref. Area Type
      
      visualizationCharts = function() {
        vis_dpc <<- full_join(aval_dpc_0,aval_dpc_1,by = c("Ref_Area_Type")) %>%
          plyr::rename(.,c("dpt_count.x" = "Previous_Update","dpt_count.y" = "Current_Update")) %>%
          gather(.,dpt_Status,dpt_Count,Previous_Update:Current_Update)
        
        #g_dpc <<- ggplot(vis_dpc, aes(x = Ref_Area_Type, y = dpt_Count, fill=dpt_Status)) + 
        # geom_bar(stat="identity", position=position_dodge())+
        # ggtitle("Number of data points, by ref. area type") +
        geom_text(aes(label=dpt_Count), vjust=1.2, color="white",
                  position = position_dodge(0.9), size=3.5)+
          scale_fill_brewer(palette="Paired")+
          theme_minimal() +
          xlab("Ref.Area Type") + ylab("Data point count")
        
        addWorksheet(wb, "DptCount_byType")
        insertPlot(wb,"DptCount_byType", width = 9, height = 3.5, fileType = "png", units = "in")
        
        #. a.2.Data point count - by Ref. Area Type and time period. 
        vis_tpc <<- full_join(aval_tpc_0,aval_tpc_1,by = c("Ref_Area_Type","TimePeriod")) %>%
          plyr::rename(.,c("dpt_count_OLD" = "Previous_Update","dpt_count" = "Current_Update")) %>%
          gather(.,dpt_Status,dpt_Count,Previous_Update:Current_Update) %>%
          select(.,TimePeriod,dpt_Status,dpt_Count) %>%
          group_by(.,TimePeriod,dpt_Status) %>%
          summarise(.,dpt_Count = sum(dpt_Count)) %>%
          arrange(.,desc(dpt_Status)) %>%
          mutate(.,dpt_Count = ifelse(is.na(dpt_Count),0,dpt_Count)) %>%
          ungroup()
        
        g_tpc <<- ggplot(vis_tpc, aes(x = TimePeriod, y = dpt_Count, fill= dpt_Status)) + 
          geom_bar(stat="identity", position=position_dodge())+
          ggtitle("Number of data points, by year") +
          geom_text(aes(label=dpt_Count),hjust = 1.0,vjust = .25, color="black",angle = 90,
                    position = position_dodge(0.9), size=3.0)+
          scale_fill_brewer(palette="Set2")+
          theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
          scale_x_continuous(name="Time Period", breaks=seq(1999, 2020, 1)) + ylab("Data point count")
        
        addWorksheet(wb, "DptCount_byYear")
        insertPlot(wb,"DptCount_byYear", width = 9, height = 3.5, fileType = "png", units = "in")
        
        
        
        #. b.1.Country count - by Ref. Area Type.
        vis_fac <<- full_join(aval_fac_0,aval_fac_1,by = c("Ref_Area_Type")) %>%
          plyr::rename(.,c("GeoArea_count.x" = "Previous_Update","GeoArea_count.y" = "Current_Update")) %>%
          gather(.,geo_Status,geo_Count,Previous_Update:Current_Update)
        
        g_fac <<- ggplot(vis_fac, aes(x = Ref_Area_Type, y = geo_Count, fill=geo_Status)) + 
          geom_bar(stat="identity", position=position_dodge())+
          ggtitle("Number of countries with data, by reference area type") +
          geom_text(aes(label=geo_Count), vjust=1.2, color="black",
                    position = position_dodge(0.9), size=3.5)+
          scale_fill_brewer(palette="Paired")+
          theme_minimal() +
          xlab("Ref.Area Type") + ylab("Geo area count")
        addWorksheet(wb, "CountryCount_byType")
        insertPlot(wb,"CountryCount_byType", width = 9, height = 3.5, fileType = "png", units = "in")
        
        
        #. b.2.Country count - by Ref. Area Type and by Year.
        vis_tpga <<- full_join(aval_tpga_0,aval_tpga_1,by = c("Ref_Area_Type","TimePeriod"))%>%
          plyr::rename(.,c("RefArea_count_OLD" = "Previous_Update","country_count" = "Current_Update")) %>%
          gather(.,geo_Status,geo_Count,Previous_Update:Current_Update) %>%
          select(.,TimePeriod,geo_Status,geo_Count) %>%
          group_by(.,TimePeriod,geo_Status) %>%
          summarise(.,geo_Count = sum(geo_Count)) %>%
          arrange(.,desc(geo_Status)) %>%
          mutate(.,geo_Count = ifelse(is.na(geo_Count),0,geo_Count)) %>%
          ungroup()
        
        g_tpga <<- ggplot(vis_tpga, aes(x = TimePeriod, y = geo_Count, fill= geo_Status)) + 
          geom_bar(stat="identity", position=position_dodge())+
          ggtitle("Number of countries with data, by year") +
          geom_text(aes(label=geo_Count),hjust = 1.0,vjust = .25, color="black",angle = 90,
                    position = position_dodge(0.9), size=3.0)+
          scale_fill_brewer(palette="Set2")+
          theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
          scale_x_continuous(name="Time Period", breaks=seq(1999, 2020, 1)) + ylab("Ref. Are Count")
        #insertPlot(wb,"PlotTest", xy = c("J", 2), width = 5, height = 3.5, fileType = "png", units = "in")
        addWorksheet(wb, "CountryCount_byYear")
        insertPlot(wb,"CountryCount_byYear", width = 9, height = 3.5, fileType = "png", units = "in")
        
      }
      
          
    visualization = function() {
      vis_dpc <<- full_join(aval_dpc_0,aval_dpc_1,by = c("Ref_Area_Type")) %>%
            plyr::rename(.,c("dpt_count.x" = "Previous_Update","dpt_count.y" = "Current_Update")) %>%
            gather(.,dpt_Status,dpt_Count,Previous_Update:Current_Update)

     
        #. a.2.Data point count - by Ref. Area Type and time period. 
          vis_tpc <<- full_join(aval_tpc_0,aval_tpc_1,by = c("Ref_Area_Type","TimePeriod")) %>%
            plyr::rename(.,c("dpt_count_OLD" = "Previous_Update","dpt_count" = "Current_Update")) %>%
            gather(.,dpt_Status,dpt_Count,Previous_Update:Current_Update) %>%
            select(.,TimePeriod,dpt_Status,dpt_Count) %>%
            group_by(.,TimePeriod,dpt_Status) %>%
            summarise(.,dpt_Count = sum(dpt_Count)) %>%
            arrange(.,desc(dpt_Status)) %>%
            mutate(.,dpt_Count = ifelse(is.na(dpt_Count),0,dpt_Count)) %>%
            ungroup()

        #. b.1.Country count - by Ref. Area Type.
          vis_fac <<- full_join(aval_fac_0,aval_fac_1,by = c("Ref_Area_Type")) %>%
            plyr::rename(.,c("GeoArea_count.x" = "Previous_Update","GeoArea_count.y" = "Current_Update")) %>%
            gather(.,geo_Status,geo_Count,Previous_Update:Current_Update)

        
        #. b.2.Country count - by Ref. Area Type and by Year.
          vis_tpga <<- full_join(aval_tpga_0,aval_tpga_1,by = c("Ref_Area_Type","TimePeriod"))%>%
            plyr::rename(.,c("RefArea_count_OLD" = "Previous_Update","country_count" = "Current_Update")) %>%
            gather(.,geo_Status,geo_Count,Previous_Update:Current_Update) %>%
            select(.,TimePeriod,geo_Status,geo_Count) %>%
            group_by(.,TimePeriod,geo_Status) %>%
            summarise(.,geo_Count = sum(geo_Count)) %>%
            arrange(.,desc(geo_Status)) %>%
            mutate(.,geo_Count = ifelse(is.na(geo_Count),0,geo_Count)) %>%
            ungroup()

    }
      
      #. 2.Added vs. Deleted data 
        #. a.Data point count - by Ref. Area Type AND by Year.
        #. b.Country count - by Ref. Area Type and by Year.
      #. 3.Data variations
        #. a.Histogram of % changes (by every 20%)
        #. b....
    
    
    getAnnexCompare = function(){
      
    #. Annex comparison.
      #. Creating a column mutating "value" with differential value e.g. 3.4 (+0.3%)
      #. Creating a pivot table with mutated value column.
      #. Outputting this in a user friendly method.
      
      #. A draft pivot table
      pt <- PivotTable$new()
      pt$addData(filter(dfJ_vcompare,Annex.Sequence<21))
      pt$addColumnDataGroups("TimePeriod", addTotal = FALSE)
      pt$addColumnDataGroups("Dimidentifier",addTotal = FALSE)
      pt$addRowDataGroups("Annex.Sequence", addTotal = FALSE)
      pt$addRowDataGroups("GeoAreaName", addTotal = FALSE)
      pt$defineCalculation(calculationName="AnnexValue", summariseExpression = "max(vdiff_display)")
      pt$renderPivot()
      

      addWorksheet(wb, "Table_ComparedValues")
      pt$writeToExcelWorksheet(wb=wb, wsName="Table_ComparedValues", 
                               topRowNumber=1, leftMostColumnNumber=1, applyStyles=FALSE)

    }
      
      
  #. Saving validation data in an Access file.
     val_summary = function(){
      
      h <- matrix(0,ncol = 22,nrow = 1)
      h <- data.frame(h)
      colnames(h) <- c("ReleaseName_Current","Validated_Date","SeriesName","SeriesID","SeriesCode","IndicatorID","ValidatedBy","DimensionInfo","Unit","Unit_Check", #.1-10
                       "RefAreaCount","DataCount_PreviousUpdate","DataCount_CurrentUpdate","CountryCount_Previous","CountryCount_Current","Deleted_dpt","Added_dpt","Delta_100pct_Count","deleted_list", #.11-19
                       "NatureStatus","Nature_Current","Nature_Previous") #.20-27
      rownames(h) <- c("")
      h[1] <- c(print(unique(df111$ReleaseName)[1]))
      h[2] <- c(print(paste(Sys.Date()))) #. DateTime
      h[3] <- c(print(paste(unique(df111$SeriesDescription)[1]))) #. SeriesName
      h[4] <- c(print(paste(unique(df111$SeriesID)[1])))#.SeriesID
      h[5] <- c(print(paste(unique(df111$SeriesCode)[1]))) #.SeriesCode
      h[6] <- c(print(paste(unique(df111$Indicator)[1]))) #.IndicatorID
      h[7] <- c(print(paste(Sys.getenv("USERNAME")))) #.Validate_By
      h[8] <- c(print(paste(toString(colnames(df111)[colnames(df111) %in% DimsF$DimensionNames])))) #.DimensionInfo
      h[9] <- c(print(paste(unique(df111$Units)[1]))) #.Unit of new data set
      h[10] <- c(print(paste(UnitMessage))) #.Unit Messsage - whether units are different
      
      h[11] <- c(print(vis_fac %>% 
                         filter(.,geo_Status == "Current_Update") %>% 
                         select(.,geo_Count) %>%
                         colSums(.,na.rm = TRUE))) #. Number of ref.area with data.
      h[12] <- c(print(vis_dpc %>% 
                         filter(.,dpt_Status == "Previous_Update") %>% 
                         select(.,dpt_Count) %>%
                         colSums(.,na.rm = TRUE))) #. total number of previous data
      h[13] <- c(print(vis_dpc %>% 
                         filter(.,dpt_Status == "Current_Update") %>% 
                         select(.,dpt_Count) %>%
                         colSums(.,na.rm = TRUE))) #. total number of Current data
      h[14] <- c(print(vis_fac %>% 
                         filter(.,geo_Status == "Previous_Update", Ref_Area_Type == "3.0-Country") %>% 
                         select(.,geo_Count) %>%
                         colSums(.,na.rm = TRUE))) #. total number of countries - previous update
      h[15] <- c(print(vis_fac %>% 
                         filter(.,geo_Status == "Current_Update", Ref_Area_Type == "3.0-Country") %>% 
                         select(.,geo_Count) %>%
                         colSums(.,na.rm = TRUE))) #. total number of countries - current update
      h[16] <- c(print(sum(dfJ_vcompare$deleted,na.rm = TRUE))) #. number of deleted data
      h[17] <- c(print(sum(dfJ_vcompare$added,na.rm = TRUE))) #. number of added data.
      h[18] <- c(print(dfJ_vcompare %>%
                         filter(.,vdiff_rel>=100) %>%
                         nrow(.))) #. number of revised data points (change is >100%)
      h[19] <- c(print(paste0("see dpt_deleted sheet")))
      h[20] <- c(print(NatureMessage)) # Nature message
      h[21] <- c(print(toString(unique(df111$Nature)))) # List of Nature current
      h[22] <- c(print(toString(unique(df000$Nature)))) # List of Nature previous

      #write.csv(h,"./1. Data Processing/2020/0.3_Validation/Validation_Data_2020.csv",row.names = FALSE)
      #Connect to Access db
      #require(RODBC)
      #channel1 <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:\\Users\\Owner\\OneDrive - United Nations\\SDG_Database\\1. Data Processing\\2020\\0.3_Validation\\0_DataValidationOutputs.accdb")
      #channel1 <- odbcConnectAccess2007("./1. Data Processing/2020/0.3_Validation/0_DataValidationOutputs_2020.accdb")
      #h2 <- sqlQuery(channel1,paste("select * from Validation_Data_2020"))

      history <<- rbind(history,h)

      #sqlSave(channel1,h2,tablename = "Validation_Data_2020", append = TRUE, rownames = FALSE)
      #unlink(h)
      
      ht <- data.frame(t(h))
      addWorksheet(wb, "1_ValidationSummary")
      writeData(wb,"1_ValidationSummary",ht,rowNames = TRUE)

      }
     
      cleaning = function(){
         list <- ls()
         to_keep <- c("%!in%", "dataval","df0","df1","Dims","DimsF",
                      "display_function","GeoInfo","getAnnexCompare",
                      "history","i","num","round2","SDG_GEO","sfilter","val_summary","validation","visualization","wb")
         list_to_remove <<- list[list %!in% to_keep]
         rm(list=list_to_remove)}
  
  