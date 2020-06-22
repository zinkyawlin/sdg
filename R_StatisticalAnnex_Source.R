Sys.setenv(JAVA_HOME='C:\\Program Files (x86)\\Java\\jre1.8.0_211')  # for 32-bit version

library(dplyr)
library(openxlsx)
library(ggplot2)
library(rmarkdown)
library(pivottabler)
library(flextable)

setwd("C:/Users/Zin.Lin/OneDrive - United Nations/SDG_Database")
save(df1,file = "./2. Statistical Annex/2020/0_WorkingFiles_ZL/2020Q1Data_20200331.RData")
load("./2. Statistical Annex/2020/0_WorkingFiles_ZL/2020Q1Data_20200331.RData")
'%!in%' <- Negate('%in%')  

#. Get data
  getRecentData = function(x){
    c_filename = paste0("./9. Database Updates + History + Archives/Archive/",x)
    #. function loading data into a set dataset name.
    loadRData <- function(fileName){ #loads an RData file, and returns it
      load(fileName)
      get(ls()[ls() != "fileName"])
    }
    df1 <- loadRData(c_filename)
  }    

  #. And loading the Geo Area Info
  #. The Excel file contains all ref area in use for SDG monitoring - and is regularly updated.
  
  getGeo = function(){
    GeoInfo <<- openxlsx::read.xlsx("./0. SDG_Reference_Files/SDG_ReferenceArea_20191105_AM.xlsx",sheet = 1) #create a table containing series used in country profiles
    SDG_GEO <<- GeoInfo %>%
      select(.,M49,Ref_Area_Type,Annex.Sequence) %>%
      unique(.)} #. Getting geo information.
  getGeo()

#function for changing data type to numeric

    num = function(x) {
      require(testit)
      if(has_warning(as.numeric(as.character(x)))){x}else
      {as.numeric(as.character(x))}
    } #change the data type to numeric
    
    num2 = function(x) {
      require(testit)
      sapply(x,function(x) {if(has_warning(as.numeric(as.character(x)))){x} else
      {as.numeric(as.character(x))}
      })
    } #Looking for Non-numeric figures
    
    # function for displaying elements from a list
    display_function <- function(x){
      t <- paste0("(")
      y <- na.omit(unique(x))
      for (i in 1:length(y)){
        t <- paste0(t,y[i],ifelse(i==max(length(y)),")",","))
        
      }
      return(t)
    }

#. Function for "rounding in commerce" - "kaufmännische Rundung"
    round2 = function(x, n) {
      posneg = sign(x)
      z = abs(x)*10^n
      z = z + 0.5
      z = trunc(z)
      z = z/10^n
      z*posneg
    }

    

    

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
    
    getDim()
    
#. Function to check whether data are avilable. if not, provide the message.
    prepData = function(x) {
      if(identical(x,df1)) {
          df11 <<- df1 %>%
            ungroup() %>%
            .[,colSums(is.na(.))<nrow(.)] %>%
            mutate(.,Dimidentifier = dimid2(.)) %>%
            mutate(.,R_ID = paste0(SeriesCode,Dimidentifier)) %>%
            left_join(.,SDG_GEO,by = c("GeoAreaCode" = "M49")) %>%
            ungroup() %>%
            filter(.,Ref_Area_Type %!in% c("4.0-Not-specified","3.0-Country"))
        }
    }

    prepData(df1)
    
    temp <- df11 %>%
      select(.,SeriesCode,R_ID) %>%
      unique(.)
    write.csv(temp,"./2. Statistical Annex/2020/0_WorkingFiles_ZL/Annex_RIDUpdate_20200407.csv",row.names = FALSE)
    
    drop <- c("ID","Goal","Target","Indicator","ReleaseStatus","ReleaseName","SeriesID","isDSDSeries","SeriesObservationCount","ObservationID","Dimidentifier")
    
#. Time period search
    getTime = function(x){
      t <- unique(num2(x)) %>% filter(.,x>=2000)
      max_year <- max(x,na.rm = TRUE)
      min_year <- min(x.na.rm = TRUE)
      if(max_year == min_year) {x} else {
        if(c("2000","2005","2010","2015",max_year) %in% x) {x = c("2000","2005","2010","2015",max_year)} else {
         #. this is where the list is chosen.
          d <- max_year - min_year
          if(length(x)<3) {x} else {
            
            
            
          }
           
        }
      }
    }
    
    
    df111 <- df11 %>% 
      filter(.,R_ID == "SI_POV_DAY1") %>%
      .[,colSums(is.na(.))<nrow(.)] %>%
      .[,colnames(.) %!in% drop] %>%
      filter(.,Ref_Area_Type != "3.0-Country", Annex.Sequence<21, TimePeriod %in% c("2000","2005","2010","2015","2018")) %>% #. 
      ungroup() 
    
    fn <- df111 %>%
      select(.,FootNote) %>%
      filter(.,FootNote != "") %>%
      distinct(.) %>%
      mutate(.,fnn = id(.))
    
    df111 <- df111 %>%
      left_join(.,fn,by = "FootNote") %>%
      mutate(Value_Annex = ifelse(is.na(fnn),Value,paste(Value,fnn,sep = "^")))
    
    df112 <- df111 %>%
      dcast(.,Annex.Sequence + GeoAreaName ~ TimePeriod,value.var = "Value_Annex")

    #. Flex table version
    ft1 <- flextable(df112)
    ft1 <- autofit(ft1)
    ft1 <- fontsize(ft1, size = 9) 
    ft1
    print(ft1, preview = "docx")

#.  Annex Table Type 1   
    getTableType1 = function(){
      #. A draft pivot table
      pt <- PivotTable$new()
      pt$addData(filter(df111,Annex.Sequence<21))
      pt$addColumnDataGroups("TimePeriod", addTotal = FALSE)
      pt$addRowDataGroups("Annex.Sequence", addTotal = FALSE)
      pt$addRowDataGroups("GeoAreaName", addTotal = FALSE)
      pt$defineCalculation(calculationName="AnnexDisplay", type="value", valueName="Value")
      pt$renderPivot()
    }
    


    
#. Data for table type 2    
    include2 <- c("SI_COV_BENFTSBOTHSEX", "SI_COV_CHLDBOTHSEX","SI_COV_DISABBOTHSEX","SI_COV_MATNLFEMALE","SI_COV_PENSNBOTHSEX","SI_COV_POORBOTHSEX","SI_COV_UEMPBOTHSEX","SI_COV_VULNBOTHSEX","SI_COV_WKINJRYBOTHSEX")

    df111 <- df11 %>% 
      filter(.,R_ID %in% include2) %>%
      .[,colSums(is.na(.))<nrow(.)] %>%
      .[,colnames(.) %!in% drop] %>%
      filter(.,Ref_Area_Type != "3.0-Country") %>% #. 
      ungroup()

#.  Annex Table Type 2       
    getTableType2 = function(){
      #. A draft pivot table
      pt <- PivotTable$new()
      pt$addData(filter(df111,Annex.Sequence<21,TimePeriod == 2016))
      pt$addColumnDataGroups("TimePeriod", addTotal = FALSE)
      pt$addColumnDataGroups("R_ID", addTotal = FALSE)
      pt$addRowDataGroups("Annex.Sequence", addTotal = FALSE)
      pt$addRowDataGroups("GeoAreaName", addTotal = FALSE)
      pt$defineCalculation(calculationName="AnnexDisplay", type="value", valueName="Value")
      pt$renderPivot()
    }


#. Data for table type 3
    include3 <- c("SI_POV_EMP115+FEMALE", "SI_POV_EMP115+MALE")
    
    df111 <- df11 %>% 
      filter(.,R_ID %in% include3) %>%
      .[,colSums(is.na(.))<nrow(.)] %>%
      .[,colnames(.) %!in% drop] %>%
      filter(.,Ref_Area_Type != "3.0-Country",TimePeriod %in% c(2000,2010,2019)) %>% #. 
      ungroup()
    
#.  Annex Table Type 3     
    getTableType3 = function(){
      #. A draft pivot table
      pt <- PivotTable$new()
      pt$addData(filter(df111,Annex.Sequence<21))
      pt$addColumnDataGroups("TimePeriod", addTotal = FALSE)
      pt$addColumnDataGroups("R_ID", addTotal = FALSE)
      pt$addRowDataGroups("Annex.Sequence", addTotal = FALSE)
      pt$addRowDataGroups("GeoAreaName", addTotal = FALSE)
      pt$defineCalculation(calculationName="AnnexDisplay", type="value", valueName="Value")
      pt$renderPivot()
    }
    
    library(openxlsx)
    wb <- createWorkbook(creator = Sys.getenv("USERNAME"))
    addWorksheet(wb, "Data")
    pt$writeToExcelWorksheet(wb=wb, wsName="Data", 
                             topRowNumber=1, leftMostColumnNumber=1, 
                             applyStyles=TRUE, mapStylesFromCSS=TRUE)
    saveWorkbook(wb, file="./2. Statistical Annex/2020/0_WorkingFiles_ZL/first_example_table3.xlsx", overwrite = TRUE)
    

    
    
