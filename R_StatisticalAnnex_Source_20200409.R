Sys.setenv(JAVA_HOME='C:\\Program Files (x86)\\Java\\jre1.8.0_211')  # for 32-bit version

library(dplyr)
library(openxlsx)
library(ggplot2)
library(rmarkdown)
library(pivottabler)
library(basictabler)

    #setwd("C:/Users/Zin.Lin/OneDrive - United Nations/SDG_Database") #. Work Computer
    #setwd("C:/Users/Owner/OneDrive - United Nations/SDG_Database") #. Home Computer
    #save(df1,file = "./2. Statistical Annex/2020/0_WorkingFiles_ZL/2020Q1Data_20200331.RData")
    #load("./2. Statistical Annex/2020/0_WorkingFiles_ZL/2020Q1Data_20200331.RData")
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
      select(.,M49,Ref_Area_Type,Annex.Sequence,AnnexName) %>%
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

    # Rounding numbers - without touching the non-numeric
    round3 = function(x,n) {
      require(testit)
      sapply(x,function(x) {if(has_warning(as.numeric(as.character(x)))){x} else
      {round2(num2(x),n)}
      })
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
    
    #getDim()
    
#. Function to check whether data are avilable. if not, provide the message.
    prepData = function(x) {
      AnnexInput <<- openxlsx::read.xlsx("./2. Statistical Annex/2020/0_WorkingFiles_ZL/Annex_Input_2020.xlsx",sheet = "Input") %>%
            select(.,IndicatorCode,R_ID,C_Name,TableType,TableID)
      if(identical(x,df1)) {
          df11 <<- df1 %>%
            ungroup() %>%
            .[,colSums(is.na(.))<nrow(.)] %>%
            mutate(.,Dimidentifier = dimid2(.)) %>%
            mutate(.,R_ID = paste0(SeriesCode,Dimidentifier)) %>%
            left_join(.,SDG_GEO,by = c("GeoAreaCode" = "M49")) %>%
            ungroup() %>%
            filter(.,Ref_Area_Type %!in% c("4.0-Not-specified","3.0-Country")) %>%
            left_join(.,AnnexInput,by = "R_ID")
        }
    }

    #prepData(df1)
    
    
#. Time period search (still needs to be completed)
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
    
    getTypeSpecificData <- function(x,y,Decimal){
    
    drop <- c("ID","Goal","Target","Indicator","ReleaseStatus","ReleaseName","SeriesID","isDSDSeries","SeriesObservationCount","ObservationID","Dimidentifier")
    
    df111 <<- y %>% 
      filter(.,TableID == x) %>%
      .[,colSums(is.na(.))<nrow(.)] %>%
      .[,colnames(.) %!in% drop] %>%
      filter(.,Ref_Area_Type != "3.0-Country")  %>%
      mutate(.,Value = num2(round3(Value,Decimal)))
      
      if(c("FootNote") %!in% names(df111)) {
        df111 <<- df111 %>% mutate(FootNote = paste("NA"))
      }
    
      if(c("Source") %!in% names(df111)) {
        df111 <<- df111 %>% mutate(Source = paste("NA"))
      }
    
    if(dim(df111)[1] > 0) {
    fn <<- df111 %>%
      select(.,FootNote) %>%
      filter(.,FootNote %!in% c(""," ","NA",NA,"NULL",NULL)) %>%
      distinct(.)
    if(dim(fn)[1]>0){
      fn <<- fn %>%
      mutate(.,fnn = id(.)) %>%
      mutate(.,fnr = paste("***",fnn," ",FootNote,sep = "")) %>%
      .[order(.$fnn),]} else {
      fn <<- fn %>%
          mutate(.,fnn = id(.)) %>%
          mutate(.,fnr = paste(FootNote)) %>%
          .[order(.$fnn),]
      }

    
    df111 <<- df111 %>%
      left_join(.,fn,by = "FootNote") %>%
      mutate(Value_Annex = ifelse(is.na(fnn),Value,paste(Value,fnn,sep = "***"))) %>%
      mutate(Value_Annex = num2(round3(Value,Decimal))) %>%
      #mutate(.,FootNote = ifelse(c("FootNote") %in% names(.),paste(.$FootNote),NA)) %>% 
      #mutate(.,Source = ifelse(c("Source") %in% names(.),.$Source,NA)) %>% 
      ungroup() %>%
      distinct(.)} else
      {next}
    }
    
    #getTypeSpecificData("C020201-1",df11)
    

#.  Annex Table Type 1   
    getTableType1 = function(){
      #. A draft pivot table
      pt <- PivotTable$new()
      AnnexTheme <- list(
        fontName="Times New Roman",
        "font-size"= "0.75em",
        headerBackgroundColor = "rgb(255, 255, 255)",
        headerColor = "rgb(0, 0, 0)",
        cellBackgroundColor = "rgb(255, 255, 255)",
        cellColor = "rgb(0, 0, 0)",
        outlineCellBackgroundColor = "rgb(248, 198, 165)",
        outlineCellColor = "rgb(0, 0, 0)",
        totalBackgroundColor = "rgb(248, 198, 165)",
        totalColor = "rgb(0, 0, 0)",
        borderColor = "	rgb(211,211,211)"
      )
      pt$addData(filter(df111,Annex.Sequence<21,TimePeriod>1999))
      pt$addColumnDataGroups("TimePeriod", addTotal = FALSE)
      pt$addRowDataGroups("Annex.Sequence", addTotal = FALSE)
      pt$addRowDataGroups("AnnexName", addTotal = FALSE, header="Regions")
      pt$defineCalculation(calculationName="AnnexDisplay", type="value", valueName="Value_Annex")
      pt$theme <- AnnexTheme
      #cgroups <- pt$findColumnDataGroups(variableNames="TimePeriod")
      #rgroups <- pt$findRowDataGroups(variableNames = "GeoAreaName")
      #pt$setStyling(groups= c(cgroups,rgroups), declarations=list("background-color"="#FFFF00"))
      pt$renderPivot(showRowGroupHeaders=TRUE)
      
      bt <- pt$asBasicTable()

      bt$unmergeCells(1,2)
      bt$cells$setCell(1,2, rawValue="Regions", visible = TRUE, 
                       styleDeclarations=list("text-align"="left", "font-weight"="bold", "font-style"="italic"))
      bt$setStyling(rFrom=1, cFrom=2, rTo=1, cTo=bt$columnCount, 
                    declarations=list("text-align"="left","font-weight"="bold", "font-style"="italic"))
      bt$setStyling(rFrom=2, cFrom=2, rTo=bt$rowCount, cTo=2, 
                    declarations=list("text-align"="left","font-weight"="normal"))
      
      if ((dim(fn)[1]) > 0) {
      #. adding FootNote
      bt$cells$insertRow(bt$rowCount + 1)
      bt$cells$setCell(bt$rowCount,2, rawValue=toString(fn$fnr), 
                       styleDeclarations=list("text-align"="left","font-weight"="normal", "font-style"="italic"))
      #bt$mergeCells(rFrom=bt$rowCount, cFrom=2, rSpan=1, cSpan=bt$columnCount-1)
      bt$setStyling(rFrom=bt$rowCount, cFrom=2, rTo=bt$rowCount, cTo=2, 
                    declarations=list("text-align"="left"))
      } 
      
      #. adding Note
      bt$cells$insertRow(bt$rowCount + 1)
      bt$cells$setCell(bt$rowCount,2, rawValue="Note: XXXXX", 
                       styleDeclarations=list("text-align"="left", "font-weight"="normal", "font-style"="italic"))
      #bt$mergeCells(rFrom=bt$rowCount, cFrom=2, rSpan=1, cSpan=bt$columnCount-1)
      bt$setStyling(rFrom=bt$rowCount, cFrom=2, rTo=bt$rowCount, cTo=2, 
                    declarations=list("text-align"="left"))
      
      #. adding Source
      bt$cells$insertRow(bt$rowCount + 1)      
      bt$cells$setCell(bt$rowCount,2, rawValue="Source: XXXXX", 
                       styleDeclarations=list("text-align"="left", "font-weight"="normal", "font-style"="italic"))
      #bt$mergeCells(rFrom=bt$rowCount, cFrom=2, rSpan=1, cSpan=bt$columnCount-1)
      bt$setStyling(rFrom=bt$rowCount, cFrom=2, rTo=bt$rowCount, cTo=2, 
                    declarations=list("text-align"="left"))
      
      
      #. Adding Table Information
      bt$cells$insertRow(1)      
      bt$cells$setCell(1,1, rawValue=paste0("TableID: ",distinct(df111,TableID),"; Disaggregation:", toString(unique(df111$R_ID))), 
                       styleDeclarations=list("text-align"="left", "font-weight"="normal", "font-style"="italic"))
      #bt$mergeCells(rFrom=1, cFrom=1, rSpan=1, cSpan=bt$columnCount)
      bt$setStyling(rFrom=1, cFrom=1, rTo=1, cTo=bt$columnCount, 
                    declarations=list("text-align"="left"))
      
      #. Adding Indicator Information
      bt$cells$insertRow(1)      
      bt$cells$setCell(1,1, rawValue=paste0("IndicatorCode: ",distinct(df111,IndicatorCode)[1]), 
                       styleDeclarations=list("text-align"="left", "font-weight"="normal", "font-style"="italic"))
      #bt$mergeCells(rFrom=1, cFrom=1, rSpan=1, cSpan=bt$columnCount)
      bt$setStyling(rFrom=1, cFrom=1, rTo=1, cTo=bt$columnCount, 
                    declarations=list("text-align"="left"))
      
      #. Adding Series Information
      bt$cells$insertRow(1)      
      bt$cells$setCell(1,1, rawValue=paste0("SeriesName: ",distinct(df111,SeriesDescription)[1]), 
                       styleDeclarations=list("text-align"="left", "font-weight"="normal", "font-style"="italic"))
      #bt$mergeCells(rFrom=1, cFrom=1, rSpan=1, cSpan=bt$columnCount)
      bt$setStyling(rFrom=1, cFrom=1, rTo=1, cTo=bt$columnCount, 
                    declarations=list("text-align"="left"))
      
      
      bt$renderTable()
      addWorksheet(wb,i)
      bt$writeToExcelWorksheet(wb=wb, wsName=i, 
                               topRowNumber=1, leftMostColumnNumber=1, 
                               applyStyles=TRUE, mapStylesFromCSS=TRUE)
      

    }

    #getTableType1()
    



#.  Annex Table Type 2       
    getTableType2 = function(){
      #. A draft pivot table
      pt <- PivotTable$new()
      AnnexTheme <- list(
        fontName="Times New Roman",
        "font-size"= "0.75em",
        headerBackgroundColor = "rgb(255, 255, 255)",
        headerColor = "rgb(0, 0, 0)",
        cellBackgroundColor = "rgb(255, 255, 255)",
        cellColor = "rgb(0, 0, 0)",
        outlineCellBackgroundColor = "rgb(248, 198, 165)",
        outlineCellColor = "rgb(0, 0, 0)",
        totalBackgroundColor = "rgb(248, 198, 165)",
        totalColor = "rgb(0, 0, 0)",
        borderColor = "	rgb(211,211,211)"
      )
      pt$addData(filter(df111,Annex.Sequence<21,TimePeriod == max(df111$TimePeriod)))
      pt$addColumnDataGroups("TimePeriod", addTotal = FALSE)
      pt$addColumnDataGroups("C_Name", addTotal = FALSE)
      pt$addRowDataGroups("Annex.Sequence", addTotal = FALSE)
      pt$addRowDataGroups("AnnexName", addTotal = FALSE,header="Regions")
      pt$defineCalculation(calculationName="AnnexDisplay", type="value", valueName="Value")
      pt$theme <- AnnexTheme
      pt$renderPivot(showRowGroupHeaders=TRUE)
      
      bt <- pt$asBasicTable()
      
      bt$unmergeCells(1,2)
      bt$cells$setCell(1,2, rawValue="Regions", visible = TRUE, 
                       styleDeclarations=list("text-align"="left", "font-weight"="bold", "font-style"="italic"))
      bt$setStyling(rFrom=1, cFrom=2, rTo=1, cTo=bt$columnCount, 
                    declarations=list("text-align"="center","font-weight"="bold", "font-style"="italic"))
      bt$setStyling(rFrom=3, cFrom=2, rTo=bt$rowCount, cTo=2, 
                    declarations=list("text-align"="left","font-weight"="normal"))
      
      if ((dim(fn)[1]) > 0) {
        #. adding FootNote
        bt$cells$insertRow(bt$rowCount + 1)
        bt$cells$setCell(bt$rowCount,2, rawValue=toString(fn$fnr), 
                         styleDeclarations=list("text-align"="left","font-weight"="normal", "font-style"="italic"))
        #bt$mergeCells(rFrom=bt$rowCount, cFrom=2, rSpan=1, cSpan=bt$columnCount-1)
        bt$setStyling(rFrom=bt$rowCount, cFrom=2, rTo=bt$rowCount, cTo=2, 
                      declarations=list("text-align"="left"))
      } 
      
      #. adding Note
      bt$cells$insertRow(bt$rowCount + 1)
      bt$cells$setCell(bt$rowCount,2, rawValue="Note: XXXXX", 
                       styleDeclarations=list("text-align"="left", "font-weight"="normal", "font-style"="italic"))
      #bt$mergeCells(rFrom=bt$rowCount, cFrom=2, rSpan=1, cSpan=bt$columnCount-1)
      bt$setStyling(rFrom=bt$rowCount, cFrom=2, rTo=bt$rowCount, cTo=2, 
                    declarations=list("text-align"="left"))
      
      #. adding Source
      bt$cells$insertRow(bt$rowCount + 1)      
      bt$cells$setCell(bt$rowCount,2, rawValue="Source: XXXXX", 
                       styleDeclarations=list("text-align"="left", "font-weight"="normal", "font-style"="italic"))
      #bt$mergeCells(rFrom=bt$rowCount, cFrom=2, rSpan=1, cSpan=bt$columnCount-1)
      bt$setStyling(rFrom=bt$rowCount, cFrom=2, rTo=bt$rowCount, cTo=2, 
                    declarations=list("text-align"="left"))
      
      #. Adding Table Information
      bt$cells$insertRow(1)      
      bt$cells$setCell(1,1, rawValue=paste0("TableID: ",distinct(df111,TableID),"; Disaggregation:", toString(unique(df111$R_ID))), 
                       styleDeclarations=list("text-align"="left", "font-weight"="normal", "font-style"="italic"))
      #bt$mergeCells(rFrom=1, cFrom=1, rSpan=1, cSpan=bt$columnCount)
      bt$setStyling(rFrom=1, cFrom=1, rTo=1, cTo=bt$columnCount, 
                    declarations=list("text-align"="left"))
      
      #. Adding Indicator Information
      bt$cells$insertRow(1)      
      bt$cells$setCell(1,1, rawValue=paste0("IndicatorCode: ",distinct(df111,IndicatorCode)[1]), 
                       styleDeclarations=list("text-align"="left", "font-weight"="normal", "font-style"="italic"))
      #bt$mergeCells(rFrom=1, cFrom=1, rSpan=1, cSpan=bt$columnCount)
      bt$setStyling(rFrom=1, cFrom=1, rTo=1, cTo=bt$columnCount, 
                    declarations=list("text-align"="left"))
      
      #. Adding Series Information
      bt$cells$insertRow(1)      
      bt$cells$setCell(1,1, rawValue=paste0("SeriesName: ",distinct(df111,SeriesDescription)[1]), 
                       styleDeclarations=list("text-align"="left", "font-weight"="normal", "font-style"="italic"))
      #bt$mergeCells(rFrom=1, cFrom=1, rSpan=1, cSpan=bt$columnCount)
      bt$setStyling(rFrom=1, cFrom=1, rTo=1, cTo=bt$columnCount, 
                    declarations=list("text-align"="left"))
      
      
      bt$renderTable()
      addWorksheet(wb,i)
      bt$writeToExcelWorksheet(wb=wb, wsName=i, 
                               topRowNumber=1, leftMostColumnNumber=1, 
                               applyStyles=TRUE, mapStylesFromCSS=TRUE)
      
    }

#.  Annex Table Type 3     
    getTableType3 = function(){
      #. A draft pivot table
      pt <- PivotTable$new()
      AnnexTheme <- list(
        fontName="Times New Roman",
        "font-size"= "0.75em",
        headerBackgroundColor = "rgb(255, 255, 255)",
        headerColor = "rgb(0, 0, 0)",
        cellBackgroundColor = "rgb(255, 255, 255)",
        cellColor = "rgb(0, 0, 0)",
        outlineCellBackgroundColor = "rgb(248, 198, 165)",
        outlineCellColor = "rgb(0, 0, 0)",
        totalBackgroundColor = "rgb(248, 198, 165)",
        totalColor = "rgb(0, 0, 0)",
        borderColor = "	rgb(211,211,211)"
      )
      pt$addData(filter(df111,Annex.Sequence<21))
      pt$addColumnDataGroups("TimePeriod", addTotal = FALSE)
      pt$addColumnDataGroups("C_Name", addTotal = FALSE)
      pt$addRowDataGroups("Annex.Sequence", addTotal = FALSE)
      pt$addRowDataGroups("AnnexName", addTotal = FALSE,header="Regions")
      pt$defineCalculation(calculationName="AnnexDisplay", type="value", valueName="Value")
      pt$renderPivot(showRowGroupHeaders=TRUE)
      pt$theme <- AnnexTheme
      
      bt <- pt$asBasicTable()
      
      bt$unmergeCells(1,2)
      bt$cells$setCell(1,2, rawValue="Regions", visible = TRUE, 
                       styleDeclarations=list("text-align"="center", "font-weight"="bold", "font-style"="italic"))
      bt$setStyling(rFrom=1, cFrom=2, rTo=1, cTo=bt$columnCount, 
                    declarations=list("text-align"="left","font-weight"="bold", "font-style"="italic"))
      bt$setStyling(rFrom=3, cFrom=2, rTo=bt$rowCount, cTo=2, 
                    declarations=list("text-align"="left","font-weight"="normal"))
      
      if ((dim(fn)[1]) > 0) {
        #. adding FootNote
        bt$cells$insertRow(bt$rowCount + 1)
        bt$cells$setCell(bt$rowCount,2, rawValue=toString(fn$fnr), 
                         styleDeclarations=list("text-align"="left","font-weight"="normal", "font-style"="italic"))
        #bt$mergeCells(rFrom=bt$rowCount, cFrom=2, rSpan=1, cSpan=bt$columnCount-1)
        bt$setStyling(rFrom=bt$rowCount, cFrom=2, rTo=bt$rowCount, cTo=2, 
                      declarations=list("text-align"="left"))
      } 
      
      #. adding Note
      bt$cells$insertRow(bt$rowCount + 1)
      bt$cells$setCell(bt$rowCount,2, rawValue="Note: XXXXX", 
                       styleDeclarations=list("text-align"="left", "font-weight"="normal", "font-style"="italic"))
      #bt$mergeCells(rFrom=bt$rowCount, cFrom=2, rSpan=1, cSpan=bt$columnCount-1)
      bt$setStyling(rFrom=bt$rowCount, cFrom=2, rTo=bt$rowCount, cTo=2, 
                    declarations=list("text-align"="left"))
      
      #. adding Source
      bt$cells$insertRow(bt$rowCount + 1)      
      bt$cells$setCell(bt$rowCount,2, rawValue="Source: XXXXX", 
                       styleDeclarations=list("text-align"="left", "font-weight"="normal", "font-style"="italic"))
      #bt$mergeCells(rFrom=bt$rowCount, cFrom=2, rSpan=1, cSpan=bt$columnCount-1)
      bt$setStyling(rFrom=bt$rowCount, cFrom=2, rTo=bt$rowCount, cTo=2, 
                    declarations=list("text-align"="left"))
      
      
      #. Adding Table Information
      bt$cells$insertRow(1)      
      bt$cells$setCell(1,1, rawValue=paste0("TableID: ",distinct(df111,TableID),"; Disaggregation:", toString(unique(df111$R_ID))), 
                       styleDeclarations=list("text-align"="left", "font-weight"="normal", "font-style"="italic"))
      #bt$mergeCells(rFrom=1, cFrom=1, rSpan=1, cSpan=bt$columnCount)
      bt$setStyling(rFrom=1, cFrom=1, rTo=1, cTo=bt$columnCount, 
                    declarations=list("text-align"="left"))
      
      #. Adding Indicator Information
      bt$cells$insertRow(1)      
      bt$cells$setCell(1,1, rawValue=paste0("IndicatorCode: ",distinct(df111,IndicatorCode)[1]), 
                       styleDeclarations=list("text-align"="left", "font-weight"="normal", "font-style"="italic"))
      #bt$mergeCells(rFrom=1, cFrom=1, rSpan=1, cSpan=bt$columnCount)
      bt$setStyling(rFrom=1, cFrom=1, rTo=1, cTo=bt$columnCount, 
                    declarations=list("text-align"="left"))
      
      #. Adding Series Information
      bt$cells$insertRow(1)      
      bt$cells$setCell(1,1, rawValue=paste0("SeriesName: ",distinct(df111,SeriesDescription)[1]), 
                       styleDeclarations=list("text-align"="left", "font-weight"="normal", "font-style"="italic"))
      #bt$mergeCells(rFrom=1, cFrom=1, rSpan=1, cSpan=bt$columnCount)
      bt$setStyling(rFrom=1, cFrom=1, rTo=1, cTo=bt$columnCount, 
                    declarations=list("text-align"="left"))
      
      
      bt$renderTable()
      addWorksheet(wb,i)
      bt$writeToExcelWorksheet(wb=wb, wsName=i, 
                               topRowNumber=1, leftMostColumnNumber=1, 
                               applyStyles=TRUE, mapStylesFromCSS=TRUE)
      
      
    }
    
    #library(openxlsx)
    #wb <- createWorkbook(creator = Sys.getenv("USERNAME"))
    #addWorksheet(wb, "Data")
    #bt$writeToExcelWorksheet(wb=wb, wsName="Data", topRowNumber=1, leftMostColumnNumber=1, applyStyles=TRUE, mapStylesFromCSS=TRUE)
    #saveWorkbook(wb, file="./2. Statistical Annex/2020/0_WorkingFiles_ZL/first_example_table3.xlsx", overwrite = TRUE)
    

    
    
