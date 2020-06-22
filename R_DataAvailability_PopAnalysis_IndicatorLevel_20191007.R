
Sys.setenv(JAVA_HOME='C:\\Program Files (x86)\\Java\\jre1.8.0_201') # for 32-bit version
library(rJava)

#Background Document:Y:\SSB\SDGs\SDG Database\2. Country Data\2017\6. Data Availability\R.Script

#Code for pulling data by Country

#set working directory
setwd("Y:/SSB/SDGs/SDG Database/2. Country Data/2018/6. Data Availability/1.PopANDRegionalAnalysis")
#OneDrive Working Directory
setwd("C:/Users/Zin.Lin/OneDrive - United Nations/P_SDGDB_Database Analysis/2019_Q3/PopAnalysis")


#load RODBC and call database
library(RODBC) #load RODBC package
library(dplyr)
library(openxlsx)

  #here is to pull data from SQL 51 Server.
  odbcChannel <<- odbcConnect("SDGDB51")
  AllDataSQL <<- paste0("select * from dbo.ObservationDn")
  mydf <<- sqlQuery(odbcChannel,AllDataSQL,as.is = TRUE) 

  #Or to load RData file from local drive. 
  load("2019Data_20191007.RData")
  mydf <- mydata

  #mydataCCA <<- dplyr::filter(mydf,Nature %in% c("C","CA"))
  #mydataEM <<- dplyr::filter(mydf,Nature %in% c("E","M"))
  #save(mydataCAC, file = "2018Data_20181004_CA-Only.RData")
  #mydata <<- mydataCCA[,c(2:4,6:16,23,25)]
  #mydata <<- mydataEM[,c(2:4,6:16,23,25)]

  #choosing the columns 
    dim_drop <- c("GeoAreaCode","SeriesCode","SeriesDescription","TimePeriod","Value","Sex","Age","Location","Bounds","Level/Status","Name of international agreement","Education level","Type of product","Type of facilities","Name of international institution","Type of occupation","Tariff regime (status)","Type of skill","Mode of transportation","Type of mobile technology","Name of non-communicable disease","Type of speed","Migratory status","Disability status","Hazard type","IHR Capacity","Cities","Quantile")
      
      Annex_dim <- c("Sex","Age","Location","Bounds","Level/Status","Name of international agreement","Education level","Type of product","Type of facilities","Name of international institution","Type of occupation","Tariff regime (status)","Type of skill","Mode of transportation","Type of mobile technology","Name of non-communicable disease","Type of speed","Migratory status","Disability status","Hazard type","IHR Capacity","Cities","Quantile")
      
      dim_drop <- c("Sex","Age","Location","Bounds","Level/Status","Name of international agreement","Education level","Type of product","Type of facilities","Name of international institution","Type of occupation","Tariff regime (status)","Type of skill","Mode of transportation","Type of mobile technology","Name of non-communicable disease","Type of speed","Migratory status","Disability status","Hazard type","IHR Capacity","Cities","Quantile")
      dim_include <- c("Goal","Target","Indicator","ReleaseName","SeriesID","SeriesCode","SeriesDescription","GeoAreaCode","GeoAreaName","TimePeriod","Value","Nature","Units")
    
    # filtering data for the   
    mydata <<- mydf[names(mydf) %in% dim_include]
    #save(mydf,file = "2018Data_20190228.RData")
    #load("2018Data_20190228.RData")

    #For Excluding E and M Only
    #mydata1 <<- dplyr::filter(mydf,Nature != "E"|Nature != "M")
    #mydata <<- mydata1[,c(2:4,6:16,23,25)]

    
    Pop.Data <<- read.xlsx("C:/Users/Zin.Lin/OneDrive - United Nations/SDG_ReferenceFiles/PopulationData_20170817_Yearbook.xlsx",sheet="POP") #create a table containing series used in country profiles
    Pop.Data.Total <<- dplyr::filter(Pop.Data,POP.SEX=="T")
    IndicatorLevel <<- read.xlsx("C:/Users/Zin.Lin/OneDrive - United Nations/P_SDGDB_Data Modelling/SDGData_Modeling_20190118.xlsx",sheet="DB-DSD") #create the indicator code table
    IndicatorLevel <<- unique(IndicatorLevel[,c("SeriesCode","IndicatorCode","Level.Code")])
    IndicatorLevel_IndOnly <<- unique(dplyr::select(IndicatorLevel,IndicatorCode,Level.Code))
    Goal_Indicator <<- read.xlsx("C:/Users/Zin.Lin/OneDrive - United Nations/P_SDGDB_Data Modelling/SDGData_Modeling_20190118.xlsx",sheet="Goal_Indicator") #create the indicator code table
    Goal_Indicator <<- unique(Goal_Indicator)
    
    RefAreaDisaggregated_BYAREA <<- read.xlsx("C:/Users/Zin.Lin/OneDrive - United Nations/SDG_ReferenceFiles/SDG_ReferenceArea.xlsx", sheet="REF_AREA_DISAGGREGATED") #create the reference area table
    Tiering.2017 <<- read.xlsx("C:/Users/Zin.Lin/OneDrive - United Nations/SDG_ReferenceFiles/Tiering_20170905.xlsx",sheet="Main") #create data file with tier level data.
    #Unit_Limits <<- read.xlsx("Y:/SSB/SDGs/SDG Database/2. Country Data/2017/4. Data Validation/1. Documents and Reference Files/Unit_Upper.Lower.Limits.xlsx", sheet = "Unit.Limits") #create a table for the upper and lower limits of unit
    #Regions <<- read.xlsx("Y:/SSB/SDGs/SDG Database/2. Country Data/2017/6. Data Availability/Tiering_20170905.xlsx",sheet="Main") #create data file with tier level data.

    #create columns containing information about country/region disaggregations and indicator/goal information.
    mydata <- merge(x = mydata, y = Pop.Data.Total, by.x = "GeoAreaCode", by.y = "M49_CODE", all.x = TRUE)
    mydata <- merge(x = mydata, y = RefAreaDisaggregated_BYAREA, by.x = "GeoAreaCode", by.y = "M49", all.x = TRUE)
    mydata <- merge(x = mydata, y = IndicatorLevel, by = "SeriesCode", all.x = TRUE)

    #selecting only the country data
    mydata_filter <- dplyr::filter(mydata,Disaggregation == 1)
    mydata_filter <- mydata
    '%!in%' <- Negate('%in%')          
    
    #copying mydata data to val.data - to keep records of mydata
    #val.data <- mydata_filter
    val.data <- mydata_filter %>%
      filter(Value %!in% c(NA))
    
      
    
##############################################
#the following is the main function for data availability by Series and Indicator. It gives the number of data points per country for each country; total number of datapoints, number of countries with at least one datapoint, two datapoints and one each before and after 2010.

    # At Series Level
    main.fun <- function(val.data){
        library(dplyr)
    #total number of datapoints
        totcount <- data.frame(val.data) %>% 
          group_by(SeriesCode) %>% 
          count(GeoAreaCode) %>%
          rename(total.dpts.count = "n") %>%
          as.data.frame(.) %>%
          mutate(Expr2 = paste(.$SeriesCode,.$GeoAreaCode, sep = ""))
        
          
    #before2010
        before.2010.count <- as.data.frame(val.data) %>%
          filter(TimePeriod<2010) %>%
          group_by(SeriesCode) %>% 
          count(GeoAreaCode) %>%
          rename(before.ten.count = "n") %>%
          as.data.frame(.) %>%
          mutate(Expr2 = paste(.$SeriesCode, .$GeoAreaCode, sep = ""))
        
        bf2010 <- before.2010.count %>%
          select(SeriesCode,before.ten.count) %>%
          group_by(SeriesCode) %>%
          summarise_each(funs(sum))
          
          
    
    #on or after 2010
        after.n.2010.count <- as.data.frame(val.data) %>%
          filter(TimePeriod>2009) %>%
          group_by(SeriesCode) %>% 
          count(GeoAreaCode) %>%
          rename(after.n.ten.count = "n") %>%
          as.data.frame(.) %>%
          mutate(Expr2 = paste(.$SeriesCode, .$GeoAreaCode, sep = ""))
        
        af2010 <- after.n.2010.count %>%
          select(SeriesCode,after.n.ten.count) %>%
          group_by(SeriesCode) %>%
          summarise_each(funs(sum))
        
    #merging all three based on Expr2
        val.data.count <- merge(x = totcount, y = before.2010.count, by = c("Expr2","SeriesCode","GeoAreaCode"), all.x = TRUE)
        val.data.count <- merge(x = val.data.count, y = after.n.2010.count, by = c("Expr2","SeriesCode","GeoAreaCode"), all.x = TRUE)
        val.data.count <- val.data.count %>%
          select(Expr2,SeriesCode,GeoAreaCode,total.dpts.count,before.ten.count,after.n.ten.count) %>%
          mutate(at.least.one = ifelse(is.na(.$total.dpts.count),0,ifelse(.$total.dpts.count>0,1,0)),
                 at.least.two = ifelse(is.na(.$total.dpts.count),0,ifelse(.$total.dpts.count>1,1,0)),
                 analysis = ifelse(is.na(.$before.ten.count)&is.na(.$after.n.ten.count),0,ifelse((.$before.ten.count>0)&(.$after.n.ten.count>0),1,0)))
        
        gen.val <- val.data.count %>%
          select(SeriesCode,total.dpts.count,at.least.one,at.least.two,analysis)%>%
          group_by(SeriesCode) %>%
          summarise_each(funs(sum))
        
        gen.val <- merge(gen.val,bf2010,by = "SeriesCode", all = TRUE)
        gen.val <- merge(gen.val,af2010,by = "SeriesCode", all = TRUE)
        
    
    write.csv(val.data.count,"ByCountryVal_SDGDB_20191009.csv",row.names = FALSE)
    write.csv(gen.val,"SeriesVal_20191009.csv", row.names = FALSE)    
    # the following counts the data by SeriesCode
    
    Count_SC <- val.data %>%
      filter(Value %!in% c(NA,"NV","N")) %>%
      group_by(SeriesCode) %>%
      count(SeriesCode)

    Count_Ind <- val.data %>%
      filter(Value %!in% c(NA,"NV","N")) %>%
      group_by(IndicatorCode) %>%
      count(IndicatorCode)
    write.csv(Count_Ind,"IndVal_20191009.csv", row.names = FALSE)        
    
    } #main function closes
    
    main.fun(val.data)

#############################


#merging val.data.count data set with regional grouping information
val.data.reg <- merge(x = val.data.count, y = RefAreaDisaggregated_BYAREA, by.x = "GeoAreaCode",by.y = "M49", all.x = TRUE)
val.data.reg <- merge(x = val.data.reg, y = Pop.Data.Total, by.x = "GeoAreaCode",by.y = "M49_CODE", all.x = TRUE)
val.data.reg <- merge(x = val.data.reg, y = IndicatorLevel, by = "SeriesCode", all.x = TRUE)

write.csv(val.data.reg,"val.data.reg.20191009.csv")

val.data.reg <- dplyr::filter(val.data.reg,MemberList==1)
Pop.Data.Main <- merge(x = Pop.Data.Total, y = RefAreaDisaggregated_BYAREA, by.x = "M49_CODE",by.y = "M49", all.x = TRUE) #preparing population master file
Pop.Data.Main <<- filter(Pop.Data.Main,MemberList==1)


#looping starts here.

#######################################
#Data.Val.1 - by indicator


#subsetting starts
output <- matrix(0,ncol = 100, nrow = 0)
output <- data.frame(output)
colnames(output) <- c("SeriesCode","IndicatorCode","at.least.one.world","at.least.one.africa","at.least.one.asia","at.least.one.europe","at.least.one.LAC","at.least.one.northam","at.least.one.americas","at.least.one.oceania","at.least.one.apac","at.least.one.eunort","at.least.one.developed","at.least.one.developing","at.least.one.FrancophoneAfrica","at.least.one.Arab","at.least.two.world","at.least.two.africa","at.least.two.asia","at.least.two.europe","at.least.two.LAC","at.least.two.northam","at.least.two.americas","at.least.two.oceania","at.least.two.apac","at.least.two.eunort","at.least.two.developed","at.least.two.developing","at.least.two.FrancophoneAfrica","at.least.two.Arab","analysis.world","analysis.africa","analysis.asia","analysis.europe","analysis.LAC","analysis.northam","analysis.americas","analysis.oceania","analysis.apac","analysis.eunort","analysis.developed","analysis.developing","analysis.FrancophoneAfrica","analysis.Arab","tot.dpt.world","tot.dpt.africa","tot.dpt.asia","tot.dpt.europe","tot.dpt.LAC","tot.dpt.northam","tot.dpt.americas","tot.dpt.oceania","tot.dpt.apac","tot.dpt.eunort","tot.dpt.developed","tot.dpt.developing","tot.dpt.FrancophoneAfrica","tot.dpt.Arab","pop.world","pop.africa","pop.asia","pop.europe","pop.LAC","pop.northam","pop.americas","pop.oceania","pop.apac","pop.eunort","pop.developed","pop.developing","pop.FrancophoneAfrica","pop.Arab","no.country.world","no.country.africa","no.country.asia","no.country.europe","no.country.LAC","no.country.northam","no.country.americas","no.country.oceania","no.country.apac","no.country.eunort","no.country.developed","no.country.developing","no.country.FrancophoneAfrica","no.country.Arab","val.pop.world","val.pop.africa","val.pop.asia","val.pop.europe","val.pop.LAC","val.pop.northam","val.pop.americas","val.pop.oceania","val.pop.apac","val.pop.eunort","val.pop.developed","val.pop.developing","val.pop.FrancophoneAfrica","val.pop.Arab")													

#just for testing
#series.data <<- dplyr::filter(val.data.reg,SeriesCode == "DC_ODA_LLDCT000_099_YT") #subsetting by ref.area


main.fun <- function(val.data.reg){
 
  for (i in unique(val.data.reg$SeriesCode)) {
    Series.data <<- dplyr::filter(val.data.reg,SeriesCode == i) #subsetting by ref.area
    lvl.code <<- Series.data$Level.Code[1]

    #Finding availability values
    #for filtering data based on the level of applicability of indicator (aka Level.Code)
    val.a <<- if(Series.data$Level.Code[1] == 1){dplyr::filter(Series.data,MemberList==1)}else
    {if(Series.data$Level.Code[1] == 2){dplyr::filter(Series.data,MDG.Dev.Code==515)}else
    {if(Series.data$Level.Code[1] == 3){dplyr::filter(Series.data,MDG.Dev.Code==514)}else
    {if(Series.data$Level.Code[1] == 4){dplyr::filter(Series.data,Malaria.Prone==1)}else
    {if(Series.data$Level.Code[1] == 5){dplyr::filter(Series.data,FGM.Prone==1)}else
    {if(Series.data$Level.Code[1] == 6){dplyr::filter(Series.data,LLDC.SIDS.Code!=432)}else
    {"NA"}}}}}}
    
    
    #Finding population values for each region based on the indicator applicability
    pop.f <<- if(Series.data$Level.Code[1] == 1){dplyr::filter(Pop.Data.Main,MemberList==1)}else
    {if(Series.data$Level.Code[1] == 2){dplyr::filter(Pop.Data.Main,MDG.Dev.Code==515)}else
    {if(Series.data$Level.Code[1] == 3){dplyr::filter(Pop.Data.Main,MDG.Dev.Code==514)}else
    {if(Series.data$Level.Code[1] == 4){dplyr::filter(Pop.Data.Main,Malaria.Prone==1)}else
    {if(Series.data$Level.Code[1] == 5){dplyr::filter(Pop.Data.Main,FGM.Prone==1)}else
    {if(Series.data$Level.Code[1] == 6){dplyr::filter(Pop.Data.Main,LLDC.SIDS.Code!=432)}else
    {"NA"}}}}}}
    
    
    #world
    val.world <<- dplyr::filter(val.a,MemberList==1) #dataset filtered for "world"
    pdata.world <<- dplyr::filter(pop.f,MemberList==1) #population dataset filtered for "world"
    at.least.one.world <<- if(nrow(val.world)==0) {NA} else {sum(val.world$at.least.one,na.rm = TRUE)} #number of countries with at least one data point
    at.least.two.world <<- if(nrow(val.world)==0) {NA} else {sum(val.world$at.least.two,na.rm = TRUE)} #number of countries with at least two data points
    analysis.world <<- if(nrow(val.world)==0) {NA} else {sum(val.world$analysis,na.rm = TRUE)} #number of countries where analysis is possible
    tot.dpt.world <<- if(nrow(val.world)==0) {NA} else {sum(val.world$total.dpt.count,na.rm = TRUE)} #total number of datapoints count
    pop.world <<- if(nrow(pdata.world)==0) {NA} else {sum(pdata.world$POP.VALUE,na.rm = TRUE)} #total population within the region
    no.country.world <<- if(nrow(pdata.world)==0) {NA} else {sum(pdata.world$MemberList, na.rm = TRUE)} #number of countries within the region considered
    val.pop.world <<- if(nrow(val.world)==0) {NA} else {sum(val.world$POP.VALUE,na.rm = TRUE)} #population total of countries with data
    no.pop.world <<- if(nrow(val.world)==0) {NA} else {sum(val.world$MemberList,na.rm = TRUE)} #this last line is NOT needed.
    
    #Africa
    val.africa <<- dplyr::filter(val.a,MemberList==1,AFRICA==1)
    pdata.africa <<- dplyr::filter(pop.f,MemberList==1,AFRICA==1)
    at.least.one.africa <<- if(nrow(val.africa)==0) {NA} else {sum(val.africa$at.least.one,na.rm = TRUE)}
    at.least.two.africa <<- if(nrow(val.africa)==0) {NA} else {sum(val.africa$at.least.two,na.rm = TRUE)}
    tot.dpt.africa <<- if(nrow(val.africa)==0) {NA} else {sum(val.africa$total.dpt.count,na.rm = TRUE)}
    analysis.africa <<- if(nrow(val.africa)==0) {NA} else {sum(val.africa$analysis,na.rm = TRUE)}
    pop.africa <<- if(nrow(pdata.africa)==0) {NA} else {sum(pdata.africa$POP.VALUE,na.rm = TRUE)}
    no.country.africa <<- if(nrow(pdata.africa)==0) {NA} else {sum(pdata.africa$MemberList)}
    val.pop.africa <<- if(nrow(val.africa)==0) {NA} else {sum(val.africa$POP.VALUE,na.rm = TRUE)}
    no.pop.africa <<- if(nrow(val.africa)==0) {NA} else {sum(val.africa$MemberList,na.rm = TRUE)}
    
    #Asia
    val.asia <<- dplyr::filter(val.a,MemberList==1,ASIA==1)
    pdata.asia <<- dplyr::filter(pop.f,MemberList==1,ASIA==1)
    at.least.one.asia <<- if(nrow(val.asia)==0) {NA} else {sum(val.asia$at.least.one,na.rm = TRUE)}
    at.least.two.asia <<- if(nrow(val.asia)==0) {NA} else {sum(val.asia$at.least.two,na.rm = TRUE)}
    analysis.asia <<- if(nrow(val.asia)==0) {NA} else {sum(val.asia$analysis,na.rm = TRUE)}
    tot.dpt.asia <<- if(nrow(val.asia)==0) {NA} else {sum(val.asia$total.dpt.count,na.rm = TRUE)}
    pop.asia <<- if(nrow(pdata.asia)==0) {NA} else {sum(pdata.asia$POP.VALUE,na.rm = TRUE)}
    no.country.asia <<- if(nrow(pdata.asia)==0) {NA} else {sum(pdata.asia$MemberList)}
    val.pop.asia <<- if(nrow(val.asia)==0) {NA} else {sum(val.asia$POP.VALUE,na.rm = TRUE)}
    no.pop.asia <<- if(nrow(val.asia)==0) {NA} else {sum(val.asia$MemberList,na.rm = TRUE)}
    
    #Europe
    val.europe <<- dplyr::filter(val.a,MemberList==1,EUROPE==1)
    pdata.europe <<- dplyr::filter(pop.f,MemberList==1,EUROPE==1)
    at.least.one.europe <<- if(nrow(val.europe)==0) {NA} else {sum(val.europe$at.least.one,na.rm = TRUE)}
    at.least.two.europe <<- if(nrow(val.europe)==0) {NA} else {sum(val.europe$at.least.two,na.rm = TRUE)}
    analysis.europe <<- if(nrow(val.europe)==0) {NA} else {sum(val.europe$analysis,na.rm = TRUE)}
    tot.dpt.europe <<- if(nrow(val.europe)==0) {NA} else {sum(val.europe$total.dpt.count,na.rm = TRUE)}
    pop.europe <<- if(nrow(pdata.europe)==0) {NA} else {sum(pdata.europe$POP.VALUE,na.rm = TRUE)}
    no.country.europe <<- if(nrow(pdata.europe)==0) {NA} else {sum(pdata.europe$MemberList)}
    val.pop.europe <<- if(nrow(val.europe)==0) {NA} else {sum(val.europe$POP.VALUE,na.rm = TRUE)}
    no.pop.europe <<- if(nrow(val.europe)==0) {NA} else {sum(val.europe$MemberList,na.rm = TRUE)}
    
    #LAC
    val.LAC <<- dplyr::filter(val.a,MemberList==1,LAC==1)
    pdata.LAC <<- dplyr::filter(pop.f,MemberList==1,LAC==1)
    at.least.one.LAC <<- if(nrow(val.LAC)==0) {NA} else {sum(val.LAC$at.least.one,na.rm = TRUE)}
    at.least.two.LAC <<- if(nrow(val.LAC)==0) {NA} else {sum(val.LAC$at.least.two,na.rm = TRUE)}
    analysis.LAC <<- if(nrow(val.LAC)==0) {NA} else {sum(val.LAC$analysis,na.rm = TRUE)}
    tot.dpt.LAC <<- if(nrow(val.LAC)==0) {NA} else {sum(val.LAC$total.dpt.count,na.rm = TRUE)}
    pop.LAC <<- if(nrow(pdata.LAC)==0) {NA} else {sum(pdata.LAC$POP.VALUE,na.rm = TRUE)}
    no.country.LAC <<- if(nrow(pdata.LAC)==0) {NA} else {sum(pdata.LAC$MemberList)}
    val.pop.LAC <<- if(nrow(val.LAC)==0) {NA} else {sum(val.LAC$POP.VALUE,na.rm = TRUE)}
    no.pop.LAC <<- if(nrow(val.LAC)==0) {NA} else {sum(val.LAC$MemberList,na.rm = TRUE)}
    
    #NorthAmerica
    val.northam <<- dplyr::filter(val.a,MemberList==1,NORTHAME==1)
    pdata.northam <<- dplyr::filter(pop.f,MemberList==1,NORTHAME==1)
    at.least.one.northam <<- if(nrow(val.northam)==0) {NA} else {sum(val.northam$at.least.one,na.rm = TRUE)}
    at.least.two.northam <<- if(nrow(val.northam)==0) {NA} else {sum(val.northam$at.least.two,na.rm = TRUE)}
    analysis.northam <<- if(nrow(val.northam)==0) {NA} else {sum(val.northam$analysis,na.rm = TRUE)}
    tot.dpt.northam <<- if(nrow(val.northam)==0) {NA} else {sum(val.northam$total.dpt.count,na.rm = TRUE)}
    pop.northam <<- if(nrow(pdata.northam)==0) {NA} else {sum(pdata.northam$POP.VALUE,na.rm = TRUE)}
    no.country.northam <<- if(nrow(pdata.northam)==0) {NA} else {sum(pdata.northam$MemberList)}
    val.pop.northam <<- if(nrow(val.northam)==0) {NA} else {sum(val.northam$POP.VALUE,na.rm = TRUE)}
    no.pop.northam <<- if(nrow(val.northam)==0) {NA} else {sum(val.northam$MemberList,na.rm = TRUE)}
    
    #Americas
    val.americas <<- dplyr::filter(val.a,MemberList==1,AMERICAS==1)
    pdata.americas <<- dplyr::filter(pop.f,MemberList==1,AMERICAS==1)
    at.least.one.americas <<- if(nrow(val.americas)==0) {NA} else {sum(val.americas$at.least.one,na.rm = TRUE)}
    at.least.two.americas <<- if(nrow(val.americas)==0) {NA} else {sum(val.americas$at.least.two,na.rm = TRUE)}
    analysis.americas <<- if(nrow(val.americas)==0) {NA} else {sum(val.americas$analysis,na.rm = TRUE)}
    tot.dpt.americas <<- if(nrow(val.americas)==0) {NA} else {sum(val.americas$total.dpt.count,na.rm = TRUE)}
    pop.americas <<- if(nrow(pdata.americas)==0) {NA} else {sum(pdata.americas$POP.VALUE,na.rm = TRUE)}
    no.country.americas <<- if(nrow(pdata.americas)==0) {NA} else {sum(pdata.americas$MemberList)}
    val.pop.americas <<- if(nrow(val.americas)==0) {NA} else {sum(val.americas$POP.VALUE,na.rm = TRUE)}
    no.pop.americas <<- if(nrow(val.americas)==0) {NA} else {sum(val.americas$MemberList,na.rm = TRUE)}
    
    
    #Oceania
    val.oceania <<- dplyr::filter(val.a,MemberList==1,OCEANIA==1)
    pdata.oceania <<- dplyr::filter(pop.f,MemberList==1,OCEANIA==1)
    at.least.one.oceania <<- if(nrow(val.oceania)==0) {NA} else {sum(val.oceania$at.least.one,na.rm = TRUE)}
    at.least.two.oceania <<- if(nrow(val.oceania)==0) {NA} else {sum(val.oceania$at.least.two,na.rm = TRUE)}
    analysis.oceania <<- if(nrow(val.oceania)==0) {NA} else {sum(val.oceania$analysis,na.rm = TRUE)}
    tot.dpt.oceania <<- if(nrow(val.oceania)==0) {NA} else {sum(val.oceania$total.dpt.count,na.rm = TRUE)}
    pop.oceania <<- if(nrow(pdata.oceania)==0) {NA} else {sum(pdata.oceania$POP.VALUE,na.rm = TRUE)}
    no.country.oceania <<- if(nrow(pdata.oceania)==0) {NA} else {sum(pdata.oceania$MemberList)}
    val.pop.oceania <<- if(nrow(val.oceania)==0) {NA} else {sum(val.oceania$POP.VALUE,na.rm = TRUE)}
    no.pop.oceania <<- if(nrow(val.oceania)==0) {NA} else {sum(val.oceania$MemberList,na.rm = TRUE)}
    
    #Asia and Pacific
    val.apac <<- dplyr::filter(val.a,MemberList==1,APAC==1)
    pdata.apac <<- dplyr::filter(pop.f,MemberList==1,APAC==1)
    at.least.one.apac <<- if(nrow(val.apac)==0) {NA} else {sum(val.apac$at.least.one,na.rm = TRUE)}
    at.least.two.apac <<- if(nrow(val.apac)==0) {NA} else {sum(val.apac$at.least.two,na.rm = TRUE)}
    analysis.apac <<- if(nrow(val.apac)==0) {NA} else {sum(val.apac$analysis,na.rm = TRUE)}
    tot.dpt.apac <<- if(nrow(val.apac)==0) {NA} else {sum(val.apac$total.dpt.count,na.rm = TRUE)}
    pop.apac <<- if(nrow(pdata.apac)==0) {NA} else {sum(pdata.apac$POP.VALUE,na.rm = TRUE)}
    no.country.apac <<- if(nrow(pdata.apac)==0) {NA} else {sum(pdata.apac$MemberList)}
    val.pop.apac <<- if(nrow(val.apac)==0) {NA} else {sum(val.apac$POP.VALUE,na.rm = TRUE)}
    no.pop.apac <<- if(nrow(val.apac)==0) {NA} else {sum(val.apac$MemberList,na.rm = TRUE)}
    
    #Europe and North America
    val.eunort <<- dplyr::filter(val.a,MemberList==1,EUNORT==1)
    pdata.eunort <<- dplyr::filter(pop.f,MemberList==1,EUNORT==1)
    at.least.one.eunort <<- if(nrow(val.eunort)==0) {NA} else {sum(val.eunort$at.least.one,na.rm = TRUE)}
    at.least.two.eunort <<- if(nrow(val.eunort)==0) {NA} else {sum(val.eunort$at.least.two,na.rm = TRUE)}
    analysis.eunort <<- if(nrow(val.eunort)==0) {NA} else {sum(val.eunort$analysis,na.rm = TRUE)}
    tot.dpt.eunort <<- if(nrow(val.eunort)==0) {NA} else {sum(val.eunort$total.dpt.count,na.rm = TRUE)}
    pop.eunort <<- if(nrow(pdata.eunort)==0) {NA} else {sum(pdata.eunort$POP.VALUE,na.rm = TRUE)}
    no.country.eunort <<- if(nrow(pdata.eunort)==0) {NA} else {sum(pdata.eunort$MemberList)}
    val.pop.eunort <<- if(nrow(val.eunort)==0) {NA} else {sum(val.eunort$POP.VALUE,na.rm = TRUE)}
    no.pop.eunort <<- if(nrow(val.eunort)==0) {NA} else {sum(val.eunort$MemberList,na.rm = TRUE)}
    
    #Developed
    val.developed <<- dplyr::filter(val.a,MemberList==1,MDG.Dev.Code==514)
    pdata.developed <<- dplyr::filter(pop.f,MemberList==1,MDG.Dev.Code==514)
    at.least.one.developed <<- if(nrow(val.developed)==0) {NA} else {sum(val.developed$at.least.one,na.rm = TRUE)}
    at.least.two.developed <<- if(nrow(val.developed)==0) {NA} else {sum(val.developed$at.least.two,na.rm = TRUE)}
    analysis.developed <<- if(nrow(val.developed)==0) {NA} else {sum(val.developed$analysis,na.rm = TRUE)}
    tot.dpt.developed <<- if(nrow(val.developed)==0) {NA} else {sum(val.developed$total.dpt.count,na.rm = TRUE)}
    pop.developed <<- if(nrow(pdata.developed)==0) {NA} else {sum(pdata.developed$POP.VALUE,na.rm = TRUE)}
    no.country.developed <<- if(nrow(pdata.developed)==0) {NA} else {sum(pdata.developed$MemberList)}
    val.pop.developed <<- if(nrow(val.developed)==0) {NA} else {sum(val.developed$POP.VALUE,na.rm = TRUE)}
    no.pop.developed <<- if(nrow(val.developed)==0) {NA} else {sum(val.developed$MemberList,na.rm = TRUE)}
    
    #Developing
    val.developing <<- dplyr::filter(val.a,MemberList==1,MDG.Dev.Code==515)
    pdata.developing <<- dplyr::filter(pop.f,MemberList==1,MDG.Dev.Code==515)
    at.least.one.developing <<- if(nrow(val.developing)==0) {NA} else {sum(val.developing$at.least.one,na.rm = TRUE)}
    at.least.two.developing <<- if(nrow(val.developing)==0) {NA} else {sum(val.developing$at.least.two,na.rm = TRUE)}
    analysis.developing <<- if(nrow(val.developing)==0) {NA} else {sum(val.developing$analysis,na.rm = TRUE)}
    tot.dpt.developing <<- if(nrow(val.developing)==0) {NA} else {sum(val.developing$total.dpt.count,na.rm = TRUE)}
    pop.developing <<- if(nrow(pdata.developing)==0) {NA} else {sum(pdata.developing$POP.VALUE,na.rm = TRUE)}
    no.country.developing <<- if(nrow(pdata.developing)==0) {NA} else {sum(pdata.developing$MemberList)}
    val.pop.developing <<- if(nrow(val.developing)==0) {NA} else {sum(val.developing$POP.VALUE,na.rm = TRUE)}
    no.pop.developing <<- if(nrow(val.developing)==0) {NA} else {sum(val.developing$MemberList,na.rm = TRUE)}
    
    #FrancophoneAfrica
    val.FrancophoneAfrica <<- dplyr::filter(val.a,MemberList==1,FrancophoneAfrica==1)
    pdata.FrancophoneAfrica <<- dplyr::filter(pop.f,MemberList==1,FrancophoneAfrica==1)
    at.least.one.FrancophoneAfrica <<- if(nrow(val.FrancophoneAfrica)==0) {NA} else {sum(val.FrancophoneAfrica$at.least.one,na.rm = TRUE)}
    at.least.two.FrancophoneAfrica <<- if(nrow(val.FrancophoneAfrica)==0) {NA} else {sum(val.FrancophoneAfrica$at.least.two,na.rm = TRUE)}
    analysis.FrancophoneAfrica <<- if(nrow(val.FrancophoneAfrica)==0) {NA} else {sum(val.FrancophoneAfrica$analysis,na.rm = TRUE)}
    tot.dpt.FrancophoneAfrica <<- if(nrow(val.FrancophoneAfrica)==0) {NA} else {sum(val.FrancophoneAfrica$total.dpt.count,na.rm = TRUE)}
    pop.FrancophoneAfrica <<- if(nrow(pdata.FrancophoneAfrica)==0) {NA} else {sum(pdata.FrancophoneAfrica$POP.VALUE,na.rm = TRUE)}
    no.country.FrancophoneAfrica <<- if(nrow(pdata.FrancophoneAfrica)==0) {NA} else {sum(pdata.FrancophoneAfrica$MemberList)}
    val.pop.FrancophoneAfrica <<- if(nrow(val.FrancophoneAfrica)==0) {NA} else {sum(val.FrancophoneAfrica$POP.VALUE,na.rm = TRUE)}
    no.pop.FrancophoneAfrica <<- if(nrow(val.FrancophoneAfrica)==0) {NA} else {sum(val.FrancophoneAfrica$MemberList,na.rm = TRUE)}
    
    #Arab
    val.Arab <<- dplyr::filter(val.a,MemberList==1,Arab==1)
    pdata.Arab <<- dplyr::filter(pop.f,MemberList==1,Arab==1)
    at.least.one.Arab <<- if(nrow(val.Arab)==0) {NA} else {sum(val.Arab$at.least.one,na.rm = TRUE)}
    at.least.two.Arab <<- if(nrow(val.Arab)==0) {NA} else {sum(val.Arab$at.least.two,na.rm = TRUE)}
    analysis.Arab <<- if(nrow(val.Arab)==0) {NA} else {sum(val.Arab$analysis,na.rm = TRUE)}
    tot.dpt.Arab <<- if(nrow(val.Arab)==0) {NA} else {sum(val.Arab$total.dpt.count,na.rm = TRUE)}
    pop.Arab <<- if(nrow(pdata.Arab)==0) {NA} else {sum(pdata.Arab$POP.VALUE,na.rm = TRUE)}
    no.country.Arab <<- if(nrow(pdata.Arab)==0) {NA} else {sum(pdata.Arab$MemberList)}
    val.pop.Arab <<- if(nrow(val.Arab)==0) {NA} else {sum(val.Arab$POP.VALUE,na.rm = TRUE)}
    no.pop.Arab <<- if(nrow(val.Arab)==0) {NA} else {sum(val.Arab$MemberList,na.rm = TRUE)}

    #produce outputs in a table.
    a <- matrix(0,ncol = 100, nrow = 1)
    a <- data.frame(a)
    colnames(a) <- c("SeriesCode","IndicatorCode","at.least.one.world","at.least.one.africa","at.least.one.asia","at.least.one.europe","at.least.one.LAC","at.least.one.northam","at.least.one.americas","at.least.one.oceania","at.least.one.apac","at.least.one.eunort","at.least.one.developed","at.least.one.developing","at.least.one.FrancophoneAfrica","at.least.one.Arab","at.least.two.world","at.least.two.africa","at.least.two.asia","at.least.two.europe","at.least.two.LAC","at.least.two.northam","at.least.two.americas","at.least.two.oceania","at.least.two.apac","at.least.two.eunort","at.least.two.developed","at.least.two.developing","at.least.two.FrancophoneAfrica","at.least.two.Arab","analysis.world","analysis.africa","analysis.asia","analysis.europe","analysis.LAC","analysis.northam","analysis.americas","analysis.oceania","analysis.apac","analysis.eunort","analysis.developed","analysis.developing","analysis.FrancophoneAfrica","analysis.Arab","tot.dpt.world","tot.dpt.africa","tot.dpt.asia","tot.dpt.europe","tot.dpt.LAC","tot.dpt.northam","tot.dpt.americas","tot.dpt.oceania","tot.dpt.apac","tot.dpt.eunort","tot.dpt.developed","tot.dpt.developing","tot.dpt.FrancophoneAfrica","tot.dpt.Arab","pop.world","pop.africa","pop.asia","pop.europe","pop.LAC","pop.northam","pop.americas","pop.oceania","pop.apac","pop.eunort","pop.developed","pop.developing","pop.FrancophoneAfrica","pop.Arab","no.country.world","no.country.africa","no.country.asia","no.country.europe","no.country.LAC","no.country.northam","no.country.americas","no.country.oceania","no.country.apac","no.country.eunort","no.country.developed","no.country.developing","no.country.FrancophoneAfrica","no.country.Arab","val.pop.world","val.pop.africa","val.pop.asia","val.pop.europe","val.pop.LAC","val.pop.northam","val.pop.americas","val.pop.oceania","val.pop.apac","val.pop.eunort","val.pop.developed","val.pop.developing","val.pop.FrancophoneAfrica","val.pop.Arab")													
    rownames(a) <- c("")
    a[1] <- c(paste(Series.data$SeriesCode[1]))
    a[2] <- c(paste(Series.data$IndicatorCode[1]))
    a[3] <- c(at.least.one.world)
    a[4] <- c(at.least.one.africa)
    a[5] <- c(at.least.one.asia)
    a[6] <- c(at.least.one.europe)
    a[7] <- c(at.least.one.LAC)
    a[8] <- c(at.least.one.northam)
    a[9] <- c(at.least.one.americas)
    a[10] <- c(at.least.one.oceania)
    a[11] <- c(at.least.one.apac)
    a[12] <- c(at.least.one.eunort)
    a[13] <- c(at.least.one.developed)
    a[14] <- c(at.least.one.developing)
    a[15] <- c(at.least.one.FrancophoneAfrica)
    a[16] <- c(at.least.one.Arab)
    a[17] <- c(at.least.two.world)
    a[18] <- c(at.least.two.africa)
    a[19] <- c(at.least.two.asia)
    a[20] <- c(at.least.two.europe)
    a[21] <- c(at.least.two.LAC)
    a[22] <- c(at.least.two.northam)
    a[23] <- c(at.least.two.americas)
    a[24] <- c(at.least.two.oceania)
    a[25] <- c(at.least.two.apac)
    a[26] <- c(at.least.two.eunort)
    a[27] <- c(at.least.two.developed)
    a[28] <- c(at.least.two.developing)
    a[29] <- c(at.least.two.FrancophoneAfrica)
    a[30] <- c(at.least.two.Arab)
    a[31] <- c(analysis.world)
    a[32] <- c(analysis.africa)
    a[33] <- c(analysis.asia)
    a[34] <- c(analysis.europe)
    a[35] <- c(analysis.LAC)
    a[36] <- c(analysis.northam)
    a[37] <- c(analysis.americas)
    a[38] <- c(analysis.oceania)
    a[39] <- c(analysis.apac)
    a[40] <- c(analysis.eunort)
    a[41] <- c(analysis.developed)
    a[42] <- c(analysis.developing)
    a[43] <- c(analysis.FrancophoneAfrica)
    a[44] <- c(analysis.Arab)
    a[45] <- c(tot.dpt.world)
    a[46] <- c(tot.dpt.africa)
    a[47] <- c(tot.dpt.asia)
    a[48] <- c(tot.dpt.europe)
    a[49] <- c(tot.dpt.LAC)
    a[50] <- c(tot.dpt.northam)
    a[51] <- c(tot.dpt.americas)
    a[52] <- c(tot.dpt.oceania)
    a[53] <- c(tot.dpt.apac)
    a[54] <- c(tot.dpt.eunort)
    a[55] <- c(tot.dpt.developed)
    a[56] <- c(tot.dpt.developing)
    a[57] <- c(tot.dpt.FrancophoneAfrica)
    a[58] <- c(tot.dpt.Arab)
    a[59] <- c(pop.world)
    a[60] <- c(pop.africa)
    a[61] <- c(pop.asia)
    a[62] <- c(pop.europe)
    a[63] <- c(pop.LAC)
    a[64] <- c(pop.northam)
    a[65] <- c(pop.americas)
    a[66] <- c(pop.oceania)
    a[67] <- c(pop.apac)
    a[68] <- c(pop.eunort)
    a[69] <- c(pop.developed)
    a[70] <- c(pop.developing)
    a[71] <- c(pop.FrancophoneAfrica)
    a[72] <- c(pop.Arab)
    a[73] <- c(no.country.world)
    a[74] <- c(no.country.africa)
    a[75] <- c(no.country.asia)
    a[76] <- c(no.country.europe)
    a[77] <- c(no.country.LAC)
    a[78] <- c(no.country.northam)
    a[79] <- c(no.country.americas)
    a[80] <- c(no.country.oceania)
    a[81] <- c(no.country.apac)
    a[82] <- c(no.country.eunort)
    a[83] <- c(no.country.developed)
    a[84] <- c(no.country.developing)
    a[85] <- c(no.country.FrancophoneAfrica)
    a[86] <- c(no.country.Arab)
    a[87] <- c(val.pop.world)
    a[88] <- c(val.pop.africa)
    a[89] <- c(val.pop.asia)
    a[90] <- c(val.pop.europe)
    a[91] <- c(val.pop.LAC)
    a[92] <- c(val.pop.northam)
    a[93] <- c(val.pop.americas)
    a[94] <- c(val.pop.oceania)
    a[95] <- c(val.pop.apac)
    a[96] <- c(val.pop.eunort)
    a[97] <- c(val.pop.developed)
    a[98] <- c(val.pop.developing)
    a[99] <- c(val.pop.FrancophoneAfrica)
    a[100] <- c(val.pop.Arab)
    
    
    output <<- rbind(output,a)
    
    }
    }
    
    
 main.fun(val.data.reg)
 write.csv(output,"output_20191009.csv")
  
  # End of data val program
  ######################################

 #Indicator Level Data Availablility
 
 #At IndicatorLevel
 main.indicator.fun <- function(val.data){
   
   #total number of datapoints
   total.dpts.count <<- val.data %>% group_by(IndicatorCode) %>% count(GeoAreaCode)
   colnames(total.dpts.count)[3]<- "total.dpt.count"
   total.dpts.count <- as.data.frame(total.dpts.count)
   total.dpts.count <- mutate(total.dpts.count,Expr2=paste(total.dpts.count$IndicatorCode,total.dpts.count$GeoAreaCode, sep = ""))
   total.dpts.count <- as.data.frame(total.dpts.count)
   
   #before2010
   before.2010 <<- dplyr::filter(val.data,TimePeriod<2010) #data set  for data before 2010
   before.2010.count <- before.2010 %>% group_by(IndicatorCode) %>% count(GeoAreaCode)
   colnames(before.2010.count)[3]<- "before.ten.count"
   before.2010.count <- as.data.frame(before.2010.count)
   before.2010.count <- mutate(before.2010.count, Expr2 = paste(before.2010.count$IndicatorCode, before.2010.count$GeoAreaCode, sep = ""))
   before.2010.count <- as.data.frame(before.2010.count)
   
   #on or after 2010
   after.n.2010 <<- dplyr::filter(val.data,TimePeriod>=2010) #data set for data on and after 2010
   after.n.2010.count <- after.n.2010 %>% group_by(IndicatorCode) %>% count(GeoAreaCode)
   colnames(after.n.2010.count)[3]<- "after.ten.count"
   after.n.2010.count <- as.data.frame(after.n.2010.count)
   after.n.2010.count <- mutate(after.n.2010.count,Expr2=paste(after.n.2010.count$IndicatorCode,after.n.2010.count$GeoAreaCode, sep = ""))
   after.n.2010.count <- as.data.frame(after.n.2010.count)
   
   #merging all three based on Expr2
   val.data.count <- merge(x = total.dpts.count, y = before.2010.count, by = "Expr2", all.x = TRUE)
   val.data.count <- merge(x = val.data.count, y = after.n.2010.count, by = "Expr2", all.x = TRUE)
   val.data.count <- dplyr::select(val.data.count,Expr2,IndicatorCode.x,GeoAreaCode.x,total.dpt.count,before.ten.count,after.ten.count)
   colnames(val.data.count)[2] <- "IndicatorCode"
   colnames(val.data.count)[3] <- "GeoAreaCode"
   val.data.count = within(val.data.count,{at.least.one = ifelse(is.na(val.data.count$total.dpt.count),0,ifelse(val.data.count$total.dpt.count>0,1,0))})
   val.data.count = within(val.data.count,{at.least.two = ifelse(is.na(val.data.count$total.dpt.count),0,ifelse(val.data.count$total.dpt.count>1,1,0))})
   val.data.count = within(val.data.count,{analysis = ifelse(is.na(val.data.count$before.ten.count)&is.na(val.data.count$after.ten.count),0,ifelse((val.data.count$before.ten.count>0)&(val.data.count$after.ten.count>0),1,0))})
   
 } #main indicator function closes
 
 #main.fun(val.data)
 
 #############################
 
 
 #merging val.data.count data set with regional grouping information
 val.data.reg <- merge(x = val.data.count, y = RefAreaDisaggregated_BYAREA, by.x = "GeoAreaCode",by.y = "M49", all.x = TRUE)
 val.data.reg <- merge(x = val.data.reg, y = Pop.Data.Total, by.x = "GeoAreaCode",by.y = "M49_CODE", all.x = TRUE)
 val.data.reg <- merge(x = val.data.reg, y = IndicatorLevel_IndOnly, by = "IndicatorCode", all.x = TRUE)
 
 write.csv(val.data.reg,"val.data.reg.EM.Ind.20191009.csv")
 
 val.data.reg <- dplyr::filter(val.data.reg,MemberList==1)
 Pop.Data.Main <- merge(x = Pop.Data.Total, y = RefAreaDisaggregated_BYAREA, by.x = "M49_CODE",by.y = "M49", all.x = TRUE) #preparing population master file
 Pop.Data.Main <<- filter(Pop.Data.Main,MemberList==1)
 
 
 #looping starts here.
 
 #######################################
 #Data.Val.1 - by indicator
 
 
 #subsetting starts
 output <- matrix(0,ncol = 100, nrow = 0)
 output <- data.frame(output)
 colnames(output) <- c("SeriesCode","IndicatorCode","at.least.one.world","at.least.one.africa","at.least.one.asia","at.least.one.europe","at.least.one.LAC","at.least.one.northam","at.least.one.americas","at.least.one.oceania","at.least.one.apac","at.least.one.eunort","at.least.one.developed","at.least.one.developing","at.least.one.FrancophoneAfrica","at.least.one.Arab","at.least.two.world","at.least.two.africa","at.least.two.asia","at.least.two.europe","at.least.two.LAC","at.least.two.northam","at.least.two.americas","at.least.two.oceania","at.least.two.apac","at.least.two.eunort","at.least.two.developed","at.least.two.developing","at.least.two.FrancophoneAfrica","at.least.two.Arab","analysis.world","analysis.africa","analysis.asia","analysis.europe","analysis.LAC","analysis.northam","analysis.americas","analysis.oceania","analysis.apac","analysis.eunort","analysis.developed","analysis.developing","analysis.FrancophoneAfrica","analysis.Arab","tot.dpt.world","tot.dpt.africa","tot.dpt.asia","tot.dpt.europe","tot.dpt.LAC","tot.dpt.northam","tot.dpt.americas","tot.dpt.oceania","tot.dpt.apac","tot.dpt.eunort","tot.dpt.developed","tot.dpt.developing","tot.dpt.FrancophoneAfrica","tot.dpt.Arab","pop.world","pop.africa","pop.asia","pop.europe","pop.LAC","pop.northam","pop.americas","pop.oceania","pop.apac","pop.eunort","pop.developed","pop.developing","pop.FrancophoneAfrica","pop.Arab","no.country.world","no.country.africa","no.country.asia","no.country.europe","no.country.LAC","no.country.northam","no.country.americas","no.country.oceania","no.country.apac","no.country.eunort","no.country.developed","no.country.developing","no.country.FrancophoneAfrica","no.country.Arab","val.pop.world","val.pop.africa","val.pop.asia","val.pop.europe","val.pop.LAC","val.pop.northam","val.pop.americas","val.pop.oceania","val.pop.apac","val.pop.eunort","val.pop.developed","val.pop.developing","val.pop.FrancophoneAfrica","val.pop.Arab")													
 
 #just for testing
 #series.data <<- dplyr::filter(val.data.reg,SeriesCode == "DC_ODA_LLDCT000_099_YT") #subsetting by ref.area
 
 
 
 
 #subsetting starts
 output <- matrix(0,ncol = 100, nrow = 0)
 output <- data.frame(output)
 colnames(output) <- c("SeriesCode","IndicatorCode","at.least.one.world","at.least.one.africa","at.least.one.asia","at.least.one.europe","at.least.one.LAC","at.least.one.northam","at.least.one.americas","at.least.one.oceania","at.least.one.apac","at.least.one.eunort","at.least.one.developed","at.least.one.developing","at.least.one.FrancophoneAfrica","at.least.one.Arab","at.least.two.world","at.least.two.africa","at.least.two.asia","at.least.two.europe","at.least.two.LAC","at.least.two.northam","at.least.two.americas","at.least.two.oceania","at.least.two.apac","at.least.two.eunort","at.least.two.developed","at.least.two.developing","at.least.two.FrancophoneAfrica","at.least.two.Arab","analysis.world","analysis.africa","analysis.asia","analysis.europe","analysis.LAC","analysis.northam","analysis.americas","analysis.oceania","analysis.apac","analysis.eunort","analysis.developed","analysis.developing","analysis.FrancophoneAfrica","analysis.Arab","tot.dpt.world","tot.dpt.africa","tot.dpt.asia","tot.dpt.europe","tot.dpt.LAC","tot.dpt.northam","tot.dpt.americas","tot.dpt.oceania","tot.dpt.apac","tot.dpt.eunort","tot.dpt.developed","tot.dpt.developing","tot.dpt.FrancophoneAfrica","tot.dpt.Arab","pop.world","pop.africa","pop.asia","pop.europe","pop.LAC","pop.northam","pop.americas","pop.oceania","pop.apac","pop.eunort","pop.developed","pop.developing","pop.FrancophoneAfrica","pop.Arab","no.country.world","no.country.africa","no.country.asia","no.country.europe","no.country.LAC","no.country.northam","no.country.americas","no.country.oceania","no.country.apac","no.country.eunort","no.country.developed","no.country.developing","no.country.FrancophoneAfrica","no.country.Arab","val.pop.world","val.pop.africa","val.pop.asia","val.pop.europe","val.pop.LAC","val.pop.northam","val.pop.americas","val.pop.oceania","val.pop.apac","val.pop.eunort","val.pop.developed","val.pop.developing","val.pop.FrancophoneAfrica","val.pop.Arab")													

 
 
 main.ind.fun <- function(val.data.reg){
   
   for (i in unique(val.data.reg$IndicatorCode)) {
     Indicator.data <<- dplyr::filter(val.data.reg,IndicatorCode == i) #subsetting by ref.area
     lvl.code <<- Indicator.data$Level.Code[1]
     
     #Finding availability values
     #for filtering data based on the level of applicability of indicator (aka Level.Code)
     val.a <<- if(Indicator.data$Level.Code[1] == 1){dplyr::filter(Indicator.data,MemberList==1)}else
     {if(Indicator.data$Level.Code[1] == 2){dplyr::filter(Indicator.data,MDG.Dev.Code==515)}else
     {if(Indicator.data$Level.Code[1] == 3){dplyr::filter(Indicator.data,MDG.Dev.Code==514)}else
     {if(Indicator.data$Level.Code[1] == 4){dplyr::filter(Indicator.data,Malaria.Prone==1)}else
     {if(Indicator.data$Level.Code[1] == 5){dplyr::filter(Indicator.data,FGM.Prone==1)}else
     {if(Indicator.data$Level.Code[1] == 6){dplyr::filter(Indicator.data,LLDC.SIDS.Code!=432)}else
     {"NA"}}}}}}
     
     
     #Finding population values for each region based on the indicator applicability
     pop.f <<- if(Indicator.data$Level.Code[1] == 1){dplyr::filter(Pop.Data.Main,MemberList==1)}else
     {if(Indicator.data$Level.Code[1] == 2){dplyr::filter(Pop.Data.Main,MDG.Dev.Code==515)}else
     {if(Indicator.data$Level.Code[1] == 3){dplyr::filter(Pop.Data.Main,MDG.Dev.Code==514)}else
     {if(Indicator.data$Level.Code[1] == 4){dplyr::filter(Pop.Data.Main,Malaria.Prone==1)}else
     {if(Indicator.data$Level.Code[1] == 5){dplyr::filter(Pop.Data.Main,FGM.Prone==1)}else
     {if(Indicator.data$Level.Code[1] == 6){dplyr::filter(Pop.Data.Main,LLDC.SIDS.Code!=432)}else
     {"NA"}}}}}}
     
     
     #world
     val.world <<- dplyr::filter(val.a,MemberList==1) #dataset filtered for "world"
     pdata.world <<- dplyr::filter(pop.f,MemberList==1) #population dataset filtered for "world"
     at.least.one.world <<- if(nrow(val.world)==0) {NA} else {sum(val.world$at.least.one,na.rm = TRUE)} #number of countries with at least one data point
     at.least.two.world <<- if(nrow(val.world)==0) {NA} else {sum(val.world$at.least.two,na.rm = TRUE)} #number of countries with at least two data points
     analysis.world <<- if(nrow(val.world)==0) {NA} else {sum(val.world$analysis,na.rm = TRUE)} #number of countries where analysis is possible
     tot.dpt.world <<- if(nrow(val.world)==0) {NA} else {sum(val.world$total.dpt.count,na.rm = TRUE)} #total number of datapoints count
     pop.world <<- if(nrow(pdata.world)==0) {NA} else {sum(pdata.world$POP.VALUE,na.rm = TRUE)} #total population within the region
     no.country.world <<- if(nrow(pdata.world)==0) {NA} else {sum(pdata.world$MemberList, na.rm = TRUE)} #number of countries within the region considered
     val.pop.world <<- if(nrow(val.world)==0) {NA} else {sum(val.world$POP.VALUE,na.rm = TRUE)} #population total of countries with data
     no.pop.world <<- if(nrow(val.world)==0) {NA} else {sum(val.world$MemberList,na.rm = TRUE)} #this last line is NOT needed.
     
     #Africa
     val.africa <<- dplyr::filter(val.a,MemberList==1,AFRICA==1)
     pdata.africa <<- dplyr::filter(pop.f,MemberList==1,AFRICA==1)
     at.least.one.africa <<- if(nrow(val.africa)==0) {NA} else {sum(val.africa$at.least.one,na.rm = TRUE)}
     at.least.two.africa <<- if(nrow(val.africa)==0) {NA} else {sum(val.africa$at.least.two,na.rm = TRUE)}
     tot.dpt.africa <<- if(nrow(val.africa)==0) {NA} else {sum(val.africa$total.dpt.count,na.rm = TRUE)}
     analysis.africa <<- if(nrow(val.africa)==0) {NA} else {sum(val.africa$analysis,na.rm = TRUE)}
     pop.africa <<- if(nrow(pdata.africa)==0) {NA} else {sum(pdata.africa$POP.VALUE,na.rm = TRUE)}
     no.country.africa <<- if(nrow(pdata.africa)==0) {NA} else {sum(pdata.africa$MemberList)}
     val.pop.africa <<- if(nrow(val.africa)==0) {NA} else {sum(val.africa$POP.VALUE,na.rm = TRUE)}
     no.pop.africa <<- if(nrow(val.africa)==0) {NA} else {sum(val.africa$MemberList,na.rm = TRUE)}
     
     #Asia
     val.asia <<- dplyr::filter(val.a,MemberList==1,ASIA==1)
     pdata.asia <<- dplyr::filter(pop.f,MemberList==1,ASIA==1)
     at.least.one.asia <<- if(nrow(val.asia)==0) {NA} else {sum(val.asia$at.least.one,na.rm = TRUE)}
     at.least.two.asia <<- if(nrow(val.asia)==0) {NA} else {sum(val.asia$at.least.two,na.rm = TRUE)}
     analysis.asia <<- if(nrow(val.asia)==0) {NA} else {sum(val.asia$analysis,na.rm = TRUE)}
     tot.dpt.asia <<- if(nrow(val.asia)==0) {NA} else {sum(val.asia$total.dpt.count,na.rm = TRUE)}
     pop.asia <<- if(nrow(pdata.asia)==0) {NA} else {sum(pdata.asia$POP.VALUE,na.rm = TRUE)}
     no.country.asia <<- if(nrow(pdata.asia)==0) {NA} else {sum(pdata.asia$MemberList)}
     val.pop.asia <<- if(nrow(val.asia)==0) {NA} else {sum(val.asia$POP.VALUE,na.rm = TRUE)}
     no.pop.asia <<- if(nrow(val.asia)==0) {NA} else {sum(val.asia$MemberList,na.rm = TRUE)}
     
     #Europe
     val.europe <<- dplyr::filter(val.a,MemberList==1,EUROPE==1)
     pdata.europe <<- dplyr::filter(pop.f,MemberList==1,EUROPE==1)
     at.least.one.europe <<- if(nrow(val.europe)==0) {NA} else {sum(val.europe$at.least.one,na.rm = TRUE)}
     at.least.two.europe <<- if(nrow(val.europe)==0) {NA} else {sum(val.europe$at.least.two,na.rm = TRUE)}
     analysis.europe <<- if(nrow(val.europe)==0) {NA} else {sum(val.europe$analysis,na.rm = TRUE)}
     tot.dpt.europe <<- if(nrow(val.europe)==0) {NA} else {sum(val.europe$total.dpt.count,na.rm = TRUE)}
     pop.europe <<- if(nrow(pdata.europe)==0) {NA} else {sum(pdata.europe$POP.VALUE,na.rm = TRUE)}
     no.country.europe <<- if(nrow(pdata.europe)==0) {NA} else {sum(pdata.europe$MemberList)}
     val.pop.europe <<- if(nrow(val.europe)==0) {NA} else {sum(val.europe$POP.VALUE,na.rm = TRUE)}
     no.pop.europe <<- if(nrow(val.europe)==0) {NA} else {sum(val.europe$MemberList,na.rm = TRUE)}
     
     #LAC
     val.LAC <<- dplyr::filter(val.a,MemberList==1,LAC==1)
     pdata.LAC <<- dplyr::filter(pop.f,MemberList==1,LAC==1)
     at.least.one.LAC <<- if(nrow(val.LAC)==0) {NA} else {sum(val.LAC$at.least.one,na.rm = TRUE)}
     at.least.two.LAC <<- if(nrow(val.LAC)==0) {NA} else {sum(val.LAC$at.least.two,na.rm = TRUE)}
     analysis.LAC <<- if(nrow(val.LAC)==0) {NA} else {sum(val.LAC$analysis,na.rm = TRUE)}
     tot.dpt.LAC <<- if(nrow(val.LAC)==0) {NA} else {sum(val.LAC$total.dpt.count,na.rm = TRUE)}
     pop.LAC <<- if(nrow(pdata.LAC)==0) {NA} else {sum(pdata.LAC$POP.VALUE,na.rm = TRUE)}
     no.country.LAC <<- if(nrow(pdata.LAC)==0) {NA} else {sum(pdata.LAC$MemberList)}
     val.pop.LAC <<- if(nrow(val.LAC)==0) {NA} else {sum(val.LAC$POP.VALUE,na.rm = TRUE)}
     no.pop.LAC <<- if(nrow(val.LAC)==0) {NA} else {sum(val.LAC$MemberList,na.rm = TRUE)}
     
     #NorthAmerica
     val.northam <<- dplyr::filter(val.a,MemberList==1,NORTHAME==1)
     pdata.northam <<- dplyr::filter(pop.f,MemberList==1,NORTHAME==1)
     at.least.one.northam <<- if(nrow(val.northam)==0) {NA} else {sum(val.northam$at.least.one,na.rm = TRUE)}
     at.least.two.northam <<- if(nrow(val.northam)==0) {NA} else {sum(val.northam$at.least.two,na.rm = TRUE)}
     analysis.northam <<- if(nrow(val.northam)==0) {NA} else {sum(val.northam$analysis,na.rm = TRUE)}
     tot.dpt.northam <<- if(nrow(val.northam)==0) {NA} else {sum(val.northam$total.dpt.count,na.rm = TRUE)}
     pop.northam <<- if(nrow(pdata.northam)==0) {NA} else {sum(pdata.northam$POP.VALUE,na.rm = TRUE)}
     no.country.northam <<- if(nrow(pdata.northam)==0) {NA} else {sum(pdata.northam$MemberList)}
     val.pop.northam <<- if(nrow(val.northam)==0) {NA} else {sum(val.northam$POP.VALUE,na.rm = TRUE)}
     no.pop.northam <<- if(nrow(val.northam)==0) {NA} else {sum(val.northam$MemberList,na.rm = TRUE)}
     
     #Americas
     val.americas <<- dplyr::filter(val.a,MemberList==1,AMERICAS==1)
     pdata.americas <<- dplyr::filter(pop.f,MemberList==1,AMERICAS==1)
     at.least.one.americas <<- if(nrow(val.americas)==0) {NA} else {sum(val.americas$at.least.one,na.rm = TRUE)}
     at.least.two.americas <<- if(nrow(val.americas)==0) {NA} else {sum(val.americas$at.least.two,na.rm = TRUE)}
     analysis.americas <<- if(nrow(val.americas)==0) {NA} else {sum(val.americas$analysis,na.rm = TRUE)}
     tot.dpt.americas <<- if(nrow(val.americas)==0) {NA} else {sum(val.americas$total.dpt.count,na.rm = TRUE)}
     pop.americas <<- if(nrow(pdata.americas)==0) {NA} else {sum(pdata.americas$POP.VALUE,na.rm = TRUE)}
     no.country.americas <<- if(nrow(pdata.americas)==0) {NA} else {sum(pdata.americas$MemberList)}
     val.pop.americas <<- if(nrow(val.americas)==0) {NA} else {sum(val.americas$POP.VALUE,na.rm = TRUE)}
     no.pop.americas <<- if(nrow(val.americas)==0) {NA} else {sum(val.americas$MemberList,na.rm = TRUE)}
     
     
     #Oceania
     val.oceania <<- dplyr::filter(val.a,MemberList==1,OCEANIA==1)
     pdata.oceania <<- dplyr::filter(pop.f,MemberList==1,OCEANIA==1)
     at.least.one.oceania <<- if(nrow(val.oceania)==0) {NA} else {sum(val.oceania$at.least.one,na.rm = TRUE)}
     at.least.two.oceania <<- if(nrow(val.oceania)==0) {NA} else {sum(val.oceania$at.least.two,na.rm = TRUE)}
     analysis.oceania <<- if(nrow(val.oceania)==0) {NA} else {sum(val.oceania$analysis,na.rm = TRUE)}
     tot.dpt.oceania <<- if(nrow(val.oceania)==0) {NA} else {sum(val.oceania$total.dpt.count,na.rm = TRUE)}
     pop.oceania <<- if(nrow(pdata.oceania)==0) {NA} else {sum(pdata.oceania$POP.VALUE,na.rm = TRUE)}
     no.country.oceania <<- if(nrow(pdata.oceania)==0) {NA} else {sum(pdata.oceania$MemberList)}
     val.pop.oceania <<- if(nrow(val.oceania)==0) {NA} else {sum(val.oceania$POP.VALUE,na.rm = TRUE)}
     no.pop.oceania <<- if(nrow(val.oceania)==0) {NA} else {sum(val.oceania$MemberList,na.rm = TRUE)}
     
     #Asia and Pacific
     val.apac <<- dplyr::filter(val.a,MemberList==1,APAC==1)
     pdata.apac <<- dplyr::filter(pop.f,MemberList==1,APAC==1)
     at.least.one.apac <<- if(nrow(val.apac)==0) {NA} else {sum(val.apac$at.least.one,na.rm = TRUE)}
     at.least.two.apac <<- if(nrow(val.apac)==0) {NA} else {sum(val.apac$at.least.two,na.rm = TRUE)}
     analysis.apac <<- if(nrow(val.apac)==0) {NA} else {sum(val.apac$analysis,na.rm = TRUE)}
     tot.dpt.apac <<- if(nrow(val.apac)==0) {NA} else {sum(val.apac$total.dpt.count,na.rm = TRUE)}
     pop.apac <<- if(nrow(pdata.apac)==0) {NA} else {sum(pdata.apac$POP.VALUE,na.rm = TRUE)}
     no.country.apac <<- if(nrow(pdata.apac)==0) {NA} else {sum(pdata.apac$MemberList)}
     val.pop.apac <<- if(nrow(val.apac)==0) {NA} else {sum(val.apac$POP.VALUE,na.rm = TRUE)}
     no.pop.apac <<- if(nrow(val.apac)==0) {NA} else {sum(val.apac$MemberList,na.rm = TRUE)}
     
     #Europe and North America
     val.eunort <<- dplyr::filter(val.a,MemberList==1,EUNORT==1)
     pdata.eunort <<- dplyr::filter(pop.f,MemberList==1,EUNORT==1)
     at.least.one.eunort <<- if(nrow(val.eunort)==0) {NA} else {sum(val.eunort$at.least.one,na.rm = TRUE)}
     at.least.two.eunort <<- if(nrow(val.eunort)==0) {NA} else {sum(val.eunort$at.least.two,na.rm = TRUE)}
     analysis.eunort <<- if(nrow(val.eunort)==0) {NA} else {sum(val.eunort$analysis,na.rm = TRUE)}
     tot.dpt.eunort <<- if(nrow(val.eunort)==0) {NA} else {sum(val.eunort$total.dpt.count,na.rm = TRUE)}
     pop.eunort <<- if(nrow(pdata.eunort)==0) {NA} else {sum(pdata.eunort$POP.VALUE,na.rm = TRUE)}
     no.country.eunort <<- if(nrow(pdata.eunort)==0) {NA} else {sum(pdata.eunort$MemberList)}
     val.pop.eunort <<- if(nrow(val.eunort)==0) {NA} else {sum(val.eunort$POP.VALUE,na.rm = TRUE)}
     no.pop.eunort <<- if(nrow(val.eunort)==0) {NA} else {sum(val.eunort$MemberList,na.rm = TRUE)}
     
     #Developed
     val.developed <<- dplyr::filter(val.a,MemberList==1,MDG.Dev.Code==514)
     pdata.developed <<- dplyr::filter(pop.f,MemberList==1,MDG.Dev.Code==514)
     at.least.one.developed <<- if(nrow(val.developed)==0) {NA} else {sum(val.developed$at.least.one,na.rm = TRUE)}
     at.least.two.developed <<- if(nrow(val.developed)==0) {NA} else {sum(val.developed$at.least.two,na.rm = TRUE)}
     analysis.developed <<- if(nrow(val.developed)==0) {NA} else {sum(val.developed$analysis,na.rm = TRUE)}
     tot.dpt.developed <<- if(nrow(val.developed)==0) {NA} else {sum(val.developed$total.dpt.count,na.rm = TRUE)}
     pop.developed <<- if(nrow(pdata.developed)==0) {NA} else {sum(pdata.developed$POP.VALUE,na.rm = TRUE)}
     no.country.developed <<- if(nrow(pdata.developed)==0) {NA} else {sum(pdata.developed$MemberList)}
     val.pop.developed <<- if(nrow(val.developed)==0) {NA} else {sum(val.developed$POP.VALUE,na.rm = TRUE)}
     no.pop.developed <<- if(nrow(val.developed)==0) {NA} else {sum(val.developed$MemberList,na.rm = TRUE)}
     
     #Developing
     val.developing <<- dplyr::filter(val.a,MemberList==1,MDG.Dev.Code==515)
     pdata.developing <<- dplyr::filter(pop.f,MemberList==1,MDG.Dev.Code==515)
     at.least.one.developing <<- if(nrow(val.developing)==0) {NA} else {sum(val.developing$at.least.one,na.rm = TRUE)}
     at.least.two.developing <<- if(nrow(val.developing)==0) {NA} else {sum(val.developing$at.least.two,na.rm = TRUE)}
     analysis.developing <<- if(nrow(val.developing)==0) {NA} else {sum(val.developing$analysis,na.rm = TRUE)}
     tot.dpt.developing <<- if(nrow(val.developing)==0) {NA} else {sum(val.developing$total.dpt.count,na.rm = TRUE)}
     pop.developing <<- if(nrow(pdata.developing)==0) {NA} else {sum(pdata.developing$POP.VALUE,na.rm = TRUE)}
     no.country.developing <<- if(nrow(pdata.developing)==0) {NA} else {sum(pdata.developing$MemberList)}
     val.pop.developing <<- if(nrow(val.developing)==0) {NA} else {sum(val.developing$POP.VALUE,na.rm = TRUE)}
     no.pop.developing <<- if(nrow(val.developing)==0) {NA} else {sum(val.developing$MemberList,na.rm = TRUE)}
     
     #FrancophoneAfrica
     val.FrancophoneAfrica <<- dplyr::filter(val.a,MemberList==1,FrancophoneAfrica==1)
     pdata.FrancophoneAfrica <<- dplyr::filter(pop.f,MemberList==1,FrancophoneAfrica==1)
     at.least.one.FrancophoneAfrica <<- if(nrow(val.FrancophoneAfrica)==0) {NA} else {sum(val.FrancophoneAfrica$at.least.one,na.rm = TRUE)}
     at.least.two.FrancophoneAfrica <<- if(nrow(val.FrancophoneAfrica)==0) {NA} else {sum(val.FrancophoneAfrica$at.least.two,na.rm = TRUE)}
     analysis.FrancophoneAfrica <<- if(nrow(val.FrancophoneAfrica)==0) {NA} else {sum(val.FrancophoneAfrica$analysis,na.rm = TRUE)}
     tot.dpt.FrancophoneAfrica <<- if(nrow(val.FrancophoneAfrica)==0) {NA} else {sum(val.FrancophoneAfrica$total.dpt.count,na.rm = TRUE)}
     pop.FrancophoneAfrica <<- if(nrow(pdata.FrancophoneAfrica)==0) {NA} else {sum(pdata.FrancophoneAfrica$POP.VALUE,na.rm = TRUE)}
     no.country.FrancophoneAfrica <<- if(nrow(pdata.FrancophoneAfrica)==0) {NA} else {sum(pdata.FrancophoneAfrica$MemberList)}
     val.pop.FrancophoneAfrica <<- if(nrow(val.FrancophoneAfrica)==0) {NA} else {sum(val.FrancophoneAfrica$POP.VALUE,na.rm = TRUE)}
     no.pop.FrancophoneAfrica <<- if(nrow(val.FrancophoneAfrica)==0) {NA} else {sum(val.FrancophoneAfrica$MemberList,na.rm = TRUE)}
     
     #Arab
     val.Arab <<- dplyr::filter(val.a,MemberList==1,Arab==1)
     pdata.Arab <<- dplyr::filter(pop.f,MemberList==1,Arab==1)
     at.least.one.Arab <<- if(nrow(val.Arab)==0) {NA} else {sum(val.Arab$at.least.one,na.rm = TRUE)}
     at.least.two.Arab <<- if(nrow(val.Arab)==0) {NA} else {sum(val.Arab$at.least.two,na.rm = TRUE)}
     analysis.Arab <<- if(nrow(val.Arab)==0) {NA} else {sum(val.Arab$analysis,na.rm = TRUE)}
     tot.dpt.Arab <<- if(nrow(val.Arab)==0) {NA} else {sum(val.Arab$total.dpt.count,na.rm = TRUE)}
     pop.Arab <<- if(nrow(pdata.Arab)==0) {NA} else {sum(pdata.Arab$POP.VALUE,na.rm = TRUE)}
     no.country.Arab <<- if(nrow(pdata.Arab)==0) {NA} else {sum(pdata.Arab$MemberList)}
     val.pop.Arab <<- if(nrow(val.Arab)==0) {NA} else {sum(val.Arab$POP.VALUE,na.rm = TRUE)}
     no.pop.Arab <<- if(nrow(val.Arab)==0) {NA} else {sum(val.Arab$MemberList,na.rm = TRUE)}
     
     #produce outputs in a table.
     a <- matrix(0,ncol = 100, nrow = 1)
     a <- data.frame(a)
     colnames(a) <- c("IndicatorCode","IndicatorCode","at.least.one.world","at.least.one.africa","at.least.one.asia","at.least.one.europe","at.least.one.LAC","at.least.one.northam","at.least.one.americas","at.least.one.oceania","at.least.one.apac","at.least.one.eunort","at.least.one.developed","at.least.one.developing","at.least.one.FrancophoneAfrica","at.least.one.Arab","at.least.two.world","at.least.two.africa","at.least.two.asia","at.least.two.europe","at.least.two.LAC","at.least.two.northam","at.least.two.americas","at.least.two.oceania","at.least.two.apac","at.least.two.eunort","at.least.two.developed","at.least.two.developing","at.least.two.FrancophoneAfrica","at.least.two.Arab","analysis.world","analysis.africa","analysis.asia","analysis.europe","analysis.LAC","analysis.northam","analysis.americas","analysis.oceania","analysis.apac","analysis.eunort","analysis.developed","analysis.developing","analysis.FrancophoneAfrica","analysis.Arab","tot.dpt.world","tot.dpt.africa","tot.dpt.asia","tot.dpt.europe","tot.dpt.LAC","tot.dpt.northam","tot.dpt.americas","tot.dpt.oceania","tot.dpt.apac","tot.dpt.eunort","tot.dpt.developed","tot.dpt.developing","tot.dpt.FrancophoneAfrica","tot.dpt.Arab","pop.world","pop.africa","pop.asia","pop.europe","pop.LAC","pop.northam","pop.americas","pop.oceania","pop.apac","pop.eunort","pop.developed","pop.developing","pop.FrancophoneAfrica","pop.Arab","no.country.world","no.country.africa","no.country.asia","no.country.europe","no.country.LAC","no.country.northam","no.country.americas","no.country.oceania","no.country.apac","no.country.eunort","no.country.developed","no.country.developing","no.country.FrancophoneAfrica","no.country.Arab","val.pop.world","val.pop.africa","val.pop.asia","val.pop.europe","val.pop.LAC","val.pop.northam","val.pop.americas","val.pop.oceania","val.pop.apac","val.pop.eunort","val.pop.developed","val.pop.developing","val.pop.FrancophoneAfrica","val.pop.Arab")													
     rownames(a) <- c("")
     a[1] <- c(paste(Indicator.data$IndicatorCode[1]))
     a[2] <- c(paste(Indicator.data$IndicatorCode[1]))
     a[3] <- c(at.least.one.world)
     a[4] <- c(at.least.one.africa)
     a[5] <- c(at.least.one.asia)
     a[6] <- c(at.least.one.europe)
     a[7] <- c(at.least.one.LAC)
     a[8] <- c(at.least.one.northam)
     a[9] <- c(at.least.one.americas)
     a[10] <- c(at.least.one.oceania)
     a[11] <- c(at.least.one.apac)
     a[12] <- c(at.least.one.eunort)
     a[13] <- c(at.least.one.developed)
     a[14] <- c(at.least.one.developing)
     a[15] <- c(at.least.one.FrancophoneAfrica)
     a[16] <- c(at.least.one.Arab)
     a[17] <- c(at.least.two.world)
     a[18] <- c(at.least.two.africa)
     a[19] <- c(at.least.two.asia)
     a[20] <- c(at.least.two.europe)
     a[21] <- c(at.least.two.LAC)
     a[22] <- c(at.least.two.northam)
     a[23] <- c(at.least.two.americas)
     a[24] <- c(at.least.two.oceania)
     a[25] <- c(at.least.two.apac)
     a[26] <- c(at.least.two.eunort)
     a[27] <- c(at.least.two.developed)
     a[28] <- c(at.least.two.developing)
     a[29] <- c(at.least.two.FrancophoneAfrica)
     a[30] <- c(at.least.two.Arab)
     a[31] <- c(analysis.world)
     a[32] <- c(analysis.africa)
     a[33] <- c(analysis.asia)
     a[34] <- c(analysis.europe)
     a[35] <- c(analysis.LAC)
     a[36] <- c(analysis.northam)
     a[37] <- c(analysis.americas)
     a[38] <- c(analysis.oceania)
     a[39] <- c(analysis.apac)
     a[40] <- c(analysis.eunort)
     a[41] <- c(analysis.developed)
     a[42] <- c(analysis.developing)
     a[43] <- c(analysis.FrancophoneAfrica)
     a[44] <- c(analysis.Arab)
     a[45] <- c(tot.dpt.world)
     a[46] <- c(tot.dpt.africa)
     a[47] <- c(tot.dpt.asia)
     a[48] <- c(tot.dpt.europe)
     a[49] <- c(tot.dpt.LAC)
     a[50] <- c(tot.dpt.northam)
     a[51] <- c(tot.dpt.americas)
     a[52] <- c(tot.dpt.oceania)
     a[53] <- c(tot.dpt.apac)
     a[54] <- c(tot.dpt.eunort)
     a[55] <- c(tot.dpt.developed)
     a[56] <- c(tot.dpt.developing)
     a[57] <- c(tot.dpt.FrancophoneAfrica)
     a[58] <- c(tot.dpt.Arab)
     a[59] <- c(pop.world)
     a[60] <- c(pop.africa)
     a[61] <- c(pop.asia)
     a[62] <- c(pop.europe)
     a[63] <- c(pop.LAC)
     a[64] <- c(pop.northam)
     a[65] <- c(pop.americas)
     a[66] <- c(pop.oceania)
     a[67] <- c(pop.apac)
     a[68] <- c(pop.eunort)
     a[69] <- c(pop.developed)
     a[70] <- c(pop.developing)
     a[71] <- c(pop.FrancophoneAfrica)
     a[72] <- c(pop.Arab)
     a[73] <- c(no.country.world)
     a[74] <- c(no.country.africa)
     a[75] <- c(no.country.asia)
     a[76] <- c(no.country.europe)
     a[77] <- c(no.country.LAC)
     a[78] <- c(no.country.northam)
     a[79] <- c(no.country.americas)
     a[80] <- c(no.country.oceania)
     a[81] <- c(no.country.apac)
     a[82] <- c(no.country.eunort)
     a[83] <- c(no.country.developed)
     a[84] <- c(no.country.developing)
     a[85] <- c(no.country.FrancophoneAfrica)
     a[86] <- c(no.country.Arab)
     a[87] <- c(val.pop.world)
     a[88] <- c(val.pop.africa)
     a[89] <- c(val.pop.asia)
     a[90] <- c(val.pop.europe)
     a[91] <- c(val.pop.LAC)
     a[92] <- c(val.pop.northam)
     a[93] <- c(val.pop.americas)
     a[94] <- c(val.pop.oceania)
     a[95] <- c(val.pop.apac)
     a[96] <- c(val.pop.eunort)
     a[97] <- c(val.pop.developed)
     a[98] <- c(val.pop.developing)
     a[99] <- c(val.pop.FrancophoneAfrica)
     a[100] <- c(val.pop.Arab)
     
     
     output <<- rbind(output,a)
     
   }
 }
 
 
 main.ind.fun(val.data.reg)
 write.csv(output,"output_20191009_IndicatorLevelData.csv")
 
 
 
 
 

  #################################



#CountryLevelDataAvailablity_At indicatorLevel
 #total number of datapoints
 tdpt <<- val.data %>% group_by(IndicatorCode) %>% count(GeoAreaCode)
 colnames(tdpt)[3]<- "total.dpt.count"
 tdpt <- as.data.frame(tdpt)
 tdpt <- mutate(tdpt,Expr2=paste(tdpt$IndicatorCode,tdpt$GeoAreaCode, sep = ""))
 tdpt <- as.data.frame(tdpt)
 
 #before2010
 before.2010 <<- dplyr::filter(val.data,TimePeriod<2010) #data set  for data before 2010
 before.2010.count <- before.2010 %>% group_by(IndicatorCode) %>% count(GeoAreaCode)
 colnames(before.2010.count)[3]<- "before.ten.count"
 before.2010.count <- as.data.frame(before.2010.count)
 before.2010.count <- mutate(before.2010.count, Expr2 = paste(before.2010.count$IndicatorCode, before.2010.count$GeoAreaCode, sep = ""))
 before.2010.count <- as.data.frame(before.2010.count)
 
 #on or after 2010
 after.n.2010 <<- dplyr::filter(val.data,TimePeriod>=2010) #data set for data on and after 2010
 after.n.2010.count <- after.n.2010 %>% group_by(IndicatorCode) %>% count(GeoAreaCode)
 colnames(after.n.2010.count)[3]<- "after.ten.count"
 after.n.2010.count <- as.data.frame(after.n.2010.count)
 after.n.2010.count <- mutate(after.n.2010.count,Expr2=paste(after.n.2010.count$IndicatorCode,after.n.2010.count$GeoAreaCode, sep = ""))
 after.n.2010.count <- as.data.frame(after.n.2010.count)
 
 #merging all three based on Expr2
 val.data.count <- merge(x = tdpt, y = before.2010.count, by = "Expr2", all.x = TRUE)
 val.data.count <- merge(x = val.data.count, y = after.n.2010.count, by = "Expr2", all.x = TRUE)
 val.data.count <- dplyr::select(val.data.count,Expr2,IndicatorCode.x,GeoAreaCode.x,total.dpt.count,before.ten.count,after.ten.count)
 colnames(val.data.count)[2] <- "IndicatorCode"
 colnames(val.data.count)[3] <- "GeoAreaCode"
 val.data.count = within(val.data.count,{at.least.one = ifelse(is.na(val.data.count$total.dpt.count),0,ifelse(val.data.count$total.dpt.count>0,1,0))})
 val.data.count = within(val.data.count,{at.least.two = ifelse(is.na(val.data.count$total.dpt.count),0,ifelse(val.data.count$total.dpt.count>1,1,0))})
 val.data.count = within(val.data.count,{analysis = ifelse(is.na(val.data.count$before.ten.count)&is.na(val.data.count$after.ten.count),0,ifelse((val.data.count$before.ten.count>0)&(val.data.count$after.ten.count>0),1,0))})
 
 Ind_level_count <- merge(x = val.data.count, y = RefAreaDisaggregated_BYAREA, by.x = "GeoAreaCode",by.y = "M49", all.x = TRUE)
 Ind_level_count <- merge(x = Ind_level_count, y = Pop.Data.Total, by.x = "GeoAreaCode",by.y = "M49_CODE", all.x = TRUE)
 Ind_level_count <- merge(x = Ind_level_count, y = IndicatorLevel, by = "IndicatorCode", all.x = TRUE)

 
 FrancoAfrica <- dplyr::filter(Ind_level_count,FrancophoneAfrica == 1)
 FrancoAfrica1 <- unique(FrancoAfrica[,c(1,2,7)])
 FrancoAfrica2 <<- dcast(FrancoAfrica1,IndicatorCode ~ GeoAreaCode, value.var = "at.least.one", na.rm = TRUE)
 FrancoAfrica3 <<- merge(x = FrancoAfrica2, y = Goal_Indicator, by = "IndicatorCode", all.x = TRUE)
 FrancoAfrica3 <<- as.data.frame(FrancoAfrica3[,-c(1,28)])
 FrancoAfrica4 <<- FrancoAfrica3 %>% group_by(GoalID) %>% summarise_all(funs(sum),na.rm = TRUE)
 write.csv(FrancoAfrica4,"FrancoAfrica4_COnly.csv")
 
 Arab <- dplyr::filter(Ind_level_count,Arab == 1)
 Arab1 <- unique(Arab[,c(1,2,7)])
 Arab2 <<- dcast(Arab1,IndicatorCode ~ GeoAreaCode, value.var = "at.least.one", na.rm = TRUE)
 Arab3 <<- merge(x = Arab2, y = Goal_Indicator, by = "IndicatorCode", all.x = TRUE)
 Arab3 <<- as.data.frame(Arab3[,-c(1,25)])
 Arab4 <<- Arab3 %>% group_by(GoalID) %>% summarise_all(funs(sum),na.rm = TRUE)
 write.csv(Arab4,"Arab4_COnly.csv")
 
 
 
 Global <- dplyr::filter(Ind_level_count,Disaggregation == 1)
 Global1 <- unique(Global[,c(1,2,7)])
 Global2 <<- merge(x = Global1, y = Goal_Indicator, by = "IndicatorCode", all.x = TRUE)
 Global3 <<- unique(Global2[,-c(length(Global2))])
 Global4 <<- Global3 %>% group_by(GoalID) %>% count(GeoAreaCode)
 Global5 <<- dcast(Global4,GeoAreaCode ~ GoalID, value.var = "n", na.rm = TRUE)

 write.csv(Global5,"Global5_AllData.csv")
 
 
 
 seriesinfo <- dplyr::select(mydata,SeriesCode,SeriesDescription) %>%
   unique(.)
 
 write.csv(seriesinfo,"SeriesInfo_20191007.csv")
 