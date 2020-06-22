Sys.setenv(JAVA_HOME='C:\\Program Files (x86)\\Java\\jre1.8.0_211') # for 32-bit version
library(rJava)

#set working directory
setwd("C:/Users/Owner/OneDrive - United Nations/SDG_Database")


#load RODBC and call database
library(RODBC) #load RODBC package
library(dplyr)
library(data.table)
library(openxlsx)
library(ggplot2)
library(psych)
library(tidyr)
library(ReporteRs)
library(officer)
library(stringr)

#function for changing data type to numeric
    num = function(x) {
      require(testit)
      if(has_warning(as.numeric(as.character(x)))){x}else
      {as.numeric(as.character(x))}
    } #change the data type to numeric
    
    num2 = function(x) {
      require(testit)
      if(has_warning(as.numeric(as.character(x)))){x}else
      {round2(as.numeric(as.character(x)),1)}
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
#. Negation
  '%!in%' <- Negate('%in%')


#. Downloading data from SQL server. Make sure that the latest ReleaseName is used.
    odbcChannel <<- odbcConnect("SDGDB51")
    AllDataSQL <<- paste0("select * from dbo.ObservationDn where ReleaseName = '2019.Q2.G.02'")
    mydf <<- sqlQuery(odbcChannel,AllDataSQL,as.is = TRUE) 
    mydata0 <- mydf %>%
      filter(.,Value %!in% c("N","NA","NULL")) %>%
      make.names(names(.))
    #save(mydf,file = "2019Data_20190805.RData")

#. loading RData from the archive folder.
    load("C:/Users/Owner/OneDrive - United Nations/SDG_Database/9. Database Updates + History + Archives/Archive/2020_Q1.1_AllData_After_20200529.RData")
    mydata0 <- SDGData_Q1.1_2020_Initial %>%
      filter(.,Value %!in% c("N","NA","NULL")) %>%
      mutate(.,Value = num(Value))
    #mydata0 <- make.names(names(mydata0))
    
    
    
    #. Testing the data file.
    tb <- mydata0 %>%
      select(.,Value) %>%
      table(.) %>%
      data.frame(.)

#. Preparing the conditions
    Profile.Series <<- read.xlsx("./3. Country Snapshots and Profiles/InputFiles/C.Profile.Conditions_20181119.xlsx",sheet="Conditions") %>% #create a table containing series used in country profiles
      filter(Profile.Series,Profile.Series==1, P.IndicatorCode != "C200203") #removing the duplicate datapoints of this multipurpose series
    
    GeoInfo <- openxlsx::read.xlsx("./0. SDG_Reference_Files/SDG_ReferenceArea_20191105_AM.xlsx",sheet = 1) #create a table containing series used in country profiles
    SDG_GEO <- GeoInfo %>%
      #select(.,M49,Ref_Area_Type,Annex.Sequence) %>%
      unique(.)

    #RefAreaDisaggregated_BYAREA <<- read.xlsx("C:/Users/Zin.Lin/OneDrive - United Nations/SDG_ReferenceFiles/ReferenceArea_20190128.xlsx", sheet = "REF_AREA_DISAGGREGATED", sheetName="REF_AREA_DISAGGREGATED") #create the reference area table

#Functions for preparing additional metadata for data.
    sqlname_dv <<- paste0("select Code, Description from dbo.DimensionValue")
    DimData <<- sqlQuery(odbcChannel,sqlname_dv,as.is = TRUE) #it shouldn't take much to load.  
    colnames(DimData)[2] <- "DimensionValueName"
    
    
    sqlname_dim <<- paste0("select * from dbo.Dimension")
    Dims <<- sqlQuery(odbcChannel,sqlname_dim,as.is = TRUE) #it shouldn't take much to load. 
      colnames(Dims)[2] <- "DimensionNames"
        #. If no access to internal SQL server, use this file.
        load("./0.3_SDGCR_Tables/dim_names.RData")
        Dims <- dim_names
        colnames(Dims)[2] <- "DimensionNames"
    
    DimsF <<- Dims %>%
      filter(.,isAttribute == 0,DimensionNames %!in% c("Freq","Reporting Type"))
      
    
    save(DimData1, file = "./3. Country Snapshots and Profiles/InputFiles/DimData1.RData")
    save(Dims4, file = "./3. Country Snapshots and Profiles/InputFiles/Dims4.RData")
    load("./3. Country Snapshots and Profiles/InputFiles/DimData1.RData")
    load("./3. Country Snapshots and Profiles/InputFiles/Dims4.RData")

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
################## functions end ########################

#. Creating the dimension identifier(s) of each record.
mydata1 <- mydata0 %>%
      mutate(., Dimidentifier = dimid2(.)) %>%
      mutate(., Dimidentifierx = paste0(SeriesCode,Dimidentifier)) # produces a concatenated string of dimension values

    include <- mydata1 %>%
      select(.,Dimidentifierx) %>%
      distinct(.)

    mydf <- mydata1 %>%
      .[,colSums(is.na(.))<nrow(.)]
    


#. Create columns containing information about country/region disaggregations and indicator/goal information.
  mydf_geo <- mydf %>%
    left_join(.,SDG_GEO, by = c("GeoAreaCode" = "M49"))
  
  #For Ghana
  mydf_geo <- mydf_geo %>% filter(.,GeoAreaCode == 288)

# the following line creates unique series identifiers.
  mydf_pseries <- mydf_geo %>%
    full_join(.,Profile.Series, by = c("Dimidentifierx")) %>%
    mutate(.,HIV.filter = paste0(.$Dimidentifierx,.$AFRICA,sep = ""),Zeros = paste(.$zero.removal,.$Value,sep = "")) %>%
    filter(.,Profile.Series == 1)
    
#. Final data set for profile creation.
  profile.data <- mydf_pseries %>%
    filter(.,TimePeriod >= 2000, HIV.filter != "SH_HIV_INCDALLAGEBOTHSEX") %>%
    filter(.,Zeros !="10", Zeros !="10.00000000000000") %>%
    mutate(.,Value = ifelse(Value == "> 95",">95",num(Value))) %>%
    drop_na(Value) %>%
    filter(.,Profile.Series == 1)
  
  zero.value.data <- profile.data %>% 
    filter(.,Zeros=="10" | Zeros=="10.00000000000000") #set of not application zeros values
  
#. test to see if the data filtering works
          s <- Profile.Series %>%
            select(.,Profile.Series,Dimidentifierx) %>%
            filter(.,Profile.Series == 1)
          
          t <- include %>%
            full_join(.,s,by = "Dimidentifierx")
          write.csv(t,"./3. Country Snapshots and Profiles/InputFiles/test.csv")

          
#. if only application, test it first.
          profile.data$GeoAreaName[profile.data$GeoAreaName=="Sint Maarten (Dutch part)\t"]<-"Sint Maarten (Dutch part)"


#Main text generation function
#Variables involved: dataset,area,goalid,goalname,seriesid,texttype1,texttype2,texttype3,texttype4,texttype5

text.gen <- function(profile.data){
  for (i in unique(profile.data$GeoAreaCode)) {
    ref.data <<- dplyr::filter(profile.data,GeoAreaCode == i) #subsetting by ref.area
    ref.text <<- paste(ref.data$GeoAreaName[1])
    filename <<- paste("./3. Country Snapshots and Profiles/2019_Q4/",ref.text,".docx",sep = "")
    p.doc <<- read_docx(path = "./3. Country Snapshots and Profiles/InputFiles/Template.docx")
    p.doc <<- body_add_par(p.doc,ref.text,style = 'P.Header')    
    for (k in sort(unique(ref.data$Goal.ID),decreasing = FALSE)){
      goal.data <<- dplyr::filter(ref.data,Goal.ID == k)
      g.text <<- paste(goal.data$Goal.Name[1])
      p.doc <<- body_add_par(p.doc,g.text,style = 'P.Goal')
      goal.data <<- goal.data[order(goal.data$Indicator),]
      for (j in unique(goal.data$Dimidentifierx)){
        test.data <<- dplyr::filter(goal.data,Dimidentifierx == j)
        series.text<<-if(test.data$Text.type[1] == 1){text.type.1(test.data)}else
          {if(test.data$Text.type[1] == 2){text.type.2(test.data)}else
            {if(test.data$Text.type[1] == 3){text.type.3(test.data)}else
              {if(test.data$Text.type[1] == 4){text.type.4(test.data)}else
                {if(test.data$Text.type[1] == 7){text.type.7(test.data)}else
                  {if(test.data$Text.type[1] == 8){text.type.8(test.data)}else
                    {if(test.data$Text.type[1] == 9){text.type.9(test.data)}else
                      {if(test.data$Text.type[1] == 10){text.type.10(test.data)}else
                        {if(test.data$Text.type[1] == 11){text.type.11(test.data)}else
                          {if(test.data$Text.type[1] == 12){text.type.12(test.data)}else
                            {if(test.data$Text.type[1] == 13){text.type.13(test.data)}else
                              {if(test.data$Text.type[1] == 14){text.type.14(test.data)}else
                                {if(test.data$Text.type[1] == 15){text.type.15(test.data)}else
                    {"NA"}}}}}}}}}}}}}
        p.doc <<- body_add_par(p.doc,series.text,style = "BulletList")

        } #subsetting by series closes. test.data closes.
  
      } #subsetting by Goal closes. goal.data closes.
    p.doc <<- body_add_par(p.doc,"________________________", style = "P.Header.Footer")
    p.doc <<- body_add_par(p.doc,"Note (1): This fact sheet was prepared by the UN Statistics Division on selected indicators.  More data and information are available in the Sustainable Development Goal Indicators Database (http://unstats.un.org/sdgs/indicators/database/).", style = "P.Header.Footer")
    p.doc <<- body_add_par(p.doc,"Note (2): Some Goals may have been omitted from this profile due to a lack of data availability.", style = "P.Header.Footer")
    
    print(p.doc,target = filename) %>% 
      invisible()
    
    } #subsetting by ref.area closes. profile.data closes.
  
  } #function closes.

#Information about status of progress: decline/increase?#
#variables involved: dataset,min,max,textfordown,textforup
prog <<- function(test.data){
  if(num(str_fix(min.obs)) > num(str_fix(max.obs)))
  {paste(test.data$DA3.Down[1])}else
  {if(num(str_fix(min.obs)) < num(str_fix(max.obs)))
    {paste(test.data$DA3.Up[1])}else
    {NA}
    } 
}

prog.15 <<- function(test.data){
  if(num(str_fix(min.obs))<0.01)
  {"nearly no coverage"}else
  {paste(profile.round(min.obs),test.data$P.Unit.Second[1],sep = "")}
}

prog.10 <<- function(test.data){
  if(num(str_fix(max.obs))<10)
  {paste(test.data$DA3.Down)[1]}else
  {paste(test.data$DA3.Up)[1]}
}

prog.12 <<- function(test.data){
  if(num(str_fix(max.obs))>0)
  {paste(test.data$DA3.Up)[1]}else
  {paste(test.data$DA3.Down)[1]}
}

prog.mmr.min <<- function(test.data){
  if(num(str_fix(min.obs))>1)
  {" deaths"}else
  {" death"}
}

prog.mmr.max <<- function(test.data){
  if(num(str_fix(max.obs))>1)
  {" deaths"}else
  {" death"}
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

#. string remover.
str_fix <- function(x) {
  if(grepl(">",x)){num(str_remove(x,">"))} else
    if(grepl("<",x)){num(str_remove(x,"<"))} else 
    {num(x)}
}


profile.round = function(x){
  if(grepl(">",x)){paste("more than ",str_remove(x,">"))}else #if obs value is >95, then type "greater than 95"
  {if(grepl("<",x)){paste("less than ",str_remove(x,"<"))}else #if obs value is <5, then type "less than 5"
  {if(abs(num(x)) < 0.99) {round2(num(x),2)}else #if obs value is less than 0.99, then 2 decimal points
  {if(abs(num(x)) >= 0.99 & abs(num(x)) < 10){round2(num(x),1)}else # if obs value is between 0.99 and 10, then 1 decimal point
  {if(abs(num(x)) >= 10){round2(num(x),0)}else #if obs value is greater than 10, then no decimal point
  {round(num(x),0)}}}}}}





text.type.1 <<- function(test.data){
  n.yr <<- length(test.data$TimePeriod)
  min.yr <<- min(test.data$TimePeriod)
  max.yr <<- max(test.data$TimePeriod)
  min.yr.data <<- dplyr::filter(test.data,TimePeriod == min.yr)
  min.obs <<- min.yr.data$Value
  max.yr.data <<- dplyr::filter(test.data,TimePeriod == max.yr)
  max.obs <<- max.yr.data$Value
  dif.obs <<- ifelse((max.obs == ">95" | max.obs == "<5" | min.obs == "<5" | max.obs == "<2.5" | min.obs=="<2.5" ),0,abs(num(str_fix(max.obs))-num(str_fix(min.obs))))
  
  if(n.yr==1)
  {text <- paste(test.data$DA2.1[1],profile.round(max.obs),test.data$P.Unit[1]," in ", min.yr, ".",sep = "")} else
  {if(n.yr>1 & min.yr>=2010)
    {text <- paste(test.data$DA2.1[1],profile.round(max.obs),test.data$P.Unit[1]," in ", max.yr, ".",sep = "")} else
  {if(min.obs == max.obs | max.obs == ">95" | max.obs == "<5" |  max.obs == "<2.5" | min.obs == "<2.5"| dif.obs < 0.05*abs(num(str_fix(max.obs))) | num(str_fix(max.obs)) < .25*num(test.data$median[1]) )
    {text <- paste(test.data$DA2.1[1],profile.round(max.obs),test.data$P.Unit[1]," in ", max.yr, ".",sep = "")} else
  {text <- paste(test.data$DA3.1[1],prog(test.data),profile.round(min.obs),test.data$P.Unit[1]," in ",min.yr," to ",profile.round(max.obs),test.data$P.Unit[1]," in ",max.yr,".",sep = "") }
  } 
  }
  print(text)
}


text.type.2 <<- function(test.data){
  n.yr <<- length(test.data$TimePeriod)
  min.yr <<- min(test.data$TimePeriod)
  max.yr <<- max(test.data$TimePeriod)
  min.yr.data <<- dplyr::filter(test.data,TimePeriod == min.yr)
  min.obs <<- min.yr.data$Value
  max.yr.data <<- dplyr::filter(test.data,TimePeriod == max.yr)
  max.obs <<- max.yr.data$Value
  dif.obs <<- ifelse((max.obs == ">95" | max.obs == "<5" | min.obs == "<5" | max.obs == "<2.5" | min.obs=="<2.5" ),0,abs(num(str_fix(max.obs))-num(str_fix(min.obs))))
  
  if(n.yr==1)
  {text <- paste(test.data$T.CapIn[1],max.yr,", ",profile.round(max.obs),test.data$P.Unit[1],test.data$DA2.1[1],".",sep = "")} else
  {if(n.yr>1 & min.yr>=2010)
  {text <- paste(test.data$T.CapIn[1],max.yr,", ",profile.round(max.obs),test.data$P.Unit[1],test.data$DA2.1[1],".",sep = "")} else
  {if(min.obs == max.obs | max.obs == ">95" | max.obs == "<5" |  max.obs == "<2.5" | min.obs == "<2.5"| dif.obs < 0.01*abs(num(str_fix(max.obs))) | num(str_fix(max.obs)) < num(test.data$median[1]) )
  {text <- paste(test.data$T.CapIn[1],max.yr,", ",profile.round(max.obs),test.data$P.Unit[1],test.data$DA2.1[1],".",sep = "")} else
  {text <- paste(test.data$T.CapIn[1],max.yr,", ",profile.round(max.obs),test.data$P.Unit[1],test.data$DA3.1[1],",",prog(test.data),profile.round(min.obs),test.data$P.Unit[1],"in ",min.yr,".",sep = "") }
  } 
  }
  print(text)
}



text.type.3 <<- function(test.data){
  n.yr <<- length(test.data$TimePeriod)
  min.yr <<- min(test.data$TimePeriod)
  max.yr <<- max(test.data$TimePeriod)
  min.yr.data <<- dplyr::filter(test.data,TimePeriod == min.yr)
  min.obs <<- min.yr.data$Value
  max.yr.data <<- dplyr::filter(test.data,TimePeriod == max.yr)
  max.obs <<- max.yr.data$Value
  dif.obs <<- ifelse((max.obs == ">95" | max.obs == "<5" | min.obs == "<5" | max.obs == "<2.5" | min.obs=="<2.5" ),0,abs(num(str_fix(max.obs))-num(str_fix(min.obs))))
  
  if(n.yr==1)
  {text <- paste(test.data$T.CapIn[1],max.yr,", ",test.data$DA2.1[1],profile.round(max.obs),test.data$P.Unit[1],test.data$DA2.2[1],".",sep = "")} else
  {if(n.yr>1 & min.yr>=2010)
  {text <- paste(test.data$T.CapIn[1],max.yr,", ",test.data$DA2.1[1],profile.round(max.obs),test.data$P.Unit[1],test.data$DA2.2[1],".",sep = "")} else
  {if(min.obs == max.obs | max.obs == ">95" | max.obs == "<5" |  max.obs == "<2.5" | min.obs == "<2.5"| dif.obs < 0.01*abs(num(str_fix(max.obs))) | num(str_fix(max.obs)) < num(test.data$median[1]) )
  {text <- paste(test.data$T.CapIn[1],max.yr,", ",test.data$DA2.1[1],profile.round(max.obs),test.data$P.Unit[1],test.data$DA2.2[1], ".",sep = "")} else
  {text <- paste(test.data$T.CapIn[1],max.yr,", ",test.data$DA2.1[1],profile.round(max.obs),test.data$P.Unit[1],test.data$DA2.2[1],".",sep = "")}
  } 
  }
  print(text)
}

text.type.4 <<- function(test.data){
  n.yr <<- length(test.data$TimePeriod)
  min.yr <<- min(test.data$TimePeriod)
  max.yr <<- max(test.data$TimePeriod)
  min.yr.data <<- dplyr::filter(test.data,TimePeriod == min.yr)
  min.obs <<- min.yr.data$Value
  max.yr.data <<- dplyr::filter(test.data,TimePeriod == max.yr)
  max.obs <<- max.yr.data$Value
  dif.obs <<- ifelse((max.obs == ">95" | max.obs == "<5" | min.obs == "<5" | max.obs == "<2.5" | min.obs=="<2.5" ),0,abs(num(str_fix(max.obs))-num(str_fix(min.obs))))
  
  if(n.yr==1)
  {text <- paste(test.data$DA2.1[1],profile.round(max.obs)," in ",max.yr,", meaning ",profile.round(max.obs)*100,test.data$DA2.2[1],".",sep = "")} else
  {if(n.yr>1 & min.yr>=2010)
  {text <- paste(test.data$DA2.1[1],profile.round(max.obs)," in ",max.yr,", meaning ",profile.round(max.obs)*100,test.data$DA2.2[1],".",sep = "")} else
  {if(min.obs == max.obs | max.obs == ">95" | max.obs == "<5" |  max.obs == "<2.5" | min.obs == "<2.5"| dif.obs < 0.01*abs(num(str_fix(max.obs))) | num(str_fix(max.obs)) < num(test.data$median[1]) )
  {text <- paste(test.data$DA2.1[1],profile.round(max.obs)," in ",max.yr,", meaning ",profile.round(max.obs)*100,test.data$DA2.2[1], ".",sep = "")} else
  {text <- paste(test.data$DA2.1[1],profile.round(max.obs)," in ",max.yr,", meaning ",profile.round(max.obs)*100,test.data$DA2.2[1],".",sep = "")}
  } 
  }
  print(text)
}


text.type.7 <<- function(test.data){
  n.yr <<- length(test.data$TimePeriod)
  min.yr <<- min(test.data$TimePeriod)
  max.yr <<- max(test.data$TimePeriod)
  min.yr.data <<- dplyr::filter(test.data,TimePeriod == min.yr)
  min.obs <<- min.yr.data$Value
  max.yr.data <<- dplyr::filter(test.data,TimePeriod == max.yr)
  max.obs <<- max.yr.data$Value
  dif.obs <<- ifelse((max.obs == ">95" | max.obs == "<5" | min.obs == "<5" | max.obs == "<2.5" | min.obs=="<2.5" ),0,abs(num(str_fix(max.obs))-num(str_fix(min.obs))))
  
  if(n.yr==1)
  {text <- paste(test.data$DA2.1[1],profile.round(max.obs),test.data$P.Unit[1]," in ", max.yr, ".",sep = "")} else
  {if(n.yr>1 & min.yr>=2010)
  {text <- paste(test.data$DA2.1[1],profile.round(max.obs),test.data$P.Unit[1]," in ", max.yr,".",sep = "")} else
  {if(min.obs == max.obs | max.obs == ">95" | max.obs == "<5" |  max.obs == "<2.5" | min.obs == "<2.5"| dif.obs < 0.01*abs(num(str_fix(max.obs))) | num(str_fix(max.obs)) < num(test.data$median[1]) )
  {text <- paste(test.data$DA2.1[1],profile.round(max.obs),test.data$P.Unit[1]," in ", max.yr, ".",sep = "")} else
  {text <- paste(test.data$DA2.1[1],profile.round(max.obs),test.data$P.Unit[1]," in ", max.yr,".",sep = "")}
  } 
  }
  print(text)
}

text.type.8 <<- function(test.data){
  n.yr <<- length(test.data$TimePeriod)
  min.yr <<- min(test.data$TimePeriod)
  max.yr <<- max(test.data$TimePeriod)
  min.yr.data <<- dplyr::filter(test.data,TimePeriod == min.yr)
  min.obs <<- min.yr.data$Value
  max.yr.data <<- dplyr::filter(test.data,TimePeriod == max.yr)
  max.obs <<- max.yr.data$Value
  dif.obs <<- ifelse((max.obs == ">95" | max.obs == "<5" | min.obs == "<5" | max.obs == "<2.5" | min.obs=="<2.5" ),0,abs(num(str_fix(max.obs))-num(str_fix(min.obs))))
  
  if(n.yr==1)
  {text <- paste(test.data$T.CapIn[1],max.yr,", ",profile.round(max.obs),test.data$P.Unit[1],test.data$DA2.1[1], ".",sep = "")} else
  {if(n.yr>1 & min.yr>=2010)
  {text <- paste(test.data$T.CapIn[1],max.yr,", ",profile.round(max.obs),test.data$P.Unit[1],test.data$DA2.1[1],".",sep = "")} else
  {if(min.obs == max.obs | max.obs == ">95" | max.obs == "<5" |  max.obs == "<2.5" | min.obs == "<2.5"| dif.obs < 0.01*abs(num(str_fix(max.obs))) | num(str_fix(max.obs)) < num(test.data$median[1]) )
  {text <- paste(test.data$T.CapIn[1],max.yr,", ",profile.round(max.obs),test.data$P.Unit[1],test.data$DA2.1[1], ".",sep = "")} else
  {text <- paste(test.data$T.CapIn[1],max.yr,", ",profile.round(max.obs),test.data$P.Unit[1],test.data$DA2.1[1],".",sep = "") }
  } 
  }
  print(text)
}

text.type.9 <<- function(test.data){
  n.yr <<- length(test.data$TimePeriod)
  min.yr <<- min(test.data$TimePeriod)
  max.yr <<- max(test.data$TimePeriod)
  min.yr.data <<- dplyr::filter(test.data,TimePeriod == min.yr)
  min.obs <<- min.yr.data$Value
  max.yr.data <<- dplyr::filter(test.data,TimePeriod == max.yr)
  max.obs <<- max.yr.data$Value
  dif.obs <<- ifelse((max.obs == ">95" | max.obs == "<5" | min.obs == "<5" | max.obs == "<2.5" | min.obs=="<2.5" ),0,abs(num(str_fix(max.obs))-num(str_fix(min.obs))))
  
  if(n.yr==1)
  {text <- paste(test.data$T.CapIn[1],max.yr,", ",test.data$DA2.1[1],profile.round(max.obs),test.data$P.Unit[1],".",sep = "")} else
  {if(n.yr>1 & min.yr>=2010)
  {text <- paste(test.data$T.CapIn[1],max.yr,", ",test.data$DA2.1[1],profile.round(max.obs),test.data$P.Unit[1],".",sep = "")} else
  {if(min.obs == max.obs | max.obs == ">95" | max.obs == "<5" |  max.obs == "<2.5" | min.obs == "<2.5"| dif.obs < 0.01*abs(num(str_fix(max.obs))) | num(str_fix(max.obs)) < num(test.data$median[1]) )
  {text <- paste(test.data$T.CapIn[1],max.yr,", ",test.data$DA2.1[1],profile.round(max.obs),test.data$P.Unit[1], ".",sep = "")} else
  {text <- paste(test.data$T.CapIn[1],max.yr,", ",test.data$DA2.1[1],profile.round(max.obs),test.data$P.Unit[1],", ",prog(test.data),profile.round(min.obs),test.data$P.Unit.Second[1]," in ",min.yr,".",sep = "") }
  } 
  }
  print(text)
}

text.type.10 <<- function(test.data){
  n.yr <<- length(test.data$TimePeriod)
  min.yr <<- min(test.data$TimePeriod)
  max.yr <<- max(test.data$TimePeriod)
  min.yr.data <<- dplyr::filter(test.data,TimePeriod == min.yr)
  min.obs <<- min.yr.data$Value
  max.yr.data <<- dplyr::filter(test.data,TimePeriod == max.yr)
  max.obs <<- max.yr.data$Value
  dif.obs <<- ifelse((max.obs == ">95" | max.obs == "<5" | min.obs == "<5" | max.obs == "<2.5" | min.obs=="<2.5" ),0,abs(num(str_fix(max.obs))-num(str_fix(min.obs))))
  
  if(n.yr==1)
  {text <- paste(test.data$T.CapIn[1],max.yr,", ",test.data$DA2.1[1],profile.round(max.obs),test.data$P.Unit[1],".",prog.10(test.data),sep = "")} else
  {if(n.yr>1 & min.yr>=2010)
  {text <- paste(test.data$T.CapIn[1],max.yr,", ",test.data$DA2.1[1],profile.round(max.obs),test.data$P.Unit[1],".",prog.10(test.data),".",sep = "")} else
  {if(min.obs == max.obs | max.obs == ">95" | max.obs == "<5" |  max.obs == "<2.5" | min.obs == "<2.5"| dif.obs < 0.01*abs(num(str_fix(max.obs))) | num(str_fix(max.obs)) < num(test.data$median[1]) )
  {text <- paste(test.data$T.CapIn[1],max.yr,", ",test.data$DA2.1[1],profile.round(max.obs),test.data$P.Unit[1],".",prog.10(test.data), ".",sep = "")} else
  {text <- paste(test.data$T.CapIn[1],max.yr,", ",test.data$DA2.1[1],profile.round(max.obs),test.data$P.Unit[1],".",prog.10(test.data),".",sep = "")}
  } 
  }
  print(text)
}

text.type.11 <<- function(test.data){
  n.yr <<- length(test.data$TimePeriod)
  min.yr <<- min(test.data$TimePeriod)
  max.yr <<- max(test.data$TimePeriod)
  min.yr.data <<- dplyr::filter(test.data,TimePeriod == min.yr)
  min.obs <<- min.yr.data$Value
  max.yr.data <<- dplyr::filter(test.data,TimePeriod == max.yr)
  max.obs <<- max.yr.data$Value
  dif.obs <<- ifelse((max.obs == ">95" | max.obs == "<5" | min.obs == "<5" | max.obs == "<2.5" | min.obs=="<2.5" ),0,abs(num(str_fix(max.obs))-num(str_fix(min.obs))))
  
  if(n.yr==1)
  {text <- paste(test.data$T.CapIn[1],max.yr,", ",test.data$DA2.1[1],profile.round(max.obs),test.data$P.Unit[1],".",sep = "")} else
  {if(n.yr>1 & min.yr>=2010)
  {text <- paste(test.data$T.CapIn[1],max.yr,", ",test.data$DA2.1[1],profile.round(max.obs),test.data$P.Unit[1],".",sep = "")} else
  {if(min.obs == max.obs | max.obs == ">95" | max.obs == "<5" |  max.obs == "<2.5" | min.obs == "<2.5"| dif.obs < 0.01*abs(num(str_fix(max.obs))) | num(str_fix(max.obs)) < num(test.data$median[1]) )
  {text <- paste(test.data$T.CapIn[1],max.yr,", ",test.data$DA2.1[1],profile.round(max.obs),test.data$P.Unit[1], ".",sep = "")} else
  {text <- paste(test.data$T.CapIn[1],max.yr,", ",test.data$DA2.1[1],profile.round(max.obs),test.data$P.Unit[1],".",sep = "") }
  } 
  }
  print(text)
}



text.type.12 <<- function(test.data){
  n.yr <<- length(test.data$TimePeriod)
  min.yr <<- min(test.data$TimePeriod)
  max.yr <<- max(test.data$TimePeriod)
  min.yr.data <<- dplyr::filter(test.data,TimePeriod == min.yr)
  min.obs <<- min.yr.data$Value
  max.yr.data <<- dplyr::filter(test.data,TimePeriod == max.yr)
  max.obs <<- max.yr.data$Value
  text <- paste(test.data$DA3.1[1],max.yr,", ",test.data$GeoAreaName[1],prog.12(test.data), ".",sep = "")
  
  print(text)
}

text.type.13 <<- function(test.data){
  n.yr <<- length(test.data$TimePeriod)
  min.yr <<- min(test.data$TimePeriod)
  max.yr <<- max(test.data$TimePeriod)
  min.yr.data <<- dplyr::filter(test.data,TimePeriod == min.yr)
  min.obs <<- min.yr.data$Value
  max.yr.data <<- dplyr::filter(test.data,TimePeriod == max.yr)
  max.obs <<- max.yr.data$Value
  dif.obs <<- ifelse((max.obs == ">95" | max.obs == "<5" | min.obs == "<5" | max.obs == "<2.5" | min.obs=="<2.5" ),0,abs(num(str_fix(max.obs))-num(str_fix(min.obs))))
  
  if(n.yr==1)
  {text <- paste(test.data$T.CapIn[1],max.yr,", ",test.data$DA2.1[1],profile.round(max.obs)*100,test.data$P.Unit[1],test.data$DA2.2[1],".",sep = "")} else
  {if(n.yr>1 & min.yr>=2010)
  {text <- paste(test.data$T.CapIn[1],max.yr,", ",test.data$DA2.1[1],profile.round(max.obs)*100,test.data$P.Unit[1],test.data$DA2.2[1],".",sep = "")} else
  {if(min.obs == max.obs | max.obs == ">95" | max.obs == "<5" |  max.obs == "<2.5" | min.obs == "<2.5"| dif.obs < 0.01*abs(num(str_fix(max.obs))))
  {text <- paste(test.data$T.CapIn[1],max.yr,", ",test.data$DA2.1[1],profile.round(max.obs)*100,test.data$P.Unit[1],test.data$DA2.2[1], ".",sep = "")} else
  {text <- paste(test.data$T.CapIn[1],max.yr,", ",test.data$DA2.1[1],profile.round(max.obs)*100,test.data$P.Unit[1],test.data$DA2.2[1],".",sep = "")}
  } 
  }
  print(text)
}

text.type.14 <<- function(test.data){
  n.yr <<- length(test.data$TimePeriod)
  min.yr <<- min(test.data$TimePeriod)
  max.yr <<- max(test.data$TimePeriod)
  min.yr.data <<- dplyr::filter(test.data,TimePeriod == min.yr)
  min.obs <<- min.yr.data$Value
  max.yr.data <<- dplyr::filter(test.data,TimePeriod == max.yr)
  max.obs <<- max.yr.data$Value
  dif.obs <<- ifelse((max.obs == ">95" | max.obs == "<5" | min.obs == "<5" | max.obs == "<2.5" | min.obs=="<2.5" ),0,abs(num(str_fix(max.obs))-num(str_fix(min.obs))))
  
  if(n.yr==1)
  {text <- paste(test.data$DA2.1[1],profile.round(max.obs),prog.mmr.max(test.data),test.data$P.Unit[1]," in ", min.yr, ".",sep = "")} else
  {if(n.yr>1 & min.yr>=2010)
  {text <- paste(test.data$DA2.1[1],profile.round(max.obs),prog.mmr.max(test.data),test.data$P.Unit[1]," in ", max.yr, ".",sep = "")} else
  {if(min.obs == max.obs | max.obs == ">95" | max.obs == "<5" |  max.obs == "<2.5" | min.obs == "<2.5"| dif.obs < 0.01*abs(num(str_fix(max.obs))) | num(str_fix(max.obs)) < num(test.data$median[1]) )
  {text <- paste(test.data$DA2.1[1],profile.round(max.obs),prog.mmr.max(test.data),test.data$P.Unit[1]," in ", max.yr, ".",sep = "")} else
  {text <- paste(test.data$DA3.1[1],prog(test.data),profile.round(min.obs),prog.mmr.min(test.data),test.data$P.Unit[1]," in ",min.yr," to ",profile.round(max.obs),prog.mmr.max(test.data),test.data$P.Unit[1]," in ",max.yr,".",sep = "") }
  } 
  }
  print(text)
}

text.type.15 <<- function(test.data){
  n.yr <<- length(test.data$TimePeriod)
  min.yr <<- min(test.data$TimePeriod)
  max.yr <<- max(test.data$TimePeriod)
  min.yr.data <<- dplyr::filter(test.data,TimePeriod == min.yr)
  min.obs <<- min.yr.data$Value
  max.yr.data <<- dplyr::filter(test.data,TimePeriod == max.yr)
  max.obs <<- max.yr.data$Value
  dif.obs <<- ifelse((max.obs == ">95" | max.obs == "<5" | min.obs == "<5" | max.obs == "<2.5" | min.obs=="<2.5"),0,abs(num(str_fix(max.obs))-num(str_fix(min.obs))))
  
  if(n.yr==1)
  {text <- paste(test.data$T.CapIn[1],max.yr,", ",test.data$DA2.1[1],profile.round(max.obs),test.data$P.Unit[1],".",sep = "")} else
  {if(n.yr>1 & min.yr>=2010)
  {text <- paste(test.data$T.CapIn[1],max.yr,", ",test.data$DA2.1[1],profile.round(max.obs),test.data$P.Unit[1],".",sep = "")} else
  {if(min.obs == max.obs | max.obs == ">95" | max.obs == "<5" | max.obs == "<2.5" | min.obs == "<2.5"| dif.obs < 0.01*abs(num(str_fix(max.obs))) | num(str_fix(max.obs)) < num(test.data$median[1]) )
  {text <- paste(test.data$T.CapIn[1],max.yr,", ",test.data$DA2.1[1],profile.round(max.obs),test.data$P.Unit[1], ".",sep = "")} else
  {text <- paste(test.data$T.CapIn[1],max.yr,", ",test.data$DA2.1[1],profile.round(max.obs),test.data$P.Unit[1],",",prog(test.data),prog.15(test.data)," in ",min.yr,".",sep = "") }
  } 
  }
  print(text)
}

#. Run this function for country profiles.
text.gen(profile.data)

################################################### Country profile generation program ends.


write.csv(profile.data,"profile.data.csv")


##### the following create country/regional profiles with an Annex containing the column charts.

swr = function(string, nwrap=70) {
  paste(strwrap(string, width=nwrap), collapse="\n")
}
swr = Vectorize(swr)

text.gen.gg <- function(profile.data.temp){
  for (i in unique(profile.data.temp$GeoAreaCode)) {
    ref.data <<- dplyr::filter(profile.data.temp,GeoAreaCode == i) #subsetting by ref.area
    ref.text <<- paste(ref.data$GeoAreaName[1])
    filename <<- paste("./3. Country Snapshots and Profiles/2019_Q4/test/",ref.text,".docx",sep = "")
    p.doc <<- read_docx(path = "./3. Country Snapshots and Profiles/InputFiles/Template.docx")
    p.doc <<- body_add_par(p.doc,ref.text,style = 'P.Header')    
    for (k in sort(unique(ref.data$Goal.ID),decreasing = FALSE)){
      goal.data <<- dplyr::filter(ref.data,Goal.ID == k)
      g.text <<- paste(goal.data$Goal.Name[1])
      p.doc <<- body_add_par(p.doc,g.text,style = 'P.Goal')
      goal.data <<- goal.data[order(goal.data$Indicator),]
      for (j in unique(goal.data$Dimidentifierx)){
        test.data <<- dplyr::filter(goal.data,Dimidentifierx == j)
        series.text<<-if(test.data$Text.type[1] == 1){text.type.1(test.data)}else
        {if(test.data$Text.type[1] == 2){text.type.2(test.data)}else
        {if(test.data$Text.type[1] == 3){text.type.3(test.data)}else
        {if(test.data$Text.type[1] == 4){text.type.4(test.data)}else
        {if(test.data$Text.type[1] == 7){text.type.7(test.data)}else
        {if(test.data$Text.type[1] == 8){text.type.8(test.data)}else
        {if(test.data$Text.type[1] == 9){text.type.9(test.data)}else
        {if(test.data$Text.type[1] == 10){text.type.10(test.data)}else
        {if(test.data$Text.type[1] == 11){text.type.11(test.data)}else
        {if(test.data$Text.type[1] == 12){text.type.12(test.data)}else
        {if(test.data$Text.type[1] == 13){text.type.13(test.data)}else
        {if(test.data$Text.type[1] == 14){text.type.14(test.data)}else
        {if(test.data$Text.type[1] == 15){text.type.15(test.data)}else
        {"NA"}}}}}}}}}}}}}
        p.doc <<- body_add_par(p.doc,series.text,style = "BulletList")
        
      } #subsetting by series closes. test.data closes.
      
    } #subsetting by Goal closes. goal.data closes.
    p.doc <<- body_add_par(p.doc,"________________________", style = "P.Header.Footer")
    p.doc <<- body_add_par(p.doc,"Note (1): This fact sheet was prepared by the UN Statistics Division on selected indicators.  More data and information are available in the Sustainable Development Goal Indicators Database (http://unstats.un.org/sdgs/indicators/database/).", style = "P.Header.Footer")
    p.doc <<- body_add_par(p.doc,"Note (2): Some Goals may have been omitted from this profile due to a lack of data availability.", style = "P.Header.Footer") %>%
        body_add_break()
    p.doc <<- body_add_par(p.doc,"Annex",style = 'P.Header')
    run_pagebreak()
    

    for (k in sort(unique(ref.data$Goal.ID),decreasing = FALSE)){
      goal.data <<- dplyr::filter(ref.data,Goal.ID == k)
      g.text <<- paste(goal.data$Goal.Name[1])
      p.doc <<- body_add_par(p.doc,g.text,style = 'P.Goal')
      goal.data <<- goal.data[order(goal.data$Indicator),]
      for (j in unique(goal.data$Dimidentifierx)){
        test.data <<- dplyr::filter(goal.data,Dimidentifierx == j)
        gdf <<- test.data %>% 
          mutate(.,Value.Rev = apply(as.array(Value),1,str_fix)) %>%
          mutate(.,Value.Rev = round2(Value.Rev,1))
        gg_plot <<- ggplot(gdf, aes(x = TimePeriod, y = Value.Rev)) + 
          geom_bar(stat = "identity", fill = "palegreen1", position = "dodge", width = 0.5) +
          #ggtitle(swr(unique(test.data$SeriesDescription)[1])) +
          geom_text(aes(label=num2(Value)),position=position_dodge(width=0.9), vjust=-0.25, size = 2) +
          xlab("Year") + ylab("Value") +
          scale_x_continuous(name="Year", limits=c(1999,2019),breaks=seq(1999,2019,1)) +
          scale_y_continuous(name = "Value") +
          labs(caption = paste0("Source: The Global SDG Indicators Database.")) +
          theme_bw() + theme(legend.position = "none", plot.caption = element_text(size = 5)) + 
          theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8), axis.title = element_text(size = 10),axis.text.y = element_text(size = 8))

        
        series.title <<- paste(test.data$SeriesDescription[1])
        
        #p.doc <<- body_add_par(p.doc,value = series.title,style = "rPlotLegend")
        if( capabilities(what = "png"))
          body_add_gg(p.doc, value = gg_plot, style = "Table_Style",width = 7, height = 3, res = 300)

        p.doc <<- body_add_par(p.doc,value = series.title, style = "rPlotLegend")
        print(p.doc,target = filename) %>% 
          invisible()
        
      } #subsetting by series closes. test.data closes.
      run_linebreak()
    }

  } #subsetting by ref.area closes. profile.data closes.
} #function closes.

text.gen.gg(profile.data)
##### test scripts.




