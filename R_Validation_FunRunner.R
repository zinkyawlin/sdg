
setwd("C:/Users/Zin.Lin/OneDrive - United Nations/SDG_Database")

  source("./15. RScripts/R_2020_Validation.R")
  getGeo()
  getDim()
  getDataSQL("2020_Q1_AllData_Prior_20200331.RData","2020_Q1_AllData_After_20200331.RData")
  #. Following two codes remove values that are not convertiable to numeric. Bruteforce approach.
  df0 <- df0 %>%
    mutate(.,Value = gsub(",","",Value)) %>%
    mutate(.,Value = gsub(">","",Value)) %>%
    mutate(.,Value = gsub("<","",Value)) %>%
    filter(., Value %!in% c("N","NV",NA,"NA"))
  df1 <- df1 %>%
    mutate(.,Value = gsub(",","",Value)) %>%
    mutate(.,Value = gsub(">","",Value)) %>%
    mutate(.,Value = gsub("<","",Value)) %>%
    filter(., Value %!in% c("N","NV",NA,"NA","NaN"))
  history <- read.csv(file = "./1. Data Processing/2020/0.3_Validation/Validation_Data_Template.csv")
  to_keep <- c("%!in%", "dataval","df0","df1","Dims","DimsF",
               "display_function","GeoInfo","getAnnexCompare",
               "history","i","num","round2","SDG_GEO","sfilter",
               "val_summary","validation","visualization",
               "s_status","dimid2","dimid","s_renamed","to_keep")
  for (i in unique(df1$SeriesCode)){
    wb <<- openxlsx::createWorkbook()
    scode <- paste0(i)
    sfilter(scode)
    if(nrow(df00) == 0) {
      next } 
    s_status(df11)
    s_status(df00)
    s_renamed(df000)
    s_renamed(df111)
    dataval(df000)
    dataval(df111)

    if(validation(df001,df112)=="No numeric data. Skipping validation") {
      list <- ls()
      list_to_remove <- list[list %!in% to_keep]
      rm(list=list_to_remove)
      next
    } else {
      #validation(df001,df112)
      visualization()
      #getAnnexCompare()
      val_summary()
      fname <- paste0("./1. Data Processing/2020/0.3_Validation/",scode,".xlsx")
      saveWorkbook(wb,file = fname, overwrite = TRUE)
      list <- ls()
      list_to_remove <- list[list %!in% to_keep]
      rm(list=list_to_remove)
    }

  }
  write.csv(history,"./1. Data Processing/2020/0.3_Validation/Validation_Data_2020.csv",row.names = TRUE)


colnames(df1)

