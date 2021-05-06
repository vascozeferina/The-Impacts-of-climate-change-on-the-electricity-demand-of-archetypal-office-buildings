# Upload Results
rm(list=ls())
gc()

mypackages<-c("boot",
              "dplyr", 
              "devtools",
              "data.table",
              "eplusr",
              "fs",
              "gdata",
              "ggpubr",
              "grid",
              "gridExtra",
              "ggplot2", 
              "ncdf4",
              "plotly",
              "readxl",
              "sprof",
              "sensitivity",
              "sjstats",
              "stringr",
              "tidyr",
              "tidyverse",
              "tools",
              "TTR",
              "writexl",
              "xlsx",
              "zoo",
              "zonator")

#install.packages(mypackages) # rest omitted
lapply(mypackages, require, character.only = TRUE)

setwd("C:/Users/mbgnwvz2/Dropbox (The University of Manchester)/01_PhD Research/13_R_Analysis/860_R_Files")

# Location of all Random Weather Files
folder_path <- "02_Output_WF/01_Random/"
files <- list.files( path=folder_path,pattern = "*.epw", full.names = TRUE)

############# Upload all data from weather files in the list ############

df_out <- {}
for (n in files){
  Int_Var <- File_Read_CDD(n)
  df_out <- rbind(df_out, Int_Var)
}

Time_Seq_Header <- function(x){
  x <- x%>%
    mutate(Hour_Seq = 1:8760) %>%
    mutate(Day_Seq = rep(1:365,each=24)) %>%
    mutate(Date_Seq = substr(datetime, start = 1, stop = 10))
  return(x)
}

CDD <- function(x,base){
  c <- x%>%
    group_by(Day_Seq)%>%
    summarise(mean=mean(dry_bulb_temperature)-base)%>%
    filter(mean>0)
  
  value <- data.frame(sum(c$mean))
  names(value) <- paste('CDD',base,sep='')
  return(value)
}

Sum_TMY <- function(x){
  test <- x %>% summarise(
    max_dbt = max(dry_bulb_temperature),
    mean_dbt = mean(dry_bulb_temperature),
    max_iri = max(horizontal_infrared_radiation_intensity_from_sky),
    mean_iri = mean(horizontal_infrared_radiation_intensity_from_sky),
    max_dnr = max(direct_normal_radiation),
    mean_dnr = mean(direct_normal_radiation),
    max_dif_hr = max(diffuse_horizontal_radiation),
    mean_dif_hr = mean(diffuse_horizontal_radiation),
    max_ghr = max(global_horizontal_radiation),
    mean_ghr = mean(global_horizontal_radiation),
    mean_RH = mean(relative_humidity) )
  
}

File_Read <- function(file_name){
  Test <- read_epw(file_name)
  a <- Test$data()
  
  out_df <- data.frame(F_Name = file_name, Sum_TMY(a))
  return(out_df)
}

File_Read_CDD <- function(file_name){
  Test <- read_epw(file_name)
  a <- Test$data()
  
  x <- Time_Seq_Header(a)
  
  
  
  out_df <- data.frame(F_Name = file_name, Sum_TMY(a), CDD = CDD(x,20))
  return(out_df)
}


######################## ####################


'Original_Singapore-rcp85-95%-2080-2099.epw'

# Organise The Data
df_worked <- df_out %>%
  mutate(Loc = case_when(
    str_detect(F_Name,'Singapore') ~ 'C1',
    str_detect(F_Name,'Cairo') ~ 'C2',
    str_detect(F_Name,'Athens') ~ 'C3',
    str_detect(F_Name,'Beijing') ~ 'C4',
    str_detect(F_Name,'Lisbon') ~ 'C5',
    str_detect(F_Name,'London') ~ 'C6')) %>%
  mutate(Case = str_extract(F_Name, "(?<=Case_)[:digit:]*"))


#Save upload weather metrics for the whole pathway sample

save(df_out, df_worked, file = "Pathway_Climate_Data.RData")


#Uploads weather metrics without going through all weather files
load("Pathway_Climate_Data.RData")


#Create Plots with analysis of all weather variables
PlotA <- ggplot(df_worked, aes(x = mean_dbt, y = max_dbt, col = Loc)) + 
  geom_point() +
  xlab('Mean DBT [ °C ]') + 
  ylab('Max DBT [ °C ]')

file <- paste('04_Charts/06_Weather_Anal/','AVGDBT_MaxDBT.png',sep='')
ggsave(file,width = 26, height = 13, units = "cm")

PlotB <- ggplot(df_worked, aes(x = mean_dbt, y = mean_RH, col = Loc)) + 
  geom_point() +
  xlab('Mean DBT [ °C ]') + 
  ylab('Mean RH [ % ]')


expression("NO"[3]^-{}*" (mgN/L)")

file <- paste('04_Charts/06_Weather_Anal/','AVGDBT_RH.png',sep='')
ggsave(file,width = 26, height = 13, units = "cm")



PlotC <- ggplot(df_worked, aes(x = mean_dbt, y = mean_dnr, col = Loc)) + 
  geom_point() + 
  xlab('Mean DBT [ °C ]') + 
  ylab(expression("Mean DNR [ W.m"^-2*" ]"))
    
file <- paste('04_Charts/06_Weather_Anal/','AVGDBT_DNR.png',sep='')
ggsave(file,width = 26, height = 13, units = "cm")

PlotD <- ggplot(df_worked, aes(x = CDD20, y = max_dbt, col = Loc)) + 
  geom_point() +
  xlab('CDD') + 
  ylab('Max. DBT [ °C ]') +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  guides(color = guide_legend(nrow = 1)) +
  labs(col="Location")
  
  #geom_text(data=annotations,aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=annotateText))

file <- paste('04_Charts/06_Weather_Anal/','CDD_MaxDBT.png',sep='')
ggsave(file,width = 24, height = 12, units = "cm")


figure <- ggarrange(PlotA, PlotB, PlotC, PlotD,
                    labels = c("a) ", "b) ", "c) ", "d) "),
                    ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom",legend.grob = get_legend(PlotD)) +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  guides(color = guide_legend(nrow = 1))
  
figure

file <- paste('04_Charts/06_Weather_Anal/','Combined_4Plots.png',sep='')
ggsave(file,width = 24, height = 16, units = "cm", figure)

#Create a Sample with Base Cases
folder_path <- "01_Inputs/01_Weather_Files/00_6_TMY"
list_files_base <- list.files( path=folder_path,pattern = "*.epw", full.names = TRUE)

df_out2 <- {}
for (n in list_files_base){
  Int_Var <- File_Read_CDD(n)
  df_out <- rbind(df_out, Int_Var)
  
}

df_worked_Based <- df_out %>%
  mutate(Loc = case_when(
    str_detect(F_Name,'Singapore') ~ 'C1',
    str_detect(F_Name,'Cairo') ~ 'C2',
    str_detect(F_Name,'Athens') ~ 'C3',
    str_detect(F_Name,'Beijing') ~ 'C4',
    str_detect(F_Name,'Lisbon') ~ 'C5',
    str_detect(F_Name,'London') ~ 'C6')) %>%
  mutate(Case = str_extract(F_Name, "(?<=Case_)[:digit:]*")) %>%
  mutate(Case = as.numeric(Case))

df_worked <- df_worked %>%
  mutate(Case = as.numeric(Case))

# Plot of Max DBT random generation

ggplot(df_worked, aes(x=Case , y= max_dbt)) +
  geom_point() + 
  facet_grid( ~ Loc)

##Tables 

Table_out <- df_worked_Based %>% 
  group_by(Loc) %>%
  summarise(Lim_Max = max(max_dbt),
            Lim_Min = min(max_dbt),
            Delta = Lim_Max - Lim_Min,
            CDD_Max = max(CDD20),
            CDD_Base = min(CDD20),
            Ratio_CDD = (CDD_Max/CDD_Base - 1) *100)
            
file <- paste('04_Charts/06_Weather_Anal/','Weather_Analysis.xlsx',sep='')
write.xlsx(Table_out, 
           file, 
           sheetName = "Table", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
  
#Box Plot of Maximum Temperatures

ggplot(df_worked,aes(x = Loc, y = max_dbt))+
  geom_boxplot()


File_Read_CDD('01_Inputs/01_Weather_Files/01_WeatherShift/C1_Singapore/Original_Singapore-rcp85-95%-2080-2099.epw') %>% pull(CDD20)
File_Read_CDD('01_Inputs/01_Weather_Files/01_WeatherShift/C6_London/Original_C6_London-rcp85-95%-2080-2099.epw') %>% pull(CDD20)


4545/2724 -1
4292/2724 -1
4292-2724

432/7-1


