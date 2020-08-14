options(digits = 22,scipen = 999) #Must have for reading in hauljoin
memory.limit(4000)
library(RODBC)
library(reshape2)
library(tidyverse)
library(ggplot2)
library(dplyr)

#subareas 1-6 is the standard area, I think
species1 = 10130 #Flathead sole
species2 = 10140 #Bering flounder
WhichSpecies = species2 #species1 or species2 or both

age_bins<-seq(from = 1, to = 21, by = 1)
CodeDir<-"C:\\GitProjects\\newsbss\\"
source(paste0(CodeDir,"FUNCTIONS\\BIN_AGE_DATA.r"))
MyFolder = "Survey_Age_Length"
MyDir = "\\\\AKC0SS-N086\\REFM_Users\\Carey.McGilliard\\My Documents\\FlatfishAssessments\\2018\\BSAI_Flathead\\Data\\"
OutDir<-"\\\\AKC0SS-N086\\REFM_Users\\Carey.McGilliard\\My Documents\\FlatfishAssessments\\2018\\BSAI_Flathead\\Writeups\\Plots\\"

# Read in data (SQL query follows), and if have already read in data, can use the following:
#also, if already ran this code there is a file further down that you can open rather than this one.
AL.df<-read.csv("\\\\AKC0SS-N086\\REFM_Users\\Carey.McGilliard\\My Documents\\FlatfishAssessments\\2018\\BSAI_Flathead\\Data\\ebs_strata_age_10130_10140.csv")

# #REM GET EBS HAUL TABLE - INCLUDING PLUSNW AREA
# drop  table haulname; 
# drop  view haulname; 
# create table haulname as 
# SELECT  to_number(to_char(a.start_time,'yyyy')) year,A.*
# FROM RACEBASE.HAUL A
# JOIN RACE_DATA.V_CRUISES B
# ON (B.CRUISEJOIN = A.CRUISEJOIN)
# WHERE A.PERFORMANCE >= 0
# AND A.HAUL_TYPE = 3
# AND A.STATIONID IS NOT NULL
# AND A.STRATUM IN (10,20,31,32,41,42,43,50,61,62,82,90)
# AND B.SURVEY_DEFINITION_ID = 98;

# #REM ADD IN THE NMFS_AREA AND SUBAREA TO EBS HAUL-STATION TABLE
# drop table ebs_strata_subareas;
# create table ebs_strata_subareas as select 
# h.YEAR,
# h.CRUISEJOIN,
# h.HAULJOIN,
# h.REGION,  
# h.VESSEL,
# h.CRUISE,
# h.HAUL,
# h.HAUL_TYPE,
# h.PERFORMANCE,
# h.START_TIME,
# h.DURATION,
# h.DISTANCE_FISHED,
# h.NET_WIDTH,
# h.NET_MEASURED,
# h.NET_HEIGHT,
# h.STRATUM,
# decode(h.stratum,10,1,20,2,31,3,32,3,41,4,42,4,43,4,50,5,61,6,62,6,82,8,90,9,-9) SUBAREA,
# e.NMFS_AREA,
# h.START_LATITUDE,
# h.END_LATITUDE,
# h.START_LONGITUDE,
# h.END_LONGITUDE,
# h.STATIONID,
# h.GEAR_DEPTH,
# h.BOTTOM_DEPTH,
# h.BOTTOM_TYPE,
# h.SURFACE_TEMPERATURE,
# h.GEAR_TEMPERATURE,
# h.SUBSAMPLE,
# h.ABUNDANCE_HAUL
# from ebs_nmfs_areas e, haulname h where e.stationid=h.stationid;
# grant select on ebs_strata_subareas to public;


# #REM LINK THE HAUL DATA TO THE SPECIMEN DATA
# drop table ebs_strata_age_10130_10140;
# create table ebs_strata_age_10130_10140 as select 
# s.SPECIMENID,
# s.BIOSTRATUM,
# s.SPECIES_CODE,
# s.LENGTH,
# s.WEIGHT,
# s.SEX,
# s.AGE,
# h.YEAR,
# h.CRUISEJOIN,
# h.HAULJOIN,
# h.REGION,
# h.VESSEL,
# h.CRUISE,
# h.HAUL,
# h.HAUL_TYPE,
# h.PERFORMANCE,
# h.START_TIME,
# h.DURATION,
# h.DISTANCE_FISHED,
# h.NET_WIDTH,
# h.NET_MEASURED,
# h.NET_HEIGHT,
# h.STRATUM,
# h.SUBAREA,
# h.NMFS_AREA,
# h.START_LATITUDE,
# h.END_LATITUDE,
# h.START_LONGITUDE,
# h.END_LONGITUDE,
# h.STATIONID,
# h.GEAR_DEPTH,
# h.BOTTOM_DEPTH,
# h.BOTTOM_TYPE,
# h.SURFACE_TEMPERATURE,
# h.GEAR_TEMPERATURE,
# h.SUBSAMPLE,
# h.ABUNDANCE_HAUL
# from racebase.specimen s, ebs_strata_subareas h where s.hauljoin=h.hauljoin
# and s.hauljoin=h.hauljoin and s.species_code in (10130, 10140) 
# order by s.species_code, h.cruise, h.vessel, h.haul, s.specimenid;
# grant select on ebs_strata_age_10130_10140 to public;

# select * from nichold.ebs_strata_age_10130_10140 
# order by species_code, cruise, vessel, haul, specimenid;

#Below this took too much memory allocation, but worked in SQL (should be about the same as the above)
# ALQuery<-paste0("SELECT RACEBASE.SPECIMEN.HAULJOIN,\n ",
#   "RACEBASE.SPECIMEN.REGION,\n ",
#   "RACEBASE.SPECIMEN.SPECIMENID,\n ",
#   "RACEBASE.SPECIMEN.BIOSTRATUM,\n ",
#   "RACEBASE.SPECIMEN.SPECIES_CODE,\n ",
#   "RACEBASE.SPECIMEN.LENGTH,\n ",
#   "RACEBASE.SPECIMEN.WEIGHT,\n ",
#   "RACEBASE.SPECIMEN.SEX,\n ",
#   "RACEBASE.SPECIMEN.AGE,\n ",
#   "RACEBASE.HAUL.START_TIME,\n ",
#   "RACEBASE.HAUL.BOTTOM_DEPTH,\n ",
#   "RACEBASE.HAUL.STRATUM,\n ",
#   "RACEBASE.HAUL.GEAR_TEMPERATURE,\n ",
#   "RACEBASE.HAUL.BOTTOM_TYPE,\n ",
#   "RACEBASE.HAUL.GEAR_DEPTH,\n ",
#   "RACEBASE.HAUL.PERFORMANCE,\n ",
#   "RACEBASE.HAUL.DURATION,\n ",
#   "RACEBASE.HAUL.DISTANCE_FISHED,\n ",
#   "RACEBASE.HAUL.NET_WIDTH,\n ",
#   "RACEBASE.HAUL.NET_HEIGHT,\n ",
#   "RACEBASE.HAUL.NET_MEASURED,\n ",
#   "RACEBASE.HAUL.START_LATITUDE,\n ",
#   "RACEBASE.HAUL.END_LATITUDE,\n ",
#   "RACEBASE.HAUL.START_LONGITUDE,\n ",
#   "RACEBASE.HAUL.END_LONGITUDE,\n ",
#   "RACEBASE.HAUL.SURFACE_TEMPERATURE,\n ",
#   "RACEBASE.HAUL.GEAR,\n ",
#   "RACEBASE.HAUL.ABUNDANCE_HAUL,\n ",
#   "NICHOLD.EBS_STRATA_SUBAREAS.NMFS_AREA,\n ",
#   "NICHOLD.EBS_STRATA_SUBAREAS.SUBAREA,\n ",
#   "NICHOLD.EBS_STRATA_SUBAREAS.STRATUM,\n ",
#   "NICHOLD.EBS_STRATA_SUBAREAS.BOTTOM_DEPTH\n ",
# "FROM RACEBASE.SPECIMEN\n ",
# "INNER JOIN RACEBASE.HAUL\n ",
# "ON RACEBASE.SPECIMEN.HAULJOIN = RACEBASE.HAUL.HAULJOIN\n ",
# "INNER JOIN NICHOLD.EBS_STRATA_SUBAREAS\n ",
# "ON RACEBASE.HAUL.STRATUM           = NICHOLD.EBS_STRATA_SUBAREAS.STRATUM\n ",
# "WHERE RACEBASE.SPECIMEN.REGION     = 'BS'\n ",
# "AND NICHOLD.EBS_STRATA_SUBAREAS.REGION = 'BS'\n ",
# "AND RACEBASE.SPECIMEN.SPECIES_CODE = 10130\n ",
# "AND RACEBASE.HAUL.ABUNDANCE_HAUL   = 'Y'")

# AFSC <- odbcConnect("AFSC","","") #mcgilliardc
# AL.df<- sqlQuery(AFSC,ALQuery)

#--------------------------------------------------------------------


#--------------------------------------------------------------------
#Can start here by reading in AL.df if it was already created earlier


AL.df$START_TIME <- as.Date(AL.df$START_TIME,format = "%m/%d/%Y") 
AL.df$Quarters<-lubridate::quarter(AL.df$START_TIME)
AL.df$Months<-lubridate::month(AL.df$START_TIME)
#AL.df$YEAR<-lubridate::year(AL.df$START_TIME)
AL.df$Cohort<-AL.df$YEAR - AL.df$AGE
#x <- as.Date("01/01/2009", format = "%m/%d/%Y"); lubridate::year(x)

#get rid of observations without lengths AND ages:
#I can never remember how to do this without looking it up.
 AL.df<-AL.df[complete.cases(AL.df$AGE) & complete.cases(AL.df$LENGTH),]
  #AL.df$GrowthMorph<-"DN"
  # AL.df$GrowthMorph[AL.df$REGULATORY_AREA_NAME == "EASTERN GOA"]<-"EASTERN"
  # AL.df$GrowthMorph[AL.df$REGULATORY_AREA_NAME != "EASTERN GOA"]<-"NOT_EASTERN"

#bin ages:
AL.df<-BIN_AGE_DATA(AL.df,age_bins)  

#------------------------------------------------------------------
write.csv(AL.df,file = paste0(MyDir,MyFolder,"\\Age_Length.csv"))
#------------------------------------------------------------------

#---------------------------------------------------------------
#Read in the data if it was written earlier:
AL.df<-read.csv(file = paste0(MyDir,MyFolder,"\\Age_Length.csv"))
#--------------------------------------------------------------

#------------------------------------------------------
#Both species BF and FHS are included:
if (WhichSpecies=="both") {
	AL.df<-AL.df[AL.df$SPECIES_CODE==species1 | AL.df$SPECIES_CODE==species2,]
    sp_label = "both"
	}

if (WhichSpecies==species1) {
	AL.df<-AL.df[AL.df$SPECIES_CODE==species1,]
	sp_label = species1
}

if (WhichSpecies == species2) {
	AL.df<-AL.df[AL.df$SPECIES_CODE==species2,]
	sp_label = species2
}
#----------------------------------------------------

#--------------------------------------------------------------------
#Length-At_Age Plots
#--------------------------------------------------------------------
#Jitter ages:
AL.df$Age<-AL.df$aBIN
AL.df$Length<-AL.df$LENGTH/10
AL.df$Age<-AL.df$Age + runif(n =nrow(AL.df),min = 0, max = 0.25)
AL.df$Length<-AL.df$Length + runif(n =nrow(AL.df),min = 0, max = 0.25)
AL.df<-AL.df[AL.df$SEX!=3,]
AL.df$Sex[AL.df$SEX==1]<-"Male"
AL.df$Sex[AL.df$SEX==2]<-"Female"
p<-ggplot(AL.df,aes(x = Age, y = Length)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey"))

#Growth by year
p + facet_wrap(~ YEAR)
p + facet_grid(YEAR~Sex)

#p + facet_grid(Sex~ GrowthMorph)
#dev.copy(png,paste0(OutDir,"Survey_LatAge_by_Cohort_Sex_GrowthMorph.png"))
#dev.off()

p + facet_grid(SUBAREA ~ Sex)
dev.copy(png,paste0(OutDir,"Survey_LatAge_by_Cohort_Sex_Subarea_sp_",sp_label,".png"))
dev.off()

#Growth by depth
p + facet_wrap(~ BOTTOM_DEPTH) #could use some binning

#Growth by INPFC Area
p + facet_grid(NMFS_AREA~Sex)

# #p + facet_wrap(~REGULATORY_AREA_NAME)
# p + facet_grid(INPFC_AREA ~ MAX_DEPTH)
# #p + facet_grid(REGULATORY_AREA_NAME ~ MAX_DEPTH)
# p + facet_grid(YEAR~ MAX_DEPTH) + theme(panel.grid.major = element_line())
# p + facet_grid(YEAR ~ INPFC_AREA)
# p + facet_grid(YEAR ~ REGULATORY_AREA_NAME)

p + facet_grid(YEAR ~ Sex)

#by sex (both species)
AL.df %>% filter(SEX == 2) %>% ggplot(aes(x = Age, y = Length)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
facet_wrap(~YEAR)
dev.copy(png,paste0(OutDir,"Survey_LatAge_by_Cohort_Females_Year_sp_",sp_label,".png"))
dev.off()

AL.df %>% filter(SEX == 1) %>% ggplot(aes(x = Age, y = Length)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
facet_wrap(~YEAR)
dev.copy(png,paste0(OutDir,"Survey_LatAge_by_Cohort_Males_Year_sp_",sp_label,".png"))
dev.off()

#by NMFS-Area
AL.df %>% filter(SEX == 2) %>% ggplot(aes(x = Age, y = Length)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
facet_wrap(~NMFS_AREA)
dev.copy(png,paste0(OutDir,"Survey_LatAge_by_Cohort_Females_NMFS_Area_sp_",sp_label,".png"))
dev.off()

AL.df %>% filter(SEX == 1) %>% ggplot(aes(x = Age, y = Length)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
facet_wrap(~NMFS_AREA)
dev.copy(png,paste0(OutDir,"Survey_LatAge_by_Cohort_Males_NMFS_Area_sp_",sp_label,".png"))
dev.off()

#by survey subarea
AL.df %>% filter(SEX == 2) %>% ggplot(aes(x = Age, y = Length)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
facet_wrap(~SUBAREA)
dev.copy(png,paste0(OutDir,"Survey_LatAge_by_Cohort_Females_Subarea_sp_",sp_label,".png"))
dev.off()

AL.df %>% filter(SEX == 1) %>% ggplot(aes(x = Age, y = Length)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
facet_wrap(~SUBAREA)
dev.copy(png,paste0(OutDir,"Survey_LatAge_by_Cohort_Males_Subarea_sp_",sp_label,".png"))
dev.off()

#by Month
AL.df %>% filter(SEX == 2) %>% ggplot(aes(x = Age, y = Length)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
facet_wrap(~Months)
dev.copy(png,paste0(OutDir,"Survey_LatAge_by_Cohort_Females_Month_sp_",sp_label,".png"))
dev.off()

AL.df %>% filter(SEX == 1) %>% ggplot(aes(x = Age, y = Length)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
facet_wrap(~Months)
dev.copy(png,paste0(OutDir,"Survey_LatAge_by_Cohort_Males_Month_sp_",sp_label,".png"))
dev.off()


#--------------------------------------------------------------------
#Weight-At_Age Plots
#--------------------------------------------------------------------
# w<-ggplot(AL.df,aes(x = AGE, y = WEIGHT)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey"))
# w + facet_wrap(~ YEAR)

#AL.df<-AL.df[complete.cases(AL.df$YEAR)==T,]
#by year and sex
ALweight.df<-AL.df[complete.cases(AL.df$WEIGHT),]
ALweight.df %>% filter(SEX == 2) %>% ggplot(aes(x = Age, y = WEIGHT)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
facet_wrap(~YEAR)
dev.copy(png,paste0(OutDir,"Survey_WtAge_by_Cohort_Females_Year_sp_",sp_label,".png"))
dev.off()

ALweight.df %>% filter(SEX == 1) %>% ggplot(aes(x = Age, y = WEIGHT)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
facet_wrap(~YEAR)
dev.copy(png,paste0(OutDir,"Survey_WtAge_by_Cohort_Males_Year_sp_",sp_label,".png"))
dev.off()

#by NMFS Area and sex
ALweight.df %>% filter(SEX == 2) %>% ggplot(aes(x = Age, y = WEIGHT)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
facet_wrap(~NMFS_AREA)
dev.copy(png,paste0(OutDir,"Survey_WtAge_by_Cohort_Females_NMFS_Area_sp_",sp_label,".png"))
dev.off()

ALweight.df %>% filter(SEX == 1) %>% ggplot(aes(x = Age, y = WEIGHT)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
facet_wrap(~NMFS_AREA)
dev.copy(png,paste0(OutDir,"Survey_WtAge_by_Cohort_Males_NMFS_Area_sp_",sp_label,".png"))
dev.off()

#by survey subarea
ALweight.df %>% filter(SEX == 2) %>% ggplot(aes(x = Age, y = WEIGHT)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
facet_wrap(~SUBAREA)
dev.copy(png,paste0(OutDir,"Survey_WtAge_by_Cohort_Females_Subarea_sp_",sp_label,".png"))
dev.off()

ALweight.df %>% filter(SEX == 1) %>% ggplot(aes(x = Age, y = WEIGHT)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
facet_wrap(~SUBAREA)
dev.copy(png,paste0(OutDir,"Survey_WtAge_by_Cohort_Males_Subarea_sp_",sp_label,".png"))
dev.off()

#by month
ALweight.df %>% filter(SEX == 2) %>% ggplot(aes(x = Age, y = WEIGHT)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
facet_wrap(~Months)
dev.copy(png,paste0(OutDir,"Survey_WtAge_by_Cohort_Females_Month_sp_",sp_label,".png"))
dev.off()

ALweight.df %>% filter(SEX == 1) %>% ggplot(aes(x = Age, y = WEIGHT)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
facet_wrap(~Months)
dev.copy(png,paste0(OutDir,"Survey_WtAge_by_Cohort_Males_Month_sp_",sp_label,".png"))
dev.off()

# w + facet_wrap(~REGULATORY_AREA_NAME)
# w + facet_wrap(~INPFC_AREA)

# w + facet_grid(REGULATORY_AREA_NAME~YEAR)
# w + facet_grid(MAX_DEPTH ~ YEAR)

#--------------------------------------------------------------------
#Weight-Length relationships
#--------------------------------------------------------------------
wl<-ggplot(ALweight.df,aes(x = LENGTH, y = WEIGHT)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey"))

wl + facet_wrap(~YEAR)
dev.copy(png,paste0(OutDir,"Survey_WtLength_by_Cohort_Year_sp_",sp_label,".png"))
dev.off()

# wl + facet_wrap(~REGULATORY_AREA_NAME)
# wl + facet_wrap(~MAX_DEPTH)


#--------------------------------------------------------------------
#Mean weight-at-age by year
#--------------------------------------------------------------------
MeanWA.df<-aggregate(WEIGHT ~ YEAR + SEX + AGE +Cohort,ALweight.df,mean)
MeanWA.df$MeanWeight<-MeanWA.df$WEIGHT/1000
MeanWA.df<-subset(MeanWA.df,select = -c(WEIGHT))
MeanWA.df<-MeanWA.df[order(MeanWA.df$YEAR,MeanWA.df$AGE),]
# wa<-ggplot(MeanWA.df,aes(x = AGE, y = MeanWeight)) 
# wa + geom_line(data = NULL,aes(colour = YEAR))

wa<-ggplot(MeanWA.df,aes(x=AGE,y=MeanWeight)) + geom_line(size=1) + ylab("Mean Weight (kg)")
wa + facet_grid(SEX~YEAR)

#By sex and year
MeanWA.df %>% filter(SEX == 2) %>% ggplot(aes(x = AGE, y = MeanWeight)) + geom_point(data = NULL,stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
facet_wrap(~YEAR)  
dev.copy(png,paste0(OutDir,"Survey_MeanWtAge_Females_Year_sp_",sp_label,".png"))
dev.off()

MeanWA.df %>% filter(SEX == 1) %>% ggplot(aes(x = AGE, y = MeanWeight)) + geom_point(data = NULL,stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
facet_wrap(~YEAR)  
dev.copy(png,paste0(OutDir,"Survey_MeanWtAge_Males_Year_sp_",sp_label,".png"))
dev.off()

#didn't disaggregate by subarea - would need to do another aggregate function to get this.
# #By survey subarea
# MeanWA.df %>% filter(SEX == 2) %>% ggplot(aes(x = AGE, y = MeanWeight)) + geom_point(data = NULL,stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
# facet_wrap(~SUBAREA)  
# dev.copy(png,paste0(OutDir,"Survey_MeanWtAge_Females_Subarea_sp_",sp_label,".png"))
# dev.off()

# MeanWA.df %>% filter(SEX == 1) %>% ggplot(aes(x = AGE, y = MeanWeight)) + geom_point(data = NULL,stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
# facet_wrap(~SUBAREA)  
# dev.copy(png,paste0(OutDir,"Survey_MeanWtAge_Males_Subarea_sp_",sp_label,".png"))
# dev.off()

#By Cohort, sex, and year
MeanWA.df %>% filter(SEX == 2) %>% ggplot(aes(x = AGE, y = MeanWeight)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
facet_wrap(~YEAR)  
dev.copy(png,paste0(OutDir,"Survey_MeanWtAge_by_Cohort_Females_Year_sp_",sp_label,".png"))
dev.off()

MeanWA.df %>% filter(SEX == 1) %>% ggplot(aes(x = AGE, y = MeanWeight)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
facet_wrap(~YEAR)  
dev.copy(png,paste0(OutDir,"Survey_MeanWtAge_by_Cohort_Males_Year_sp_",sp_label,".png"))
dev.off()
#----------------------------------------------------------------------------


#--------------------------------------------------------
#Only applies to GOA:
# AL.df %>% filter(SEX == 2) %>% ggplot(aes(x = Age, y = Length)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
# facet_grid(YEAR ~ REGULATORY_AREA_NAME)
# dev.copy(png,paste0(OutDir,"Survey_LatAge_by_Cohort_Females_Year_FMPSubarea.png"))
# dev.off()


# AL.df %>% filter(SEX == 1) %>% ggplot(aes(x = Age, y = Length)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
# facet_grid(YEAR ~ REGULATORY_AREA_NAME)
# dev.copy(png,paste0(OutDir,"Survey_LatAge_by_Cohort_Males_Year_FMPSubarea.png"))
# dev.off()


# AL.df %>% filter(SEX == 2) %>% ggplot(aes(x = Age, y = Length)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
# facet_grid(MAX_DEPTH ~ REGULATORY_AREA_NAME)
# dev.copy(png,paste0(OutDir,"Survey_LatAge_by_Cohort_Females_DEPTH_FMPSubarea.png"))
# dev.off()

# AL.df %>% filter(SEX == 1) %>% ggplot(aes(x = Age, y = Length)) + geom_point(data = NULL, aes(colour = factor(Cohort)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey")) +
# facet_grid(MAX_DEPTH ~ REGULATORY_AREA_NAME)
# dev.copy(png,paste0(OutDir,"Survey_LatAge_by_Cohort_Males_DEPTH_FMPSubarea.png"))
# dev.off()
#Growth by quarters
#p + facet_wrap(~Quarters) #not super meaningful for the survey

#Growth by bottom type - doesn't make a difference
# p + facet_wrap(~ BOTTOM_TYPE)

# p<-ggplot(AL.df,aes(x = AGE, y = LENGTH)) + geom_point(data = NULL, aes(colour = factor(YEAR)),stat = "identity") + theme(panel.grid.major = element_line(colour = "grey"))
#------------------------------------------------------------------

#-----------------------------------------------------------------
#Below was Moved to Get_GOA_Mean_Survey_Wt_At_Age
#-----------------------------------------------------------------

# #---------------------------------------
# #Mean weight-at-age
# #---------------------------------------

# GrowthMorphMeanWA.df<-aggregate(WEIGHT ~ aBIN + SEX + GrowthMorph,AL.df,mean)
# AggMeanWA.df<-aggregate(WEIGHT ~ SEX + aBIN ,AL.df,mean)
# AggMeanWA.df$SEX[AggMeanWA.df$SEX == 1] = "Male"
# AggMeanWA.df$SEX[AggMeanWA.df$SEX == 2] = "Female"

# #MeanWA.df$WtKg<-MeanWA.df$WEIGHT/1000

# #MeanWA.df<-MeanWA.df[order(MeanWA.df$SEX,MeanWA.df$aBIN),]
# #FlipWA_Fem.df<-MeanWA.df %>% filter(SEX == 2) %>% dcast(SEX ~ aBIN,sum)
# Flip.df<-AggMeanWA.df %>% mutate(WEIGHT = WEIGHT/1000) %>% dcast(SEX ~ aBIN,sum)
# write.csv(Flip.df,file = paste0(MyDir,"Survey_Age_Length\\Aggregated_Survey_Mean_Weight_at_Age.csv"))

# #MeanWA.df$SexAge<-paste0()
# #Melt/cast:
# #bin ages:


#--------------------------------------------------------------
# SELECT RACEBASE.SPECIMEN.HAULJOIN,
#   RACEBASE.SPECIMEN.REGION,
#   RACEBASE.SPECIMEN.SPECIMENID,
#   RACEBASE.SPECIMEN.BIOSTRATUM,
#   RACEBASE.SPECIMEN.SPECIES_CODE,
#   RACEBASE.SPECIMEN.LENGTH,
#   RACEBASE.SPECIMEN.SEX,
#   RACEBASE.SPECIMEN.AGE,
#   RACEBASE.HAUL.START_TIME,
#   RACEBASE.HAUL.BOTTOM_DEPTH,
#   RACEBASE.HAUL.STRATUM,
#   RACEBASE.HAUL.GEAR_TEMPERATURE,
#   RACEBASE.HAUL.BOTTOM_TYPE,
#   RACEBASE.HAUL.GEAR_DEPTH,
#   RACEBASE.HAUL.PERFORMANCE,
#   RACEBASE.HAUL.DURATION,
#   RACEBASE.HAUL.DISTANCE_FISHED,
#   RACEBASE.HAUL.NET_WIDTH,
#   RACEBASE.HAUL.NET_HEIGHT,
#   RACEBASE.HAUL.NET_MEASURED,
#   RACEBASE.HAUL.START_LATITUDE,
#   RACEBASE.HAUL.END_LATITUDE,
#   RACEBASE.HAUL.START_LONGITUDE,
#   RACEBASE.HAUL.END_LONGITUDE,
#   RACEBASE.HAUL.SURFACE_TEMPERATURE,
#   RACEBASE.HAUL.GEAR,
#   RACEBASE.HAUL.ABUNDANCE_HAUL,
#   GOA.GOA_STRATA.INPFC_AREA,
#   GOA.GOA_STRATA.MIN_DEPTH,
#   GOA.GOA_STRATA.MAX_DEPTH,
#   GOA.GOA_STRATA.DESCRIPTION,
#   GOA.GOA_STRATA.REGULATORY_AREA_NAME,
#   GOA.GOA_STRATA.STRATUM_TYPE
# FROM RACEBASE.SPECIMEN
# INNER JOIN RACEBASE.HAUL
# ON RACEBASE.SPECIMEN.HAULJOIN = RACEBASE.HAUL.HAULJOIN
# INNER JOIN GOA.GOA_STRATA
# ON RACEBASE.HAUL.STRATUM           = GOA.GOA_STRATA.STRATUM
# WHERE RACEBASE.SPECIMEN.REGION     = 'GOA'
# AND RACEBASE.SPECIMEN.SPECIES_CODE = 10200
# AND RACEBASE.HAUL.ABUNDANCE_HAUL   = 'Y'


#Older:
# ALQuery<-paste0("SELECT RACEBASE.SPECIMEN.HAULJOIN,\n ",
#   "RACEBASE.SPECIMEN.REGION,\n ",
#   "RACEBASE.SPECIMEN.SPECIMENID,\n ",
#   "RACEBASE.SPECIMEN.BIOSTRATUM,\n ",
#   "RACEBASE.SPECIMEN.SPECIES_CODE,\n ",
#   "RACEBASE.SPECIMEN.LENGTH,\n ",
#   "RACEBASE.SPECIMEN.SEX,\n ",
#   "RACEBASE.SPECIMEN.AGE,\n ",
#   "RACEBASE.HAUL.START_TIME,\n ",
#   "RACEBASE.HAUL.BOTTOM_DEPTH,\n ",
#   "RACEBASE.HAUL.STRATUM,\n ",
#   "RACEBASE.HAUL.GEAR_TEMPERATURE,\n ",
#   "RACEBASE.HAUL.BOTTOM_TYPE,\n ",
#   "RACEBASE.HAUL.GEAR_DEPTH,\n ",
#   "RACEBASE.HAUL.PERFORMANCE,\n ",
#   "RACEBASE.HAUL.DURATION,\n ",
#   "RACEBASE.HAUL.DISTANCE_FISHED,\n ",
#   "RACEBASE.HAUL.NET_WIDTH,\n ",
#   "RACEBASE.HAUL.NET_HEIGHT,\n ",
#   "RACEBASE.HAUL.NET_MEASURED,\n ",
#   "RACEBASE.HAUL.START_LATITUDE,\n ",
#   "RACEBASE.HAUL.END_LATITUDE,\n ",
#   "RACEBASE.HAUL.START_LONGITUDE,\n ",
#   "RACEBASE.HAUL.END_LONGITUDE,\n ",
#   "RACEBASE.HAUL.SURFACE_TEMPERATURE,\n ",
#   "RACEBASE.HAUL.GEAR,\n ",
#   "RACEBASE.HAUL.ABUNDANCE_HAUL\n ",
# "FROM RACEBASE.SPECIMEN\n ",
# "INNER JOIN RACEBASE.HAUL\n ",
# "ON RACEBASE.SPECIMEN.HAULJOIN      = RACEBASE.HAUL.HAULJOIN\n ",
# "WHERE RACEBASE.SPECIMEN.REGION     = 'GOA'\n ",
# "AND RACEBASE.SPECIMEN.SPECIES_CODE = 10200\n ",
# "AND RACEBASE.HAUL.ABUNDANCE_HAUL   = 'Y'")
