setwd("~/RStuff/SBDP/DCI-Project/Raw-Data")
library(dplyr)
library(lubridate)
library(survival)
library(survminer)
library(coxme)
library(ggplot2)
library(frailtySurv)


#Opening Matrilines raw file
matrilines <- read.csv(file = "Matrilines_30May23_date_fixed.csv", header=T, strip.white=T, na.strings = "")

# Subsetting to project-relevant columns
MatrilinesRevelantSubset <- select(matrilines, MomName, MomID, CalfName, CalfID, AssignedBDay, BDayAccuracy, DeathDate, DeathDateAccuracy, Interval.between.mothers.last.sighting)

# making new column for "next calf" birth date, accuracy, name, and code
MatrilinesRevelantSubset %>% 
  group_by(MomID) %>% 
  mutate (NextCalfBDay = lead(AssignedBDay)) %>%
  group_by(MomID) %>%
  mutate (NextCalfBDayAccuracy = lead(BDayAccuracy)) %>%
  group_by(MomID) %>%
  mutate(NextCalfName = lead(CalfName)) %>%
  group_by(MomID) %>%
  mutate(NextCalfID = lead(CalfID)) -> AddNextCalfInfo

#making dates dates, goes to YYYY-MM-DD
AddNextCalfInfo %>%
  mutate(AssignedBDay = as.Date(AssignedBDay, "%d/%m/%Y")) %>%
  mutate(DeathDate = as.Date(DeathDate, "%d/%m/%Y")) %>%
  mutate(NextCalfBDay = as.Date(NextCalfBDay, "%d/%m/%Y")) -> AsDate

#making new column for calf age @ death 
# time in days, lubridate prints unit label "days" when it does the calculation
AsDate %>%
  mutate(CalfAgeDeath = DeathDate - AssignedBDay) -> AddCalfAgeAtDeath

#filtering for calves that died before age 3
AddCalfAgeAtDeath %>%
  filter(CalfAgeDeath < 1095.75) -> CalvesDieBefore3

#filtering for next calf BDay and DDay desired accuracies
CalvesDieBefore3 %>%
  filter(BDayAccuracy == "MonthEstimate"| BDayAccuracy == "DayEstimate"| BDayAccuracy == "WeekEstimate"| is.na(BDayAccuracy)) %>%
  filter(NextCalfBDayAccuracy == "MonthEstimate"| NextCalfBDayAccuracy == "DayEstimate" | NextCalfBDayAccuracy == "WeekEstimate" | is.na(NextCalfBDayAccuracy)) %>%
  filter(DeathDateAccuracy == "MonthEstimate" | DeathDateAccuracy == "DayEstimate" | DeathDateAccuracy == "WeekEstimate" | is.na(DeathDateAccuracy)) -> BDayDDayAccurate

###COMPILE ALL ALL DOLPHIN SIGHTINGS FROM SURVEYS AND FOCALS AND CALCULATE SIGHTING GAPS 
###AND MAX SIGHITING GAPS
####################################################################################
####################################################################################
###Download all survey sightings and all focal sightings, make sure to convert the date 
###format in excel to one that R recognizes from the database and load them into R
survey <- read.csv("C:/Users/mmacq/OneDrive/Documents/RStuff/SBDP/DCI-Project/Raw-Data/Survey-dolphin-27-Jun-2023_date_fixed.csv", header = TRUE)

focal <- read.csv("C:/Users/mmacq/OneDrive/Documents/RStuff/SBDP/DCI-Project/Raw-Data/Focal-dolphin-27-Jun-2023_date_fixed.csv", header = TRUE)

##for both surveys and focals, select the relevent columns. 
survey %>% select(Observation.Date, Dolphin.ID, Dolphin.ID.Certainty)  -> survey.filt

focal %>% select(Observation.ID, Observation.Date, Dolphin.ID, Dolphin.ID.Certainty) -> focal.filt


###combine surveys and focals into all sightings, remove all low certainty IDs and convert the date format
###then calculate the gap between each row ?? lubridate thinks gap is in days but its years, which is what I want. date.convert = YYYY-MM-DD
survey.filt %>% bind_rows(focal.filt) %>% 
  filter(Dolphin.ID.Certainty != "LOW" )%>% 
  mutate(date.convert = as.Date(Observation.Date, "%d/%m/%Y")) %>%
  select(Dolphin.ID, date.convert) %>%
  group_by(Dolphin.ID) %>%
  arrange(Dolphin.ID, date.convert) %>%
  mutate(gap = ((date.convert - lag(date.convert))/365.25)) -> sightings.sort


#### Count total number of sightings for each dolphin
sightings.sort %>%
  group_by(Dolphin.ID) %>%
  add_count(Dolphin.ID, name = "Total.sightings") -> Total.sightings


###pull out the maximum gap for each dolphin. Then check for duplicates; otherwise, if a 
#dolphin has two gaps of the same value, it'll be put into the dataset twice.

Total.sightings %>% 
  group_by(Dolphin.ID) %>% 
  top_n(1, gap) %>% 
  mutate(dup = duplicated(Dolphin.ID)) %>% 
  filter(dup == FALSE) -> max.gap

#making new column for time to next birth after calf death (using day, week and month accuracy)
BDayDDayAccurate %>%
  mutate(TimeToNextBirth = NextCalfBDay - DeathDate) -> AddTimeToNextBirth

#add column for seasonality in calf birth 
AddTimeToNextBirth %>%
  mutate(DeathMonth = month(DeathDate)) %>%
  mutate(Season = ifelse(DeathMonth>8, 1,0)) -> Seasonality

#bringing in Life.table and select only Dolphin.ID, birth.date.convert, and death.date.convert
read.csv(file = "C:/Users/mmacq/OneDrive/Documents/RStuff/SBDP/DCI-Project/Raw-Data/Life.History.All.27-Jun-2023_date_fixed.csv", header = T, strip.white = T, na.strings = "") %>%
  select(Dolphin.ID, Dolphin.Name, Sex, Sex.Certainty, Birth.Date, Death.Date) -> LifeTableSubset

#join LifeTableSubset and max.gap
Seasonality %>%
  left_join(LifeTableSubset, by = c("MomID" = "Dolphin.ID")) %>%
  left_join(max.gap, by = c("MomID" = "Dolphin.ID")) -> JoinLifeTable.and.max.gap

######## !!! DONT RUN THIS FILTERING STEP !!!
#filter for gaps <2 yrs 
JoinLifeTable.and.max.gap %>%
  filter(gap <= 2) -> Gap.Less.Than.2yr 

JoinLifeTable.and.max.gap %>%
  filter(gap <= 3) -> Gap.Less.Than.3yr

JoinLifeTable.and.max.gap %>%
  filter(Total.sightings >= 20) -> AtLeast20

##### EDIT THIS STEP BASED ON ABOVE
#making mom Bday a date
Gap.Less.Than.3yr %>%
mutate(Mom.BDay = as.Date(Birth.Date, "%d/%m/%Y")) %>%
mutate(Mom.DDay = as.Date(Death.Date, "%d/%m/%Y")) -> MomBDayAsDate

#get mom age at "first birth"
MomBDayAsDate %>%
  mutate(MomAgeAtFirstBirth = AssignedBDay-Mom.BDay) -> AddMomAgeAtFirstBirth

# make CalfAgeDeath, TimeToNextBirth, and MomAgeAtFirstBirth years not days
# lubridate columns still printing "days" label, its not its years.
AddMomAgeAtFirstBirth %>%
  mutate(CalfAgeDeathYrs = CalfAgeDeath/365.25) %>%
  mutate(TimeToNextBirth = TimeToNextBirth/365.25) %>%
  mutate(MomAgeAtFirstBirth = MomAgeAtFirstBirth/365.25) -> TimeInYears

#create column for event/censored - after the "first calf" died, did she have another?
TimeInYears %>%
  mutate(EventCensored = ifelse(TimeToNextBirth>1, 1,0)) %>%
  mutate(EventCensored = ifelse(is.na(TimeToNextBirth), 0,1)) -> AddEventCensored

# create "present" column and make it a "date" class
AddEventCensored %>%
  mutate(Present = today()) -> Today

# create columns for if mom is dead or alive and censored
Today %>%
  mutate(MomDeadCensored = ifelse(EventCensored == 0 & !is.na(Mom.DDay) & is.na(TimeToNextBirth), 1,0)) %>%
  mutate(MomAliveCensored = ifelse(EventCensored == 0 & is.na(Mom.DDay) & is.na(TimeToNextBirth), 1,0)) -> MomDeadAliveCensored

#create time to event column exclude case where calf outlived mom
#This sample excludes moms/kids who have the same assigned death date so TimeToEvent = 0
MomDeadAliveCensored %>%
  mutate(TimeToEvent = ifelse(EventCensored == 1, print(TimeToNextBirth), ifelse(MomDeadCensored == 1, 
  print((Mom.DDay-DeathDate)/365), ifelse(MomAliveCensored == 1, print((Present-DeathDate)/365),0)))) %>%
  filter(TimeToEvent > 0)-> AddTimeToEvent
  
#create age categories
AddTimeToEvent %>%
  mutate(CalfAgeDeathCategories = ifelse(CalfAgeDeathYrs < 0.25, "a.Calf.under.3mo",
                                         ifelse(CalfAgeDeathYrs < 1, "b.Calf.3mo.to.1y",
                                                ifelse(CalfAgeDeathYrs < 2, "c.Calf.1y.to.2y", "d.Calf.2y.to.3y")))) -> AddCalfAgeDeathCats
#create columns for age categories
AddCalfAgeDeathCats %>%
  mutate(a.Calf.under.3mo = ifelse(CalfAgeDeathCategories == "a.Calf.under.3mo", 1, 0)) %>%
  mutate(b.Calf.3mo.to.1y = ifelse(CalfAgeDeathCategories == "b.Calf.3mo.to.1y", 1, 0)) %>%
  mutate(c.Calf.1y.to.2y = ifelse(CalfAgeDeathCategories == "c.Calf.1y.to.2y", 1, 0)) %>%
  mutate(d.Calf.2y.to.3y = ifelse(CalfAgeDeathCategories == "d.Calf.2y.to.3y", 1, 0)) -> CalfDeathCatsBinary

#### Not sure if we want to run this... probably, 2-3 range was messing up the model
#remove calves w/ age death >2yrs
CalfDeathCatsBinary %>%
  filter(CalfAgeDeath < 730.5) -> CalfDieBefore2

##################################################################################################### 
################################# *** !!! SELECTED MODEL !!! *** ###########################################################
#run w/ no calf death >2
coxme(Surv(TimeToEvent, EventCensored) ~ CalfAgeDeathCategories + MomAgeAtFirstBirth + Season + (1 | MomID), data = CalfDieBefore2) -> Coxme.Age.Cat.Under2
print(Coxme.Age.Cat.Under2)
################################# !!! SELECTED PLOT !!!  ###########################################################  
surv_fit(Surv(TimeToEvent, EventCensored) ~ CalfAgeDeathCategories, data = CalfDieBefore2) -> FitDieBefore2
ggsurvplot(FitDieBefore2, data = CalfDieBefore2, fun = "event", xlab = "Time (years)", ylab = "Completion probability", font.main = c(16, "plain", "black"),
           font.x = c(14, "plain", "black"), font.y = c(14, "plain", "black"),
           font.tickslab = c(12, "plain", "black"),  legend = c("top", "bottom", "left", "right", "none"),
           legend.title = "Strata", font.legend = c(10,"plain", "black")) 

#ATTEMPT TO MAKE PLOT LOOK GOOD -- changed to smaller fonts to see table, could undo
#interesting to see risk table, have to make dimensions at least 600 to get it all to fit and look nice though
surv_fit(Surv(TimeToEvent, EventCensored) ~ CalfAgeDeathCategories, data = CalfDieBefore2) -> FitDieBefore2      
ggsurvplot(FitDieBefore2, data = CalfDieBefore2, fun = "event", color = NULL, palette = "hue",
           break.time.by = 1, surv.scale = c("default", "percent"),
           conf.int = FALSE, conf.int.fill = "gray", censor = TRUE, pval = FALSE,
           pval.size = 5, pval.coord = c(NULL, NULL), main = NULL, xlab = "Time (years)",
           ylab = "Completion probability", font.main = c(16, "plain", "black"),
           font.x = c(16, "plain", "black"), font.y = c(16, "plain", "black"),
           font.tickslab = c(10, "plain", "black"), xlim = NULL, ylim = NULL,
           legend = c(0.8, 0.3), legend.title = "Calf age at death:", 
           legend.labs = c("under 3 months", "3-12 months", "12-24 months"), 
           font.legend = c(10,"plain", "black"), 
           
          risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           surv.median.line = "hv", # Specify median survival
           
           ggtheme = ggplot2::theme_classic())


#############################SUBSETS TO VIEW #############################
# Subset with all info -- 229 obs, calves die before age 2, excluded DCIs <= 0.
CalfDieBefore2 %>%
  select(MomName, MomID, CalfName, CalfID, AssignedBDay, BDayAccuracy, DeathDate, DeathDateAccuracy, 
         NextCalfName, NextCalfID, NextCalfBDay, NextCalfBDayAccuracy, Season,
         Dolphin.Name, gap, Mom.BDay, Mom.DDay, MomAgeAtFirstBirth,
         CalfAgeDeathYrs, EventCensored, MomDeadCensored, MomAliveCensored, 
        TimeToNextBirth, TimeToEvent, CalfAgeDeathCategories) -> Reduced_sample

write.csv(Reduced_sample, file = "Sample-27-Jun-2022.csv")

CalfDieBefore2 %>%
  select(MomName, CalfName, AssignedBDay, BDayAccuracy, DeathDate, DeathDateAccuracy, NextCalfName, NextCalfBDay, NextCalfBDayAccuracy, CalfAgeDeathYrs, CalfAgeDeathCategories, TimeToNextBirth) -> Minimal.sample

Minimal.sample %>%
  filter(CalfAgeDeathCategories == "b.Calf.3mo.to.1y") -> Three.months.to.one.year

# Subset for me
CalfDieBefore2 %>%
  select(EventCensored, TimeToEvent, CalfAgeDeathCategories) -> MeSubset
write.csv(MeSubset, file = "Me.csv")

###################################################################################################################
####################### CALCULATE MEAN DBI AND SE FOR EACH AGE GROUP ##############################################
### Below this, calculates # of conceptions within two months and percent of conceptions within each age group under 2 months ##
###################################################################################################################
MeSubset %>%
  filter(EventCensored == 1) %>%
  group_by(CalfAgeDeathCategories) %>%
  summarise(mean.T = (mean(TimeToEvent)*12), mean.yrs = (mean(TimeToEvent)), sd.T = (sd(TimeToEvent)*12), sd.yrs = (sd(TimeToEvent)), n.Age.Cats = n()) -> Me.Compare

print(Me.Compare)
###################################################################################################################
########################################### !!! NEW STUFF !!! #####################################################
################## Filter for DBI's 14 months or less (conceptions within 2months; 1 year gestation + 2 months)##################################################
#################### Count females in each age cat that gave birth within 14 mos ##################################
CalfDieBefore2 %>%
  select(MomID, EventCensored, TimeToEvent, CalfAgeDeathCategories) %>%
  filter(EventCensored == 1) %>%
  filter(TimeToEvent <= 1.167) -> DCI.under.2mo
print(DCI.under.2mo)
###################### Make CSV to view #################################################
CalfDieBefore2 %>%
  select(MomID, CalfName, AssignedBDay, BDayAccuracy, DeathDate, DeathDateAccuracy, NextCalfName, NextCalfBDay, NextCalfBDayAccuracy, EventCensored, TimeToEvent, CalfAgeDeathCategories) %>%
  filter(EventCensored == 1) %>%
  filter(TimeToEvent <= 1.167) -> DBI.under.14.months
write.csv(DBI.under.14.months, file = "Conception.by.2.mo.csv")


DCI.under.2mo %>%
  group_by(CalfAgeDeathCategories) %>%
  summarise(Count = n()) -> within.2.mo
print(within.2.mo)

Me.Compare %>%
  left_join(within.2.mo, by = c("CalfAgeDeathCategories" = "CalfAgeDeathCategories")) -> Rapid.join
 
Rapid.join %>%
  mutate(Percent.rapid = (Count/n.Age.Cats)*100) -> Add.percent.rapid
print(Add.percent.rapid) 


###################### NOT USED ##################################################
############################ Filter for DBI's 15 months or less ##################################################
#################### Count females in each age cat that gave birth within 15 mos ##################################
CalfDieBefore2 %>%
  select(MomID, EventCensored, TimeToEvent, CalfAgeDeathCategories) %>%
  filter(EventCensored == 1) %>%
  filter(TimeToEvent <= 1.25) -> DBI.under.3mo



################## make csv to view ###############################################
CalfDieBefore2 %>%
  select(MomID, CalfName, AssignedBDay, BDayAccuracy, DeathDate, DeathDateAccuracy, NextCalfName, NextCalfBDay, NextCalfBDayAccuracy, EventCensored, TimeToEvent, CalfAgeDeathCategories) %>%
  filter(EventCensored == 1) %>%
  filter(TimeToEvent <= 1.25) -> DBI.under.15.months
write.csv(DBI.under.15.months, file = "Conception.by.3.mo.csv")

DBI.under.3mo %>%
  group_by(CalfAgeDeathCategories) %>%
  summarise(Count = n()) -> Within.3mo