setwd("C:/Users/mmacq/OneDrive/Documents/RStuff/SBDP/DCI-Project/DCI-Project")
library(dplyr)
library(lubridate)


# first, read in life history data for calves, filter for only known mothers and organize.
life<-read.csv(file="./RawData/Life-hist.csv", header=T, sep=",", na.strings= "") %>%
  dplyr::select(Dolphin.ID,
                Dolphin.Name, 
                Sex, 
                Mother.ID, 
                Haplotype,
                Birth.Date,
                Birth.Date.Primary.Assigner,
                Birth.Date.Accuracy,
                Death.Date,
                Death.Date.Primary.Assigner,
                Death.Date.Accuracy) %>%
  filter(Mother.ID != "NA") %>%
  arrange(Birth.Date)%>%
  arrange(Mother.ID)


# add information for second calves in interval, when available, and correct date formats
Matrilines<-life%>%
  group_by(Mother.ID) %>% 
  mutate (NextCalfBDay = lead(Birth.Date)) %>%
  group_by(Mother.ID) %>%
  mutate (NextCalfBDayAccuracy = lead(Birth.Date.Accuracy)) %>%
  group_by(Mother.ID) %>%
  mutate(NextCalfName = lead(Dolphin.Name)) %>%
  group_by(Mother.ID) %>%
  mutate(NextCalfID = lead(Dolphin.ID)) %>%
  group_by(Mother.ID) %>%
  mutate(NextCalfBDayAssigner = lead(Birth.Date.Primary.Assigner))

Matrilines$Birth.Date<-ymd(Matrilines$Birth.Date)
Matrilines$Death.Date<-ymd(Matrilines$Death.Date)
Matrilines$NextCalfBDay<-ymd(Matrilines$NextCalfBDay)


# read in life history for mother, rename columns, and correct date formats
Mom_life<-read.csv(file="Life-hist.csv", header=T, sep=",", na.strings= "") %>%
  dplyr::select(Dolphin.ID,
                Dolphin.Name, 
                Birth.Date,
                Death.Date) %>%
  rename(Mom.ID = Dolphin.ID)%>%
  rename(Mom.Name = Dolphin.Name)%>%
  rename(MomBDay = Birth.Date)%>%
  rename(MomDDay = Death.Date)

Mom_life$MomBDay<-ymd(Mom_life$MomBDay)
Mom_life$MomDDay<-ymd(Mom_life$MomDDay)


# join "life" table with calf information with mother's info
Mat_join<- left_join(Matrilines, Mom_life, by = c("Mother.ID" = "Mom.ID"))


# add calf age at death, mom age at "first birth" in the interval, and time to "next birth" 
# NOTE: some DBIs negative because next calf is born before other death date assigned
Mat_join<- mutate(Mat_join, CalfAgeDeath=as.numeric(Death.Date - Birth.Date, units="days"))
Mat_join<- mutate(Mat_join, MomAgeBirth=as.numeric(Birth.Date - MomBDay, units="days"))
Mat_join<- mutate(Mat_join, TimeToNextBirth=as.numeric(NextCalfBDay - Death.Date, units="days"))



#filtering for calves that died before age 3 and time to next birth is non-negative (some next calves born before other dies)
#filtering for next calf BDay and DDay desired accuracies
#add season (y/n) for first calf birth ("in season" = 1 when occurred September through January consistent with Mann et al., 2000)
DBISample<-Mat_join %>%
  filter(is.na(TimeToNextBirth) | TimeToNextBirth > 0)%>%
  filter(CalfAgeDeath < 1095.75) %>%
  filter(Birth.Date.Accuracy != "YEAR ESTIMATE")%>%
  filter(Death.Date.Accuracy != "YEAR ESTIMATE")%>%
  filter(is.na(TimeToNextBirth) | NextCalfBDayAccuracy != "YEAR ESTIMATE") %>%
  mutate(DeathMonth = month(Death.Date)) %>%
  mutate(Season = ifelse(DeathMonth == 9 | DeathMonth == 10 | DeathMonth == 11 | DeathMonth == 12 | DeathMonth == 1, 1,0))
                     

# Here, write csv which is required to examine sighting gaps among the moms in the sample
write.csv(DBISample, "mort_sample.csv")

################################################################################################
############## PAUSE - go to "Sighting-gaps-moms.R" to generate list of females who make the cut
Intervals<- read.csv(file="interval-two.csv", header=T, sep=",", strip.white=T, na.strings= "") %>%
  dplyr::select(Dolphin.Name,
                Gap,
                ObsCount)

# filter DCI sample to calves whose mothers meet the 2yr sighting gap requirements
# 173 intervals with new approach, 3 yr gap-- 108 moms (two year gap 173 intervals, 105 moms)
Sample<-DBISample %>%
  filter(Dolphin.Name %in% Intervals$Dolphin.Name)


# make CalfAgeDeath, TimeToNextBirth, and MomAgeAtFirstBirth years not days
# create column for event/censored - after the "first calf" died, did she have another?
# create time to event column exclude case where calf outlived mom
    # NOTE: This sample excludes moms/kids who have the same assigned death date so TimeToEvent = 0
# create age categories
# create columns for age categories (binary)
FinalSample<-Sample %>%
  mutate(Present = today()) %>%
  mutate(CalfAgeDeathYrs = CalfAgeDeath/365.25) %>%
  mutate(TimeToNextBirth = TimeToNextBirth/365.25) %>%  
  mutate(MomAgeBirth = MomAgeBirth/365.25) %>%
  
  mutate(EventCensored = ifelse(TimeToNextBirth>1, 1,0)) %>%
  mutate(EventCensored = ifelse(is.na(TimeToNextBirth), 0,1)) %>%
  mutate(MomDeadCensored = ifelse(EventCensored == 0 & !is.na(MomDDay) & is.na(TimeToNextBirth), 1,0)) %>%
  mutate(MomAliveCensored = ifelse(EventCensored == 0 & is.na(MomDDay) & is.na(TimeToNextBirth), 1,0))%>%

  mutate(TimeToEvent = ifelse(EventCensored == 1, print(TimeToNextBirth), ifelse(MomDeadCensored == 1, 
  print((MomDDay-Death.Date)/365.25), ifelse(MomAliveCensored == 1, print((Present-Death.Date)/365.25),0)))) %>%

  mutate(CalfAgeDeathCategories = ifelse(CalfAgeDeathYrs < 0.25, "a.Calf.under.3mo",
                                         ifelse(CalfAgeDeathYrs < 1, "b.Calf.3mo.to.1y",
                                                ifelse(CalfAgeDeathYrs < 2, "c.Calf.1y.to.2y", "d.Calf.2y.to.3y")))) %>%

  mutate(a.Calf.under.3mo = ifelse(CalfAgeDeathCategories == "a.Calf.under.3mo", 1, 0)) %>%
  mutate(b.Calf.3mo.to.1y = ifelse(CalfAgeDeathCategories == "b.Calf.3mo.to.1y", 1, 0)) %>%
  mutate(c.Calf.1y.to.2y = ifelse(CalfAgeDeathCategories == "c.Calf.1y.to.2y", 1, 0)) %>%
  mutate(d.Calf.2y.to.3y = ifelse(CalfAgeDeathCategories == "d.Calf.2y.to.3y", 1, 0)) %>%


  #### Not sure if we want to run this... probably, 2-3 range was messing up the model
  ## 2025 Meredith says yes
#remove calves w/ age death >2yrs... 162 intervals
  filter(CalfAgeDeath < 730.5) 

# 101 females
Finalmom<-unique(FinalSample$Mother.ID)

write.csv(FinalSample, "C:/Users/mmacq/OneDrive/Documents/RStuff/SBDP/DCI-Project/DCI-Project/RawData/Final-sample.csv")

### Summaries for viewing
Summary<-FinalSample%>%
filter(EventCensored == 1) %>%
  group_by(CalfAgeDeathCategories) %>%
  summarise(mean.T = (mean(TimeToEvent)*12), mean.yrs = (mean(TimeToEvent)), dci = ((mean(TimeToEvent)*12)-12), sd.T = (sd(TimeToEvent)*12), sd.yrs = (sd(TimeToEvent)), n.Age.Cats = n())

print(Summary)

Event2<-FinalSample%>%
  filter(EventCensored == "1")

Censored<-FinalSample%>%
  filter(EventCensored == "0")

Overall<-FinalSample%>%
  filter(EventCensored == 1)%>%
  summarise(Mean = (mean(TimeToEvent)), SD = sd(TimeToEvent))
print(Overall)

Newborn<-FinalSample%>%
  filter(EventCensored == 1) %>%
  filter(CalfAgeDeathCategories == "a.Calf.under.3mo")
  
  
plot(Newborn$DeathMonth, Newborn$TimeToEvent)

