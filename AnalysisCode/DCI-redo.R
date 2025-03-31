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


# read in another copy of life history for mother, rename columns, and correct date formats
Mom_life<-read.csv(file="./RawData/Life-hist.csv", header=T, sep=",", na.strings= "") %>%
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
write.csv(DBISample, "./IntermediateData/mort_sample.csv")

################################################################################################
############## PAUSE - go to "Sighting-gaps-moms.R" to generate list of females who make the cut
Intervals<- read.csv(file="./IntermediateData/interval-two.csv", header=T, sep=",", strip.white=T, na.strings= "") %>%
  dplyr::select(Dolphin.Name,
                Gap,
                ObsCount)


# filter DCI sample to calves whose mothers meet the 2yr sighting gap requirements
# OLD 173 intervals with new approach, 3 yr gap-- 108 moms (two year gap 173 intervals, 105 moms)
# New (30-Mar-2025 158 intervals )
Sample<- left_join(DBISample, Intervals, by = c("Dolphin.Name" = "Dolphin.Name")) %>%
  filter(Dolphin.Name %in% Intervals$Dolphin.Name)
  

# make CalfAgeDeath, TimeToNextBirth, and MomAgeAtFirstBirth years not days
# create column for event/censored - after the "first calf" died, did she have another?
# create time to event column exclude case where calf outlived mom
    # NOTE: This sample excludes moms/kids who have the same assigned death date so TimeToEvent = 0
# create age categories
# create columns for age categories (binary)
# remove calves w/ age death >2yrs
# FinalSample is 150 intervals
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

  filter(CalfAgeDeath < 730.5) 

# 101 females, 150 intervals
Finalmom<-unique(FinalSample$Mother.ID)

write.csv(FinalSample, "C:/Users/mmacq/OneDrive/Documents/RStuff/SBDP/DCI-Project/DCI-Project/IntermediateData/Final-sample.csv")

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

Janet<-FinalSample%>%
  filter(Birth.Date.Primary.Assigner == "JM")


# Calculate authorship contributions
# JM 50%, DIC 10.7%, EK 8%
BirthDateCredit <- FinalSample %>%
  group_by(Birth.Date.Primary.Assigner)%>% 
  add_count(Birth.Date.Primary.Assigner) %>%
  filter(!is.na(Birth.Date.Primary.Assigner)) %>%
 mutate(Percent = (n/149)*100) # 150 birth dates in the sample, 1 assigner is NA

# JM 33%, DIC 22%, VF 16%, CCC and ERJ 11%, MMQ 5%
BirthDateLast5 <- FinalSample %>%
  filter(Birth.Date >= "2020-01-01") %>% 
  filter(!is.na(Birth.Date.Primary.Assigner)) %>% # 18 birth dates in last 5 years in the sample, none are NA
  group_by(Birth.Date.Primary.Assigner)%>%
  count() %>%
  mutate(Percent = (n/18)*100)

# JM 67% then next highest is VF with ~9%
DeathDateCredit <- FinalSample %>%
  group_by(Death.Date.Primary.Assigner) %>%
  add_count(Death.Date.Primary.Assigner) %>%
  filter(!is.na(Death.Date.Primary.Assigner)) %>% #146 death dates in sample, 4 are NA
mutate(Percent = (n/146)*100) 
  
# VF 42%, CCC 32%, JM and DIC have next highest contribution 10%, then MMQ 5%
DeathDateLast5 <- FinalSample %>%
  filter(Death.Date >= "2020-01-01") %>% 
  filter(!is.na(Death.Date.Primary.Assigner)) %>% # 19 death dates in last 5 years in the sample, none are NA
  group_by(Death.Date.Primary.Assigner)%>%
  count() %>%
  mutate(Percent = (n/19)*100)

# JM 41%, DIC 15%, EK 8%
NextCalfCredit <- FinalSample %>%
  group_by(NextCalfBDayAssigner) %>%
  add_count(NextCalfBDayAssigner) %>%
  filter(!is.na(NextCalfBDay)) %>% #117 next calf bdays in sample, so 33 are NA
  mutate(Percent = (n/117)*100) 

#JM 30%, DIC 26%, VF 17%, CCC 13%, MMQ 8%, ERJ 4%
NextCalfLast5 <- FinalSample %>%
  filter(NextCalfBDay >= "2020-01-01") %>% 
  filter(!is.na(NextCalfBDayAssigner))  %>% # 23 next calf bdays in last 5 years in the sample 
  group_by(NextCalfBDayAssigner)%>%
  count() %>%
  mutate(Percent = (n/23)*100)
  

###### birthdays pooled --- prob not necessary

Bdays <- BirthDateCredit%>%
  select(Birth.Date.Primary.Assigner,
         n)

NextBday <- NextCalfCredit %>%
  select(NextCalfBDayAssigner,
         n)

# JM 46%, DIC 12%, EK 8%
allBdays <- left_join(Bdays, NextBday, by = c("Birth.Date.Primary.Assigner" = "NextCalfBDayAssigner"))%>%
  mutate(n.y = replace(n.y,is.na(n.y),0)) %>%
  mutate(TotalBdays = n.x + n.y) %>%
  unique()%>%
  mutate(Percent = (TotalBdays/266)*100)
  
  
