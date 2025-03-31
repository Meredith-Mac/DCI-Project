setwd("C:/Users/mmacq/OneDrive/Documents/RStuff/SBDP/DCI-Project/DCI-Project")
library(dplyr)
library(lubridate)


# first, read in data. Convert observation dates into a format that R recognizes
survey<-read.csv(file="./RawData/survey.csv", header=T, sep=",", na.strings= "") %>%
  dplyr::select(Observation.ID,
                Observation.Date,
                Dolphin.ID,
                Dolphin.ID.Certainty) 

survey$Observation.Date<-ymd(survey$Observation.Date)

focal<-read.csv(file="./RawData/focal.csv", header=T, sep=",", strip.white=T, na.strings= "") %>%
  dplyr::select(Observation.ID,
                Observation.Date,
                Dolphin.ID,
                Dolphin.ID.Certainty) 

focal$Observation.Date<-ymd(focal$Observation.Date)

### COMPILE ALL ALL DOLPHIN SIGHTINGS FROM SURVEYS AND FOCALS AND CALCULATE SIGHTING GAPS 
### AND MAX SIGHITING GAPS
####################################################################################

### combine surveys and focals into all sightings, remove all low certainty IDs
all_sightings<-bind_rows(focal, survey) %>% 
  filter(Dolphin.ID.Certainty != "LOW") %>%
  group_by(Dolphin.ID) %>%
  arrange(Dolphin.ID, Observation.Date)


# bring in mom info from mortality sample and join mom info to all sightings, correct date formats
momlist<-read.csv("./IntermediateData/mort_sample.csv") %>%
  dplyr::select(Mother.ID,
                Mom.Name,
                MomBDay, 
                MomDDay)

momlist$MomBDay<-ymd(momlist$MomBDay)
momlist$MomDDay<-ymd(momlist$MomDDay)


# join all sightings for moms in the sample 
sightmom<-left_join(all_sightings, momlist, by = c("Dolphin.ID" = "Mother.ID"))

# remove sightings of dolphins not in the mom list, hence no info joined for Mom.Name
momsightings<-sightmom %>%
  filter(Mom.Name != "NA")

# bring in calf info
calflist<-read.csv("./IntermediateData/mort_sample.csv")%>%
  dplyr::select(Mother.ID,
                Dolphin.Name,
                NextCalfName,
                Death.Date,
                NextCalfBDay)

calflist$Death.Date<-ymd(calflist$Death.Date)
calflist$NextCalfBDay<-ymd(calflist$NextCalfBDay)

# join mom sighting info to all calves.. for females with multiple intervals, will duplicate all of her sighting history by number of intervals she has
df<-left_join(calflist, momsightings, by = c("Mother.ID" = "Dolphin.ID"))

#filter obs dates during interval between death date of the 1st calf and next calf bday or all obs if no next calf born yet
intervalsight<-df%>%
  filter((Observation.Date > Death.Date) & ((Observation.Date < NextCalfBDay) | is.na(NextCalfBDay)))

#if no next calf born yet, tag obs where obs date is > four years after the initial calf death date
# 2025 Meredith isnt removing them here-- remove obs that are four years after a calf date and still no new calf born (still censored four years later) and post-covid years
#2025 Meredith is flagging them, and flagging inital calf death dates during covid times
# NOTE TO FUTURE MEREEDITH, if we want to include post-covid data, need to do this differently !!!!!!!
Tag<-intervalsight%>%
  mutate(CensoredFour=ifelse(is.na(NextCalfBDay) & (Observation.Date > (Death.Date+1461)), 1, 0)) %>%
 # filter(CensoredFour==0)%>%
  mutate(Covid=ifelse(Death.Date >= "2019-01-01", 1, 0))%>%
  distinct(Observation.Date, Mom.Name, .keep_all = TRUE)
  

#add number of obs of mom within each interval, calculate sighting gaps in days
Gaps<-Tag%>%
  group_by(Dolphin.Name)%>%
  add_count(Mother.ID)%>%
  rename(ObsCount = n)%>%
  mutate(Gap=as.numeric(Observation.Date - lag(Observation.Date), units="days"))

# pull out max gap during each interval (so by calf rather than mom)
MaxGap<-Gaps%>%
  group_by(Dolphin.Name)%>%
  slice_max(Gap)%>%
  mutate(dup = duplicated(Dolphin.Name)) %>% 
  filter(dup == FALSE)

# Don'nt run
# pull out only intervals with less than 3 yr gap between calf death and next calf birth, and mom has at least 5 sightings within this interval
Gaps.Less.Three<-MaxGap%>%
  filter(Gap<=1095.75)%>%
  filter(ObsCount>5)

# less than 2 yr gap between calf death and next calf birth, and mom has >5 sightings within the interval from calf death to next birth or !!! within >4 years from that death date -- is this accurate???
# 158 intervals
Gap.Two<-MaxGap%>%
  filter(Gap<=730) %>% #211 intervals
  filter(ObsCount>5) #160 intervals

################################################################################
#Gap.One<- MaxGap%>%
 # filter(Gap<=365.25) %>%
 # filter(ObsCount>5)

#Gap.Three<-MaxGap%>%
 # filter(Gap<=1095.75)%>%
  #filter(ObsCount>5) 



#write csv to cross reference moms in mortality sample
write.csv(Gap.Two, "./IntermediateData/interval-two.csv")

################################################################################

#101 moms
Mom2<-unique(Gap.Two$Mom.Name)


################################################################################
#look at intervals during covid - for moms who are event, ok under 2 year and 5 sightings restraint.
Covid <-MaxGap%>%
  filter(Tag == 1) %>%
  filter(!is.na(NextCalfName)) %>%
  filter(Gap<=730)%>%
  filter(ObsCount>5)

