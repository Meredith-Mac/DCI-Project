setwd("C:/Users/mmacq/OneDrive/Documents/RStuff/SBDP/DCI-Project/DCI-Project/RawData")
library(survival)
library(survminer)
library(coxme)
library(ggplot2)
library(frailtySurv)
library(ggsurvfit)


#first, load in data
CalfDieBefore2<-read.csv("Final-sample.csv")

##################################################################################################### 
################################# *** !!! SELECTED MODEL !!! *** ###########################################################
Model<-coxme(Surv(TimeToEvent, EventCensored) ~ CalfAgeDeathCategories + MomAgeBirth + Season + (1 | Mother.ID), data = CalfDieBefore2)

print(Model)

ExSeason<-coxme(Surv(TimeToEvent, EventCensored) ~ CalfAgeDeathCategories + MomAgeBirth + (1 | Mother.ID), data = CalfDieBefore2)

print(ExSeason)
################################# !!! SELECTED PLOT !!!  ###########################################################  
KMPlot<-surv_fit(Surv(TimeToEvent, EventCensored) ~ CalfAgeDeathCategories, data = CalfDieBefore2)

ggsurvplot(KMPlot, data = CalfDieBefore2, fun = "event", xlab = "Time (years)", ylab = "Completion probability", font.main = c(16, "plain", "black"),
           font.x = c(14, "plain", "black"), font.y = c(14, "plain", "black"),
           font.tickslab = c(12, "plain", "black"),  legend = c("top", "bottom", "left", "right", "none"),
           legend.title = "Strata", font.legend = c(10,"plain", "black")) 

### classic, clean looking plot
OGPlot<- ggsurvplot(KMPlot, data = CalfDieBefore2, fun = "event", color = NULL, palette = "hue",
break.time.by = 1, surv.scale = c("default", "percent"),
conf.int = FALSE, conf.int.fill = "gray", censor = TRUE, pval = FALSE,
pval.size = 5, pval.coord = c(NULL, NULL), main = NULL, xlab = "Time (years)",
ylab = "Completion probability", font.main = c(16, "plain", "black"),
font.x = c(16, "plain", "black"), font.y = c(16, "plain", "black"),
font.tickslab = c(10, "plain", "black"), xlim = NULL, ylim = NULL,
legend = c(0.8, 0.3), legend.title = "Calf age at death:", 
legend.labs = c("under 3 months", "3-12 months", "12-24 months"), 
font.legend = c(10,"plain", "black"), 
ggtheme = ggplot2::theme_classic())


#interesting to see risk table
p<-survfit2(Surv(TimeToEvent, EventCensored) ~ CalfAgeDeathCategories, data = CalfDieBefore2) %>%
ggsurvfit(type="cumhaz") +
  add_risktable()
       
   Pretty <- p + labs(x = "Time (years)", y = "Completion probability")+
      add_legend_title("Calf age at death:") +
      scale_color_manual(values = c('red', 'green', 'blue'),
                         labels = c('under 3 months', '3-12 months', '12-24 months')) +
      scale_fill_manual(values = c('red', 'green', 'blue'),
                        labels = c('under 3 months', '3-12 months', '12-24 months'))+
      scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))+
      add_risktable_strata_symbol(symbol = "\U25CF", size = 10)+
      theme_classic()+
      theme(legend.position = c(0.8, 0.3))
               
    ggsave(plot = Pretty, file = "KM-with-Risk.pdf", device = "pdf", width = 5, 
           height = 6, units = "in", dpi = 600)       
    
###############################################################################
# plot season effect
           
SeasonPlot<-surv_fit(Surv(TimeToEvent, EventCensored) ~ Season, data = CalfDieBefore2)          
ggsurvplot(SeasonPlot, data = CalfDieBefore2, fun = "event", color = NULL, palette = "hue",
           break.time.by = 1, surv.scale = c("default", "percent"),
           conf.int = FALSE, conf.int.fill = "gray", censor = TRUE, pval = FALSE,
           pval.size = 5, pval.coord = c(NULL, NULL), main = NULL, xlab = "Time (years)",
           ylab = "Completion probability", font.main = c(16, "plain", "black"),
           font.x = c(16, "plain", "black"), font.y = c(16, "plain", "black"),
           font.tickslab = c(10, "plain", "black"), xlim = NULL, ylim = NULL,
           legend = c(0.8, 0.3), legend.title = "Calf age at death:", 
           legend.labs = c("in-season morality", "out-of-season mortality"), 
           font.legend = c(10,"plain", "black"), 
           ggtheme = ggplot2::theme_classic())
           
