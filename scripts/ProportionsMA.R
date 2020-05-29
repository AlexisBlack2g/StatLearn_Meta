#General set up:

library(tidyverse)
library(metafor)

#load and prep the data
source("scripts/calculateES.R")
db <-
  db %>% 
  mutate(StimuliType = ifelse(test_lang_program=="natural", "1", "0")) %>% 
  mutate(StimuliType = as.factor(StimuliType)) %>% 
  mutate(age.C = mean_age_1-mean(mean_age_1, na.rm=TRUE)) %>% 
  mutate(familiarization = familiarization_time-mean(familiarization_time, na.rm=TRUE)) %>% 
  rename(gender = gender_1)
db$collapse <- paste(db$study_ID, db$expt_num, db$same_infant, sep = "_")

for(independent in unique(db$collapse)){
  if(length(db[db$collapse==independent])>1){
    sub = db[db$collapse==independent, ]
    sub$d_calc <- median(sub$d_calc)
    sub$d_var_calc <- median(sub$d_var_calc)
    sub$g_calc <- median(sub$g_calc)
    sub$g_var_calc <- median(sub$g_var_calc)
    sub$corr_imputed <- median(sub$corr_imputed)
    db <- db[!(db$collapse==independent),]
    db <- rbind(db, sub[1,])
  }
}

dat <-
  db %>% 
  rename(cite = short_cite,
         yi = g_calc,
         vi = g_var_calc)
dat <-
  dat %>% 
  mutate(study_ref = paste(cite, expt_num, same_infant, sep=',')) %>% 
  arrange(desc(yi))
dat$se = sqrt(dat$vi)
dat$lowerci = (-1.96*dat$se)+dat$yi
dat$upperci = (1.96*dat$se)+dat$yi

#To look at data using proportions instead of mean difference effect sizes. This
#analysis uses the looking time data as a binary outcome measure: you either
#prefer the novel or you prefer the familiar items, but essentially, the *degree*
#to which you prefer these items is uninterpretable

#Taken to an extreme, we could imagine a situation where each infant looked at 
#novel items 100 msec longer than familiar items; if all infants do so, it's a 
#robust effect.  It will, in fact, be equally robust across both analyses, 
#because the mean difference effect size depends on the sample variance.  These 
#analyses would differ, however, if the longer looking times were varied, but 
#most infants did look longer to the familiar items.  In this case, the
#proportional analysis would yeild a stronger effect than the mean difference
#analysis

#First we create the variables we need. The data has been coded such that the
#proportion preference reflects the direction of preference reported in the
#study.  We need for these to reflect the different direction of preference
#scores.

prop <-
  dat %>% 
  mutate(n_E = ifelse(familiarity=="novelty", proportion_preference*n_1,
                      n_1-(proportion_preference*n_1))) %>% 
  filter(n_E>0)

#fix the missing values in the cue_conflict column for later
#prop$cue_conflict_TP[prop$cue_conflict_TP == ""] <- "no"

#Calculate Proportion based effect size
prop <- escalc(measure="PR", xi=n_E, ni=n_1, data=prop)
prop$se = sqrt(prop$vi)
prop$lowerci = (-1.96*prop$se)+prop$yi
prop$upperci = (1.96*prop$se)+prop$yi


#The meta-analysis function from metafor can now perform the necessary calculations
#We use the binomial-normal model, instead of normal-normal (see Stijnen et al., 2010)

prop_rma <- rma.glmm(measure="PLO", xi=as.integer(n_E), ni=n_1, data=prop) #this yields 
#a warning, need to modify class of xi to integer
summary(prop_rma)
predict(prop_rma, transf=transf.ilogit, digits=3)

#In other words, there is a 56% incidence of a novelty preference in SL tasks;
#i.e. 6% above and beyond what you would expect by chance alone.  This is the
#more conservative estimate, given the error produced by the binomial-normal
#model (but the values produced are nearly identical).

#The Forest Plot
#prop <-
#  prop %>% 
#  select(cite, expt_num, same_infant, mean_age_1, yi, vi, StimuliType) %>% 
#  arrange(desc(yi))

ggplot(prop, aes(y=reorder(cite, -yi), x=yi, xmin=lowerci, xmax=upperci))+
  geom_point(aes(color = StimuliType))+
  geom_errorbarh(height=.1, aes(colour = StimuliType))+
  scale_x_continuous(limits=c(-.1,1.1), name='Proportion')+
  ylab('Reference')+
  geom_vline(xintercept=0, color='grey', linetype='dashed')+
  geom_vline(xintercept=0.56, color='black', linetype='dashed')

#We can also do this with the conceptual replications alone
prop_DR_rma <-rma(measure = "PLO", xi = as.integer(n_E), ni = n_1, 
                  data = subset(prop, DirectRep == "yes"))
summary(prop_DR_rma)
predict(prop_DR_rma, transf=transf.ilogit, digits=3)

ggplot(data = subset(prop, DirectRep == "yes"),
      aes(y=reorder(study_ref, -yi), x=yi, xmin=lowerci, xmax=upperci))+
  geom_point(aes(colour = StimuliType))+
  geom_errorbarh(height=.1, aes(colour = StimuliType))+
  scale_x_continuous(limits=c(-.1,1.1), name='Proportion')+
  ylab('Reference')+
  geom_vline(xintercept=0, color='grey', linetype='dashed')+
  geom_vline(xintercept=0.54, color='black', linetype='dashed')

#Interestingly, the log odds estimate is not significant

#moderators?
prop_rma_Age <-rma.glmm(measure="PLO", xi=as.integer(n_E), ni=n_1, mods = age.C,
                    data=prop)
prop_rma_ST <-rma.glmm(measure="PLO", xi=as.integer(n_E), ni=n_1, mods = StimuliType,  #doesn't work, not sure why
               data=prop)
prop_rma_CC <-rma.glmm(measure="PLO", xi=as.integer(n_E), ni=n_1, mods = cue_conflict_TP,  #doesn't work
               data=prop)

#well, graphically then
ggplot(prop, aes(mean_age_1/30.44, yi, color = StimuliType))+
  geom_point() + 
  geom_line(y= 0, linetype="dotted") + 
  geom_smooth(method = "glm") + 
  xlab("Age in months") + 
  ylab("Effect size") 
         
ggplot(prop, aes(mean_age_1/30.44, yi, color = cue_conflict_TP))+
  geom_point() + 
  geom_line(y= 0, linetype="dotted") + 
  geom_smooth(method = "glm") + 
  xlab("Age in months") + 
  ylab("Effect size") 

