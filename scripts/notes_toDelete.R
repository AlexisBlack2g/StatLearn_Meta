rma_age_DR <- rma.mv(g_calc, g_var_calc, mods = age.C, data = db_DR, random = ~ 1 | short_cite)
summary(rma_age_DR)

rma_age_DR2 <- rma.mv(g_calc, g_var_calc, mods = ~ age.C + factor(StimuliType), 
                      data = db_DR, random = ~ 1 | short_cite)
summary(rma_age_DR2)

rma_stimuli_DR=rma.mv(g_calc, g_var_calc, mods= ~factor(StimuliType), data=db_DR, random = ~ 1|short_cite)
summary(rma_stimuli_DR)

db$collapse <- paste(db$short_cite, db$expt_num, db$same_infant, sep = "_")

sub <- 
  db %>% 
  group_by(collapse) %>% 
  filter(n()>1) %>% 
  mutate(d_calc = median(d_calc),
         d_var_calc = median(d_var_calc),
         g_calc = median(g_calc),
         g_var_calc = median(g_var_calc),
         corr_imputed = median(corr_imputed)) %>% 
  filter(duplicated(collapse))


db <- anti_join(db, sub, by = "collapse")
db <- full_join(db, sub)

*****
  
  
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


####
dat <-
  db %>% 
  rename(cite = short_cite,
         yi = g_calc,
         vi = g_var_calc)  %>% 
  mutate(study_ref = paste(cite, expt_num, same_infant, sep=',')) %>% 
  arrange(desc(yi)) %>% 
  mutate(se = sqrt(vi),
         lowerci = (-1.96*se)+yi,
         upperci = (1.96*se)+yi,
         cue_conflict_TP = ifelse(cue_conflict_TP == "yes", "yes", "no"))

#blue = cue-conflict; red = no cue conflict

ggplot(dat, aes(y=reorder(study_ref, -yi), x=yi, xmin=lowerci, xmax=upperci))+
  #Add data points and color them black
  geom_point(aes(color = cue_conflict_TP))+
  #Add 'special' points for the summary estimates, by making them diamond shaped
  #  geom_point(data=subset(dat, tester=='Summary'), color='black', shape=18, size=4)+
  #add the CI error bars
  geom_errorbarh(height=.1)+
  #Specify the limits of the x-axis and relabel it to something more meaningful
  scale_x_continuous(limits=c(-1.5,1.5), name='Standardized Mean Difference (g)')+
  #Give y-axis a meaningful label
  ylab('Reference')+
  #Add a vertical dashed line indicating an effect size of zero, for reference
  geom_vline(xintercept=0, color='grey', linetype='dashed')+
  #Add a vertical line to show mean effect
  geom_vline(xintercept=0.19, color='black', linetype='dashed')+
  #Create sub-plots (i.e., facets) based on levels of setting
  #And allow them to have their own unique axes (so authors don't redundantly repeat)
  #facet_grid(setting~., scales= 'free', space='free')+
  #Apply my APA theme
  apatheme

ggplot(aes(x = se, y = yi), data = dat) +
  #Regression line for the FP asymmetry
  geom_smooth(data=subset(dat, StimuliType == "0"), 
              aes(x = se, y = yi), method = "lm", colour = "#F8766D", 
              alpha = .5, se = FALSE) +
  geom_smooth(data=subset(dat, StimuliType == "1"), 
              aes(x = se, y = yi), method = "lm", colour = "#00BFC4", 
              alpha = .5, se = FALSE) +
  #Add your data-points to the scatterplot
  geom_point(aes(size = 2.5, colour=StimuliType)) +
  #Give the x- and y- axes informative labels
  xlab('Standard Error') + ylab('Hedges\' g')+
  #Now using the 'dfCI' data-frame we created, plot dotted lines corresponding
  #to the lower and upper limits of your 95% CI region
  #And dashed lines corresponding to your 99% CI region
  #Add lines corresponding to 0 and estimate
  geom_line(aes(x = se.seq, y = 0), linetype = 'solid', data = dfCI) +
  geom_line(aes(x = se.seq, y = estimate), linetype = 'dashed', data = dfCI) +
  geom_line(aes(x = se.seq, y = ll95), linetype = 'dotted', data = dfCI) +
  geom_line(aes(x = se.seq, y = ul95), linetype = 'dotted', data = dfCI) +
  #  geom_line(aes(x = se.seq, y = ll99), linetype = 'dashed', data = dfCI) +
  #  geom_line(aes(x = se.seq, y = ul99), linetype = 'dashed', data = dfCI) +
  #Now plot dotted lines corresponding to the 95% CI of your meta-analytic estimate
  #geom_segment(aes(x = min(se.seq), y = meanll95, xend = max(se.seq), yend = meanll95), linetype='dotted', data=dfCI) +
  #geom_segment(aes(x = min(se.seq), y = meanul95, xend = max(se.seq), yend = meanul95), linetype='dotted', data=dfCI) +
  #Reverse the x-axis ordering (se) so that the tip of the funnel will appear
  #at the top of the figure once we swap the x- and y-axes...
  scale_x_reverse()+
  #Specify the range and interval for the tick-marks of the y-axis (Zr);
  #Choose values that work for you based on your data
  scale_y_continuous(breaks=seq(-.45,0.8,0.25))+
  #And now we flip the axes so that SE is on y- and Zr is on x-
  coord_flip()+
  #Finally, apply my APA-format theme (see code at end of post).
  #You could, alternatively, specify theme_bw() instead.
  apatheme

#age should matter for cue conflict but not for no cue conflict
#age interaction
#DR
ggplot(db_DR, aes(mean_age_1/30.44, g_calc))+
  geom_point(aes(color = StimuliType, shape = StimuliType), size=4) + 
  geom_line(y= 0, linetype="dotted") + 
  geom_smooth(data=subset(db_DR, StimuliType=="0"), method = "glm", color = "#D55E00") +
  geom_smooth(data=subset(db_DR, StimuliType=="1"), method = "glm", color = "#56B4E9") +
  xlab("Age in months") + 
  ylab("Effect size Hedges' g") +
  #scale_color_grey() +
  apatheme +
  theme(legend.position="top") +
  scale_colour_discrete(name  ="Stimuli Type",
                        breaks=c("0", "1"),
                        labels=c("Synthetic", "Natural")) +
  scale_shape_discrete(name  ="Stimuli Type",
                      breaks=c("0", "1"),
                      labels=c("Synthetic", "Natural"))
ggsave("test.tiff", units="in", width=6, height=4, dpi=300, compression = 'lzw')

#Whole dataset
dat2 <-
  dat %>% 
  mutate(cue_conflict_TP = ifelse(cue_conflict_TP=="yes", "yes", "no"))

ggplot(dat2, aes(mean_age_1/30.44, yi))+
  geom_point(aes(color = cue_conflict_TP, shape = cue_conflict_TP, size = weights_g+.2)) + 
  geom_line(y= 0, linetype="dotted") + 
  geom_smooth(data=subset(dat, cue_conflict_TP == "yes"), method = "glm", color = "#00BFC4") +
  geom_smooth(data=subset(dat, cue_conflict_TP == "no"), method = "glm", color = "#F8766D") +
  xlab("Age in months") + 
  ylab("Effect size Hedges' g") +
  #scale_color_grey() +
  apatheme +
  theme(legend.position="right") +
  scale_colour_discrete(name  ="Conflict\nwith\nTP cue?",
                        breaks=c("no", "yes"),
                        labels=c("No", "Yes")) +
  scale_size(name  = "Conflict\nwith\nTP cue?",
                       breaks=c("no", "yes"),
                       labels=c("No", "Yes")) +
  scale_shape_discrete(name  = "Conflict\nwith\nTP cue?",
                       breaks=c("no", "yes"),
                       labels=c("No", "Yes"))
ggsave("cueConflictXAge.tiff", units="in", width=6, height=4, dpi=300, compression = 'lzw')


#proportions
prop <-
  dat %>% 
  mutate(n_E = ifelse(familiarity=="novelty", proportion_preference*n_1,
                      n_1-(proportion_preference*n_1))) %>% 
  filter(n_E>0)

prop_rma <- rma(measure="PLO", xi=n_E, ni=n_1, data=prop)
summary(prop_rma)
predict(prop_rma, transf=transf.ilogit, digits=3)

ggplot(aes(x = se, y = n_E/n_1), data = prop) +
  #Regression line for the FP asymmetry
  geom_smooth(data=subset(prop, cue_conflict_TP == "no"), 
              aes(x = se, y = yi), method = "lm", colour = "#F8766D", 
              alpha = .5, se = FALSE) +
  geom_smooth(data=subset(prop, cue_conflict_TP == "yes"), 
              aes(x = se, y = yi), method = "lm", colour = "#00BFC4", 
              alpha = .5, se = FALSE) +
  #Add your data-points to the scatterplot
  geom_point(aes(size = 2.5, colour=cue_conflict_TP)) +
  #Give the x- and y- axes informative labels
  xlab('Standard Error') + ylab('prop')+
  #Now using the 'dfCI' data-frame we created, plot dotted lines corresponding
  #to the lower and upper limits of your 95% CI region
  #And dashed lines corresponding to your 99% CI region
  #Add lines corresponding to 0 and estimate
  geom_line(aes(x = se.seq, y = 0), linetype = 'solid', data = dfCI) +
  geom_line(aes(x = se.seq, y = estimate), linetype = 'dashed', data = dfCI) +
  geom_line(aes(x = se.seq, y = ll95), linetype = 'dotted', data = dfCI) +
  geom_line(aes(x = se.seq, y = ul95), linetype = 'dotted', data = dfCI) +
  #  geom_line(aes(x = se.seq, y = ll99), linetype = 'dashed', data = dfCI) +
  #  geom_line(aes(x = se.seq, y = ul99), linetype = 'dashed', data = dfCI) +
  #Now plot dotted lines corresponding to the 95% CI of your meta-analytic estimate
  #geom_segment(aes(x = min(se.seq), y = meanll95, xend = max(se.seq), yend = meanll95), linetype='dotted', data=dfCI) +
  #geom_segment(aes(x = min(se.seq), y = meanul95, xend = max(se.seq), yend = meanul95), linetype='dotted', data=dfCI) +
  #Reverse the x-axis ordering (se) so that the tip of the funnel will appear
  #at the top of the figure once we swap the x- and y-axes...
  scale_x_reverse()+
  #Specify the range and interval for the tick-marks of the y-axis (Zr);
  #Choose values that work for you based on your data
  scale_y_continuous(breaks=seq(-.45,0.8,0.25))+
  #And now we flip the axes so that SE is on y- and Zr is on x-
  coord_flip()+
  #Finally, apply my APA-format theme (see code at end of post).
  #You could, alternatively, specify theme_bw() instead.
  apatheme

## Stimuli Naturalness X Age, All
ggplot(db, aes(mean_age_1/30.44, g_calc))+
  geom_point(aes(color = StimuliType, shape = StimuliType), size=4) + 
  geom_line(y= 0, linetype="dotted") + 
  geom_smooth(data=subset(db, StimuliType == "1"), method = "glm", color = "#00BFC4") +
  geom_smooth(data=subset(db, StimuliType == "0"), method = "glm", color = "#F8766D") +
  xlab("Age in months") + 
  ylab("Effect size Hedges' g") +
  #scale_color_grey() +
  apatheme +
  theme(legend.position="top") +
  scale_colour_discrete(name  ="Stimuli Naturalness",
                        breaks=c("0", "1"),
                        labels=c("Synthetic", "Natural")) +
  scale_shape_discrete(name  = "Stimuli Naturalness",
                       breaks=c("0", "1"),
                       labels=c("Synthetic", "Natural"))
ggsave("StimuliXAge_All.tiff", units="in", width=6, height=4, dpi=300, compression = 'lzw')
