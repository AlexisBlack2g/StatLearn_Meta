apatheme=theme_bw()+
  theme(#panel.grid.major=element_blank(),
    #panel.grid.minor=element_blank(),
    #panel.border=element_blank(),
    axis.line = element_line(),
    text = element_text(family = 'Times', size = 14),
    #axis.title.x = element_text(family = 'Times', size = 14),
    legend.position = 'none')


### This code is adapted from: https://sakaluk.wordpress.com/2016/02/16/7-make-it-pretty-plots-for-met#a-analysis/
#Store the meta-analytic estimate and its standard error from whatever model you run (substitute your own values)
#rma_DR
estimate = x$estimate
se = x$se

#Store a vector of values that spans the range from 0
#to the max value of impression (standard error) in your dataset.
#Make the increment (the final value) small enough (I choose 0.001)
#to ensure your whole range of data is captured
se.seq=seq(0, max(db_DR$se), 0.001)

#Now, compute vectors of the lower-limit and upper limit values for
#the 95% CI region, using the range of SE that you generated in the previous step, 
#and the stored value of your meta-analytic estimate.
ll95 = estimate-(1.96*se.seq)
ul95 = estimate+(1.96*se.seq)

#You can do this for a 99% CI region too
ll99 = estimate-(3.29*se.seq)
ul99 = estimate+(3.29*se.seq)

#And finally, do the same thing except now calculating the confidence interval
#for your meta-analytic estimate based on the stored value of its standard error
meanll95 = estimate-(1.96*se)
meanul95 = estimate+(1.96*se)

#Now, smash all of those calculated values into one data frame (called 'dfCI').
dfCI = data.frame(ll95, ul95, ll99, ul99, se.seq, estimate, meanll95, meanul95)

library(ggrepel)
fp <- ggplot(aes(x = se, y = g_calc), data = db_DR) +
  geom_smooth(aes(x = se, y = g_calc), method = "lm", colour = "darkgrey", alpha = .5, se = FALSE) +
  xlab('Standard Error') + ylab('Hedges\' g')+
  geom_line(aes(x = se.seq, y = 0), linetype = 'solid', data = dfCI) +
  geom_line(aes(x = se.seq, y = estimate), linetype = 'dashed', data = dfCI) +
  geom_line(aes(x = se.seq, y = ll95), linetype = 'dotted', data = dfCI) +
  geom_line(aes(x = se.seq, y = ul95), linetype = 'dotted', data = dfCI) +
  scale_x_reverse()+
  scale_y_continuous(breaks=seq(-.45,0.8,0.25))+
  coord_flip()+
 # geom_label_repel(aes(label=short_cite), size = 3, box.padding = .25, point.padding = .25, colour = "darkgrey") +
  geom_point(size = 3.5, alpha = .3) +
  apatheme
 
age <- ggplot(aes(x = mean_age_months, y = g_calc), data = db_DR) +
  geom_point() +
  geom_smooth(method = lm, colour = "darkgrey", alpha = .5) +
  apatheme +
  ylab("Hedges' g") +
  xlab("Mean age in months")

library(cowplot)
DR_fp_age <- plot_grid(fp,age, ncol = 1)

#Word length plot, Plot db_DR, 
rma_DR_modUnif_Intercept <- rma.mv(g_calc, g_var_calc, data = db_DR, mods = ~0 + uniformity, 
                         random = ~1| short_cite/same_infant, method = "REML")
xUnif <- (coef(summary(rma_DR_modUnif_Intercept)))
xUnif$uniformity <-c("no", "yes")
xUnif$g_calc = xUnif$estimate

unif <- ggplot(db_DR, aes(x = uniformity, y = g_calc, colour = uniformity)) +
  #geom_dotplot(binaxis = "y", stackdir = "center", alpha = .3, aes(dotsize = 1/g_var_calc)) +
  geom_violin(scale = "count") +
  geom_jitter(aes(size = 1/g_var_calc), alpha = .25, width = .25) +
  geom_abline(intercept = 0, slope = 0, lty = 3) +
  apatheme +
  geom_point(data = xUnif, aes(x=uniformity, y = g_calc, shape = uniformity), 
             size=6, position=position_dodge(width=0.9)) +
  geom_errorbar(data = xUnif, aes(ymin = ci.lb, ymax = ci.ub, x = uniformity), 
                width=.05, position=position_dodge(width=0.9)) +
  xlab("Uniform word length") +
  ylab("Effect Size (Hedges' g)") +
  scale_x_discrete(labels = c("No", "Yes")) +
  theme(plot.margin = margin(4,2,2,2))



#Stimulus Plot db_DR, fig.cap = "Statistical learning as a function stimulus construction type (conceptual replications)"}
xRep <- (coef(summary(rma_DR_modStim_Intercept)))
xRep$StimuliType <-c("0", "1")
xRep$g_calc = xRep$estimate

stim <- ggplot(db_DR, aes(x = StimuliType, y = g_calc, colour = StimuliType)) +
  #geom_dotplot(binaxis = "y", stackdir = "center", alpha = .3, aes(dotsize = 1/g_var_calc)) +
  geom_violin(scale = "count") +
  geom_jitter(aes(size = 1/g_var_calc), alpha = .25, width = .25) +
  geom_abline(intercept = 0, slope = 0, lty = 3) +
  apatheme +
  geom_point(data = xRep, aes(x=StimuliType, y = g_calc, shape = StimuliType), 
             size=6, position=position_dodge(width=0.9)) +
  geom_errorbar(data = xRep, aes(ymin = ci.lb, ymax = ci.ub, x = StimuliType), 
                width=.05, position=position_dodge(width=0.9)) +
  xlab("Stimulus Type") +
  ylab("") +
  scale_x_discrete(labels = c("Synthetic", "Natural")) +
  theme(plot.margin = margin(4,2,2,2))


library(cowplot)
xoff <- 0.025# relative x position of label, within one plot
yoff <- 0.965 # relative y position of label, within one plot

bottom_row <- plot_grid(unif, stim) +
  draw_label(label="B",x=.015,y=.975, fontfamily = "serif", fontface = "bold", size = 18) +
  draw_label(label="C",x=xoff+.5,y=.975, fontfamily = "serif", fontface = "bold", size = 18)

DR_plots <- 
plot_grid(fp, NULL, bottom_row, ncol = 1, rel_heights = c(1.5,.05, 1)) + 
  draw_label(label = "A",
             x = .015, y = .975,
             fontfamily = "serif",
             fontface = "bold",
             size = 18)

save_plot("figures/DR_plots.pdf", DR_plots, base_height = 8, base_width = 7.5)
###
##Figure 3

### This code is from: https://sakaluk.wordpress.com/2016/02/16/7-make-it-pretty-plots-for-met#a-analysis/
#Store the meta-analytic estimate and its standard error from whatever model you run (substitute your own values)
rma_all <- rma.mv(g_calc, g_var_calc, data=db, random = ~1 | short_cite/same_infant, method = "REML")
xAll <- xtable(coef(summary(rma_all)))
estimate = xAll$estimate
se = xAll$se

#Store a vector of values that spans the range from 0
#to the max value of impression (standard error) in your dataset.
#Make the increment (the final value) small enough (I choose 0.001)
#to ensure your whole range of data is captured
se.seq=seq(0, max(db$se), 0.001)

#Now, compute vectors of the lower-limit and upper limit values for
#the 95% CI region, using the range of SE that you generated in the previous step, 
#and the stored value of your meta-analytic estimate.
ll95 = estimate-(1.96*se.seq)
ul95 = estimate+(1.96*se.seq)

#You can do this for a 99% CI region too
ll99 = estimate-(3.29*se.seq)
ul99 = estimate+(3.29*se.seq)

#And finally, do the same thing except now calculating the confidence interval
#for your meta-analytic estimate based on the stored value of its standard error
meanll95 = estimate-(1.96*se)
meanul95 = estimate+(1.96*se)

#Now, smash all of those calculated values into one data frame (called 'dfCI').
#You might get a warning about '...row names were found from a short variable...'
#You can ignore it.
dfCI = data.frame(ll95, ul95, ll99, ul99, se.seq, estimate, meanll95, meanul95)

fp2 <- ggplot(aes(x = se, y = g_calc), data = db) +
  geom_smooth(aes(x = se, y = g_calc), method = "lm", colour = "darkgrey", alpha = .5, se = FALSE) +
  xlab('Standard Error') + ylab('Hedges\' g')+
  geom_line(aes(x = se.seq, y = 0), linetype = 'solid', data = dfCI) +
  geom_line(aes(x = se.seq, y = estimate), linetype = 'dashed', data = dfCI) +
  geom_line(aes(x = se.seq, y = ll95), linetype = 'dotted', data = dfCI) +
  geom_line(aes(x = se.seq, y = ul95), linetype = 'dotted', data = dfCI) +
  scale_x_reverse()+
  scale_y_continuous(breaks=seq(-.45,0.8,0.25))+
  coord_flip()+
  #geom_label_repel(aes(label=same_infant_calc), size = 2, box.padding = 0, point.padding = 0) +
  geom_point(size = 3.5, alpha = .3) +
  apatheme

age2 <- ggplot(aes(x = mean_age_months, y = g_calc), data = db) +
  geom_point() +
  geom_smooth(method = lm, colour = "darkgrey", alpha = .5, fill = "lightgrey") +
  apatheme +
  ylab("Hedges' g") +
  xlab("Mean age in months")

FP2_Age2 <- plot_grid(fp2,age2, ncol = 1)

##Figure 5
#Panel A

rma_3way <- rma.mv(g_calc, g_var_calc, mods = ~0 + ThreeWayStim, data = db, random = ~1| short_cite/same_infant)
#summary(rma_3way)

x3 <- (coef(summary(rma_3way)))
x3$ThreeWayStim <-c("0", "1", "2")
x3$g_calc = x3$estimate

ThreeWayFig <- ggplot(db, aes(x = ThreeWayStim, y = g_calc, colour = ThreeWayStim)) +
  #geom_dotplot(binaxis = "y", stackdir = "center", alpha = .3, aes(dotsize = 1/g_var_calc)) +
  geom_violin(scale = "count") +
  geom_jitter(aes(size = 1/g_var_calc), alpha = .25, width = .25) +
  geom_abline(intercept = 0, slope = 0, lty = 3) +
  apatheme +
  geom_point(data = x3, aes(x=ThreeWayStim, y = g_calc, shape = ThreeWayStim), 
             size=6, position=position_dodge(width=0.9)) +
  geom_errorbar(data = x3, aes(ymin = ci.lb, ymax = ci.ub, x = ThreeWayStim), 
                width=.05, position=position_dodge(width=0.9)) +
  xlab("Stimulus Type") +
  ylab("Effect Size (Hedges' g)") +
  scale_x_discrete(labels = c("Synthetic", "Non-fluent Natural", "Fluent Natural")) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

#Panel B
ThreeWayTable <- round(coef(summary(rma_3way)), digits = 2)
ThreeWayTable$`95% CI` <- paste('[', round(ThreeWayTable$ci.lb, digits = 2), ', ', 
                              round(ThreeWayTable$ci.ub, digits = 2), ']')
row.names(ThreeWayTable) <- c("Synthetic", "Non-fluent natural", "Fluent natural")
ThreeWayTable <- ThreeWayTable %>% select(estimate, `95% CI`, zval, pval)

library(gridExtra)

ThreeWayTable <- tableGrob(ThreeWayTable, theme = ttheme_minimal(base_family = 'Times'))

ThreeWayFigPanels <- ggdraw(ThreeWayFig) + draw_grob(ThreeWayTable, x = .45, y = .75, width = .3, height = .3) 
save_plot("figures/ThreeWayFigPanels.pdf", ThreeWayFigPanels, base_height = 5, base_asp = 2.1)


##Figure6
rma_StimCoart = rma.mv(g_calc, g_var_calc, mods = ~0+StimuliType, 
                       data = subset(db, fam_lang_coarticulation == "yes"), 
                       random = ~1| short_cite/same_infant)

x4 <- (coef(summary(rma_StimCoart)))
x4$StimuliType <-c("0", "1")
x4$g_calc = x4$estimate

StimCoartFig <- ggplot(subset(db, fam_lang_coarticulation == "yes"), 
                    aes(x = StimuliType, y = g_calc, colour = StimuliType)) +
  geom_violin(scale = "count") +
  geom_jitter(aes(size = 1/g_var_calc), alpha = .25, width = .25) +
  geom_abline(intercept = 0, slope = 0, lty = 3) +
  apatheme +
  geom_point(data = x4, aes(x=StimuliType, y = g_calc, shape = StimuliType), 
             size=6, position=position_dodge(width=0.9)) +
  geom_errorbar(data = x4, aes(ymin = ci.lb, ymax = ci.ub, x = StimuliType), 
                width=.05, position=position_dodge(width=0.9)) +
  xlab("Stimulus Type") +
  ylab("Effect Size (Hedges' g)") +
  scale_x_discrete(labels = c("Coarticulated Synthetic", "Coarticulated Natural")) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 


CoartTable <- round(coef(summary(rma_StimCoart)), digits = 2)
CoartTable$`95% CI` <- paste('[', round(CoartTable$ci.lb, digits = 2), ', ', 
                              round(CoartTable$ci.ub, digits = 2), ']')
rownames(CoartTable) <- c("Synthetic", "Natural")

CoartTable <- CoartTable %>% select(estimate, `95% CI`, zval, pval)
CoartTable <- tableGrob(CoartTable, theme = ttheme_default(base_family = 'Times'))
CoartPanels <- ggdraw(StimCoartFig, xlim = c(0,1), ylim = c(0,1.17)) + 
  draw_grob(CoartTable, x = .35, y = 0.91, width = .3, height = .3)
save_plot("figures/CoartFigPanels.pdf", CoartPanels, base_height = 5)


