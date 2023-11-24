# Reproduces results from analysis of Survey 1 responses in "Expert and lay judgments of danger and recklessness 
# in adventure sports", Ebert and Durbach, J of Risk Research (2022).

library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(emmeans)
library(stringr)
library(ks)
library(DescTools)
library(effsize)
library(readxl)

load("output/combined-expert-layperson-responses-clean.Rdata")

# (1) initial cleaning

## Remove fastest responses
# xl_all <- xl_all %>% filter((sgroup == "outgroup")|(duration>672))

## recode for Poisson GLM
xl_all <- xl_all %>% mutate(danger = danger - 1, recklessness = recklessness - 1)

# (2) Analysis 1: effect of expertise

## model, danger response
min_conf_danger <- 0 # increase if want to filter by confidence, 0 for all responses
aov_d <- glm(danger ~ condition * sgroup + condition * resp_gender + conf_danger, data = xl_all %>% filter(conf_danger > min_conf_danger), family = "poisson")
qqnorm(residuals(aov_d)); abline(a=0,b=1)
summary(aov_d)

## model contrasts we want, averaged over resp_gender
custom <- list(`BaseIn-BaseOut` = c(1,0,0,0,-1,0,0,0),
               `BaseIn-CharityIn` = c(1,-1,0,0,0,0,0,0),
               `BaseOut-CharityOut` = c(0,0,0,0,1,-1,0,0),
               `BaseIn-ExRiskIn` = c(1,0,-1,0,0,0,0,0),
               `BaseOut-ExRiskOut` = c(0,0,0,0,1,0,-1,0),
               `BaseIn-IncompIn` = c(1,0,0,-1,0,0,0,0),
               `BaseOut-IncompOut` = c(0,0,0,0,1,0,0,-1))
emm_d <- emmeans(aov_d, specs = c("condition", "sgroup"), contr = custom, adjust = "tukey", type = "response", bias.adj = FALSE)
emm_d$emmeans
emm_d$contrasts

## model, recklessness response
min_conf_reck <- 0 # increase if want to filter by confidence, 0 for all responses
aov_r <- glm(recklessness ~  condition * sgroup + condition * resp_gender + sgroup * conf_reck, data = xl_all %>% filter(conf_danger > min_conf_danger), family = "poisson")
qqnorm(residuals(aov_r)); abline(a=0,b=1)
summary(aov_r)

## model contrasts we want, averaged over resp_gender
custom <- list(`BaseIn-BaseOut` = c(1,0,0,0,-1,0,0,0),
               `BaseIn-CharityIn` = c(1,-1,0,0,0,0,0,0),
               `BaseOut-CharityOut` = c(0,0,0,0,1,-1,0,0),
               `BaseIn-ExRiskIn` = c(1,0,-1,0,0,0,0,0),
               `BaseOut-ExRiskOut` = c(0,0,0,0,1,0,-1,0),
               `BaseIn-IncompIn` = c(1,0,0,-1,0,0,0,0),
               `BaseOut-IncompOut` = c(0,0,0,0,1,0,0,-1))
emm_r <- emmeans(aov_r,  specs = c("condition", "sgroup"), contr = custom, adjust = "tukey", type = "response", bias.adj = FALSE)
emm_r$emmeans
emm_r$contrasts

## effect sizes
eff_size(emm_d, sigma = sigma(aov_d), edf = 634)
eff_size(emm_r, sigma = sigma(aov_r), edf = 634)

## model means plots
danger_model_means <- emm_d$emmeans %>% data.frame() %>% mutate(name = "Danger")
reck_model_means <- emm_r$emmeans %>% data.frame() %>% mutate(name = "Recklessness")
model_means <- rbind(danger_model_means, reck_model_means)
pd <- position_dodge(0.5)
model_means <- model_means %>% dplyr::rename(response = rate, lower.CL = asymp.LCL, upper.CL = asymp.UCL)
p_inout_mm <- model_means %>% 
  mutate(sgroup = factor(sgroup, levels = c("outgroup", "ingroup"), labels = c("No experience", "Experienced"))) %>%
  ggplot(aes(x = sgroup, y = response+1, colour = condition)) + 
  facet_grid(.~name) +
  geom_point(size = 3, position = pd) +
  geom_linerange(aes(ymin = lower.CL+1, ymax = upper.CL+1), position = pd) +
  ylab("Rating") + xlab("") + 
  scale_colour_brewer(palette = "Set2") + theme_bw(base_size = 12) + theme(panel.grid = element_blank()) +
  coord_cartesian(ylim = c(min(model_means$lower.CL)+1,max(model_means$upper.CL)+1)) +
  theme(legend.position="bottom", legend.margin=margin(0,0,0,0), legend.box.margin=margin(-20,0,0,0), legend.title = element_blank())
p_inout_mm

ggsave("output/inoutgroup-comparison.png", p_inout_mm, width=11, height=3.5, dpi = 300)