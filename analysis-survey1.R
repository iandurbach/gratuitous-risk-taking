# Reproduces results from analysis of Survey 1 responses in "Differences in expert and layperson’s danger and recklessness judgments 
# about adventure sports participation" by Philip Ebert and Ian Durbach (2021).
# Last update 03.12.2021

library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(emmeans)
library(stringr)
library(ks)
library(DescTools)
library(effsize)
library(mgcv)

data <- readRDS("output/sportrisks_cleaned.Rds")

# renaming factor levels to descriptive labels
data$Q3 <- factor(data$Q3, levels = c(1,2), labels = c("female", "male"))
data <- data %>% mutate(Q4c = case_when(Q4 == 1 ~ 21, Q4 == 2 ~ 29.5, Q4 == 3 ~ 39.5, Q4 == 4 ~ 49.5, Q4 == 5 ~ 59.5 ,Q4 == 6 ~ 65))
data$Q4 <- factor(data$Q4, levels = c(1,2,3,4,5,6), labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"))
data$sport <- factor(data$sport, levels = c("golf", "running", "hillwalking", "surfing", "skitouring", "climbing"))
data$dependents <- factor(data$dependents, levels = c(FALSE, TRUE), labels = c("No dependents" ,"Has dependents"))
data$charity <- factor(data$charity, levels = c(FALSE, TRUE), labels = c("For self" ,"For charity"))
data$low_competence <- factor(data$low_competence, levels = c(TRUE, FALSE), labels = c("Low competence" ,"High competence"))
data$guide <- factor(data$guide, levels = c(FALSE, TRUE), labels = c("No guide" ,"Has guide"))
data$extreme_risk <- factor(data$extreme_risk, levels = c(FALSE, TRUE), labels = c("Baseline risk" ,"Extreme risk"))

# note: Q8_1: golf; Q8_2: skitouring; Q8_3: rock climbing; Q8_4: big-wave surfing; Q8_5: hill-walking; Q8_6: running; Q8_7: prefer not to say

# construct helper variables
data <- data %>% mutate(does_risky = factor(pmax(Q8_2, Q8_3, Q8_4), levels = c(0,1), labels = c("no", "yes")),
                        extreme_sport = as.character(sport)) %>% 
  mutate_at(vars(Q8_1:Q8_7), as.factor)

data <- data %>% mutate(extreme_sport = ifelse(sport %in% c("climbing", "surfing", "skitouring"), "extreme sport", "other sport")) %>%
  mutate(part_gender = factor(Q3, levels = c("female", "male"), labels = c("Female respondents", "Male respondents"))) %>%
  mutate(vig_gender = factor(gender, levels = c("female", "male"), labels = c("Female in vignette", "Male in vignette"))) %>%
  mutate(compguide = ifelse(guide == "Has guide", "Low comp., guided", ifelse(low_competence == "High competence", "High comp., unguided", "Low comp., unguided"))) %>%
  mutate(compguide = factor(compguide, levels = c("Low comp., unguided", "Low comp., guided", "High comp., unguided"))) %>%
  mutate(owngender = (Q3 == gender)) %>%
  mutate(owngender = factor(owngender, levels = c(TRUE, FALSE), labels = c("Same gender", "Different gender"))) %>%
  mutate(do_sport = case_when(
    sport == "golf" & Q8_1 == 1 ~ TRUE,
    sport == "skitouring" & Q8_2 == 1 ~ TRUE,
    sport == "climbing" & Q8_3 ==  1 ~ TRUE,
    sport == "surfing" & Q8_4 == 1 ~ TRUE,
    sport == "hillwalking" & Q8_5 == 1 ~ TRUE,
    sport == "running" & Q8_6 == 1 ~ TRUE,
    TRUE ~ FALSE)) 

# this removes any danger and recklessness ratings associated with low confidence (1,2)
# run this line to redo the analysis with low-confidence ratings excluded (removes 201 for D, 169 for R)
# data <- data %>% mutate(Q5_1 = ifelse(Q5_a_1 < 3, NA, Q5_1), Q6_1 = ifelse(Q6_a_1 < 3, NA, Q6_1))

# recode for Poisson GLMs
data <- data %>% mutate(Q5_1o = Q5_1, Q6_1o = Q6_1, Q5_1 = Q5_1 - 1, Q6_1 = Q6_1 - 1)
# NOTE! ALL MODEL MEANS THEREFORE NEED TO HAVE 1 ADDED BACK TO THEM TO GET BACK TO 1-9 SCALE

### Hypothesis tests, model building, plots

pd <- position_dodge(0.5)

######################
# Plot a: Extreme sports are generally regarded as more dangerous and reckless to engage in 
# than more mundane activities even when fatality rates are the same.
######################

# hold other variables fixed at baseline levels
h1data <- data %>% filter(dependents == "No dependents", charity == "For self", low_competence == "High competence", 
                          guide == "No guide", extreme_risk == "Baseline risk") 

# model
mod_h1d <- glm(Q5_1 ~ sport + vig_gender, data = h1data, family = "poisson")
qqnorm(residuals(mod_h1d)); abline(a=0,b=1)
summary(mod_h1d)

# same on recklessness
mod_h1r <- glm(Q6_1 ~ sport + vig_gender, data = h1data, family = "poisson")
qqnorm(residuals(mod_h1r)); abline(a=0,b=1)
summary(mod_h1r)

## results reported in paper

# marginal effect of sport
mod_h1d_null <- glm(Q5_1 ~ vig_gender, data = h1data, family = "poisson")
anova(mod_h1d_null, mod_h1d, test="LRT")
mod_h1r_null <- glm(Q6_1 ~ vig_gender, data = h1data, family = "poisson")
anova(mod_h1r_null, mod_h1r, test="LRT")

# model means and posthoc pairwise comparisons of sports, appears in text and plots
emm_d <- emmeans(mod_h1d, pairwise ~ sport, adjust = "tukey", type = "response", bias.adj = FALSE)
emm_r <- emmeans(mod_h1r, pairwise ~ sport, adjust = "tukey", type = "response", bias.adj = FALSE)
emm_d
emm_r

# effect of experience on ratings
mod_h1d_ownsport_main <- glm(Q5_1 ~ sport + do_sport + vig_gender, data = h1data, family = "poisson")
mod_h1d_ownsport_int <- glm(Q5_1 ~ sport * do_sport + vig_gender, data = h1data, family = "poisson")
anova(mod_h1d, mod_h1d_ownsport_main, test="LRT")
anova(mod_h1d, mod_h1d_ownsport_int, test="LRT")

mod_h1r_ownsport_main <- glm(Q6_1 ~ sport + do_sport + vig_gender, data = h1data, family = "poisson")
mod_h1r_ownsport_int <- glm(Q6_1 ~ sport * do_sport + vig_gender, data = h1data, family = "poisson")
anova(mod_h1r, mod_h1r_ownsport_main, test="LRT")
anova(mod_h1r, mod_h1r_ownsport_int, test="LRT")

# process plot for main figure
danger_model_means <- emm_d$emmeans %>% data.frame() %>% mutate(name = "danger")
reck_model_means <- emm_r$emmeans %>% data.frame() %>% mutate(name = "recklessness")
model_means <- rbind(danger_model_means, reck_model_means)
pd <- position_dodge(0.5)
p_h1_mm <- model_means %>% rename(response = rate, lower.CL = asymp.LCL, upper.CL = asymp.UCL) %>%
  ggplot(aes(x = sport, y = response + 1, colour = name)) + 
  geom_point(size = 3, position = pd) +
  geom_linerange(aes(ymin = lower.CL + 1, ymax = upper.CL + 1), position = pd) +
  ylab("Rating") + xlab("") + 
  scale_colour_brewer(palette = "Set2") + theme_bw(base_size = 12) + theme(panel.grid = element_blank()) +
  labs(subtitle = "a (vignette subset A)") +
  theme(legend.position="bottom", legend.margin=margin(0,0,0,0), legend.box.margin=margin(-20,0,0,0), legend.title = element_blank())
p_h1_mm

# additional checks -- ordinal regression 
omod_h1d <- gam(Q5_1o ~ sport + vig_gender, data = h1data, family = ocat(R=9))
omod_h1d_null <- gam(Q5_1o ~ vig_gender, data = h1data, family = ocat(R=9))
anova(omod_h1d_null, omod_h1d, test="LRT")
anova(mod_h1d_null, mod_h1d, test="LRT") # compare
omod_h1r <- gam(Q6_1o ~ sport + vig_gender, data = h1data, family = ocat(R=9))
omod_h1r_null <- gam(Q6_1o ~ vig_gender, data = h1data, family = ocat(R=9))
anova(omod_h1r_null, omod_h1r, test="LRT")
anova(mod_h1r_null, mod_h1r, test="LRT") # compare

######################
# Plot b: Variation of fatality rates for a given activity has little impact on danger and recklessness judgements
######################

# hold other variables fixed at baseline levels
h2data <- data %>% filter(dependents == "No dependents", charity == "For self", low_competence == "High competence", 
                          guide == "No guide") %>% filter(sport %in% c("skitouring")) 

h2data %>%  pivot_longer(cols = c("Q5_1", "Q6_1")) %>%
  mutate(name = factor(name, levels = c("Q5_1", "Q6_1"), labels = c("Danger", "Recklessness"))) %>%
  group_by(extreme_risk, name) %>% summarize(meanrating = mean(value), se = sd(value)/sqrt(n()))

# model 
mod_h2d <- glm(Q5_1 ~ extreme_risk, data = h2data, family = "poisson")
qqnorm(residuals(mod_h2d)); abline(a=0,b=1)
summary(mod_h2d)

mod_h2r <- glm(Q6_1 ~ extreme_risk, data = h2data, family = "poisson")
qqnorm(residuals(mod_h2r)); abline(a=0,b=1)
summary(mod_h2r)

## results reported in paper

# marginal effects of extreme_risk
summary(mod_h2d)
summary(mod_h2r)

# model means and posthoc pairwise comparisons, appears in text and plots
emm_d <- emmeans(mod_h2d, pairwise ~ extreme_risk, adjust = "tukey", type = "response", bias.adj = FALSE, sigma = sigma(mod_h2d))
emm_r <- emmeans(mod_h2r, pairwise ~ extreme_risk, adjust = "tukey", type = "response", bias.adj = FALSE, sigma = sigma(mod_h2r))
emm_d
emm_r

# process plot for main figure
danger_model_means <- emm_d$emmeans %>% data.frame() %>% mutate(name = "Danger")
reck_model_means <- emm_r$emmeans %>% data.frame() %>% mutate(name = "Recklessness")
model_means <- rbind(danger_model_means, reck_model_means)
pd <- position_dodge(0.5)
p_h2_mm <- model_means %>% rename(response = rate, lower.CL = asymp.LCL, upper.CL = asymp.UCL) %>%
  ggplot(aes(x = name, y = response + 1, colour = extreme_risk)) + 
  geom_point(size = 3, position = pd) +
  geom_linerange(aes(ymin = lower.CL + 1, ymax = upper.CL + 1), position = pd) +
  ylab("Rating") + xlab("") + 
  scale_colour_brewer(palette = "Set2") + theme_bw(base_size = 12) + theme(panel.grid = element_blank()) +
  labs(subtitle = "b (vignette subset E)") +
  coord_cartesian(ylim = c(2.25,4.75)) +
  theme(legend.position="bottom", legend.margin=margin(0,0,0,0), legend.box.margin=margin(-20,0,0,0), legend.title = element_blank())
p_h2_mm

# additional checks -- LRT comparison with null model
mod_h2d_null <- glm(Q5_1 ~ 1, data = h2data, family = "poisson")
anova(mod_h2d_null, mod_h2d, test="LRT")

mod_h2r_null <- glm(Q6_1 ~ 1, data = h2data, family = "poisson")
anova(mod_h2r_null, mod_h2r, test="LRT")

# additional checks -- ordinal regression 
omod_h2d <- gam(Q5_1o ~ extreme_risk, data = h2data, family = ocat(R=9))
summary(omod_h2d)
summary(mod_h2d) # compare
omod_h2r <- gam(Q6_1o ~ extreme_risk, data = h2data, family = ocat(R=9))
summary(omod_h2r)
summary(mod_h2r) # compare

######################
# Plot c: Gender bias affects negatively perceived recklessness of female extreme sports participants.
######################

# hold other variables fixed at baseline levels
h3data <- data %>% filter(dependents == "No dependents", charity == "For self", low_competence == "High competence", 
                          guide == "No guide", extreme_risk == "Baseline risk") 
h3data <- h3data[!is.na(h3data$part_gender) & !is.na(h3data$vig_gender), ]

# model
mod_h3d <- glm(Q5_1 ~ part_gender * vig_gender, data = h3data, family = "poisson")
qqnorm(residuals(mod_h3d)); abline(a=0,b=1)
summary(mod_h3d)
mod_h3r <- glm(Q6_1 ~ part_gender * vig_gender, data = h3data, family = "poisson")
qqnorm(residuals(mod_h3r)); abline(a=0,b=1)
summary(mod_h3r)

## results reported in paper

# model means based on on vig_gender only
mod_h3d_vg <- glm(Q5_1 ~ vig_gender, data = h3data, family = "poisson")
mod_h3r_vg <- glm(Q6_1 ~ vig_gender, data = h3data, family = "poisson")
emmeans(mod_h3d_vg, pairwise ~ vig_gender, type = "response", bias.adj = FALSE)
emmeans(mod_h3r_vg, pairwise ~ vig_gender, type = "response", bias.adj = FALSE)

# model means and posthoc pairwise comparisons including interaction
emm_d <- emmeans(mod_h3d, pairwise ~ part_gender | vig_gender, adjust = "tukey", type = "response", bias.adj = FALSE, sigma = sigma(mod_h3d))
emm_r <- emmeans(mod_h3r, pairwise ~ part_gender | vig_gender, adjust = "tukey", type = "response", bias.adj = FALSE, sigma = sigma(mod_h3r))
emm_d
emm_r

# process plot for main figure
danger_model_means <- emm_d$emmeans %>% data.frame() %>% mutate(name = "Danger")
reck_model_means <- emm_r$emmeans %>% data.frame() %>% mutate(name = "Recklessness")
model_means <- rbind(danger_model_means, reck_model_means)
pd <- position_dodge(0.5)
p_h3_mm <- model_means %>% rename(response = rate, lower.CL = asymp.LCL, upper.CL = asymp.UCL) %>%
  ggplot(aes(x = name, y = response + 1, colour = part_gender)) +
  facet_grid(. ~ vig_gender) +
  geom_point(size = 3, position = pd) +
  geom_linerange(aes(ymin = lower.CL + 1, ymax = upper.CL + 1), position = pd) +
  ylab("Rating") + xlab("") + 
  scale_colour_brewer(palette = "Set2") + theme_bw(base_size = 12) + theme(panel.grid = element_blank()) +
  labs(subtitle = "c (vignette subset A)") +
  coord_cartesian(ylim = c(2.25,4.75)) +
  theme(legend.position="bottom", legend.margin=margin(0,0,0,0), legend.box.margin=margin(-20,0,0,0), legend.title = element_blank())
p_h3_mm

# additional checks -- LRT comparison with null model
mod_h3d_null0 <- glm(Q5_1 ~ 1, data = h3data, family = "poisson")
mod_h3d_null <- glm(Q5_1 ~ vig_gender, data = h3data, family = "poisson")
anova(mod_h3d_null0, mod_h3d_null, test="LRT")
anova(mod_h3d_null0, mod_h3d_null, mod_h3d, test="LRT")
mod_h3r_null0 <- glm(Q6_1 ~ 1, data = h3data, family = "poisson")
mod_h3r_null <- glm(Q6_1 ~ vig_gender, data = h3data, family = "poisson")
anova(mod_h3r_null0, mod_h3r_null, mod_h3r, test="LRT")

# additional checks -- ordinal regression 
omod_h3d_vg <- gam(Q5_1o ~ vig_gender, data = h3data, family = ocat(R=9))
omod_h3r_vg <- gam(Q6_1o ~ vig_gender, data = h3data, family = ocat(R=9))
summary(omod_h3d_vg)
summary(mod_h3d_vg) # compare
summary(omod_h3r_vg)
summary(mod_h3r_vg) # compare

omod_h3d <- gam(Q5_1o ~ part_gender * vig_gender, data = h3data, family = ocat(R=9))
omod_h3r <- gam(Q6_1o ~ part_gender * vig_gender, data = h3data, family = ocat(R=9))

emmeans(omod_h3r, pairwise ~ part_gender | vig_gender, adjust = "tukey", type = "response", bias.adj = FALSE)
emmeans(mod_h3r, pairwise ~ part_gender | vig_gender, adjust = "tukey", type = "response", bias.adj = FALSE)

emmeans(omod_h3d, pairwise ~ part_gender | vig_gender, adjust = "tukey", type = "response", bias.adj = FALSE)
emmeans(mod_h3d, pairwise ~ part_gender | vig_gender, adjust = "tukey", type = "response", bias.adj = FALSE)

######################
# Plot d: Recklessness judgements of sports participants is partly moderated by whether or not they have dependants.
######################

# hold other variables fixed at baseline levels
h4data <- data %>% filter(charity == "For self", low_competence == "High competence", 
                          guide == "No guide", extreme_risk == "Baseline risk") %>% filter(sport %in% c("skitouring"))

# effect size
cohen.d(Q6_1~factor(dependents), data = h4data)

# model
mod_h4d <- glm(Q5_1 ~ dependents, data = h4data, family = "poisson")
qqnorm(residuals(mod_h3r)); abline(a=0,b=1)
mod_h4r <- glm(Q6_1 ~ dependents, data = h4data, family = "poisson")
qqnorm(residuals(mod_h4r)); abline(a=0,b=1)

## results reported in paper

# marginal effects of dependents
summary(mod_h4d)
summary(mod_h4r)

# model means, text and plots
emm_d <- emmeans(mod_h4d, pairwise ~ dependents, adjust = "tukey", type = "response", bias.adj = FALSE, sigma = sigma(mod_h4d))
emm_r <- emmeans(mod_h4r, pairwise ~ dependents, adjust = "tukey", type = "response", bias.adj = FALSE, sigma = sigma(mod_h4r))
emm_d
emm_r

# process plot for main figure
danger_model_means <- emm_d$emmeans %>% data.frame() %>% mutate(name = "Danger")
reck_model_means <- emm_r$emmeans %>% data.frame() %>% mutate(name = "Recklessness")
model_means <- rbind(danger_model_means, reck_model_means)
pd <- position_dodge(0.5)
p_h4_mm <- model_means %>% rename(response = rate, lower.CL = asymp.LCL, upper.CL = asymp.UCL) %>%
  ggplot(aes(x = name, y = response + 1, colour = dependents)) + 
  geom_point(size = 3, position = pd) +
  geom_linerange(aes(ymin = lower.CL + 1, ymax = upper.CL + 1), position = pd) +
  ylab("Rating") + xlab("") + 
  scale_colour_brewer(palette = "Set2") + theme_bw(base_size = 12) + theme(panel.grid = element_blank()) +
  labs(subtitle = "d (vignette subset B)") +
  coord_cartesian(ylim = c(2.25,4.75)) +
  theme(legend.position="bottom", legend.margin=margin(0,0,0,0), legend.box.margin=margin(-20,0,0,0), legend.title = element_blank())
p_h4_mm

# additional checks -- LRT comparison with null model
mod_h4d_null <- glm(Q5_1 ~ 1, data = h4data, family = "poisson")
anova(mod_h4d_null, mod_h4d, test="LRT")

mod_h4r_null <- glm(Q6_1 ~ 1, data = h4data, family = "poisson")
anova(mod_h4r_null, mod_h4r, test="LRT")

# additional checks -- ordinal regression 
omod_h4d <- gam(Q5_1o ~ dependents, data = h4data, family = ocat(R=9))
summary(omod_h4d)
summary(mod_h4d) # compare
omod_h4r <- gam(Q6_1o ~ dependents, data = h4data, family = ocat(R=9))
summary(omod_h4r)
summary(mod_h4r)

##################
# Plot e: Danger judgements are not moderated by whether the sports participant is engaging in the sport for charitable purposes. 
# Recklessness judgements may well be moderated by whether the sports participant is engaging in the sport for charitable purposes. 
##################

# hold other variables fixed at baseline levels
h5data <- data %>% filter(dependents == "No dependents", low_competence == "High competence", 
                          guide == "No guide", extreme_risk == "Baseline risk") %>% filter(sport %in% c("skitouring"))

# effect size
cohen.d(Q5_1~factor(charity), data = h5data)
cohen.d(Q6_1~factor(charity), data = h5data)

# model
mod_h5d <- glm(Q5_1 ~ charity, data = h5data, family = "poisson")
qqnorm(residuals(mod_h5d)); abline(a=0,b=1)
summary(mod_h5d)

mod_h5r <- glm(Q6_1 ~ charity, data = h5data, family = "poisson")
qqnorm(residuals(mod_h5r)); abline(a=0,b=1)
summary(mod_h5r)

## results reported in paper

# marginal effects
summary(mod_h5d)
summary(mod_h5r)

# model means, text and plot
emm_d <- emmeans(mod_h5d, pairwise ~ charity, adjust = "tukey", type = "response", bias.adj = FALSE, sigma = sigma(mod_h5d))
emm_r <- emmeans(mod_h5r, pairwise ~ charity, adjust = "tukey", type = "response", bias.adj = FALSE, sigma = sigma(mod_h5r))
emm_d
emm_r

# process plot for main figure
danger_model_means <- emm_d$emmeans %>% data.frame() %>% mutate(name = "Danger")
reck_model_means <- emm_r$emmeans %>% data.frame() %>% mutate(name = "Recklessness")
model_means <- rbind(danger_model_means, reck_model_means)
pd <- position_dodge(0.5)
p_h5_mm <- model_means %>% rename(response = rate, lower.CL = asymp.LCL, upper.CL = asymp.UCL) %>%
  ggplot(aes(x = name, y = response + 1, colour = charity)) + 
  geom_point(size = 3, position = pd) +
  geom_linerange(aes(ymin = lower.CL + 1, ymax = upper.CL + 1), position = pd) +
  ylab("Rating") + xlab("") + 
  scale_colour_brewer(palette = "Set2") + theme_bw(base_size = 12) + theme(panel.grid = element_blank()) +
  labs(subtitle = "e (vignette subset C)") +
  coord_cartesian(ylim = c(2.25,4.75)) +
  theme(legend.position="bottom", legend.margin=margin(0,0,0,0), legend.box.margin=margin(-20,0,0,0), legend.title = element_blank())
p_h5_mm

## additional checks -- LRT for marginal effects of charity
mod_h5d_null <- glm(Q5_1 ~ 1, data = h5data, family = "poisson")
anova(mod_h5d_null, mod_h5d, test="LRT")

mod_h5r_null <- glm(Q6_1 ~ 1, data = h5data, family = "poisson")
anova(mod_h5r_null, mod_h5r, test="LRT")

# additional checks -- ordinal regression 
omod_h5d <- gam(Q5_1o ~ charity, data = h5data, family = ocat(R=9))
summary(omod_h5d)
summary(mod_h5d) # compare
omod_h5r <- gam(Q6_1o ~ charity, data = h5data, family = ocat(R=9))
summary(omod_h5r)
summary(mod_h5r)

# interpreting the ordered categorical model
dat <- data.frame(charity = levels(h5data$charity)) 
ocat_predict_d = data.frame(predict(omod_h5d,type = "response", newdata = dat))
colnames(ocat_predict_d) = paste0("Pr(D=", 1:9,")")
ocat_predict_d <- ocat_predict_d %>% 
  pivot_longer(cols = 1:9) %>% 
  mutate(charity = rep(levels(h5data$charity), each = 9),
         rating = rep(1:9, 2), 
         outcome = "Danger") 

ocat_predict_r = data.frame(predict(omod_h5r,type = "response", newdata = dat))
colnames(ocat_predict_r) = paste0("Pr(D=", 1:9,")")
ocat_predict_r <- ocat_predict_r %>% 
  pivot_longer(cols = 1:9) %>% 
  mutate(charity = rep(levels(h5data$charity), each = 9),
         rating = rep(1:9, 2),
         outcome = "Recklessness") 

ocat_predict <- rbind(ocat_predict_d, ocat_predict_r)

p_h5_or <- ocat_predict %>% ggplot(aes(x= rating, y = value, fill = charity))+
  geom_bar(stat = "identity", position = position_dodge()) +
  xlab("Rating") + ylab("Probability") +
  facet_grid(. ~ outcome) +
  scale_fill_brewer(palette = "Set2") + theme_bw(base_size = 12) + theme(panel.grid = element_blank()) +
  labs(subtitle = "e (vignette subset C)") +
  scale_x_continuous(breaks = 1:9) +
  theme(legend.position="bottom", legend.margin=margin(0,0,0,0), legend.box.margin=margin(0,0,0,0), legend.title = element_blank())
p_h5_or

ocat_predict %>% ggplot(aes(x= danger, y= value))+
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_grid(charity ~ .) +
  xlab("Danger rating") + ylab("Probability") +
  theme_bw() 

p_h5_or + p_h5_mm + plot_layout(ncol = 2, widths = c())


###########################
# Plot f: Danger and Recklessness judgements are moderated by whether the sports participant is highly skilled and competent, or the sport 
# participant is assisted by a professional.
###########################

# hold other variables fixed at baseline levels
h6data <- data %>% filter(dependents == "No dependents", charity == "For self", extreme_risk == "Baseline risk") %>%
  filter(sport %in% c("skitouring"))

# model 
mod_h6d <- glm(Q5_1 ~ compguide, data = h6data, family = "poisson")
qqnorm(residuals(mod_h6d)); abline(a=0,b=1)
summary(mod_h6d)

mod_h6r <- glm(Q6_1 ~ compguide, data = h6data, family = "poisson")
qqnorm(residuals(mod_h6r)); abline(a=0,b=1)
summary(mod_h6r)

## results reported in paper

# marginal effects
mod_h6d_null <- glm(Q5_1 ~ 1, data = h6data, family = "poisson")
anova(mod_h6d_null, mod_h6d, test="LRT")

mod_h6r_null <- glm(Q6_1 ~ 1, data = h6data, family = "poisson")
anova(mod_h6r_null, mod_h6r, test="LRT")

# model means, text and plots
emm_d <- emmeans(mod_h6d, pairwise ~ compguide, adjust = "tukey", type = "response", bias.adj = FALSE, sigma = sigma(mod_h6d))
emm_r <- emmeans(mod_h6r, pairwise ~ compguide, adjust = "tukey", type = "response", bias.adj = FALSE, sigma = sigma(mod_h6r))
emm_d
emm_r

# process plot for main figure
danger_model_means <- emm_d$emmeans %>% data.frame() %>% mutate(name = "Danger")
reck_model_means <- emm_r$emmeans %>% data.frame() %>% mutate(name = "Recklessness")
model_means <- rbind(danger_model_means, reck_model_means)
pd <- position_dodge(0.5)
p_h6_mm <- model_means %>% rename(response = rate, lower.CL = asymp.LCL, upper.CL = asymp.UCL) %>%
  ggplot(aes(x = name, y = response + 1, colour = compguide)) + 
  geom_point(size = 3, position = pd) +
  geom_linerange(aes(ymin = lower.CL + 1, ymax = upper.CL + 1), position = pd) +
  ylab("Rating") + xlab("") + 
  scale_colour_brewer(palette = "Set2") + theme_bw(base_size = 12) + theme(panel.grid = element_blank()) +
  labs(subtitle = "f (vignette subset D)") +
  coord_cartesian(ylim = c(2.25,4.75)) +
  theme(legend.position="bottom", legend.margin=margin(0,0,0,0), legend.box.margin=margin(-20,0,0,0), legend.title = element_blank())
p_h6_mm

# additional checks -- ordinal regression 
omod_h6d <- gam(Q5_1o ~ compguide, data = h6data, family = ocat(R=9))
omod_h6d_null <- gam(Q5_1o ~ 1, data = h6data, family = ocat(R=9))
anova(omod_h6d_null, omod_h6d, test="LRT")
anova(mod_h6d_null, mod_h6d, test="LRT") # compare
omod_h6r <- gam(Q6_1o ~ compguide, data = h6data, family = ocat(R=9))
omod_h6r_null <- gam(Q6_1o ~ 1, data = h6data, family = ocat(R=9))
anova(omod_h6r_null, omod_h6r, test="LRT")
anova(mod_h6r_null, mod_h6r, test="LRT") # compare

omod_h6d <- gam(Q5_1o ~ compguide, data = h6data, family = ocat(R=9))

# combine plots into one
p2 <- p_h1_mm + p_h2_mm + p_h3_mm + p_h4_mm + p_h5_mm + p_h6_mm + plot_layout(ncol = 2)
p2

# uncomment to save
ggsave("output/all-tests-no-insurance.png", p2, width=11, height=11, dpi = 300)

