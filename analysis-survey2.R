# Reproduces results from analysis of Survey 2 responses in "Differences in expert and layperson’s danger and recklessness judgments 
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
library(readxl)

load("output/combined-expert-layperson-responses-clean.Rdata")

# descriptives: empirical means and SEs
xl_all %>% filter(!is.na(resp_gender)) %>% group_by(condition, resp_gender, sgroup) %>% summarize(danger = mean(danger), recklessness = mean(recklessness))
xl_all %>% filter(!is.na(resp_gender)) %>% group_by(condition, sgroup) %>% 
  summarize(meand = mean(danger), sed = sd(danger)/sqrt(n()), meanr = mean(recklessness), ser = sd(recklessness)/sqrt(n()))

# descriptives: danger/reckless relationship
xl_all <- xl_all %>% mutate(r_gt_d = (recklessness > danger),
                            r_minus_d = recklessness - danger)
table(xl_all$r_gt_d, xl_all$sgroup)
table(xl_all$r_minus_d, xl_all$sgroup)

## DANGER

# aov on log-transformed response
min_conf_danger <- 0 # increase if want to filter by confidence, 0 for all responses
aov_d <- aov(log(danger) ~ condition * sgroup + condition * resp_gender, data = xl_all %>% filter(conf_danger > min_conf_danger))
summary(aov_d)
emmeans(aov_d, ~ condition * sgroup, type = "response", bias.adj = TRUE)
# contrasts we want, averaged over resp_gender
custom <- list(`BaseIn-BaseOut` = c(1,0,0,0,-1,0,0,0),
               `BaseIn-CharityIn` = c(1,-1,0,0,0,0,0,0),
               `BaseOut-CharityOut` = c(0,0,0,0,1,-1,0,0),
               `BaseIn-ExRiskIn` = c(1,0,-1,0,0,0,0,0),
               `BaseOut-ExRiskOut` = c(0,0,0,0,1,0,-1,0),
               `BaseIn-IncompIn` = c(1,0,0,-1,0,0,0,0),
               `BaseOut-IncompOut` = c(0,0,0,0,1,0,0,-1))
emm_d <- emmeans(aov_d, specs = c("condition", "sgroup"), contr = custom, adjust = "tukey", type = "response", bias.adj = TRUE, sigma = sigma(aov_d))
emm_d$emmeans
emm_d$contrasts

## RECKLESSNESS

# aov on log-transformed response
min_conf_reck <- 0 # increase if want to filter by confidence, 0 for all responses
aov_r <- aov(log(recklessness) ~ condition * sgroup + condition * resp_gender, data = xl_all %>% filter(conf_reck > min_conf_reck))
summary(aov_r)
emmeans(aov_r, ~ condition * sgroup, type = "response", bias.adj = TRUE)
# contrasts we want
custom <- list(`BaseIn-BaseOut` = c(1,0,0,0,-1,0,0,0),
               `BaseIn-CharityIn` = c(1,-1,0,0,0,0,0,0),
               `BaseOut-CharityOut` = c(0,0,0,0,1,-1,0,0),
               `BaseIn-ExRiskIn` = c(1,0,-1,0,0,0,0,0),
               `BaseOut-ExRiskOut` = c(0,0,0,0,1,0,-1,0),
               `BaseIn-IncompIn` = c(1,0,0,-1,0,0,0,0),
               `BaseOut-IncompOut` = c(0,0,0,0,1,0,0,-1))
emm_r <- emmeans(aov_r,  specs = c("condition", "sgroup"), contr = custom, adjust = "tukey", type = "response", bias.adj = TRUE, sigma = sigma(aov_r))
emm_r$emmeans
emm_r$contrasts

# effect sizes
eff_size(emm_d, sigma = sigma(aov_d), edf = 636)
eff_size(emm_r, sigma = sigma(aov_r), edf = 636)

danger_model_means <- emm_d$emmeans %>% data.frame() %>% mutate(name = "Danger")
reck_model_means <- emm_r$emmeans %>% data.frame() %>% mutate(name = "Recklessness")
model_means <- rbind(danger_model_means, reck_model_means)
pd <- position_dodge(0.5)
p_inout_mm <- model_means %>% 
  mutate(sgroup = factor(sgroup, levels = c("outgroup", "ingroup"), labels = c("No experience", "Experienced"))) %>%
  ggplot(aes(x = sgroup, y = response, colour = condition)) + 
  facet_grid(.~name) +
  geom_point(size = 3, position = pd) +
  geom_linerange(aes(ymin = lower.CL, ymax = upper.CL), position = pd) +
  ylab("Rating") + xlab("") + 
  scale_colour_brewer(palette = "Set2") + theme_bw(base_size = 12) + theme(panel.grid = element_blank()) +
  coord_cartesian(ylim = c(min(model_means$lower.CL),max(model_means$upper.CL))) +
  theme(legend.position="bottom", legend.margin=margin(0,0,0,0), legend.box.margin=margin(-20,0,0,0), legend.title = element_blank())
p_inout_mm

ggsave("output/inoutgroup-comparison.png", p_inout_mm, width=11, height=3.5, dpi = 300)











# extracting bits needed for text 
tps_d <- emcont_d %>% data.frame() %>% mutate(contrast = as.character(contrast)) %>% 
  separate(contrast, sep = "[,/]", into = c("cond1", "sgroup1", "cond2", "sgroup2")) %>%
  mutate_at(1:4, str_trim)
mmeans_d <- emm_d$emmeans %>% data.frame()

tps_r <- emcont_r %>% data.frame() %>% mutate(contrast = as.character(contrast)) %>% 
  separate(contrast, sep = "[,/]", into = c("cond1", "sgroup1", "cond2", "sgroup2")) %>%
  mutate_at(1:4, str_trim)
mmeans_r <- emm_r$emmeans %>% data.frame()


# comparison 1: baseline in vs out
comp_d <- tps_d %>% filter(cond1 == "Baseline", cond2 == "Baseline") %>%
  left_join(mmeans_d %>% dplyr::select(-SE, - df), by = c("cond1"= "condition", "sgroup1" = "sgroup")) %>%
  rename(mean1 = response, lcl1 = lower.CL, ucl1 = upper.CL) %>%
  left_join(mmeans_d %>% dplyr::select(-SE, - df), by = c("cond2"= "condition", "sgroup2" = "sgroup")) %>%
  rename(mean2 = response, lcl2 = lower.CL, ucl2 = upper.CL)

comp_r <- tps_r %>% filter(cond1 == "Baseline", cond2 == "Baseline") %>%
  left_join(mmeans_r %>% dplyr::select(-SE, - df), by = c("cond1"= "condition", "sgroup1" = "sgroup")) %>%
  rename(mean1 = response, lcl1 = lower.CL, ucl1 = upper.CL) %>%
  left_join(mmeans_r %>% dplyr::select(-SE, - df), by = c("cond2"= "condition", "sgroup2" = "sgroup")) %>%
  rename(mean2 = response, lcl2 = lower.CL, ucl2 = upper.CL)

comp_d
comp_r

# comparison 2: extreme risk in vs out
# ingroup
comp_d_in <- tps_d %>% filter(cond1 == "Baseline", sgroup1 == "ingroup", cond2 == "Extreme risk", sgroup2 == "ingroup") %>%
  left_join(mmeans_d %>% dplyr::select(-SE, - df), by = c("cond1"= "condition", "sgroup1" = "sgroup")) %>%
  rename(mean1 = response, lcl1 = lower.CL, ucl1 = upper.CL) %>%
  left_join(mmeans_d %>% dplyr::select(-SE, - df), by = c("cond2"= "condition", "sgroup2" = "sgroup")) %>%
  rename(mean2 = response, lcl2 = lower.CL, ucl2 = upper.CL)

comp_r_in <- tps_r %>% filter(cond1 == "Baseline", sgroup1 == "ingroup", cond2 == "Extreme risk", sgroup2 == "ingroup") %>%
  left_join(mmeans_r %>% dplyr::select(-SE, - df), by = c("cond1"= "condition", "sgroup1" = "sgroup")) %>%
  rename(mean1 = response, lcl1 = lower.CL, ucl1 = upper.CL) %>%
  left_join(mmeans_r %>% dplyr::select(-SE, - df), by = c("cond2"= "condition", "sgroup2" = "sgroup")) %>%
  rename(mean2 = response, lcl2 = lower.CL, ucl2 = upper.CL)

# outgroup
comp_d_out <- tps_d %>% filter(cond1 == "Baseline", sgroup1 == "outgroup", cond2 == "Extreme risk", sgroup2 == "outgroup") %>%
  left_join(mmeans_d %>% dplyr::select(-SE, - df), by = c("cond1"= "condition", "sgroup1" = "sgroup")) %>%
  rename(mean1 = response, lcl1 = lower.CL, ucl1 = upper.CL) %>%
  left_join(mmeans_d %>% dplyr::select(-SE, - df), by = c("cond2"= "condition", "sgroup2" = "sgroup")) %>%
  rename(mean2 = response, lcl2 = lower.CL, ucl2 = upper.CL)

comp_r_out <- tps_r %>% filter(cond1 == "Baseline", sgroup1 == "outgroup", cond2 == "Extreme risk", sgroup2 == "outgroup") %>%
  left_join(mmeans_r %>% dplyr::select(-SE, - df), by = c("cond1"= "condition", "sgroup1" = "sgroup")) %>%
  rename(mean1 = response, lcl1 = lower.CL, ucl1 = upper.CL) %>%
  left_join(mmeans_r %>% dplyr::select(-SE, - df), by = c("cond2"= "condition", "sgroup2" = "sgroup")) %>%
  rename(mean2 = response, lcl2 = lower.CL, ucl2 = upper.CL)

comp_d_in
comp_r_in
comp_d_out
comp_r_out

# comparison 3: charity in vs out
# ingroup
comp_d_in <- tps_d %>% filter(cond1 == "Baseline", sgroup1 == "ingroup", cond2 == "Charity", sgroup2 == "ingroup") %>%
  left_join(mmeans_d %>% dplyr::select(-SE, - df), by = c("cond1"= "condition", "sgroup1" = "sgroup")) %>%
  rename(mean1 = response, lcl1 = lower.CL, ucl1 = upper.CL) %>%
  left_join(mmeans_d %>% dplyr::select(-SE, - df), by = c("cond2"= "condition", "sgroup2" = "sgroup")) %>%
  rename(mean2 = response, lcl2 = lower.CL, ucl2 = upper.CL)

comp_r_in <- tps_r %>% filter(cond1 == "Baseline", sgroup1 == "ingroup", cond2 == "Charity", sgroup2 == "ingroup") %>%
  left_join(mmeans_r %>% dplyr::select(-SE, - df), by = c("cond1"= "condition", "sgroup1" = "sgroup")) %>%
  rename(mean1 = response, lcl1 = lower.CL, ucl1 = upper.CL) %>%
  left_join(mmeans_r %>% dplyr::select(-SE, - df), by = c("cond2"= "condition", "sgroup2" = "sgroup")) %>%
  rename(mean2 = response, lcl2 = lower.CL, ucl2 = upper.CL)

# outgroup
comp_d_out <- tps_d %>% filter(cond1 == "Baseline", sgroup1 == "outgroup", cond2 == "Charity", sgroup2 == "outgroup") %>%
  left_join(mmeans_d %>% dplyr::select(-SE, - df), by = c("cond1"= "condition", "sgroup1" = "sgroup")) %>%
  rename(mean1 = response, lcl1 = lower.CL, ucl1 = upper.CL) %>%
  left_join(mmeans_d %>% dplyr::select(-SE, - df), by = c("cond2"= "condition", "sgroup2" = "sgroup")) %>%
  rename(mean2 = response, lcl2 = lower.CL, ucl2 = upper.CL)

comp_r_out <- tps_r %>% filter(cond1 == "Baseline", sgroup1 == "outgroup", cond2 == "Charity", sgroup2 == "outgroup") %>%
  left_join(mmeans_r %>% dplyr::select(-SE, - df), by = c("cond1"= "condition", "sgroup1" = "sgroup")) %>%
  rename(mean1 = response, lcl1 = lower.CL, ucl1 = upper.CL) %>%
  left_join(mmeans_r %>% dplyr::select(-SE, - df), by = c("cond2"= "condition", "sgroup2" = "sgroup")) %>%
  rename(mean2 = response, lcl2 = lower.CL, ucl2 = upper.CL)

comp_d_in
comp_r_in
comp_d_out
comp_r_out

# comparison 4: competence
# ingroup
comp_d_in <- tps_d %>% filter(cond1 == "Baseline", sgroup1 == "ingroup", cond2 == "Low competence", sgroup2 == "ingroup") %>%
  left_join(mmeans_d %>% dplyr::select(-SE, - df), by = c("cond1"= "condition", "sgroup1" = "sgroup")) %>%
  rename(mean1 = response, lcl1 = lower.CL, ucl1 = upper.CL) %>%
  left_join(mmeans_d %>% dplyr::select(-SE, - df), by = c("cond2"= "condition", "sgroup2" = "sgroup")) %>%
  rename(mean2 = response, lcl2 = lower.CL, ucl2 = upper.CL)

comp_r_in <- tps_r %>% filter(cond1 == "Baseline", sgroup1 == "ingroup", cond2 == "Low competence", sgroup2 == "ingroup") %>%
  left_join(mmeans_r %>% dplyr::select(-SE, - df), by = c("cond1"= "condition", "sgroup1" = "sgroup")) %>%
  rename(mean1 = response, lcl1 = lower.CL, ucl1 = upper.CL) %>%
  left_join(mmeans_r %>% dplyr::select(-SE, - df), by = c("cond2"= "condition", "sgroup2" = "sgroup")) %>%
  rename(mean2 = response, lcl2 = lower.CL, ucl2 = upper.CL)

# outgroup
comp_d_out <- tps_d %>% filter(cond1 == "Baseline", sgroup1 == "outgroup", cond2 == "Low competence", sgroup2 == "outgroup") %>%
  left_join(mmeans_d %>% dplyr::select(-SE, - df), by = c("cond1"= "condition", "sgroup1" = "sgroup")) %>%
  rename(mean1 = response, lcl1 = lower.CL, ucl1 = upper.CL) %>%
  left_join(mmeans_d %>% dplyr::select(-SE, - df), by = c("cond2"= "condition", "sgroup2" = "sgroup")) %>%
  rename(mean2 = response, lcl2 = lower.CL, ucl2 = upper.CL)

comp_r_out <- tps_r %>% filter(cond1 == "Baseline", sgroup1 == "outgroup", cond2 == "Low competence", sgroup2 == "outgroup") %>%
  left_join(mmeans_r %>% dplyr::select(-SE, - df), by = c("cond1"= "condition", "sgroup1" = "sgroup")) %>%
  rename(mean1 = response, lcl1 = lower.CL, ucl1 = upper.CL) %>%
  left_join(mmeans_r %>% dplyr::select(-SE, - df), by = c("cond2"= "condition", "sgroup2" = "sgroup")) %>%
  rename(mean2 = response, lcl2 = lower.CL, ucl2 = upper.CL)

comp_d_in
comp_r_in
comp_d_out
comp_r_out





# aov on log-transformed response
aov_d <- aov(log(danger) ~ condition * sgroup + condition * resp_gender, data = xl_all)
summary(aov_d)
emmeans(aov_d, ~ condition * sgroup, type = "response", bias.adj = TRUE)


# hold other variables fixed at baseline levels
h5data <- xl_all %>% filter(condition %in% c("Baseline", "Charity"), sgroup == "outgroup")

# aov on log-transformed response
aov_h5d <- aov(log(danger) ~ condition * resp_gender, data = h5data)
summary(aov_h5d)
emmeans(aov_h5d, ~ condition, type = "response", bias.adj = TRUE)
emmeans(aov_h5d, pairwise ~ condition, adjust = "none", type = "response", bias.adj = TRUE, sigma = sigma(aov_h5d))


aov_h5r <- aov(log(recklessness) ~ condition, data = h5data)
summary(aov_h5r)
emmeans(aov_h5r, ~ condition, type = "response", bias.adj = TRUE)






# plot all conditions, by ski-touring activity level
danger_model_means <- emm_d$emmeans %>% data.frame() %>% mutate(name = "danger")
reck_model_means <- emm_r$emmeans %>% data.frame() %>% mutate(name = "recklessness")
model_means <- rbind(danger_model_means, reck_model_means)
pd <- position_dodge(0.5)
p_inout_mm <- model_means %>% 
  #filter(!is.na(resp_gender)) %>%
  #filter(!is.na(ski_touring)) %>%
  #filter(condition %in% c("baseline", "extreme")) %>%
  #pivot_longer(cols = c("danger", "recklessness")) %>%
  mutate(sgroup = factor(sgroup, levels = c("outgroup", "ingroup"), labels = c("No experience", "Experienced"))) %>%
  mutate(name = factor(name, levels = c("danger", "recklessness"), labels = c("Danger", "Recklessness"))) %>%
  ggplot(aes(x = sgroup, y = response, colour = condition)) + 
  geom_point(size = 3, position = pd) +
  geom_linerange(aes(ymin = lower.CL, ymax = upper.CL), position = pd) +
  facet_grid(. ~ name) +
  ylab("Rating") + xlab("") + 
  scale_colour_brewer(palette = "Set2") + theme_bw(base_size = 14) + theme(panel.grid = element_blank()) +
  #labs(title = "Hypothesis 2", subtitle = "Supported*") +
  #labs(subtitle = "Hypothesis 2") +
  coord_cartesian(ylim = c(1.25, 5.25)) +
  theme(legend.position="bottom", legend.margin=margin(0,0,0,0), legend.box.margin=margin(-20,0,0,0), legend.title = element_blank())

p_inout_mm

ggsave("../paper/inoutgroup-comparison.png", p_inout_mm, width=11, height=3.5, dpi = 300)









# aov on log-transformed response
xl_extreme <- xl_all %>% filter(condition %in% c("Baseline", "Extreme risk")) %>% droplevels()
aov_d <- aov(log(danger) ~ condition * sgroup + condition * resp_gender, data = xl_extreme)
summary(aov_d)
emmeans(aov_d, ~ condition * sgroup, type = "response", bias.adj = TRUE)
# pairwise tests
# doing all pairwise tests but only interested in some of them (=m)
m <- 4
emm_d <- emmeans(aov_d, pairwise ~ condition * sgroup, adjust = "none", type = "response", bias.adj = TRUE, sigma = sigma(aov_d))
emcont_d <- summary(emm_d$contrasts)
emcont_d$p.value <- 1 - (1-emcont_d$p.value)^(m)
emcont_d
emm_d$emmeans


# aov on log-transtormed response
xl_charity <- xl_all %>% filter(condition %in% c("Baseline", "Charity")) %>% droplevels()
aov_d <- aov(log(danger) ~ condition * sgroup + condition * resp_gender, data = xl_charity)
summary(aov_d)
emmeans(aov_d, ~ condition * sgroup, type = "response", bias.adj = TRUE)
# pairwise tests
# doing all pairwise tests but only interested in some of them (=m)
m <- 4
emm_d <- emmeans(aov_d, pairwise ~ condition * sgroup, adjust = "none", type = "response", bias.adj = TRUE, sigma = sigma(aov_d))
emcont_d <- summary(emm_d$contrasts)
emcont_d$p.value <- 1 - (1-emcont_d$p.value)^(m)
emcont_d
emm_d$emmeans







aov_r <- aov(log(recklessness) ~ condition * sgroup + condition*resp_gender, data = xl_extreme)
summary(aov_r)
emmeans(aov_r, ~ condition * sgroup, type = "response", bias.adj = TRUE)
# pairwise tests
# doing all pairwise tests but only interested in some of them (=m)
m <- 4
emm_r <- emmeans(aov_r, pairwise ~ condition * sgroup, adjust = "none", type = "response", bias.adj = TRUE, sigma = sigma(aov_r))
emcont_r <- summary(emm_r$contrasts)
emcont_r$p.value <- 1 - (1-emcont_r$p.value)^(m)
emcont_r
emm_r$emmeans









# aov on log-transformed response
aov <- aov(log(danger) ~ condition * sgroup + condition * resp_gender, data = xl_all)
summary(aov)
# https://cran.r-project.org/web/packages/emmeans/vignettes/transformations.html
emmeans(aov, ~ condition * sgroup, type = "response", bias.adj = TRUE)
# pairwise tests
# doing all pairwise tests but only interested in some of them (=m)
m <- 4
emm <- emmeans(aov, pairwise ~ condition * sgroup, adjust = "none", type = "response", bias.adj = TRUE, sigma = sigma(aov))
emm <- summary(emm$contrasts)
emm$p.value <- 1 - (1-emm$p.value)^(m)
emm
emm$emmeans

aov <- aov(log(danger) ~ recklessness * sgroup * resp_gender, data = xl_all)
summary(aov)
