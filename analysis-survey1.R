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
  mutate(owngender = factor(owngender, levels = c(TRUE, FALSE), labels = c("Same gender", "Different gender"))) 

# this removes any danger and recklessness ratings associated with low confidence (1,2)
# run this line to redo the analysis with low-confidence ratings excluded (removes 201 for D, 169 for R)
# data <- data %>% mutate(Q5_1 = ifelse(Q5_a_1 < 3, NA, Q5_1), Q6_1 = ifelse(Q6_a_1 < 3, NA, Q6_1))

### Hypothesis tests, model building, plots

pd <- position_dodge(0.5)

# Plot a: Extreme sports are generally regarded as more dangerous and reckless to engage in 
# than more mundane activities even when fatality rates are the same.

# hold other variables fixed at baseline levels
h1data <- data %>% filter(dependents == "No dependents", charity == "For self", low_competence == "High competence", 
                          guide == "No guide", extreme_risk == "Baseline risk") 

# effect size
cohen.d(Q5_1~factor(extreme_sport), data = h1data)
cohen.d(Q6_1~factor(extreme_sport), data = h1data)

# aov on log-transformed response
aov_h1d <- aov(log(Q5_1) ~ sport + vig_gender, data = h1data)
# https://cran.r-project.org/web/packages/emmeans/vignettes/transformations.html
emmeans(aov_h1d, ~ sport, type = "response", bias.adj = TRUE)
# pairwise tests
emmeans(aov_h1d, pairwise ~ sport, adjust = "tukey", type = "response", bias.adj = TRUE, sigma = sigma(aov_h1d))

# same on recklessness
aov_h1r <- aov(log(Q6_1) ~ sport + vig_gender, data = h1data)
emmeans(aov_h1r, ~ sport, type = "response", bias.adj = TRUE)
emmeans(aov_h1r, pairwise ~ sport, adjust = "tukey", type = "response", bias.adj = TRUE, sigma = sigma(aov_h1r))

# model means plots
emm_d <- emmeans(aov_h1d, pairwise ~ sport, adjust = "tukey", type = "response", bias.adj = TRUE, sigma = sigma(aov_h1d))
emm_r <- emmeans(aov_h1r, pairwise ~ sport, adjust = "tukey", type = "response", bias.adj = TRUE, sigma = sigma(aov_h1r))

danger_model_means <- emm_d$emmeans %>% data.frame() %>% mutate(name = "danger")
reck_model_means <- emm_r$emmeans %>% data.frame() %>% mutate(name = "recklessness")
model_means <- rbind(danger_model_means, reck_model_means)
pd <- position_dodge(0.5)
p_h1_mm <- model_means %>% 
  ggplot(aes(x = sport, y = response, colour = name)) + 
  geom_point(size = 3, position = pd) +
  geom_linerange(aes(ymin = lower.CL, ymax = upper.CL), position = pd) +
  ylab("Rating") + xlab("") + 
  scale_colour_brewer(palette = "Set2") + theme_bw(base_size = 12) + theme(panel.grid = element_blank()) +
  labs(subtitle = "a (vignette subset A)") +
  theme(legend.position="bottom", legend.margin=margin(0,0,0,0), legend.box.margin=margin(-20,0,0,0), legend.title = element_blank())
p_h1_mm


# Plot b: Variation of fatality rates for a given activity has little impact on danger and recklessness judgements

# hold other variables fixed at baseline levels
h2data <- data %>% filter(dependents == "No dependents", charity == "For self", low_competence == "High competence", 
                          guide == "No guide") %>% filter(sport %in% c("skitouring")) 

h2data %>%  pivot_longer(cols = c("Q5_1", "Q6_1")) %>%
  mutate(name = factor(name, levels = c("Q5_1", "Q6_1"), labels = c("Danger", "Recklessness"))) %>%
  group_by(extreme_risk, name) %>% summarize(meanrating = mean(value), se = sd(value)/sqrt(n()))

# aov on log-transformed response
aov_h2d <- aov(log(Q5_1) ~ extreme_risk, data = h2data)
summary(aov_h2d)
emmeans(aov_h2d, ~ extreme_risk, type = "response", bias.adj = TRUE)
emmeans(aov_h2d, pairwise ~ extreme_risk, type = "response", bias.adj = TRUE, sigma = sigma(aov_h2d))

aov_h2r <- aov(log(Q6_1) ~ extreme_risk, data = h2data)
summary(aov_h2r)
emmeans(aov_h2r, ~ extreme_risk, type = "response", bias.adj = TRUE)

# model means plots
emm_d <- emmeans(aov_h2d, pairwise ~ extreme_risk, adjust = "tukey", type = "response", bias.adj = TRUE, sigma = sigma(aov_h2d))
emm_r <- emmeans(aov_h2r, pairwise ~ extreme_risk, adjust = "tukey", type = "response", bias.adj = TRUE, sigma = sigma(aov_h2r))

danger_model_means <- emm_d$emmeans %>% data.frame() %>% mutate(name = "Danger")
reck_model_means <- emm_r$emmeans %>% data.frame() %>% mutate(name = "Recklessness")
model_means <- rbind(danger_model_means, reck_model_means)
pd <- position_dodge(0.5)
p_h2_mm <- model_means %>% 
  ggplot(aes(x = name, y = response, colour = extreme_risk)) + 
  geom_point(size = 3, position = pd) +
  geom_linerange(aes(ymin = lower.CL, ymax = upper.CL), position = pd) +
  ylab("Rating") + xlab("") + 
  scale_colour_brewer(palette = "Set2") + theme_bw(base_size = 12) + theme(panel.grid = element_blank()) +
  labs(subtitle = "b (vignette subset E)") +
  coord_cartesian(ylim = c(2.25,4.75)) +
  theme(legend.position="bottom", legend.margin=margin(0,0,0,0), legend.box.margin=margin(-20,0,0,0), legend.title = element_blank())
p_h2_mm


# H3: Gender bias affects negatively perceived recklessness of female extreme sports participants.

# hold other variables fixed at baseline levels
h3data <- data %>% filter(dependents == "No dependents", charity == "For self", low_competence == "High competence", 
                          guide == "No guide", extreme_risk == "Baseline risk") 

aov_h3d <- aov(log(Q5_1) ~ part_gender + vig_gender, data = h3data)
summary(aov_h3d)
emmeans(aov_h3d, ~ vig_gender, type = "response", bias.adj = TRUE)

aov_h3d <- aov(log(Q5_1) ~ part_gender * vig_gender, data = h3data)
summary(aov_h3d)
emmeans(aov_h3d, ~ part_gender | vig_gender, type = "response", bias.adj = TRUE)
emmeans(aov_h3d, pairwise ~ part_gender | vig_gender, type = "response", bias.adj = TRUE, sigma = sigma(aov_h3d))
emmeans(aov_h3d, pairwise ~ vig_gender | part_gender, type = "response", bias.adj = TRUE, sigma = sigma(aov_h3d))

aov_h3r <- aov(log(Q6_1) ~ part_gender + vig_gender, data = h3data)
summary(aov_h3r)
emmeans(aov_h3r, ~ vig_gender, type = "response", bias.adj = TRUE)

aov_h3r <- aov(log(Q6_1) ~ part_gender * vig_gender, data = h3data)
summary(aov_h3r)
emmeans(aov_h3r, ~ part_gender | vig_gender, type = "response", bias.adj = TRUE)
emmeans(aov_h3r, pairwise ~ part_gender | vig_gender, adjust = "tukey", type = "response", bias.adj = TRUE, sigma = sigma(aov_h3r))
emmeans(aov_h3r, pairwise ~ vig_gender | part_gender, type = "response", bias.adj = TRUE, sigma = sigma(aov_h3r))

# model means plots
emm_d <- emmeans(aov_h3d, pairwise ~ part_gender | vig_gender, adjust = "tukey", type = "response", bias.adj = TRUE, sigma = sigma(aov_h3d))
emm_r <- emmeans(aov_h3r, pairwise ~ part_gender | vig_gender, adjust = "tukey", type = "response", bias.adj = TRUE, sigma = sigma(aov_h3r))

danger_model_means <- emm_d$emmeans %>% data.frame() %>% mutate(name = "Danger")
reck_model_means <- emm_r$emmeans %>% data.frame() %>% mutate(name = "Recklessness")
model_means <- rbind(danger_model_means, reck_model_means)
pd <- position_dodge(0.5)
p_h3_mm <- model_means %>% 
  ggplot(aes(x = name, y = response, colour = part_gender)) +
  facet_grid(. ~ vig_gender) +
  geom_point(size = 3, position = pd) +
  geom_linerange(aes(ymin = lower.CL, ymax = upper.CL), position = pd) +
  ylab("Rating") + xlab("") + 
  scale_colour_brewer(palette = "Set2") + theme_bw(base_size = 12) + theme(panel.grid = element_blank()) +
  labs(subtitle = "c (vignette subset A)") +
  coord_cartesian(ylim = c(2.25,4.75)) +
  theme(legend.position="bottom", legend.margin=margin(0,0,0,0), legend.box.margin=margin(-20,0,0,0), legend.title = element_blank())
p_h3_mm

# Plot d: Recklessness judgements of sports participants is partly moderated by whether or not they have dependants.

# hold other variables fixed at baseline levels
h4data <- data %>% filter(charity == "For self", low_competence == "High competence", 
                          guide == "No guide", extreme_risk == "Baseline risk") %>% filter(sport %in% c("skitouring"))

# effect size
cohen.d(Q6_1~factor(dependents), data = h4data)

# aov on log-transformed response
aov_h4d <- aov(log(Q5_1) ~ dependents, data = h4data)
summary(aov_h4d)
emmeans(aov_h4d, ~ dependents, type = "response", bias.adj = TRUE)

aov_h4r <- aov(log(Q6_1) ~ dependents, data = h4data)
summary(aov_h4r)
emmeans(aov_h4r, ~ dependents, type = "response", bias.adj = TRUE)

# model means plots
emm_d <- emmeans(aov_h4d, pairwise ~ dependents, adjust = "tukey", type = "response", bias.adj = TRUE, sigma = sigma(aov_h4d))
emm_r <- emmeans(aov_h4r, pairwise ~ dependents, adjust = "tukey", type = "response", bias.adj = TRUE, sigma = sigma(aov_h4r))

danger_model_means <- emm_d$emmeans %>% data.frame() %>% mutate(name = "Danger")
reck_model_means <- emm_r$emmeans %>% data.frame() %>% mutate(name = "Recklessness")
model_means <- rbind(danger_model_means, reck_model_means)
pd <- position_dodge(0.5)
p_h4_mm <- model_means %>% 
  ggplot(aes(x = name, y = response, colour = dependents)) + 
  geom_point(size = 3, position = pd) +
  geom_linerange(aes(ymin = lower.CL, ymax = upper.CL), position = pd) +
  ylab("Rating") + xlab("") + 
  scale_colour_brewer(palette = "Set2") + theme_bw(base_size = 12) + theme(panel.grid = element_blank()) +
  labs(subtitle = "d (vignette subset B)") +
  coord_cartesian(ylim = c(2.25,4.75)) +
  theme(legend.position="bottom", legend.margin=margin(0,0,0,0), legend.box.margin=margin(-20,0,0,0), legend.title = element_blank())
p_h4_mm

# Plot e: Danger judgements are not moderated by whether the sports participant is engaging in the sport for charitable purposes. 
# Recklessness judgements may well be moderated by whether the sports participant is engaging in the sport for charitable purposes. 

# hold other variables fixed at baseline levels
h5data <- data %>% filter(dependents == "No dependents", low_competence == "High competence", 
                          guide == "No guide", extreme_risk == "Baseline risk") %>% filter(sport %in% c("skitouring"))

# effect size
cohen.d(Q5_1~factor(charity), data = h5data)
cohen.d(Q6_1~factor(charity), data = h5data)

# aov on log-transformed response
aov_h5d <- aov(log(Q5_1) ~ charity, data = h5data)
summary(aov_h5d)
emmeans(aov_h5d, ~ charity, type = "response", bias.adj = TRUE)

aov_h5r <- aov(log(Q6_1) ~ charity, data = h5data)
summary(aov_h5r)
emmeans(aov_h5r, ~ charity, type = "response", bias.adj = TRUE)

# model means plots
emm_d <- emmeans(aov_h5d, pairwise ~ charity, adjust = "tukey", type = "response", bias.adj = TRUE, sigma = sigma(aov_h5d))
emm_r <- emmeans(aov_h5r, pairwise ~ charity, adjust = "tukey", type = "response", bias.adj = TRUE, sigma = sigma(aov_h5r))

danger_model_means <- emm_d$emmeans %>% data.frame() %>% mutate(name = "Danger")
reck_model_means <- emm_r$emmeans %>% data.frame() %>% mutate(name = "Recklessness")
model_means <- rbind(danger_model_means, reck_model_means)
pd <- position_dodge(0.5)
p_h5_mm <- model_means %>% 
  ggplot(aes(x = name, y = response, colour = charity)) + 
  geom_point(size = 3, position = pd) +
  geom_linerange(aes(ymin = lower.CL, ymax = upper.CL), position = pd) +
  ylab("Rating") + xlab("") + 
  scale_colour_brewer(palette = "Set2") + theme_bw(base_size = 12) + theme(panel.grid = element_blank()) +
  labs(subtitle = "e (vignette subset C)") +
  coord_cartesian(ylim = c(2.25,4.75)) +
  theme(legend.position="bottom", legend.margin=margin(0,0,0,0), legend.box.margin=margin(-20,0,0,0), legend.title = element_blank())
p_h5_mm

# Plot f: Danger and Recklessness judgements are moderated by whether the sports participant is highly skilled and competent, or the sport 
# participant is assisted by a professional.

# hold other variables fixed at baseline levels
h6data <- data %>% filter(dependents == "No dependents", charity == "For self", extreme_risk == "Baseline risk") %>%
  filter(sport %in% c("skitouring"))

# aov on log-transformed response
aov_h6d <- aov(log(Q5_1) ~ compguide, data = h6data)
summary(aov_h6d)
emmeans(aov_h6d, pairwise ~ compguide, adjust = "tukey", type = "response", bias.adj = TRUE, sigma = sigma(aov_h6d))

aov_h6r <- aov(log(Q6_1) ~ compguide, data = h6data)
summary(aov_h6r)
emmeans(aov_h6r, pairwise ~ compguide, adjust = "tukey", type = "response", bias.adj = TRUE, sigma = sigma(aov_h6d))

# model means plots
emm_d <- emmeans(aov_h6d, pairwise ~ compguide, adjust = "tukey", type = "response", bias.adj = TRUE, sigma = sigma(aov_h6d))
emm_r <- emmeans(aov_h6r, pairwise ~ compguide, adjust = "tukey", type = "response", bias.adj = TRUE, sigma = sigma(aov_h6r))

danger_model_means <- emm_d$emmeans %>% data.frame() %>% mutate(name = "Danger")
reck_model_means <- emm_r$emmeans %>% data.frame() %>% mutate(name = "Recklessness")
model_means <- rbind(danger_model_means, reck_model_means)
pd <- position_dodge(0.5)
p_h6_mm <- model_means %>% 
  ggplot(aes(x = name, y = response, colour = compguide)) + 
  geom_point(size = 3, position = pd) +
  geom_linerange(aes(ymin = lower.CL, ymax = upper.CL), position = pd) +
  ylab("Rating") + xlab("") + 
  scale_colour_brewer(palette = "Set2") + theme_bw(base_size = 12) + theme(panel.grid = element_blank()) +
  labs(subtitle = "f (vignette subset D)") +
  coord_cartesian(ylim = c(2.25,4.75)) +
  theme(legend.position="bottom", legend.margin=margin(0,0,0,0), legend.box.margin=margin(-20,0,0,0), legend.title = element_blank())
p_h6_mm

# combine plots into one
p2 <- p_h1_mm + p_h2_mm + p_h3_mm + p_h4_mm + p_h5_mm + p_h6_mm + plot_layout(ncol = 2)
p2

# uncomment to save
ggsave("output/all-tests-no-insurance.png", p2, width=11, height=11, dpi = 300)

### Additional comparisons and small analyses

# Do people who do extreme sports rate their own less risky
ownsport <- data %>% 
  filter(sport != "hillwalking") %>%
  pivot_longer(cols = c("Q5_1", "Q6_1")) %>%
  mutate(name = factor(name, levels = c("Q5_1", "Q6_1"), labels = c("Danger", "Recklessness"))) %>%
  filter(dependents == "No dependents", charity == "For self", low_competence == "High competence", 
         guide == "No guide", extreme_risk == "Baseline risk") %>%
  mutate(do_sport = case_when(
    sport == "golf" & Q8_1 == 1 ~ TRUE,
    sport == "skitouring" & Q8_2 == 1 ~ TRUE,
    sport == "climbing" & Q8_3 ==  1~ TRUE,
    sport == "surfing" & Q8_4 == 1 ~ TRUE,
    sport == "hillwalking" & Q8_5 == 1 ~ TRUE,
    sport == "running" & Q8_6 == 1 ~ TRUE,
    TRUE ~ FALSE)) %>%
  mutate(extr_sport = ifelse(sport %in% c("surfing", "climbing", "skitouring"), TRUE, FALSE)) %>% 
  mutate(extr_sport = factor(extr_sport, levels = c(FALSE, TRUE), 
                             labels = c("golf, \n running", "climbing, surfing,\n skitouring")),
         do_sport = factor(do_sport, levels = c(FALSE, TRUE), labels = c("Participant does not do this sport", "Participant does this sport"))) %>%
                               ggplot(aes(x = extr_sport, y = value, colour = does_risky)) + 
  stat_summary(fun.y = "mean", geom = "point", position = pd) + 
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", position = pd) + 
  ylab("Rating") + xlab("") + 
  facet_grid(.~ name) +
  scale_colour_brewer(palette = "Set2") + theme_bw(base_size = 12) + theme(panel.grid = element_blank()) +
  theme(legend.position="bottom", legend.margin=margin(0,0,0,0), legend.box.margin=margin(-20,0,0,0), legend.title = element_blank())
ownsport

# modelling

# hold other variables fixed at baseline levels
os_data <- data %>% 
  # remove hillwalking as neither extreme nor non-ext
  filter(sport != "hillwalking") %>%
  filter(dependents == "No dependents", charity == "For self", low_competence == "High competence", 
         guide == "No guide", extreme_risk == "Baseline risk") %>%
  mutate(do_sport = case_when(
    sport == "golf" & Q8_1 == 1 ~ TRUE,
    sport == "skitouring" & Q8_2 == 1 ~ TRUE,
    sport == "climbing" & Q8_3 ==  1 ~ TRUE,
    sport == "surfing" & Q8_4 == 1 ~ TRUE,
    sport == "hillwalking" & Q8_5 == 1 ~ TRUE,
    sport == "running" & Q8_6 == 1 ~ TRUE,
    TRUE ~ FALSE)) %>%
  mutate(extr_sport = ifelse(sport %in% c("surfing", "climbing", "skitouring"), TRUE, FALSE)) 

m9d <- lm(log(Q5_1) ~ sport * do_sport, data = os_data)
summary(m9d)
m9r <- lm(log(Q6_1) ~ sport * do_sport, data = os_data)
summary(m9r)

### Presentation stuff -- putting into convenient form for latex

aov_d <- list(aov_h1d, aov_h2d, aov_h3d, aov_h4d, aov_h5d, aov_h6d)

res_d <- c()
vars_d <- c()
hyp_d <- c()
for(i in 1:length(aov_d)){
  varnames <- row.names(summary(aov_d[[i]])[[1]])
  nvars <- length(summary(aov_d[[i]])[[1]]$`F value`)
  for(j in 1:(nvars-1)){
    this_var <- trimws(varnames[j])
    this_res <- paste0("$F_{", 
                       summary(aov_d[[i]])[[1]]$`Df`[j], ",", summary(aov_d[[i]])[[1]]$`Df`[nvars], "} = ",
                       round(summary(aov_d[[i]])[[1]]$`F value`[j],1), ", p = ", round(summary(aov_d[[i]])[[1]]$`Pr(>F)`[j], 3),"$")
    res_d <- c(res_d, this_res)  
    vars_d <- c(vars_d, this_var)
    hyp_d <- c(hyp_d, i)
  }
}

aov_r <- list(aov_h1r, aov_h2r, aov_h3r, aov_h4r, aov_h5r, aov_h6r)

res_r <- c()
vars_r <- c()
hyp_r <- c()
for(i in 1:length(aov_r)){
  varnames <- row.names(summary(aov_r[[i]])[[1]])
  nvars <- length(summary(aov_r[[i]])[[1]]$`F value`)
  for(j in 1:(nvars-1)){
    this_var <- trimws(varnames[j])
    this_res <- paste0("$F_{", 
                       summary(aov_r[[i]])[[1]]$`Df`[j], ",", summary(aov_r[[i]])[[1]]$`Df`[nvars], "} = ",
                       round(summary(aov_r[[i]])[[1]]$`F value`[j],1), ", p = ", round(summary(aov_r[[i]])[[1]]$`Pr(>F)`[j], 3),"$")
    res_r <- c(res_r, this_res)  
    vars_r <- c(vars_r, this_var)
    hyp_r <- c(hyp_r, i)
  }
}

# summarise hypotheses 1-6
hyp1_6 <- data.frame(outcome = c(rep("Danger", 9), rep("Recklessness", 9)),
                     hypothesis = c(hyp_d, hyp_r), varnms = c(vars_d, vars_r), fortex = c(res_d, res_r))
hyp1_6