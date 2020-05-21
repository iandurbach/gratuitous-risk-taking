library(tidyverse)
library(patchwork)
library(emmeans)
library(stringr)
library(ks)
library(DescTools)
library(stargazer)

data <- readRDS("output/sportrisk_cleaned.Rds")

### Cleaning data

# renaming factor levels to descriptive labels
data$Q3 <- factor(data$Q3, levels = c(1,2), labels = c("female", "male"))
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

# H1: Extreme sports are generally regarded as more dangerous and reckless to engage in 
# than more mundane activities even when fatality rates are the same.

# plot
p_h1 <- data %>% 
  pivot_longer(cols = c("Q5_1", "Q6_1")) %>%
  mutate(name = factor(name, levels = c("Q5_1", "Q6_1"), labels = c("Danger", "Recklessness"))) %>%
  filter(dependents == "No dependents", charity == "For self", low_competence == "High competence", 
         guide == "No guide", extreme_risk == "Baseline risk") %>%
  ggplot(aes(x = sport, y = value, colour = name)) + 
  stat_summary(fun.y = "mean", geom = "point", position = pd) + 
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", position = pd) + 
  ylab("Rating") + xlab("") + 
  scale_colour_brewer(palette = "Set2") + theme_bw(base_size = 12) + theme(panel.grid = element_blank()) +
  #labs(title = "Hypothesis 1", subtitle = "Supported") +
  labs(subtitle = "Hypothesis 1") +
  theme(legend.position="bottom", legend.margin=margin(0,0,0,0), legend.box.margin=margin(-20,0,0,0), legend.title = element_blank())
p_h1

# modelling

# hold other variables fixed at baseline levels
h1data <- data %>% filter(dependents == "No dependents", charity == "For self", low_competence == "High competence", 
                          guide == "No guide", extreme_risk == "Baseline risk") 

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

# H2: Variation of fatality rates for a given activity has little impact on danger and recklessness judgements
p_h2 <- data %>% 
  filter(dependents == "No dependents", charity == "For self", low_competence == "High competence", 
         guide == "No guide") %>%
  filter(sport %in% c("skitouring")) %>%
  pivot_longer(cols = c("Q5_1", "Q6_1")) %>%
  mutate(name = factor(name, levels = c("Q5_1", "Q6_1"), labels = c("Danger", "Recklessness"))) %>%
  ggplot(aes(x = name, y = value, colour = extreme_risk)) + 
  stat_summary(fun.y = "mean", geom = "point", position = pd) + 
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", position = pd) + 
  ylab("Rating") + xlab("") + 
  scale_colour_brewer(palette = "Set2") + theme_bw(base_size = 12) + theme(panel.grid = element_blank()) +
  #labs(title = "Hypothesis 2", subtitle = "Supported*") +
  labs(subtitle = "Hypothesis 2") +
  coord_cartesian(ylim = c(2.35,4.65)) +
  theme(legend.position="bottom", legend.margin=margin(0,0,0,0), legend.box.margin=margin(-20,0,0,0), legend.title = element_blank())

p_h2

# modelling

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

# H3: Gender bias affects negatively perceived recklessness of female extreme sports participants.
p_h3 <- data %>% 
  filter(!is.na(part_gender)) %>%
  pivot_longer(cols = c("Q5_1", "Q6_1")) %>%
  mutate(name = factor(name, levels = c("Q5_1", "Q6_1"), labels = c("Danger", "Recklessness"))) %>%
  filter(dependents == "No dependents", charity == "For self", low_competence == "High competence", 
         guide == "No guide", extreme_risk == "Baseline risk") %>%
  ggplot(aes(x = name, y = value, colour = part_gender)) + 
  facet_grid(.~vig_gender) +
  stat_summary(fun.y = "mean", geom = "point", position = pd) + 
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", position = pd) + 
  ylab("Rating") + xlab("") + 
  scale_colour_brewer(palette = "Set2") + theme_bw(base_size = 12) + theme(panel.grid = element_blank()) +
  labs(subtitle = "Hypothesis 3") +
  coord_cartesian(ylim = c(2.25,4.65)) +
  theme(legend.position="bottom", legend.margin=margin(0,0,0,0), legend.box.margin=margin(-20,0,0,0), legend.title = element_blank())

p_h3

# modelling

# hold other variables fixed at baseline levels
h3data <- data %>% filter(dependents == "No dependents", charity == "For self", low_competence == "High competence", 
                          guide == "No guide") 

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

# H4: Recklessness judgements of sports participants is partly moderated by whether or not they have dependants.

p_h4 <- data %>% 
  pivot_longer(cols = c("Q5_1", "Q6_1")) %>%
  mutate(name = factor(name, levels = c("Q5_1", "Q6_1"), labels = c("Danger", "Recklessness"))) %>%
  filter(charity == "For self", low_competence == "High competence", 
         guide == "No guide", extreme_risk == "Baseline risk") %>%
  # note also have running here
  filter(sport %in% c("skitouring")) %>%
  ggplot(aes(x = name, y = value, colour = dependents)) +
  stat_summary(fun.y = "mean", geom = "point", position = pd) + 
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", position = pd) + 
  ylab("Rating") + xlab("") + 
  scale_colour_brewer(palette = "Set2") + theme_bw(base_size = 12) + theme(panel.grid = element_blank()) +
  labs(subtitle = "Hypothesis 4") +
  coord_cartesian(ylim = c(2.35,4.65)) +
  theme(legend.position="bottom", legend.margin=margin(0,0,0,0), legend.box.margin=margin(-20,0,0,0), legend.title = element_blank())

p_h4

# modelling

# hold other variables fixed at baseline levels
h4data <- data %>% filter(charity == "For self", low_competence == "High competence", 
                          guide == "No guide", extreme_risk == "Baseline risk") %>% filter(sport %in% c("skitouring"))

# aov on log-transformed response
aov_h4d <- aov(log(Q5_1) ~ dependents, data = h4data)
summary(aov_h4d)
emmeans(aov_h4d, ~ dependents, type = "response", bias.adj = TRUE)

aov_h4r <- aov(log(Q6_1) ~ dependents, data = h4data)
summary(aov_h4r)
emmeans(aov_h4r, ~ dependents, type = "response", bias.adj = TRUE)

# H5a: Danger judgements are not moderated by whether the sports participant is engaging in the sport for charitable purposes.
# H5b: Recklessness judgements may well be moderated by whether the sports participant is engaging in the sport for charitable purposes. 

p_h5 <- data %>% 
  pivot_longer(cols = c("Q5_1", "Q6_1")) %>%
  mutate(name = factor(name, levels = c("Q5_1", "Q6_1"), labels = c("Danger", "Recklessness"))) %>%
  filter(dependents == "No dependents", low_competence == "High competence", 
         guide == "No guide", extreme_risk == "Baseline risk") %>%
  filter(sport %in% c("skitouring")) %>%
  ggplot(aes(x = name, y = value, colour = charity)) + 
  stat_summary(fun.y = "mean", geom = "point", position = pd) + 
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", position = pd) + 
  ylab("Rating") + xlab("") + 
  scale_colour_brewer(palette = "Set2") + theme_bw(base_size = 12) + theme(panel.grid = element_blank()) +
  labs(subtitle = "Hypothesis 5a & 5b") +
  coord_cartesian(ylim = c(2.35,4.65)) +
  theme(legend.position="bottom", legend.margin=margin(0,0,0,0), legend.box.margin=margin(-20,0,0,0), legend.title = element_blank())

p_h5

# modelling

# hold other variables fixed at baseline levels
h5data <- data %>% filter(dependents == "No dependents", low_competence == "High competence", 
                          guide == "No guide", extreme_risk == "Baseline risk") %>% filter(sport %in% c("skitouring"))

# aov on log-transformed response
aov_h5d <- aov(log(Q5_1) ~ charity, data = h5data)
summary(aov_h5d)
emmeans(aov_h5d, ~ charity, type = "response", bias.adj = TRUE)

aov_h5r <- aov(log(Q6_1) ~ charity, data = h5data)
summary(aov_h5r)
emmeans(aov_h5r, ~ charity, type = "response", bias.adj = TRUE)

# H6: Danger and Recklessness judgements are moderated by whether the sports participant is highly skilled and competent, or the sport 
# participant is assisted by a professional.

p_h6 <- data %>% 
  pivot_longer(cols = c("Q5_1", "Q6_1")) %>%
  mutate(name = factor(name, levels = c("Q5_1", "Q6_1"), labels = c("Danger", "Recklessness"))) %>%
  filter(dependents == "No dependents", charity == "For self", extreme_risk == "Baseline risk") %>%
  filter(sport %in% c("skitouring")) %>%
  ggplot(aes(x = name, y = value, colour = compguide)) + 
  stat_summary(fun.y = "mean", geom = "point", position = pd) + 
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", position = pd) + 
  ylab("Rating") + xlab("") + 
  scale_colour_brewer(palette = "Set2") + theme_bw(base_size = 12) + theme(panel.grid = element_blank()) +
  labs(subtitle = "Hypothesis 6") +
  coord_cartesian(ylim = c(2.35,4.65)) +
  theme(legend.position="bottom", legend.margin=margin(0,0,0,0), legend.box.margin=margin(-20,0,0,0), legend.title = element_blank())

p_h6

# modelling

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

# H7a: Whether or not sports participants need to purchase additional life-insurance is predicted by relevant perceived danger and recklessness judgement.
# H7b: The amount of additional life-insurance premium is predicted by the degree to which the relevant sport is considered dangerous and reckless.

# dataset for yes/no response -- remove any 'don't know' and any extreme risk 
data_ins <- data %>% 
  replace_na(list(Q7_a = 0)) %>% filter(Q7 != 3) %>% mutate(Q7 = 2 - Q7) %>%
  filter(extreme_risk == "Baseline risk") 

# dataset for amount of RTP is only for those who answered "Yes" above
# remove anyone who said > 1000 pounds
data_insY <- data_ins %>% filter(Q7_a > 0) %>% filter(Q7_a <= 1000) %>%
  filter(extreme_risk == "Baseline risk")

# plot contour lines showing 90% data range; beware interpreting outside these lines!
# https://stackoverflow.com/questions/23437000/how-to-plot-a-contour-line-showing-where-95-of-values-fall-within-in-r-and-in
set.seed(111)
# rm NAs in D and R ratings
cdata <- data_ins %>% select(Q5_1, Q6_1) %>% filter(complete.cases(.))
d <- data.frame(x = cdata$Q5_1 + rnorm(nrow(cdata), 0, 0.15), y = cdata$Q6_1 + rnorm(nrow(cdata), 0, 0.15))
kd <- ks::kde(d, compute.cont=TRUE)
contour_90 <- with(kd, contourLines(x=eval.points[[1]], y=eval.points[[2]], z=estimate, levels=cont["10%"])[[1]])
contour_90 <- data.frame(contour_90)

set.seed(111)
# rm NAs in D and R ratings
cdataY <- data_insY %>% select(Q5_1, Q6_1) %>% filter(complete.cases(.))
d <- data.frame(x = cdataY$Q5_1 + rnorm(nrow(cdataY), 0, 0.15), y = cdataY$Q6_1 + rnorm(nrow(cdataY), 0, 0.15))
kd <- ks::kde(d, compute.cont=TRUE)
contour_90Y <- with(kd, contourLines(x=eval.points[[1]], y=eval.points[[2]], z=estimate, levels=cont["10%"])[[1]])
contour_90Y <- data.frame(contour_90Y)

# here we're plotting predictions, so need to fit models first

# RTP01 ~ danger/recklessness 

# fit
mod0 <- glm(Q7 ~ (Q6_1*Q5_1), data = data_ins, family = "binomial")
summary(mod0)
# odds ratios
exp(cbind(coef(mod0), confint(mod0)))

# these are to get the effects quoted for 1-unit increases in D 
preddata <- expand.grid(Q5_1 = seq(from = 1, to = 3, by = 1), Q6_1 = c(1,mean(data$Q6_1)))
preds <- predict(mod0, newdata = preddata, se = TRUE, type = "response")
preddata <- preddata %>% mutate(pred = preds$fit, se = preds$se.fit)
preddata %>% mutate(logitpred = pred/(1-pred), logitpred_lci = (pred - 2*se)/ (1 - (pred - 2*se)),
                    logitpred_uci = (pred + 2*se)/ (1 - (pred + 2*se))) %>%
  group_by(Q6_1) %>% mutate(or = logitpred / lag(logitpred),
                            or_lci = logitpred_lci / lag(logitpred_lci),
                            or_uci = logitpred_uci / lag(logitpred_uci))

# these are to get the effects quoted for 1-unit increases in R
preddata <- expand.grid(Q6_1 = seq(from = 1, to = 3, by = 1), Q5_1 = c(1,mean(data$Q5_1)))
preds <- predict(mod0, newdata = preddata, se = TRUE, type = "response")
preddata <- preddata %>% mutate(pred = preds$fit, se = preds$se.fit)
preddata %>% mutate(logitpred = pred/(1-pred), logitpred_lci = (pred - 2*se)/ (1 - (pred - 2*se)),
                    logitpred_uci = (pred + 2*se)/ (1 - (pred + 2*se))) %>%
  group_by(Q5_1) %>% mutate(or = logitpred / lag(logitpred),
                            or_lci = logitpred_lci / lag(logitpred_lci),
                            or_uci = logitpred_uci / lag(logitpred_uci))

# these are the plotted predictions in the main figure
preddata <- expand.grid(Q5_1 = seq(from = 1, to = 9, by = 0.05),
                        Q6_1 = seq(from = 1, to = 9, by = 0.05))

preds <- predict(mod0, newdata = preddata, se = TRUE, type = "response")
preddata <- preddata %>% mutate(pred = preds$fit, se = preds$se.fit)

# plot
p_h7a <- preddata %>% 
  ggplot(aes(x = Q5_1, y = Q6_1)) + 
  geom_tile(aes(colour = pred, fill = pred)) +
  geom_jitter(data = data_ins, colour = "black", width = 0.05, height = 0.05, alpha = 0.05) +
  geom_path(data = contour_90, aes(x = x, y = y)) +
  scale_fill_distiller(name = "P(insure)", palette = "RdBu", limits = c(0,1)) + 
  scale_colour_distiller(name = "P(insure)", palette = "RdBu", limits = c(0,1)) +
  coord_cartesian(xlim = c(1,8), ylim = c(1,8), expand = FALSE) + theme_bw(base_size = 12) +
  xlab("Danger") + ylab("Recklessness") + 
  labs(subtitle = "Hypothesis 7a") +
  theme(legend.position="bottom", legend.margin=margin(0,0,0,0), legend.box.margin=margin(0,0,0,0), legend.key.width=unit(1.5,"cm"))
p_h7a

# RTP Amount ~ danger/recklessness 
# log transform sucks in the tail

# fit
mod1 <- lm(log(Q7_a) ~ (Q6_1+Q5_1), data = data_insY)
summary(mod1)

# these are to get the effects quoted for 1-unit increases in D 
preddata <- expand.grid(Q5_1 = seq(from = 1, to = 3, by = 1),
                        Q6_1 = c(1,mean(data$Q6_1)))
preds <- predict(mod1, newdata = preddata, se = TRUE, type = "response")
preddata <- preddata %>% mutate(pred = preds$fit, se = preds$se.fit)
preddata %>% mutate(exppred = exp(pred), exppred_lci = exp(pred - 2*se),
                    exppred_uci = exp(pred + 2*se)) %>%
  group_by(Q6_1) %>% mutate(or = exppred / lag(exppred),
                            or_lci = exppred_lci / lag(exppred_lci),
                            or_uci = exppred_uci / lag(exppred_uci))

# these are to get the effects quoted for 1-unit increases in R
preddata <- expand.grid(Q6_1 = seq(from = 1, to = 3, by = 1),
                        Q5_1 = c(1,mean(data$Q5_1)))
preds <- predict(mod1, newdata = preddata, se = TRUE, type = "response")
preddata <- preddata %>% mutate(pred = preds$fit, se = preds$se.fit)
preddata %>% mutate(exppred = exp(pred), exppred_lci = exp(pred - 2*se),
                    exppred_uci = exp(pred + 2*se)) %>%
  group_by(Q5_1) %>% mutate(or = exppred / lag(exppred),
                            or_lci = exppred_lci / lag(exppred_lci),
                            or_uci = exppred_uci / lag(exppred_uci))

# these are the plotted predictions
preddata1 <- expand.grid(Q5_1 = seq(from = 1, to = 8, by = 0.05),
                         Q6_1 = seq(from = 1, to = 8, by = 0.05))
preds1 <- predict(mod1, newdata = preddata1, se = TRUE, type = "response")
preddata1 <- preddata1 %>% mutate(pred = preds1$fit, se = preds1$se.fit)

# plot
p_h7b <- preddata1 %>% 
  ggplot(aes(x = Q5_1, y = Q6_1)) + 
  geom_tile(aes(colour = exp(pred), fill = exp(pred))) +
  geom_jitter(data = data_insY, colour = "black", width = 0.05, height = 0.05, alpha = 0.05) +
  #geom_density_2d(data = data_ins, colour = "black", alpha = 0.5, h = 4, bins = 5) +
  geom_path(data = contour_90Y, aes(x = x, y = y)) +
  scale_fill_distiller(name = "Recommended premium", palette = "RdBu", limits = c(40,205)) + 
  scale_colour_distiller(name = "Recommended premium", palette = "RdBu", limits = c(40,205)) +
  coord_cartesian(xlim = c(1,8), ylim = c(1,8), expand = FALSE) + theme_bw(base_size = 12) +
  xlab("Danger") + ylab("Recklessness") + 
  #labs(title = "Hypothesis 8", subtitle = "Supported") +
  labs(subtitle = "Hypothesis 7b") +
  theme(legend.position="bottom", legend.margin=margin(0,0,0,0), legend.box.margin=margin(0,0,0,0), legend.key.width=unit(1.5,"cm"))
p_h7b

# combine plots into one
p <- p_h1 + p_h2 + p_h3 + p_h4 + p_h5 + p_h6 + p_h7a + p_h7b + plot_layout(ncol = 2)
p

# uncomment to save
# ggsave("all-tests.png", p, width=11, height=13, dpi = 300)

### Additional comparisons and small analyses

## assessing effect on insurance of baseline vs extreme risk

# effect on RTP01 of shift in D and R
preddata <- data.frame(Q6_1 = c(2,5), Q5_1 = c(2,5))
preds <- predict(mod0, newdata = preddata, se = TRUE, type = "response")
preddata <- preddata %>% mutate(pred = preds$fit, se = preds$se.fit)
preddata %>% mutate(odds = pred/(1-pred))

# effect on RTP amonut of shift in D and R
preddata <- data.frame(Q6_1 = c(2,5), Q5_1 = c(2,5))
preds <- predict(mod1, newdata = preddata, se = TRUE, type = "response")
preddata <- preddata %>% mutate(pred = preds$fit, se = preds$se.fit)
preddata %>% mutate(odds = exp(pred))

# effect on RTP01 of shift from base to extreme risk
inv.logit = function(x){exp(x) / (1 + exp(x))}
# here we just want to compare baseline to extreme, so set other variables to baseline
mod02 <- glm(Q7 ~ extreme_risk, data = h2data %>% filter(Q7 != 3) %>% mutate(Q7 = 2 - Q7), family = "binomial")
summary(mod02)
# odds of saying yes for baseline risk
logit_br <- predict(mod02, data.frame(extreme_risk = "Baseline risk"))
p_br <- inv.logit(logit_br)
odds_br <- p_br / (1 - p_br)
# odds of saying yes for extreme risk
logit_er <- predict(mod02, data.frame(extreme_risk = "Extreme risk"))
p_er <- inv.logit(logit_er)
odds_er <- p_er / (1 - p_er)
# odds ratio
odds_er/odds_br
# predicted probs
emmeans(mod02,  ~ extreme_risk, type = "response", bias.adj = TRUE)

# same steps for effect on RTP AMmount of shift from base to extreme risk
mod12 <- lm(log(Q7_a) ~ extreme_risk, data = h2data %>% filter(Q7 != 3, Q7_a > 0, Q7_a <= 1000))
summary(mod12)

amt_br <- exp(predict(mod12, data.frame(extreme_risk = "Baseline risk")))
amt_er <- exp(predict(mod12, data.frame(extreme_risk = "Extreme risk")))
amt_br
amt_er
amt_er/amt_br

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

m9d <- lm(log(Q5_1) ~ extr_sport * do_sport, data = os_data)
summary(m9d)
m9r <- lm(log(Q6_1) ~ extr_sport * do_sport, data = os_data)
summary(m9r)

emmeans(m9d, ~ do_sport | extr_sport, type = "response", bias.adj = TRUE)
emmeans(m9d, pairwise ~ do_sport | extr_sport, type = "response", bias.adj = TRUE, sigma = sigma(m9d))
emmeans(m9r, ~ do_sport | extr_sport, type = "response", bias.adj = TRUE)
emmeans(m9r, pairwise ~ do_sport | extr_sport, type = "response", bias.adj = TRUE, sigma = sigma(m9r))

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

# hyp 7-8 just do manually
summary(mod0)
summary(mod1)

# or 
xx <- summary(mod0)
hyp_7a <- str_c(rownames(xx$coefficients), ": ", round(xx$coefficients[,1], 3), " (", round(xx$coefficients[,2], 3), "), z = ", 
               round(xx$coefficients[,3], 1), ", p = ", round(xx$coefficients[,4], 3))
xx <- summary(mod1)
hyp_7b <- str_c(rownames(xx$coefficients), ": ", round(xx$coefficients[,1], 3), " (", round(xx$coefficients[,2], 3), "), z = ", 
               round(xx$coefficients[,3], 1), ", p = ", round(xx$coefficients[,4], 3))
hyp_7a
hyp_7b

stargazer(mod0, mod1, title="Regression Results",
          dep.var.labels=c("RTP Any","log(RTP Amount)"),
          covariate.labels=c("Recklessness","Danger", "Interaction"), digits = 2, 
          omit.stat=c("LL","ser","f"), ci=TRUE, ci.level=0.90, single.row=TRUE)

PseudoR2(mod0, which = c("Cox", "Nagel", "McFadden"))
