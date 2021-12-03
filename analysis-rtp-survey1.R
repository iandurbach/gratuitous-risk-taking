# Runs additional "request to pay" analyses reported in "Gratuitous Risk and Recklessness: An experimental and philosophical study" by 
# Philip Ebert, Ian Durbach, and Claire Field (2021).
# Last update 03.12.2021

library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(emmeans)
library(stringr)
library(ks)
library(DescTools)
library(stargazer)
library(effsize)

data <- readRDS("output/sportrisks_cleaned.Rds")

### Cleaning data

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
h2data <- data %>% filter(dependents == "No dependents", charity == "For self", low_competence == "High competence", 
                          guide == "No guide") %>% filter(sport %in% c("skitouring")) 
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

stargazer(mod0, mod1, title="Regression Results",
          dep.var.labels=c("RTP Any","log(RTP Amount)"),
          covariate.labels=c("Recklessness","Danger", "Interaction"), digits = 2, 
          omit.stat=c("LL","ser","f"), ci=TRUE, ci.level=0.90, single.row=TRUE)

PseudoR2(mod0, which = c("Cox", "Nagel", "McFadden"))
