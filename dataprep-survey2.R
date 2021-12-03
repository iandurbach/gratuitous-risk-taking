# Data preprocessing to be run before analysis of Survey 2 responses in "Differences in expert and layperson’s danger and recklessness judgments 
# about adventure sports participation" by Philip Ebert and Ian Durbach (2021).
# Analysis code in "differences-analysis-survey2.R"
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

# Outgroup

data <- readRDS("output/sportrisks_cleaned.Rds")

### Cleaning data

# renaming factor levels to descriptive labels
data$Q3 <- factor(data$Q3, levels = c(1,2), labels = c("female", "male"))
data$sport <- factor(data$sport, levels = c("golf", "running", "hillwalking", "surfing", "skitouring", "climbing"))
data$dependents <- factor(data$dependents, levels = c(FALSE, TRUE), labels = c("No dependents" ,"Has dependents"))
data$charity <- factor(data$charity, levels = c(FALSE, TRUE), labels = c("For self" ,"For charity"))
data$low_competence <- factor(data$low_competence, levels = c(TRUE, FALSE), labels = c("Low competence" ,"High competence"))
data$guide <- factor(data$guide, levels = c(FALSE, TRUE), labels = c("No guide" ,"Has guide"))
data$extreme_risk <- factor(data$extreme_risk, levels = c(FALSE, TRUE), labels = c("Baseline risk" ,"Extreme risk"))

data_red_wskiers <- data %>% rename(danger = Q5_1, conf_danger = Q5_a_1, recklessness = Q6_1, conf_reck = Q6_a_1) %>%
  filter(sport == "skitouring") %>%
  dplyr::select(danger, conf_danger, recklessness, conf_reck, dependents, charity, low_competence, guide, extreme_risk) 

data_red <- data %>% rename(danger = Q5_1, conf_danger = Q5_a_1, recklessness = Q6_1, conf_reck = Q6_a_1) %>%
  filter(sport == "skitouring") %>%
  filter(Q8_2 == FALSE) %>% # no skitourers
  dplyr::select(danger, conf_danger, recklessness, conf_reck, dependents, charity, low_competence, guide, extreme_risk, gender, resp_gender = Q3)

## In group

x <- read_xlsx("data/sais/sais_avalanche.xlsx", trim_ws = TRUE) %>% as.data.frame()

# include only those who agreed
x <- x %>% filter(!is.na(Q51)) %>% filter(str_detect(Q51, "Yes")) 
# and those who ski as primary activity 
# x <- x %>% filter(!is.na(Q2_1)) %>% filter(str_detect(Q2_1, "Primary"))

xl <- data.frame(ski_touring = rep(x$Q2_1, 4),
                 condition = rep(c("baseline", "extreme", "charity", "lackcompetence"), each = nrow(x)),
                 danger = c(x$Q104_baseline, x$Q173_extreme, x$Q184_charity, x$Q206_lackcompetence),
                 conf_danger = c(x$Q105_baseline, x$Q174_extreme, x$Q185_charity, x$Q207_lackcompetence),
                 recklessness = c(x$Q106_baseline, x$Q175_extreme, x$Q186_charity, x$Q208_lackcompetence),
                 conf_reck = c(x$Q107_baseline, x$Q176_extreme, x$Q187_charity, x$Q209_lackcompetence),
                 insurance_yn = c(x$Q108_baseline, x$Q177_extreme, x$Q188_charity, x$Q210_lackcompetence),
                 insurance_amt = c(x$Q109_baseline, x$Q178_extreme, x$Q189_charity, x$Q211_lackcompetence), 
                 resp_gender = rep(x$Q27, 4),
                 gender = "male")

# parse ratings
xl <- xl %>% 
  mutate(ski_touring = na_if(ski_touring, "-99")) %>%
  mutate(ski_touring_primary = str_detect(ski_touring, "Primary")) %>%
  mutate(danger = na_if(danger, "-99")) %>% mutate(danger = str_extract(danger, "(\\d)+")) %>% 
  mutate(recklessness = na_if(recklessness, "-99")) %>% mutate(recklessness = str_extract(recklessness, "(\\d)+")) %>%
  mutate(danger = as.numeric(danger), 
         recklessness = as.numeric(recklessness),
         condition = recode(condition, 
                            `baseline` = "Baseline",
                            `charity` = "Charity",
                            `extreme` = "Extreme risk",
                            `lackcompetence` = "Low competence"),
         conf_danger = recode(conf_danger, 
                              `Not at all confident` = 1,
                              `Slightly confident` = 2,
                              `Moderately confident` = 3,
                              `Very confident` = 4,
                              `Extremely confident` = 5),
         conf_reck = recode(conf_reck, 
                            `Not at all confident` = 1,
                            `Slightly confident` = 2,
                            `Moderately confident` = 3,
                            `Very confident` = 4,
                            `Extremely confident` = 5),
         insurance_yn = recode(insurance_yn, `Yes` = "Yes", `No` = "No", .default = NA_character_))


####### 

xl_in  <- xl %>% filter(!is.na(danger))


# Baseline
x1 <- data_red %>% 
  filter(dependents == "No dependents", charity == "For self", low_competence == "High competence", 
         guide == "No guide", extreme_risk == "Baseline risk") %>% 
  dplyr::select(danger, conf_danger, recklessness, conf_reck, gender, resp_gender) %>%
  mutate(condition = "Baseline", ski_touring_primary = TRUE, sgroup = "outgroup")

# Charity
x2 <- data_red %>% 
  filter(dependents == "No dependents", charity == "For charity", low_competence == "High competence", 
         guide == "No guide", extreme_risk == "Baseline risk") %>% 
  dplyr::select(danger, conf_danger, recklessness, conf_reck, gender, resp_gender) %>%
  mutate(condition = "Charity", ski_touring_primary = TRUE, sgroup = "outgroup")

# Extreme risk
x3 <- data_red %>% 
  filter(dependents == "No dependents", charity == "For self", low_competence == "High competence", 
         guide == "No guide", extreme_risk == "Extreme risk") %>% 
  dplyr::select(danger, conf_danger, recklessness, conf_reck, gender, resp_gender) %>%
  mutate(condition = "Extreme risk", ski_touring_primary = TRUE, sgroup = "outgroup")

# Low competence
x4 <- data_red %>% 
  filter(dependents == "No dependents", charity == "For self", low_competence == "Low competence", 
         guide == "No guide", extreme_risk == "Baseline risk") %>% 
  dplyr::select(danger, conf_danger, recklessness, conf_reck, gender, resp_gender) %>%
  mutate(condition = "Low competence", ski_touring_primary = TRUE, sgroup = "outgroup")

xl_out <- rbind(x1,x2,x3,x4) %>% filter(gender == "male")

xl_in <- xl_in %>% mutate(sgroup = "ingroup") %>%
  dplyr::select(danger, conf_danger, recklessness, conf_reck, condition, ski_touring_primary, gender, resp_gender, sgroup)

xl_all <- rbind(xl_in, xl_out) %>% 
  mutate(resp_gender = recode(resp_gender,
                              `neither category` = NA_character_,
                              `prefer not to say` = NA_character_,
                              `female` = "Female",
                              `male` = "Male"))

save(xl_all, file = "output/combined-expert-layperson-responses-clean.Rdata")
