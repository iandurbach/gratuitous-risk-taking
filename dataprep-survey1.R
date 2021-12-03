# Data preprocessing to be run before analysis of Survey 1 responses in "Differences in expert and layperson’s danger and recklessness judgments 
# about adventure sports participation" by Philip Ebert and Ian Durbach (2021).
# Analysis code in "differences-analysis-survey1.R"
# Last update 21.05.2020

library(readxl)
library(dplyr)

# male baseline
climbing <- read_xlsx("data/climbing.xlsx")
golf <- read_xlsx("data/golf.xlsx")
running <- read_xlsx("data/running.xlsx")
skitouring <- read_xlsx("data/skitouring.xlsx")
surfing <- read_xlsx("data/surfing.xlsx")
hillwalking <- read_xlsx("data/hillwalking.xlsx")

climbing$sport <- "climbing"
golf$sport <- "golf"
running$sport <- "running"
skitouring$sport <- "skitouring"
surfing$sport <- "surfing"
hillwalking$sport <- "hillwalking"

data_0 <- rbind(climbing, golf, running, skitouring, surfing, hillwalking)
data_0$gender <- "male"
data_0$dependents <- FALSE
data_0$charity <- FALSE
data_0$low_competence <- FALSE
data_0$guide <- FALSE
data_0$extreme_risk <- FALSE

# female baseline
climbing_1 <- read_xlsx("data/climbing_1.xlsx")
golf_1 <- read_xlsx("data/golf_1.xlsx")
running_1 <- read_xlsx("data/running_1.xlsx")
skitouring_1 <- read_xlsx("data/skitouring_1.xlsx")
surfing_1 <- read_xlsx("data/surfing_1.xlsx")
hillwalking_1 <- read_xlsx("data/hillwalking_1.xlsx")

climbing_1$sport <- "climbing"
golf_1$sport <- "golf"
running_1$sport <- "running"
skitouring_1$sport <- "skitouring"
surfing_1$sport <- "surfing"
hillwalking_1$sport <- "hillwalking"

data_1 <- rbind(climbing_1, golf_1, running_1, skitouring_1, surfing_1, hillwalking_1)
data_1$gender <- "female"
data_1$dependents <- FALSE
data_1$charity <- FALSE
data_1$low_competence <- FALSE
data_1$guide <- FALSE
data_1$extreme_risk <- FALSE

# dependents
running_2 <- read_xlsx("data/running_2.xlsx")[,-2]
skitouring_2 <- read_xlsx("data/skitouring_2.xlsx")[,-2]

data_2 <- rbind(running_2, skitouring_2)
data_2$sport <- c(rep("running", nrow(running_2)), rep("skitouring", nrow(skitouring_2)))
data_2$gender <- "male"
data_2$dependents <- TRUE
data_2$charity <- FALSE
data_2$low_competence <- FALSE
data_2$guide <- FALSE
data_2$extreme_risk <- FALSE

running_3 <- read_xlsx("data/running_3.xlsx")[,-2]
skitouring_3 <- read_xlsx("data/skitouring_3.xlsx")[,-2]

data_3 <- rbind(running_3, skitouring_3)
data_3$sport <- c(rep("running", nrow(running_3)), rep("skitouring", nrow(skitouring_3)))
data_3$gender <- "female"
data_3$dependents <- TRUE
data_3$charity <- FALSE
data_3$low_competence <- FALSE
data_3$guide <- FALSE
data_3$extreme_risk <- FALSE

# charity
skitouring_4 <- read_xlsx("data/skitouring_4.xlsx")[,-2]
skitouring_5 <- read_xlsx("data/skitouring_5.xlsx")[,-2]

data_45 <- rbind(skitouring_4, skitouring_5)
data_45$sport <- "skitouring"
data_45$gender <- c(rep("male", nrow(skitouring_4)), rep("female", nrow(skitouring_5)))
data_45$dependents <- FALSE
data_45$charity <- TRUE
data_45$low_competence <- FALSE
data_45$guide <- FALSE
data_45$extreme_risk <- FALSE

# low competence
skitouring_6 <- read_xlsx("data/skitouring_6.xlsx")[,-2]
skitouring_7 <- read_xlsx("data/skitouring_7.xlsx")[,-2]

data_67 <- rbind(skitouring_6, skitouring_7)
data_67$sport <- "skitouring"
data_67$gender <- c(rep("male", nrow(skitouring_6)), rep("female", nrow(skitouring_7)))
data_67$dependents <- FALSE
data_67$charity <- FALSE
data_67$low_competence <- TRUE
data_67$guide <- FALSE
data_67$extreme_risk <- FALSE

# guide 
skitouring_8 <- read_xlsx("data/skitouring_8.xlsx")[,-2]
skitouring_9 <- read_xlsx("data/skitouring_9.xlsx")[,-2]

data_89 <- rbind(skitouring_8, skitouring_9)
data_89$sport <- "skitouring"
data_89$gender <- c(rep("male", nrow(skitouring_8)), rep("female", nrow(skitouring_9)))
data_89$dependents <- FALSE
data_89$charity <- FALSE
data_89$low_competence <- TRUE
data_89$guide <- TRUE
data_89$extreme_risk <- FALSE

# extreme risk
skitouring_10 <- read_xlsx("data/skitouring_10.xlsx")[,-2]
skitouring_11 <- read_xlsx("data/skitouring_11.xlsx")[,-2]

data_1011 <- rbind(skitouring_10, skitouring_11)
data_1011$sport <- "skitouring"
data_1011$gender <- c(rep("male", nrow(skitouring_10)), rep("female", nrow(skitouring_11)))
data_1011$dependents <- FALSE
data_1011$charity <- FALSE
data_1011$low_competence <- FALSE
data_1011$guide <- FALSE
data_1011$extreme_risk <- TRUE

data <- rbind(data_0, data_1, data_2, data_3, data_45, data_67, data_89, data_1011)

data <- data %>% mutate_at(1:20, as.numeric)

saveRDS(data, file = "output/sportrisks_cleaned.Rds")

