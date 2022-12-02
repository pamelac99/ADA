rm(list = ls())
pacman::p_load(readxl, tidyverse, nnet, MASS, funModeling, ggplot2, broom, survival, gtsummary)
ssa <- read_xlsx("/Users/pamela/Downloads/Advanced Data Analysis/project_ssa/GeoPoll Year End Study Raw Data.xlsx")
dim(ssa)
ssa <- dplyr::rename(ssa, c("agegroup" = "Age Group"))
ssa <- mutate(ssa, agegroup_cat = case_when(
  agegroup == "15 - 25" ~ "0",
  agegroup == "26 - 35" ~ "1",
  agegroup == "36+" ~ "2",
  TRUE ~ "NA"
))
ssa$gender_cat <- 0
ssa$gender_cat <- ifelse(ssa$Gender == "Female", 1, 0)
ssa<-dplyr::rename(ssa, c("covidvaccine"="COVID Vaccine"))
ssa <- mutate(ssa, covidvaccine_cat = case_when(
  covidvaccine == "Probably" ~ "0",
  covidvaccine == "Probably not" ~ "1",
  covidvaccine == "Unsure" ~ "2",
  covidvaccine == "Definitely" ~ "3",
  covidvaccine == "Definitely not" ~ "4",
  TRUE ~ "NA"
))
#view(ssa)
ssa_new <- ssa %>% 
  dplyr::select("gender_cat", "agegroup_cat", "covidvaccine_cat")
describe(ssa_new)
table(ssa_new$gender_cat)
table(ssa_new$agegroup_cat)
table(ssa_new$covidvaccine_cat)

#Execute a mutlinomial regression model with gender
ssa_1 <- multinom(covidvaccine_cat ~ gender_cat, ssa_new)
output <- summary(ssa_1)
output

exp(confint(ssa_1, level=0.95))
z <- summary(ssa_1)$coefficients/summary(ssa_1)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1))*2
p

exp(coef(ssa_1))

tidy(ssa_1, conf.int = TRUE, exponentiate = "TRUE")
tbl_regression(ssa_1, exp = TRUE)

#Execute a mutlinomial regression model with gender and age
ssa_2 <- multinom(covidvaccine_cat ~ gender_cat + agegroup_cat, ssa_new)
output <- summary(ssa_2)
output

exp(confint(ssa_2, level=0.95))
z <- summary(ssa_2)$coefficients/summary(ssa_2)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1))*2
p

exp(coef(ssa_2))

tidy(ssa_2, conf.int = TRUE, exponentiate = "TRUE")
tbl_regression(ssa_2, exp = TRUE)







