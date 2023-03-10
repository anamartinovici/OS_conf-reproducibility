library(gamlss)
library(mgcv)
library(tidymv)
library(dplyr)
library(ggplot2)

# loading the dataset stored in github
base_address <- "https://raw.githubusercontent.com/SG540/data_science_portfolio"
specific_address <- "/main/GAM_handedness/data_handedness.csv"
data <- read.csv(paste0(base_address, specific_address))

# setting seed
set.seed(1892)

# establishing data types
data$gender <- as.factor(data$gender)
data$school <- as.factor(data$school)
data$h <- as.numeric(data$h)
data$spat <- as.numeric(data$spat)
data$math <- as.numeric(data$math)


data <- data[sample(nrow(data)), ] # shuffling rows

# creating groups for descriptive statistics
data$h_group <- cut(data$h, c(-1, -.95, -.30, .30, .95, 1), include.lowest = TRUE) 

# subsetting the datasets by study
dat_1 <- subset(data, Exp == "Exp1") %>% dplyr::select(-c(spat))
dat_2 <- subset(data, Exp == "Exp2") %>% dplyr::select(-c(spat))
dat_3 <- subset(data, Exp == "Exp3") %>% dplyr::select(-c(school))
dat_4 <- subset(data, Exp == "Exp4")
dat_5 <- subset(data, Exp == "Exp5")


# inverting math scores in Studies 3 and 4
dat_3$math_inv <- abs(dat_3$math - max(dat_3$math))
dat_4$math_inv <- abs(dat_4$math - max(dat_4$math))

# percentage of left-handers
length(data$h[data$h < 0]) / length(data$h)

length(dat_1$h[dat_1$h < 0]) / length(dat_1$h)
length(dat_2$h[dat_2$h < 0]) / length(dat_2$h)
length(dat_3$h[dat_3$h < 0]) / length(dat_3$h)
length(dat_4$h[dat_4$h < 0]) / length(dat_4$h)
length(dat_5$h[dat_5$h < 0]) / length(dat_5$h)

# prevalence sorted by handedness groups and study
sapply(group_split(data %>%
                       group_by(h_group, Exp)), function(x) nrow(x))

# percentage of left-handers sorted by gender

perc_h <- function(x) {aggregate(h ~ gender, data = subset(x, h < 0),
                                 FUN = length)[,2] /
        aggregate(h ~ gender, data = x, FUN = length)[,2]}

perc_h(data)

perc_h(dat_1)
perc_h(dat_2)
perc_h(dat_3)
perc_h(dat_4)
perc_h(dat_5)

# table 1 ----
## N per study ----
data %>%
    group_by(Exp) %>%
    summarise(n = n(),
              mean_age = round(mean(age), 3),
              sd_age = round(sd(age), 3),
              mean_h = round(mean(h), 3),
              sd_h = round(sd(h), 3))

data %>%
    group_by(Exp, 
             gender) %>%
    summarise(n = n())

df_table2 <- data %>%
    group_by(Exp,
             h_group) %>%
    summarise(n_part = n()) %>%
    ungroup()
df_table2 <- df_table2 %>%
    group_by(Exp) %>%
    mutate(n_study = sum(n_part)) %>%
    ungroup()
df_table2 <- df_table2 %>%
    mutate(perc_part = n_part / n_study)

df_table2 %>%
    select(Exp,
           h_group,
           n_part) %>%
    tidyr::pivot_wider(names_from = h_group,
                       values_from = n_part)

df_table2 %>%
    select(Exp,
           h_group,
           perc_part) %>%
    mutate(perc_part = round(perc_part * 100, 0)) %>%
    tidyr::pivot_wider(names_from = h_group,
                       values_from = perc_part)

dat_1 %>%
    filter(gender == 0) %>%
    filter(h < -.95)
