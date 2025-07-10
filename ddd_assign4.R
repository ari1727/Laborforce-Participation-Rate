remove(list = ls())

library(tidyverse)
library(haven)

setwd("D:/Downloads/SAS_2019_honors")
plfs <- read_dta("SAS_rawdata/plfs2017.dta")

# subsetting for working age population
plfs <- subset(plfs, plfs$age >=15)

# creating new variable for status - 0 if in the labour force or 1 otherwise
plfs$lf <- ifelse(plfs$upa_status <= 81, '1', '0')


#subsetting for females
plfs_f <- subset(plfs, sex==2)

#this is for males 
plfs_m <- subset(plfs, sex == 1)

# barplot for labour force parition
ggplot(plfs_f, aes(x=lf)) +
  geom_bar()

# bar plot for marital status # got values interms of counts but not interms of proprtions
ggplot(plfs_f, aes(x= marital)) + geom_bar()


# stacked bar graph, showing marital status among working age population
#need to convert these variables to factors 
plfs_f$lf <- factor(plfs_f$lf)
plfs_f$ed <- factor(plfs_f$ed)

plfs_m$marital <- factor(plfs_m$marital)
plfs_m$social_group <- factor(plfs_m$social_group)
plfs_m$religion <- factor(plfs_m$religion)
plfs_m$hh_size <- factor(plfs_m$hh_size)

plfs_f$lf_label <- ifelse(plfs_f$lf == 0, "In Labor Force", "Out of Labor Force")
plfs_f$marital_label <- factor(plfs_f$marital, levels = c(1, 2, 3, 4),
                               labels = c("never married", "currently married", 
                                          "widowed", "divorced/separated"))


ggplot(plfs_f, aes(x = lf_label, fill = marital_label)) +
  geom_bar(position = "fill") +
  labs(title = "Marital Status among the working age population",
       x = "Labor Force Status",
       y = "Proportion") +
  scale_fill_discrete(name = "Marital Status")


# regression
# converting lf into numeric in order to do linear regression 
plfs_f$lf <- as.numeric(plfs_f$lf)
plfs_f$marital <-factor(plfs_f$marital)
plfs_f$ed <-factor(plfs_f$ed)

# formula
m1 <- lm(lf ~ marry , data = plfs_f)
m2 <- lm(lf~ marital_label, data = plfs_f)
m3 <- lm(lf ~ ed, data = plfs_f)
summary(m4)

m4 <- lm(lf ~   marital_label + ed , data = plfs_f)


m5 <- 
str(plfs_m$yrs_formaledu_p)
unique(plfs_m$yrs_formaledu_p)

plfs_f$ed <- ifelse(plfs_f$current_attend_eduinst <= 15, '0', '1') 
plfs_f$marry <- ifelse(plfs_f$marital == 2, '1', '0') 

library(stargazer)
# Create a table of regression results
stargazer(m1, m2,m3,m4, type = "html", title = "Regression", out = "regression_results1.html")
