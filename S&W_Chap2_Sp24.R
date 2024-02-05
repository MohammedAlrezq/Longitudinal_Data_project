# done on 1/22/2024
#getwd()
#setwd("C:\\Users\\Owner\\EDRE6674_LDA_Sp24")

#Install necessary packages (once installed, you don't need to do it again.####
#install.packages("haven")  ## to read spss data file
#install.packages("tidyverse") ## to use tidyverse and ggplot2

## load to memory ####
library(haven)
library(tidyverse)

# Wide -> Long form  ####
## Example: NYS tolerance
# First we need to read in the data. Here we will read SPSS data into R date frame.

tolerance_wideform <- read_sav("tolerance_wideform.sav")# to read this file, haven package need to be downloaded

#View(tolerance_wideform)
head(tolerance_wideform)

## The below used pivot_long to change daat to longer format
# Change to long form using pivot_longer() function ####
## %>% is the pipe operator in tidyverse 
tolerance_longform <- tolerance_wideform  |>  
  pivot_longer(cols = tol11:tol15, names_to = "age",  names_prefix = "tol", values_to = "tolerance")|> 
  mutate(Time = as.numeric(age) - 11) 
# comment: 
# 1 - names_prefix is to remove "tol" next to age number in orginal data

## Now we can create many different kind of graphs using ggplot ####
# Use ggplot() to create pretty figures


## Draw OLS regression straight line for each person and also draw the average OLS straight line #### 
ggplot(data=tolerance_longform, aes(age, tolerance, group = id)) +
  geom_point(size = 3) +
  #geom_line()  #connect points with straight line
  geom_smooth(method = "lm", se = FALSE, size = 0.3) +
  geom_smooth(method = "lm", aes(group = 1), color = "red", size = 1.5, se =FALSE)

## Create a side-by-side plot using facet_wrap by gender
## Use facet_wrap
ggplot(tolerance_longform, aes(age, tolerance, group = id)) +
  facet_wrap (~male) + # facet make two plots 
  geom_point(size = 3) +
  #geom_line()
  geom_smooth(method = "lm", se = FALSE, size = 0.3) +
  geom_smooth(method = "lm", aes(group = 1), color = "red", size = 1.5, se =FALSE)


# Now we create a HighExposure dummy variable by median-split ####
median(tolerance_longform$exposure)
tolerance_longform
tolerance_longform <- tolerance_longform %>% mutate(HighExposure = ifelse(exposure >= median(exposure), 1, 0)) %>% select(id, age, Time, tolerance, male, exposure, HighExposure)

## Create a side-by-side plot using facet_wrap by gender
## Use facet_wrap
ggplot(tolerance_longform, aes(age, tolerance, group = id)) +
  facet_wrap (~HighExposure) +
  geom_point(size = 3) +
  #geom_line()
  geom_smooth(method = "lm", se = FALSE, size = 0.3) +
  geom_smooth(method = "lm", aes(group = 1), color = "red", size = 1.5, se =FALSE)

## Create a side-by-side plot that has two grouping variables ####
## Use facet_wrap
ggplot(tolerance_longform, aes(age, tolerance, group = id)) +
  facet_wrap (~male + HighExposure, labeller = label_both) +
  geom_point(size = 3) +
  #geom_line()
  geom_smooth(method = "lm", se = FALSE, size = 0.3) +
  geom_smooth(method = "lm", aes(group = 1), color = "red", size = 1.5, se =FALSE)


## Use facet_grid
ggplot(tolerance_longform, aes(age, tolerance, group = id)) +
  theme_bw() +                    #make figure background as black and white.
  facet_grid (male ~ HighExposure, labeller = label_both) +
  geom_point(size = 3) +
  #geom_line()
  geom_smooth(method = "lm", se = FALSE, size = 0.3) +
  geom_smooth(method = "lm", aes(group = 1), color = "red", size = 1.5, se =FALSE)

