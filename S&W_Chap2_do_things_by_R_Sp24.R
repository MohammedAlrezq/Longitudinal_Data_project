# this is from classs practice on 02/05/2024
getwd()
setwd("~") #Check the home directory for this computer (~ means home directory)
setwd("~/EDRE6674_LDA_Sp24")




#Use R Studio File -> Import Dataset and Copy/Paste
library(haven)
tolerance_longform <- read_sav("Chapter_2/tolerance_longform.sav")
View(tolerance_longform)
dim(tolerance_longform)
colnames(tolerance_longform)
#str(tolerance_longform)

#install.packages("tidyverse")
library(tidyverse)

## Side-by-side box plot from longform data file####
ggplot(tolerance_longform, aes(x=as.factor(age), y=TOL)) +
  geom_boxplot() +
  labs(x="age", y="Tolerance to deviance")

#install.packages("psych")
library(psych)
## We use psych package in order to detailed descriptive statistics.


## Creating a panel of individual plots ####
## use facet_wrap to put in one panel####
## Slide 23 in S&W_Chap2.pptx ####
ggplot(tolerance_longform, aes(age, TOL, group=id)) +
  facet_wrap(~id, labeller=label_both) +
  geom_point(size=2)                         #add points

## Slide 24 in S&W_Chap2.pptx ####
## Connecting points by straight interpolation lines ####
ggplot(tolerance_longform, aes(age, TOL, group=id)) +
  facet_wrap(~id, labeller=label_both) +
  geom_point(size=2) +                          #add points
  geom_line()  #connect points by straight lines

## Slide 25 in S&W_Chap2.pptx ####
## Connecting points by spline (smooth) interpolation lines ####
## use package ggalt ####
#install.packages("ggalt")
library(ggalt) #we need ggalt package to add OLS line or/and spline

ggplot(tolerance_longform, aes(age, TOL, group=id)) +
  facet_wrap(~id, labeller=label_both) +
  geom_point(size=2) +                          #add points
  geom_xspline()  #connect points by smooth splines

######################### Add OLS straight line ############################
## Slide 26 in S&W_Chap2.pptx ####
## Overlay OLS straight line ####
ggplot(tolerance_longform, aes(age, TOL, group=id)) +
  facet_wrap(~id, labeller=label_both) +
  geom_point(size=2) +                          #add points
  geom_smooth(method="lm", se=FALSE, size=0.2)  #add individual loess line
###############################################################################

## Slide 29 in S&W_Chap2.pptx ####
## Change to smooth curve (i.e., loess) ####
ggplot(tolerance_longform, aes(age, TOL, group=id)) +
  facet_wrap(~id, labeller=label_both) +
  geom_point(size=2) +                          #add points
  geom_smooth(method="loess", se=FALSE, size=0.2)  #add individual loess line

## Slide 27 in S&W_Chap2.pptx ####
## Creating individual plots in one figure ####
## with individual OLS straight lines and overall reg line ####
ggplot(tolerance_longform, aes(age, TOL, group=id)) +
  geom_point(size=2) +                          #add points
  geom_smooth(method="lm", se=FALSE, size=0.2) +  #add individual OLS straight line
  geom_smooth(method="lm", aes(group=1), color="red", size=1.2, se=FALSE) #add an overall OLS line

## Slide 28 in S&W_Chap2.pptx ####
## Change to loess ####
ggplot(tolerance_longform, aes(age, TOL, group=id)) +
  geom_point(size=2) +                          #add points
  geom_smooth(method="loess", se=FALSE, size=0.2) +  #add individual OLS straight line
  geom_smooth(method="loess", aes(group=1), color="red", size=1.2, se=FALSE) #add an overall OLS line

## Collect intercept(i.e., intial status) and slope estimates in order to conduct OLS regression individually ####
## Slide 31 in S&W_Chap2.pptx (data creation)####
OLS_coeffs <- tolerance_longform %>% group_by(id) %>% summarize(OLS_intercept=lm(TOL~Time)$coeff[1], OLS_slope=lm(TOL~Time)$coeff[2])
OLS_coeffs

## Slide 32 in S&W_Chap2.pptx (tables)####
## Working with the OLS intercepts and slopes ####
## descriptive statistics and correlations
psych::describe(OLS_coeffs[,c("OLS_intercept", "OLS_slope")])
psych::corr.test(OLS_coeffs[,c("OLS_intercept", "OLS_slope")])

## Slide 32 in S&W_Chap2.pptx (3 figures)####
## Creating histograms of OLS intercepts and slopes (Slide 32 in S&W Chap2.pptx) ####
#install.packages("gridExtra")
library(gridExtra)
fig_1 <- ggplot(OLS_coeffs, aes(OLS_intercept)) +
  geom_dotplot(dotsize=1) 
fig_2 <- ggplot(OLS_coeffs, aes(OLS_slope)) +
  geom_dotplot(dotsize=1) 

fig_3 <- ggplot(OLS_coeffs, aes(OLS_intercept, OLS_slope)) +
  geom_point(size=2) +                          #add points
  geom_smooth(method="lm", se=FALSE, size=0.2)  #add OLS straight line

#To check the actual values of intercept and slope
summary(lm(OLS_slope ~ OLS_intercept, data=OLS_coeffs))

grid.arrange(fig_1, fig_2, fig_3, nrow=1)

## Slide 37 in S&W_Chap2.pptx (Preparation of data)####
## Merge with male and exposure variable in wide form data set ####
library(haven)
tolerance_wideform <- read_sav("Tolerance data (Chap2)/tolerance_wideform.sav")
#View(tolerance_wideform)

OLS_coeffs_2 <- left_join(OLS_coeffs, tolerance_wideform) %>% select(id, OLS_intercept, OLS_slope, male, exposure) %>% 
  mutate(OLS_coeffs, High_Exposure = ifelse(exposure>=median(exposure), 1, 0))
OLS_coeffs_2
median(OLS_coeffs_2$exposure)

## Now we can draw fiures
#4 Figures in slide 37 & 38 in S&W Chap2.pptx ####
p1 <- ggplot(OLS_coeffs_2, aes(male,  OLS_intercept)) +
  geom_point(size=2) +                          #add points
  geom_smooth(method="lm", se=FALSE, size=0.2)  #add OLS straight line

summary(lm(OLS_intercept ~ male, data=OLS_coeffs_2))

p2 <- ggplot(OLS_coeffs_2, aes(High_Exposure,  OLS_intercept)) +
  geom_point(size=2) +                          #add points
  geom_smooth(method="lm", se=FALSE, size=0.2)  #add OLS straight line

summary(lm(OLS_intercept ~ High_Exposure, data=OLS_coeffs_2))

p3 <- ggplot(OLS_coeffs_2, aes(male,  OLS_slope)) +
  geom_point(size=2) +                          #add points
  geom_smooth(method="lm", se=FALSE, size=0.2)  #add OLS straight line

summary(lm(OLS_slope ~ male, data=OLS_coeffs_2))

p4 <- ggplot(OLS_coeffs_2, aes(High_Exposure,  OLS_slope)) +
  geom_point(size=2) +                          #add points
  geom_smooth(method="lm", se=FALSE, size=0.2)  #add OLS straight line

summary(lm(OLS_slope ~ High_Exposure, data=OLS_coeffs_2))

grid.arrange(p1, p2, p3, p4, nrow=2)

## correlations in slides 37 & 38 ####
psych::corr.test(OLS_coeffs_2[,c("OLS_intercept", "OLS_slope", "male", "exposure", "High_Exposure")])

