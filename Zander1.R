######################################

# R code for zander CPUE data

######################################

# Start by loading packages
library(anytime)
library(arm)
library(bit)
library(brinla)
library(car)
library(cellranger)
library(DHARMa)
library(gargle)
library(GGally)
library(ggeffects)
library(ggplot2)
library(ggpubr)
library(glmmTMB)
library(grid)
library(gridExtra)
library(INLA)
library(inlatools)
library(lattice)
library(lawstat)
library(lme4)
library(mgcv)
library(nlme)
library(outliers)
library(performance)
library(plotly)
library(plyr)
library(tidyverse)
library(scales)
library(sjlabelled)
library(sjmisc)
library(sjPlot)
library(timechange)
library(tzdb)
library(vroom)

# Install the latest stable version of INLA:
# install.packages("INLA", repos=c(getOption("repos"),
#                 INLA="https://inla.r-inla-download.org/R/stable"),
#                 dep=TRUE)
library(INLA)

# Also install brinla
# install_github("julianfaraway/brinla")
library(brinla)

######################################

#Import zander data
zan <- read_csv(file = "zander.csv")

# HOUSEKEEPING

# Define preferred figure format
My_theme <- theme(axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.ticks.x=element_blank(),
                  panel.background = element_blank(),
                  panel.border = element_rect(fill = NA, size = 1),
                  strip.background = element_rect(fill = "white", 
                                                  color = "white", size = 1),
                  text = element_text(size = 14),
                  panel.grid.major = element_line(colour = "white", size = 0.1),
                  panel.grid.minor = element_line(colour = "white", size = 0.1))

# A function for dotplots
multi_dotplot <- function(filename, Xvar, Yvar){
  filename %>%
    ggplot(aes(x = {{Xvar}})) +
    geom_point(aes(y = {{Yvar}})) +
    theme_bw() +
    coord_flip() +
    labs(x = "Order of Data")}

######################################

# DATA EXPLORATION

#Use 'str' to inspect the dataframe
str(zan)

# Are there missing values?
colSums(is.na(zan))
# Year   Month   Mon  Station  Gear   Catch  Effort 
# 0       0       0       0       0       0       0 

# Balance among years?
table(zan$Year)
# 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010  
# 61   62   67   61   68   69   73   76   70   68   71   75   63

# 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 
# 71   67   75   79   72   64   69   70   74   61

# Balance among months?
table(zan$Month)
# Apr Aug Dec Feb Jan Jul Jun Mar May Nov Oct Sep 
# 135 141 124 104 106 142 156 105 154 142 142 135 

# Create variable combining year and month
zan$YrMon <- anydate(paste(zan$Year, zan$Mon, sep = "_"))
table(zan$YrMon)
# Rather a lot of levels - perhaps keep year and month separate
# and nest month in year

# Balance among stations?
zan$fStn <- as.factor(zan$Station)
table(zan$Station)
# 47  51  52 
# 601 378 607 

# Balance among gear types?
table(zan$Gear)
# Fyke Gillnet    Line  Others   Trawl 
# 593     823      48     110      12 
# Poor balance

# Just keep Fyke and Gillnet
zan1 <- zan[zan$Gear %in% c("Fyke", "Gillnet"), ]
table(zan1$Gear)

# Fyke Gillnet 
# 593     823 
# Better

# Range of catch
range(zan1$Catch)
# 0.001 - 30.038 (no zeros)
# Were zeros removed?

# Define preferred figure format
My_theme <- theme(panel.background = element_blank(),
                  panel.border = element_rect(fill = NA, size = 1),
                  strip.background = element_rect(fill = "white", 
                                                  color = "white", 
                                                  size = 1),
                  text = element_text(size = 13),
                  panel.grid.major = element_line(colour = "white", 
                                                  size = 0.1),
                  panel.grid.minor = element_line(colour = "white", 
                                                  size = 0.1))

# Plot catch over time
ggplot(zan1, aes(x = YrMon, y = (Catch))) +
  geom_line(size = 0.5) +
  My_theme +
  xlab("Time") + ylab("Catch (kg)")

# Plot catch over time in different stations with different gears
ggplot(zan1, aes(x=YrMon, y=Catch)) +
  geom_line(color="steelblue") + 
  geom_point() +
  xlab("") +
  My_theme +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  scale_x_date(limit=c(as.Date("1998-01-01"),as.Date("2020-12-01"))) +
  ylim(0,31) +
  facet_grid(Station~Gear)
# Drop Fyke data?

######################################

# OUTLIERS

#Order data
zan1 <- zan1 %>%
  mutate(order = seq(1:nrow(zan1)))

#Select continuous variables to plot
p1 <- multi_dotplot(zan1, order, Catch)
p2 <- multi_dotplot(zan1, order, Effort)

#Plot as a grid
grid.arrange(p1, p2, nrow = 1)

# Use Grubbs' test?

######################################

#NORMALITY AND HOMOGENEITY OF DEPENDENT VARIABLES

# Frequency polygon plot for catch
zan1 %>% ggplot(aes(Catch)) +
  geom_freqpoly(bins = 4) +
  labs(x = "Zander caught", y = "Frequency") +
  My_theme +
  theme(panel.border = element_rect(colour = "black", 
                                    fill=NA, size = 1))

#Shapiro-Wilk test for deviation from normality
shapiro.test(zan1$Catch)

# data:  zan1$Catch
# W = 0.67877, p-value < 2.2e-16
# Departure from normality...but not critical

# Patterns in the variance? (Evidence for lack of homogeneity)
ggplot(zan1, aes(x = Effort, y = (Catch))) +
  ylim(-1,30) + xlim(0,61000) +
  geom_jitter(shape = 16, size = 2.5, alpha = 0.3, height = 0.25, width = 0.5) +
  theme(panel.background = element_blank()) +
  theme(panel.border = element_rect(fill = NA, size = 1)) +
  theme(strip.background = element_rect(fill = "white", 
                                        color = "white", size = 1)) +
  theme(text = element_text(size=13)) +
  xlab("Effort") + ylab("Zander catch (kg)") +
  facet_grid(fStn~Gear)
# Fyke data are not helpful 
# Departure from homogeneity in Gillnet data

######################################

#CALCULATE NUMBER OF ZEROS

# What is the percentage of zeros in the response variable

round(sum(zan1$Catch == 0) * 100 / nrow(zan1),0)
#0%

######################################

# COLLINEARITY
Coll <- c("fStn", "Gear", "Effort", "Year")

# Obtain summary using the ggpairs command from the GGally library
ggpairs(zan1[,Coll], ggplot2::aes(alpha = 0.9, colour = fStn))
# Nothing serious

#Calculate Variance Inflation Factor (VIF)
round(vif(lm(Catch ~ fStn + Gear + Effort + Year + Month,
                     data = zan1)),2)

#         GVIF Df GVIF^(1/(2*Df))
# fStn    1.29  2            1.07
# Gear    2.31  1            1.52
# Effort  2.74  1            1.65
# Year    1.08  1            1.04
# Month   1.40 11            1.02

# No obvious evidence of variance inflation (in a Gaussian lm)

######################################

# INTERACTIONS

# Catch and effort
ggplot(zan1, aes(x = Effort, y = (Catch))) +
  geom_point(shape = 16, size = 3, alpha = 0.7) +
  geom_smooth(method = 'lm', colour = 'red', se = FALSE, size = 1.5) +
  My_theme +
  xlab("Effort") + ylab("Catch") +
  facet_grid(.~Station)

ggplot(zan1, aes(x = Effort, y = (Catch))) +
  geom_point(shape = 16, size = 3, alpha = 0.7) +
  geom_smooth(method = 'lm', colour = 'red', se = FALSE, size = 1.5) +
  My_theme +
  xlab("Effort") + ylab("Catch") +
  facet_grid(.~Gear)
# Possible interaction - but we may drop Fyke
# (looks like gear x effort collinearity)

# Catch and year
ggplot(zan1, aes(x = Year, y = (Catch))) +
  geom_point(shape = 16, size = 3, alpha = 0.7) +
  geom_smooth(method = 'lm', colour = 'red', se = FALSE, size = 1.5) +
  My_theme +
  xlab("Year") + ylab("Catch") +
  facet_grid(.~Station)

ggplot(zan1, aes(x = Year, y = (Catch))) +
  geom_point(shape = 16, size = 3, alpha = 0.7) +
  geom_smooth(method = 'lm', colour = 'red', se = FALSE, size = 1.5) +
  My_theme +
  xlab("Year") + ylab("Catch") +
  facet_grid(.~Gear)
# Possible interaction - but we may drop Fyke

# Catch and month
ggplot(zan1, aes(x = Mon, y = (Catch))) +
  geom_point(shape = 16, size = 3, alpha = 0.7) +
  geom_smooth(method = 'lm', colour = 'red', se = FALSE, size = 1.5) +
  My_theme +
  xlab("Month") + ylab("Catch") +
  facet_grid(.~Station)

ggplot(zan1, aes(x = Mon, y = (Catch))) +
  geom_point(shape = 16, size = 3, alpha = 0.7) +
  geom_smooth(method = 'lm', colour = 'red', se = FALSE, size = 1.5) +
  My_theme +
  xlab("Month") + ylab("Catch") +
  facet_grid(.~Gear)
# Possible interaction - but we may drop Fyke

# Fyke net data will cause problems - just analyse gillnet data
zan2 <- zan1[zan1$Gear %in% c("Gillnet"), ]
zan3 <- droplevels(zan2)
table(zan3$Gear)

# Gillnet 
# 823 

#####################################
# 
# The data exploration showed:
#   
# 1.	No outliers
# 2.  Departure from normality in response variable
# 3.	Some departure from homogeneity
# 4.	No zeros in the response variable
# 5.	No serious collinearity
# 6.  Some imbalance (but refining the data removes this problem)
# 7.  Data are a time series (i.e. dependency due to Year and Months within years)
#     'Year' and 'Month' cannot be treated as a fixed effects.
#     They also cannot be treated as random terms - levels are not independent.
#     A simple GLM is not an option...but neither is a GLMM.
#     We need a time-series analysis to standardise these data.

#####################################

# Aim: Standardise zander gillnet catch (kg) as a function  
#      of effort, region, years, and months within years

# Apply (Bayesian) INLA model for time-series analysis with
# a random walk model of order 1 (RW1)
# This model has 2 residual components, trend due to time (year) + pure noise

# Start with intercept only model. 
# Catch is modelled as a function of year and 'rw1' imposes a temporal trend

# Create a formula
f1 <- Catch ~ + f(Year, model = "rw1")

# And fit with INLA with a gamma distribution
# A gamma distribution is strictly positive (no zeros) and skewed (like our catch data)
I1 <- inla(f1, 
           control.compute = list(dic = TRUE), #estimate dic for model comparison
           family = "Gamma",
           data = zan3)

# And compare with Gaussian (not appropriate for these data...)
I2 <- inla(f1, 
           control.compute = list(dic = TRUE),
           family = "gaussian",
           data = zan3)

# Compare models with DIC (deviance information criterion - like AIC)
round(I1$dic$dic,0) #4179 <- gamma fits better
round(I2$dic$dic,0) #5107

# Plot the time(year) trend
Yearsm <- I1$summary.random$Year
Fit1   <- I1$summary.fitted.values[,"mean"]

par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
plot(Yearsm[,1:2], type='l',
     xlab = 'Year', 
     ylab = 'Random walk trend',
     ylim = c(-1, 1))
abline(h=0, lty=3)
lines(Yearsm[, c(1, 4)], lty=2)
lines(Yearsm[, c(1, 6)], lty=2)
# Differs from zero...

# Add Effort to model
f2 <- Catch ~ Effort + f(Year, model = "rw1")
I2 <- inla(f2, 
           control.compute = list(dic = TRUE),
           family = "Gamma",
           data = zan3)
round(I1$dic$dic,0) #4179
round(I2$dic$dic,0) #3532 <- including Effort improves fit

Yearsm <- I2$summary.random$Year
plot(Yearsm[,1:2], type='l',
     xlab = 'Year', 
     ylab = 'Random walk trend',
     ylim = c(-0.2, 0.2) )
abline(h=0, lty=3)
lines(Yearsm[, c(1, 4)], lty=2)
lines(Yearsm[, c(1, 6)], lty=2)

# E2 <- zan3$Catch - I2$summary.fitted.values$mean
# acf(E2)

# Fit model with rw2 (should be a smoother fit)
f3 <- Catch ~ Effort + f(Year, model = "rw2")

I3 <- inla(f3, 
           control.compute = list(dic = TRUE),
           family = "Gamma",
           data = zan3)

round(I2$dic$dic,0) #3532 
round(I3$dic$dic,0) #3525 <- rw2 better than rw1

# E3 <- zan3$Catch - I3$summary.fitted.values$mean
# acf(E3)

Yearsm <- I3$summary.random$Year
plot(Yearsm[,1:2], type='l',
     xlab = 'Year', 
     ylab = 'Random walk trend',
     ylim = c(-2, 2) )
abline(h=0, lty=3)
lines(Yearsm[, c(1, 4)], lty=2)
lines(Yearsm[, c(1, 6)], lty=2)
# That may be too smooth - rw1 probably better

# More complex model - month nested in year
f4 <- Catch ~ Effort + 
  f(Year, 
    model = "rw2") +
  f(Mon, 
    model = "rw2", cyclic = TRUE)  

I4 <- inla(f4, 
           control.predictor = list(compute = TRUE),
           control.compute = list(dic = TRUE),
           family = "gamma",
           data = zan3)

round(I3$dic$dic,0) #3525
round(I4$dic$dic,0) #3201 <- including month improves fit

Fit4 <- I4$summary.fitted.values[,"mean"]

par(mfrow = c(1,2), mar = c(5,5,2,2), cex.lab = 1.5)
Yearsm   <- I4$summary.random$Year
plot(Yearsm[,1:2], type='l',
     xlab = 'Year', 
     ylab = 'Random walk trend',
     ylim = c(-1, 1) )
abline(h=0, lty=3)
lines(Yearsm[, c(1, 4)], lty=2)
lines(Yearsm[, c(1, 6)], lty=2)

Monsm <- I4$summary.random$Mon
plot(Monsm[,1:2], type='l',
     xlab = 'MonthInYear', 
     ylab = '',
     ylim = c(-4, 4) )
abline(h=0, lty=3)
lines(Monsm[, c(1, 4)], lty=2)
lines(Monsm[, c(1, 6)], lty=2)

# Add station as a random term
f5 <- Catch ~ Effort + 
  f(Year, 
    model = "rw2") +
  f(Mon, 
    model = "rw2", cyclic = TRUE) +
  f(fStn, model = "iid") 

I5 <- inla(f5, 
           control.predictor = list(compute = TRUE),
           control.compute = list(dic = TRUE),
           family = "Gamma",
           data = zan3)


round(I4$dic$dic,0) #3204 
round(I5$dic$dic,0) #2980 <- including station improves fit

# outI5 <- I5$summary.fixed[,c("mean", "sd", "0.025quant", "0.975quant")] 
# print(outI5, digits = 2)  

Fit5 <- I5$summary.fitted.values[,"mean"]

par(mfrow = c(1,2), mar = c(5,5,2,2), cex.lab = 1.5)
Yearsm   <- I5$summary.random$Year
plot(Yearsm[,1:2], type='l',
     xlab = 'Year', 
     ylab = 'Random walk trend',
     ylim = c(-1, 1) )
abline(h=0, lty=3)
lines(Yearsm[, c(1, 4)], lty=2)
lines(Yearsm[, c(1, 6)], lty=2)

Monsm   <- I5$summary.random$Mon
plot(Monsm[,1:2], type='l',
     xlab = 'MonthInYear', 
     ylab = 'Random walk trend',
     ylim = c(-3, 3) )
abline(h=0, lty=3)
lines(Monsm[, c(1, 4)], lty=2)
lines(Monsm[, c(1, 6)], lty=2)

# Year as rw1 (less extreme smoothing)
f6 <- Catch ~ Effort + 
  f(Year, 
    model = "rw1") +
  f(Mon, 
    model = "rw2", cyclic = TRUE) +
  f(fStn, model = "iid") 

I6 <- inla(f6, 
           control.predictor = list(compute = TRUE),
           control.compute = list(dic = TRUE),
           family = "Gamma",
           data = zan3)

round(I5$dic$dic,0) #2980 
round(I6$dic$dic,0) #2977 <- including year as rw1 improves fit (marginally)

# outI6 <- I6$summary.fixed[,c("mean", "sd", "0.025quant", "0.975quant")]
# print(outI6, digits = 2)

Fit6 <- I6$summary.fitted.values[,"mean"]

par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
Yearsm   <- I6$summary.random$Year
plot(Yearsm[,1:2], type='l',
     xlab = 'Year', 
     ylab = 'Random walk trend',
     ylim = c(-1, 1) )
abline(h=0, lty=3)
lines(Yearsm[, c(1, 4)], lty=2)
lines(Yearsm[, c(1, 6)], lty=2)

Monsm   <- I6$summary.random$Mon
plot(Monsm[,1:2], type='l',
     xlab = 'MonthInYear', 
     ylab = 'Random walk trend',
     ylim = c(-3, 3) )
abline(h=0, lty=3)
lines(Monsm[, c(1, 4)], lty=2)
lines(Monsm[, c(1, 6)], lty=2)

# This model looks good - let's look at the Bayesian residuals

# Get the fitted values and Pearson residuals
N     <- nrow(zan3)
mu1   <- I6$summary.fitted.values[1:N,"mean"] 
r     <- I6$summary.hyperpar["Precision parameter for the Gamma observations", "mean"]
VarY1 <- mu1^2 / r
E1    <- (zan3$Catch - mu1) / sqrt(VarY1)
# (Note that a Gamma GLMM cannot be overdispersed)

# Plot residuals versus fitted values.
par(mfrow = c(1, 1))
plot(x = mu1, 
     y = E1,
     xlab = "Fitted values",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2, col = 1)
# That is slightly odd....

# Plot residuals versus station
boxplot(E1 ~ Station, 
        ylab = "Pearson residuals",
        data = zan3)
abline(h = 0, lty = 2)
# OK

# Year
boxplot(E1 ~ Year, 
        ylab = "Pearson residuals",
        data = zan3)
abline(h = 0, lty = 2)
# OK

# Month
boxplot(E1 ~ Month, 
        ylab = "Pearson residuals",
        data = zan3)
abline(h = 0, lty = 2)
# OK

# Residuals versus effort
zan3$E1 <- E1
resplot1 <- ggplot() +
  geom_point(data = zan3, alpha = 0.4, size = 2,
             aes(y = E1 ,  
                 x = Effort)) +
  geom_smooth(data = zan3,                    
              aes(y = E1, 
                  x = Effort)) +
  xlab("Effort") + ylab("Pearson residuals") +
  theme(text = element_text(size = 12), legend.position="none") +
  theme(axis.text.x = element_text(size = 11, angle = 45, hjust = 0.9)) +
  My_theme +
  geom_hline(yintercept = 0, col = 2)
resplot1

# Problem - there seems to be a non-linear pattern in here...

# Do the same...but now per station
resplot1 + facet_grid(~fStn)
# Problem

# We can check with a GAM whether there are any important
# non-linear patterns
Test1 <- gam(E1 ~ s(Effort), data = zan3)
summary(Test1)

# That is 15% explained by the smoother (which is significant). So...yes there 
# is a residual effort effect

# It may be that the log link function is causing a non-linear residual pattern. Using a Gamma
# GLMM with an identity link may be an option. Unfortunately, this does not work yet in INLA...
# And even if it would run....it might result in numerical problems, as there is no 
# mechanism to stop the model from producing negative fitted values...

# So...

# Make station a fixed effect and add an interaction with effort to capture non-linearities

f7 <- Catch ~ Effort * fStn + 
  f(Year, 
    model = "rw1") +
  f(Mon, 
    model = "rw2",
        cyclic = T)

I7 <- inla(f7, 
           control.predictor = list(compute = TRUE),
           control.compute = list(config = TRUE, dic = TRUE),
           family = "Gamma",
           control.family = list(link = "log"),
           data = zan3)

round(I6$dic$dic,0) #2977 
round(I7$dic$dic,0) #2888 <- big improvement in fit...

Fit7 <- I7$summary.fitted.values[,"mean"]

par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
Yearsm   <- I7$summary.random$Year
plot(Yearsm[,1:2], type='l',
     xlab = 'Year', 
     ylab = 'Random walk trend',
     ylim = c(-1, 1) )
abline(h=0, lty=3)
lines(Yearsm[, c(1, 4)], lty=2)
lines(Yearsm[, c(1, 6)], lty=2)

Monsm   <- I7$summary.random$Mon
plot(Monsm[,1:2], type='l',
     xlab = 'MonthInYear', 
     ylab = 'Random walk trend',
     ylim = c(-3, 3) )
abline(h=0, lty=3)
lines(Monsm[, c(1, 4)], lty=2)
lines(Monsm[, c(1, 6)], lty=2)

# Get the fitted values and Pearson residuals
N     <- nrow(zan3)
mu2   <- I7$summary.fitted.values[1:N,"mean"] 
r     <- I7$summary.hyperpar["Precision parameter for the Gamma observations", "mean"]
VarY2 <- mu2^2 / r
E2    <- (zan3$Catch - mu2) / sqrt(VarY2)

# Plot residuals versus fitted values.
par(mfrow = c(1, 1))
plot(x = mu2, 
     y = E2,
     xlab = "Fitted values",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2, col = 1)
# Better

# Plot residuals versus station
boxplot(E2 ~ Station, 
        ylab = "Pearson residuals",
        data = zan3)
abline(h = 0, lty = 2)
# OK

# Year
boxplot(E2 ~ Year, 
        ylab = "Pearson residuals",
        data = zan3)
abline(h = 0, lty = 2)
# OK

# Month
boxplot(E2 ~ Month, 
        ylab = "Pearson residuals",
        data = zan3)
abline(h = 0, lty = 2)
# OK

# Residuals versus effort
zan3$E2 <- E2

resplot2 <- ggplot() +
  geom_point(data = zan3, alpha = 0.4, size = 2,
             aes(y = E2 ,  
                 x = Effort)) +
  geom_smooth(data = zan3,                    
              aes(y = E2, 
                  x = Effort)) +
  xlab("Effort") + ylab("Pearson residuals") +
  theme(text = element_text(size = 12), legend.position="none") +
  theme(axis.text.x = element_text(size = 11, angle = 45, hjust = 0.9)) +
  My_theme +
  geom_hline(yintercept = 0, col = 2)
resplot2

# Per station
resplot2 + facet_grid(~fStn)

# Combine plots with and without interaction
ggarrange(resplot1, resplot2,
          labels = c("I6", "I7"),
          ncol = 2, nrow = 1)
# A big improvement in interaction with Station included

# Check for non-linearity with a GAM 
Test2 <- gam(E2 ~ s(Effort), data = zan3)
summary(Test2)
# That is 9% explained by the smoother and is still significant! Still needs work...

# Plot temporal effects
p1 <- bind_rows(
  I7$summary.random$Year %>%
    select(Year = 1, mean = 2, lcl = 4, ucl = 6) %>%
    mutate(Model = "rw1")
) %>%
  ggplot(aes(x = Year, y = mean, ymin = lcl, ymax = ucl)) +
  geom_ribbon(alpha = 0.2, fill = "forestgreen") +
  geom_line(colour = "forestgreen") + My_theme +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "firebrick2", size=0.4) +
  ggtitle("Year") +
  theme(legend.position = "none")
ggplotly(p1)

p2 <- bind_rows(
  I7$summary.random$Mon %>%
    select(Mon = 1, mean = 2, lcl = 4, ucl = 6) %>%
    mutate(Model = "rw2")
) %>%
  ggplot(aes(x = Mon, y = mean, ymin = lcl, ymax = ucl)) +
  geom_ribbon(alpha = 0.2, fill = "dodgerblue2") +
  geom_line(colour = "dodgerblue2") + My_theme +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "firebrick2", size=0.4) +
  ggtitle("Month within Year") +
  theme(legend.position = "none")
ggplotly(p2)
grid.arrange(p1, p2, nrow = 1)

# ###########################################
# Plot effort
MyData <- ddply(zan3, 
                .(fStn), 
                summarize,
                Effort = seq(min(Effort), 
                             max(Effort), 
                             length = 50))

# Set continuous covariates 
MyData$Year <- 2002
MyData$Mon  <- 1

head(MyData)

MyData$Catch <- NA
bitt.Pred <- zan3[, colnames(MyData)]
bitt.Comb <- rbind(bitt.Pred, MyData)

# Put penalized complexity priors on hyperparameters, which should
# make the model fit better...I will need another workshop to explain this...
# PC prior:
sdres <- sd(zan3$Catch)
U <- 3*sdres
hyper.pc = list(theta = list(prior = "pc.prec", param = c(U, 0.05)))

f7 <- Catch ~ Effort * fStn + 
  f(Year, 
    model = "rw1",
    hyper = hyper.pc, 
    scale.model = TRUE,
    constr = TRUE) +
  f(Mon, 
    model = "rw2", 
    hyper = hyper.pc, 
    scale.model = TRUE,
    constr = TRUE,
    cyclic = TRUE)

Final.Pred <- inla(f7,  data = bitt.Comb,
                   family = "gamma",
                   # control.predictor=list(link="log"),
                   control.predictor = list(compute = TRUE))

N <- (nrow(zan3))
K <- nrow(MyData)
Pred <- Final.Pred$summary.fitted.values[((nrow(zan3))+1):
                                           (nrow(zan3) + 
                                            nrow(MyData)),]

# Add the relevant pieces to MyData and plot the whole thing 
MyData$mu    <- exp(Pred[,("mean")])
MyData$selow <- exp(Pred[,"0.025quant"])
MyData$seup  <- exp(Pred[,"0.975quant"])
head(MyData)

# Labels
label_stn <- c("47" = "Station 47", 
               "51" = "Station 51",
               "52" = "Station 52")

# Plot
ggplot() +
  geom_jitter(data = zan3,
              aes(y = Catch, x = Effort),
              shape = 19, size = 2.2,
              height = 0.5, width = 5, alpha = 0.3) +
  xlab("Fishing effort") + ylab("Posterior mean catch (kg)") +
  ylim(0,40) +
  theme(text = element_text(size = 13)) +
  theme(panel.background = element_blank()) +
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1)) +
  theme(strip.background = element_rect
        (fill = "white", color = "white", size = 1)) +
  geom_line(data = MyData, aes(x = Effort, y = mu), size = 1) +
  geom_ribbon(data = MyData,
              aes(x = Effort, ymax = seup, 
                  ymin = selow), alpha = 0.5, fill = "dodgerblue") +
  facet_grid(. ~ fStn, scales = "fixed", space = "fixed",
             labeller=labeller (fStn = label_stn)) +
  theme(legend.position = "none")

# Note that these fits are for January 2002...

# ###########################################################

# Plot CPUE
MyData <- ddply(zan3, 
                .(Mon), 
                summarize,
                Year = seq(min(Year), 
                             max(Year), 
                             length = 50))

# Set continuous covariates 
MyData$Effort <- 20000
MyData$fStn  <- 47

head(MyData)

MyData$Catch <- NA
bitt.Pred <- zan3[, colnames(MyData)]
bitt.Comb <- rbind(bitt.Pred, MyData)

Final.Pred <- inla(f7,  data = bitt.Comb,
                   family = "gamma",
                   # control.predictor=list(link="log"),
                   control.predictor = list(compute = TRUE))

N <- (nrow(zan3))
K <- nrow(MyData)
Pred <- Final.Pred$summary.fitted.values[((nrow(zan3))+1):
                                           (nrow(zan3) + 
                                              nrow(MyData)),]

# Add the relevant pieces to MyData and plot the whole thing 
MyData$mu    <- exp(Pred[,("mean")])
MyData$selow <- exp(Pred[,"0.025quant"])
MyData$seup  <- exp(Pred[,"0.975quant"])
head(MyData)

# Labels
label_mon <- c("1" = "Jan", 
               "2" = "Feb",
               "3" = "Mar",
               "4" = "Apr", 
               "5" = "May",
               "6" = "Jun",
               "7" = "Jul", 
               "8" = "Aug",
               "9" = "Sep",
              "10" = "Oct", 
              "11" = "Nov",
              "12" = "Dec")
# Plot
ggplot() +
  ggtitle("Gillnet CPUE for zander") +
  geom_jitter(data = zan3,
              aes(y = Catch, x = Year),
              shape = 19, size = 1.2,
              height = 0.15, width = 0.25, alpha = 0.3) +
  xlab("Year") + ylab("Posterior mean CPUE") +
  # ylim(0,20) +
  theme(text = element_text(size = 13)) +
  theme(panel.background = element_blank()) +
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1)) +
  theme(strip.background = element_rect
        (fill = "white", color = "white", size = 1)) +
  geom_line(data = MyData, aes(x = Year, y = mu), size = 1) +
  geom_ribbon(data = MyData,
              aes(x = Year, ymax = seup, 
                  ymin = selow), alpha = 0.5, fill = "red") +
  facet_wrap(. ~ Mon, scales = "fixed",
             labeller=labeller (Mon = label_mon)) +
  theme(axis.text.x = element_text(size = 11, angle = 45, hjust = 0.9)) +
  theme(legend.position = "none")

# It appears that there has been a decline in CPUE for zander since 1998

# Model could be improved with a different trend for each station and in each month
# We could also incorporate prior information more effectively....

# Estimates? Yes...but complicated with INLA

# In INLA there is no 'predict' function as for glm/lm in R. 
# Predictions must to performed as a part of the model fitting itself. 
# As prediction is the same as fitting a model with some missing data,
# we can simply set y[i] = NA for those 'locations' we want to predict.

# I will run a more advanced workshop on Bayesian fisheries analysis
# using INLA in the future...

###################################### END
