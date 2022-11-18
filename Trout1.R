######################################

# R code for brown trout CPUE data
# Data from North Uist (Scottish Outer Hebrides) catch returns (2015-16)

######################################

# Start by loading packages
library(arm)
library(car)
library(ggplot2)
library(lattice)
library(lawstat)
library(outliers)
library(tidyverse)
library(scales)
library(GGally)
library(mgcv)
library(plyr)
library(lme4)
library(gridExtra)
library(DHARMa)
library(glmmTMB)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(ggeffects)

######################################

#Import trout data
trout <- read_csv(file = "trout.csv")

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
str(trout)

# Are there missing values?
colSums(is.na(trout))

# loch  season anglers   catch 
# 0       0       0       0     
# No missing data

# Are data balanced among levels of the categorical covariates?
# Balance among lochs?
table(trout$loch)
# 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 
# 1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 

# 27 28 29 30 31 32 33 34 35 
# 1  1  1  1  1  1  1  1  1 

# Among seasons
trout$fSeason <- as.factor(trout$season)
table(trout$fSeason)
# autumn spring summer winter 
# 10     10     12      3 
# poor balance

######################################

# OUTLIERS

#Order data
trout <- trout %>%
  mutate(order = seq(1:nrow(trout)))

#Select continuous variables to plot
p1 <- multi_dotplot(trout, order, anglers)
p2 <- multi_dotplot(trout, order, catch)

#Plot as a grid
grid.arrange(p1, p2, nrow = 1)
# No obvious outliers

# Use Grubbs' test to assess whether a value that is 
# farthest (above or below) the mean is an outlier

#For anglers
grubbs.test(trout$anglers, type = 10) 
# type 10 is used to detect only one outlier

# Grubbs test for one outlier
# 
# data:  trout$anglers
# G = 1.9692, U = 0.8826, p-value = 0.7685

#For catch
grubbs.test(trout$catch, type = 10) 

# data:  trout$catch
# G = 1.99542, U = 0.87945, p-value = 0.7174

######################################

#NORMALITY AND HOMOGENEITY OF DEPENDENT VARIABLES

# Frequency plots
par(mfrow = c(1,2), mar = c(4,5,2,3), cex.lab = 1)
hist(trout$anglers,  xlab = "No. anglers",
     col = "lightblue", border = "black", main = "")
hist((trout$catch),  xlab = "Catch (kg)", 
     col = "red", border = "black",  main = "")

# Frequency polygon plot for catch
trout %>% ggplot(aes(catch)) +
  geom_freqpoly(bins = 3) +
  labs(x = "trout catch (kg)", y = "Frequency") +
  My_theme +
  theme(panel.border = element_rect(colour = "black", 
                                    fill=NA, size = 1))

# Frequency polygon plot for anglers
trout %>% ggplot(aes(anglers)) +
  geom_freqpoly(bins = 4) +
  labs(x = "Number of anglers", y = "Frequency") +
  My_theme +
  theme(panel.border = element_rect(colour = "black", 
                                    fill=NA, size = 1))


#Shapiro-Wilk test for deviation from normality
shapiro.test(trout$anglers)

# data:  trout$anglers
# W = 0.95895, p-value = 0.2126

#Shapiro-Wilk test for deviation from normality
shapiro.test(trout$catch)

# Shapiro-Wilk normality test
# 
# data:  trout$catch
# W = 0.97047, p-value = 0.4562


# Patterns in the variance? (lack of homogeneity)
ggplot(trout, aes(x = anglers, y = (catch))) +
  ylim(0,40) + xlim(-1,16) +
  geom_point(shape = 16, size = 5, alpha = 0.6) +
  theme(panel.background = element_blank()) +
  theme(panel.border = element_rect(fill = NA, size = 1)) +
  theme(strip.background = element_rect(fill = "white", 
                   color = "white", size = 1)) +
  theme(text = element_text(size=13)) +
  xlab("Anglers") + ylab("Catch (kg)")

# Variance in catch does not vary much across number of anglers
# This indicates homogeneity

# Note that the relationship might be slightly non-linear(?)

######################################

#CALCULATE NUMBER OF ZEROS

# What is the percentage of zeros in the response variable

round(sum(trout$catch == 0) * 100 / nrow(trout),0)
#6% zeros

######################################

# COLLINEARITY
Coll <- c("season", "anglers", "catch")

# Obtain summary using the ggpairs command from the GGally library
ggpairs(trout[,Coll], ggplot2::aes(alpha = 0.8, colour = season))
# No obvious collinearity
#Asta: this plot looks cool, but I don't really get what it shows

#Calculate Variance Inflation Factor (VIF)
round(vif(lm(catch ~ anglers + season,
                     data = trout)),2)

#         GVIF Df    GVIF^(1/(2*Df))
# anglers 1.29  1            1.14
# season  1.29  3            1.04

# No evidence of variance inflation (all <3)
#Asta: why do we need it here but not in the previous dataset (bitterling)

# Interaction? Plot data
ggplot(trout, aes(x = anglers, y = (catch))) +
  geom_point(shape = 16, size = 3, alpha = 0.7) +
  geom_smooth(method = 'lm', colour = 'red', se = FALSE, size = 1.5) +
  My_theme +
  xlab("Anglers") + ylab("Catch (kg)") +
  facet_wrap(~fSeason)
# Possibly

#####################################
# 
# The data exploration showed:
#   
# 1.	No significant outliers
# 2.	No evidence of departure from homogeneity.
# 3.	Small proportion of zeros in the response variable.
# 4.  Normality of response variable
# 5.	No collinearity
# 6.  Imbalance among seasons (few data for winter)

# Remove 'winter' from fSeason
trout1 <- trout[-which(trout$fSeason == "winter"),]
table(droplevels(trout1)$fSeason)
# Is it valid to remove data...?

#####################################

# Aim: Standardise trout catch as a function of effort (no. anglers)
#      among seasons

# Start with linear regression
#Make an object (M1) - which is the linear regression 'model'

M1 <- lm(catch ~ anglers + fSeason,
                 data = trout1)

M2 <- lm(catch ~ anglers * fSeason,
         data = trout1)

round(AIC(M1,M2),0)

#    df AIC
# M1  5 227 <- simpler model
# M2  7 227

#But is the model valid?

################################

#MODEL VALIDATION

#Plot the residuals vs fitted values.
Res1 <- resid(M1, test = 'pearson')
Fit1 <- fitted(M1)

par(mfrow = c(1,3), mar = c(5,5,2,2))
plot(x = Fit1, y = Res1,
     xlab = "Fitted values",
     ylab = "Pearson residuals",
      pch = 16, cex = 1.5, cex.lab = 1.5)
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)

# Plot the residuals vs parameters in the model

# Anglers
plot(x = trout1$anglers, 
     y = Res1,
     xlab = "Anglers",
     ylab = "", 
     pch = 16, cex = 1.5, cex.lab = 1.5)
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)

# Season
boxplot(Res1 ~ fSeason, 
        data = trout1,
        xlab = "Season",
        ylab = "",
        range = 0,
        col = "goldenrod2",
        pch = 16, cex.lab = 1.5)
abline(0,0, lty=2)
# They are fine


#Normality of residuals - examine plot
ggplot() +
  geom_histogram(colour = "black", fill = "red2", 
                 data = trout1, aes(Res1), bins = 8) +
  ylab("Frequency") + xlab("Pearson residuals") +
  My_theme
  #Looks fairly normal

#Confirm with Shapiro-Wilk test
shapiro.test(Res1)

#     Shapiro-Wilk normality test
# 
# data:  Res1
# W = 0.9592, p-value = 0.2612

# Influential observations in the model? 
# Measure using Cook's distance
par(mfrow = c(1, 1))
plot(cooks.distance(M1), 
     type = "h",
     xlab = "Observation", 
     ylab = "Cook's distance",
     ylim = c(-0.1, 1.2))
abline(h=1, lty = 2)
abline(h=0, lty = 2)
# All below 1 - that is fine

###############################
#So
# 1. Residuals fine
# 2. Residuals normally distributed
# 3. No evidence of model misfit
# 4. No outliers

# Assume the model is not misfit and obtain summary

###############################

#Obtain model summary
summary(M1)

#                Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      3.128      4.570   0.684  0.499   
# anglers          1.796      0.489   3.674  0.001
# fSeasonspring    7.572      3.603   2.102  0.045
# fSeasonsummer   -4.197      3.306  -1.269  0.215

# Does summer differ from spring? Use relevel
trout1$fSeason <- relevel(trout1$fSeason, ref = "spring")

# Run model again
M1 <- lm(catch ~ anglers + fSeason,
                 data = trout1)
summary(M1) 
#Summer differs from spring

#                Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     10.700      3.742   2.859  0.008
# anglers          1.796      0.489   3.674  0.001
# fSeasonautumn   -7.572      3.603  -2.102  0.045 
# fSeasonsummer  -11.769      3.465  -3.397  0.002

###############################

# Visualise the model as a figure
set_theme(
  axis.ticksize.x = 0.5,
  axis.ticksize.y = 0.5,
  axis.textsize.x = 0.5,
  axis.textsize.y = 0.5,
  axis.linecolor = "black", 
  axis.textcolor = "black",
  axis.tickslen = 0.2,
  axis.title.size = 0.9,
  axis.title.color = "black",
  axis.textsize = 0.8,
  geom.outline.size = 1,
  legend.pos = "bottom",
  legend.title.face = "italic",
  legend.backgroundcol = "white",
  legend.item.backcol = "white",
  legend.item.bordercol = "white",
  base = theme_blank())

# Define preferred figure format
My_theme <- theme(panel.background = element_blank(),
                  panel.border = element_rect(fill = NA, size = 1),
                  strip.background = element_rect(fill = "white", 
                                                 color = "white", 
                                                  size = 1),
                  text = element_text(size = 14),
                  panel.grid.major = element_line(colour = "white", 
                                                    size = 0.1),
                  panel.grid.minor = element_line(colour = "white", 
                                                    size = 0.1))

# And plot
plot_model(M1,
           type = "pred", 
           terms = c("anglers", "fSeason"),
           colors = c("red2", "blue2", "green2"),
           show.data = T,
           pred.type = c("fe"),
           title = "",
           show.legend = TRUE,
           jitter = 0.1,
           axis.title = c("No. anglers",
                          "Catch (kg)"),
           show.values = F) + My_theme +
  scale_y_continuous(limits = c(-5, 45)) + 
  scale_x_continuous(limits = c(2, 12))

# Summarise neatly in a table
tab_model(M1,
          show.zeroinf = F,
             dv.labels = c("Gaussian LM (trout)"),
           string.pred = "Coefficient",
             string.ci = "Conf. Int (95%)",
              string.p = "P-value",
               p.style = c("numeric"),
                emph.p = FALSE,
             transform = NULL)

###############################

# Use model to make estimate of standardised CPUE for trout:

# E.g. estimated catch of trout in spring with 5 anglers?
# Catch = B1 + B2 x anglers(5) + B3 x Season(spring)
# Catch = 3.13 + (1.8 * 5) + 7.57  = 19.7 kg

# Also do this using the 'predict' command

# Put specification in a dataframe
newdf = data.frame(anglers = 5, 
                   fSeason = "spring")

# Use model parameters to predict catch
P1 <- predict(M1, newdf, type = "response")
round(P1,1)
# 19.7 kg trout

# Also tabulate predictions with 'ggpredict' (from 'ggeffects' library)
ggpredict(M1, c("anglers", "fSeason"))

# And plot
# Catch as a function of effort
plot(ggpredict(M1, c("anglers")))
plot(ggpredict(M1, c("anglers", "fSeason")))

# Plot model with interaction...
plot(ggpredict(M2, c("anglers", "fSeason")))
# Some variation in slopes...maybe we should have used this model

###############################

# ANYTHING ELSE?

# 1. Try a GAM - smoother on 'anglers', with a 
# different smoother for each season
GAM1 <- gam(Res1 ~ s(anglers, 
                     by = fSeason), 
                   data = trout1)

summary(GAM1, show.signif.stars = FALSE)
# Approximate significance of smooth terms:
#                          edf    Ref.df  F       p-value
# s(anglers):fSeasonautumn 3.059  3.777   1.115   0.381
# s(anglers):fSeasonspring 1.000  1.000   1.870   0.183
# s(anglers):fSeasonwinter 1.000  1.000   2.052   0.164

# No significant smoothers


############################### END

