######################################

# R code for hilsa (Tenualosa ilisha) CPUE data
# Data from FAO Bangladesh (1992-1993)

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
library(lattice)  
library(ggplot2)
library(GGally)
library(mgcv)
library(plyr)
library(lme4)
library(car)
library(gridExtra)
library(GGally)
library(DHARMa)
library(glmmTMB)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(performance)
library(tweedie)
library(ggeffects)

######################################

#Import hils data
hils <- read_csv(file = "hilsha.csv")

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
str(hils)

# Are there missing values?
colSums(is.na(hils))

# Year    Month     Area     Boat     Gear  Nlength TripDays    Catch 
# 0        0        0        0        0        0        0        0 
# No missing data

# Are data balanced among levels of the categorical covariates?

# Balance between years?
table(hils$Year)
# 1992 1993 
# 705  516  

# Among months
table(hils$Month)
# 1   2   3   4   5   6   7   8   9   10  11  12 
# 110 125  73  55  65  88 108 139 119 112 121 106 

# Create variable combining year and month
hils$fYear <- as.factor(hils$Year)
hils$fMon  <- as.factor(hils$Month)

hils$YrMonth <- paste(hils$Year, hils$Month, sep = "_")
hils$YrMonth <- as.factor(hils$YrMonth)
table(hils$YrMonth)
# 1992_10 1992_11 1992_12  1992_7  1992_8  1992_9  1993_1  1993_2  1993_3  1993_4  1993_5  1993_6 
# 112     121     106     108     139     119     110     125      73      55      65      88 
# Names are awkward and in the wrong order...


# Rename levels
hils$YrMonth <- dplyr::recode(hils$YrMonth,
                             "1993_6"  = "Jun93", 
                             "1993_5"  = "May93", 
                             "1993_4"  = "Apr93", 
                             "1993_3"  = "Mar93", 
                             "1993_2"  = "Feb93", 
                             "1993_1"  = "Jan93", 
                             "1992_12" = "Dec92", 
                             "1992_11" = "Nov92",
                             "1992_10" = "Oct92",
                             "1992_9"  = "Sep92",
                             "1992_8"  = "Aug92",
                             "1992_7"  = "Jul92")

# Ensure time periods in chronological order
hils$YrMonth <- relevel(hils$YrMonth, ref = "Jun93")
hils$YrMonth <- relevel(hils$YrMonth, ref = "May93")
hils$YrMonth <- relevel(hils$YrMonth, ref = "Apr93")
hils$YrMonth <- relevel(hils$YrMonth, ref = "Mar93")
hils$YrMonth <- relevel(hils$YrMonth, ref = "Feb93")
hils$YrMonth <- relevel(hils$YrMonth, ref = "Jan93")
hils$YrMonth <- relevel(hils$YrMonth, ref = "Dec92")
hils$YrMonth <- relevel(hils$YrMonth, ref = "Nov92")
hils$YrMonth <- relevel(hils$YrMonth, ref = "Oct92")
hils$YrMonth <- relevel(hils$YrMonth, ref = "Sep92")
hils$YrMonth <- relevel(hils$YrMonth, ref = "Aug92")
hils$YrMonth <- relevel(hils$YrMonth, ref = "Jul92")

table(hils$YrMonth)
# Jul92 Aug92 Sep92 Oct92 Nov92 Dec92 Jan93 Feb93 Mar93 Apr93 May93 Jun93 
# 108   139   119   112   121   106   110   125    73    55    65    88  

# Among areas
table(hils$Area)
# Akhlaspur  BayofBengal  Bhola     Bolashia Chandramohan CharBhoirobi   Fosondopur       Horina 
# 6           74           10            9          180           16           15           31 
# Katakhali   Lakshmipur    Lalpur     Logimara  LowerMeghna   LowerPadma      Monipur      Mulhead 
# 29          179            4            8          364          259            7            3 
# Noakhali    Noirhat Rajrajeshwar     Ronagoal 
# 3            4           18            2 
# Severe imbalance - 3 main fishing areas: BayofBengal, LowerMeghna, LowerPadma

table(hils$Gear)
# Gillnet 
# 1221
# I filtered out other gears in Excel

######################################

# OUTLIERS

#Order data
hils <- hils %>%
  mutate(order = seq(1:nrow(hils)))

#Select continuous variables to plot
p1 <- multi_dotplot(hils, order, Nlength)
p2 <- multi_dotplot(hils, order, TripDays)
p3 <- multi_dotplot(hils, order, Catch)

#Plot as a grid
grid.arrange(p1, p2, p3, nrow = 1)
# A clear outlier in Nlength

# Use Grubbs' test?


# Remove outlier from Nlength
hils1 <- hils[which(hils$Nlength <750),]
dim(hils1)
# We have lost a datum

#Select continuous variables to plot
p1 <- multi_dotplot(hils1, order, Nlength)
p2 <- multi_dotplot(hils1, order, TripDays)
p3 <- multi_dotplot(hils1, order, Catch)
grid.arrange(p1, p2, p3, nrow = 1)
# Better

# Drop all except 3 main fishing areas
hils2 <- hils1[hils1$Area %in% c("BayofBengal", "LowerMeghna", "LowerPadma"), ]
table(hils2$Area)
# BayofBengal LowerMeghna  LowerPadma 
#          253         364         259 
# Better balance

dim(hils1) - dim(hils2)
# lost 344 rows of data...is this the right thing to do?

######################################

#NORMALITY AND HOMOGENEITY OF DEPENDENT VARIABLES

# Frequency polygon plot for catch
hils2 %>% ggplot(aes(Catch)) +
  geom_freqpoly(bins = 4) +
  labs(x = "hilsa caught", y = "Frequency") +
  My_theme +
  theme(panel.border = element_rect(colour = "black", 
                                    fill=NA, size = 1))

#Shapiro-Wilk test for deviation from normality
shapiro.test(hils2$Catch)

# Shapiro-Wilk normality test
# 
# data:  hils$Catch
# W = 0.25, p-value < 2.2e-16

# Are we concerned?


# Patterns in the variance? (Evidence for lack of homogeneity)
ggplot(hils2, aes(x = TripDays, y = (Catch))) +
  ylim(-1.5,1250) + xlim(0,12) +
  geom_jitter(shape = 16, size = 2.5, alpha = 0.3, height = 0.25, width = 0.5) +
  theme(panel.background = element_blank()) +
  theme(panel.border = element_rect(fill = NA, size = 1)) +
  theme(strip.background = element_rect(fill = "white", 
                   color = "white", size = 1)) +
  theme(text = element_text(size=13)) +
  xlab("Trip days") + ylab("Hilsa catch (kg)")

# Variance in catch varies across number of trip days
# This indicates departure from homogeneity

ggplot(hils2, aes(x = Nlength, y = (Catch))) +
  ylim(-1.5,1250) + xlim(0,510) +
  geom_jitter(shape = 16, size = 2.5, alpha = 0.3, height = 1.5, width = 5) +
  theme(panel.background = element_blank()) +
  theme(panel.border = element_rect(fill = NA, size = 1)) +
  theme(strip.background = element_rect(fill = "white", 
                                        color = "white", size = 1)) +
  theme(text = element_text(size=13)) +
  xlab("Net length (m)") + ylab("Hilsa catch (kg)")
# Same problem

######################################

#CALCULATE NUMBER OF ZEROS

# What is the percentage of zeros in the response variable

round(sum(hils2$Catch == 0) * 100 / nrow(hils2),0)
#27% - a high proportion of zero catches in the data....
# Is this too many?

######################################

# COLLINEARITY
Coll <- c("Area", "Nlength", "TripDays")

# Obtain summary using the ggpairs command from the GGally library
ggpairs(hils2[,Coll], ggplot2::aes(alpha = 0.9, colour = Area))
# Some correlation between trip length and net length (in Bay of Bengal)

#Calculate Variance Inflation Factor (VIF)
round(vif(glm(Catch ~ Area + Nlength + TripDays,
                      family = "poisson",
                      data = hils2)),2)

#         GVIF   Df      GVIF^(1/(2*Df))
# Area     1.59  2            1.12
# Nlength  1.95  1            1.40
# TripDays 1.77  1            1.33

# No obvious evidence of variance inflation

######################################

# INTERACTIONS

# Days fishing and area
ggplot(hils2, aes(x = TripDays, y = (Catch))) +
  geom_point(shape = 16, size = 3, alpha = 0.7) +
  geom_smooth(method = 'lm', colour = 'red', se = FALSE, size = 1.5) +
  My_theme +
  xlab("Days") + ylab("Catch") +
  facet_grid(.~Area)
# Difference here?

# Net length and area
ggplot(hils2, aes(x = Nlength, y = (Catch))) +
  geom_point(shape = 16, size = 3, alpha = 0.7) +
  geom_smooth(method = 'lm', colour = 'red', se = FALSE, size = 1.5) +
  My_theme +
  xlab("Net length") + ylab("Catch") +
  facet_grid(.~Area)
# Possibly?

# Days fishing and month
ggplot(hils2, aes(x = TripDays, y = (Catch))) +
  geom_point(shape = 16, size = 3, alpha = 0.7) +
  geom_smooth(method = 'lm', colour = 'red', se = FALSE, size = 1.5) +
  My_theme +
  xlab("Days") + ylab("Catch") +
  facet_grid(.~YrMonth)
# Strong seasonal effect of catch with length of trips (Aug-Nov)

# Net length and month
ggplot(hils2, aes(x = Nlength, y = (Catch))) +
  geom_point(shape = 16, size = 3, alpha = 0.7) +
  geom_smooth(method = 'lm', colour = 'red', se = FALSE, size = 1.5) +
  My_theme +
  xlab("Net length") + ylab("Catch") +
  facet_grid(.~YrMonth)
# And with nets used

# These plots indicate strong seasonality (and changes in nets used) - some collinearity

# Drop all except 3 main fishing months
hils3 <- hils2[hils2$YrMonth %in% c("Aug92", "Sep92", "Oct92"), ]
hils4 <- droplevels(hils3)
table(hils4$YrMonth)

# Aug92 Sep92 Oct92
# 86    94    86  
# Good balance


#####################################
# 
# The data exploration showed:
#   
# 1.	Outlier in net length (datum removed)
# 2.  Departure from normality
# 3.	Some departure from homogeneity
# 4.	Zeros in the response variable
# 5.	No serious collinearity
# 6.  Some imbalance (but refining the data removes this)

#####################################

# Aim: Standardise hilsa catch (kg) as a function of effort (trip days, Nlength)
#      among months and regions

# G1 <- glmmTMB(Catch ~ Area + Nlength + TripDays + YrMonth,
#                       family = "gaussian"(link = "identity"),
#                       data = hils4)
# 
# par(mfrow = c(1,1), mar = c(5,5,2,2))
# ResG1 <- resid(G1, type = "pearson")
# FitG1 <- fitted(G1)
# plot(x = FitG1,
#      y = ResG1,
#      xlab = "Fitted values",
#      ylab = "Pearson residuals",
#      pch = 16, cex.lab = 1.5)
# abline(v = 1, h = 1, lty = 2)

# Start with Poisson and include all covariates
M1 <- glmmTMB(Catch ~ Area + Nlength + TripDays + YrMonth,
                      family = "poisson"(link = "log"),
                      data = hils4)

check_overdispersion(M1)

#       dispersion ratio =    74.825
#  Pearson's Chi-Squared = 19379.679
#                p-value =   < 0.001
# The model is overdispersed
# Why? 

# Try a negative binomial
M2 <- glmmTMB(Catch ~ Area + YrMonth + Nlength + TripDays,
                      family = "nbinom1"(link = "log"),
                      ziformula=~ 0,
                      data = hils4)
check_overdispersion(M2)

#      dispersion ratio =   0.891
# Pearson's Chi-Squared = 229.806
#               p-value =   0.896
# Better. Can we improve model fit?

# Handle dependency due to area and month as random terms
M3 <- glmmTMB(Catch ~ TripDays + Nlength +
                     (1|Area) + (1|YrMonth),
                      family = "nbinom1"(link = "log"),
                      ziformula=~ 0,
                      data = hils4)
# Warning message: possibly the model is overparameterised 
#(i.e. the data does not contain enough information to estimate model parameters properly)
check_overdispersion(M3)


#      dispersion ratio =   0.879
# Pearson's Chi-Squared = 228.581
#               p-value =   0.92

# Try a zero-inflated negative binomial model (ZINB1)
M4 <- glmmTMB(Catch ~ Area + Nlength + TripDays + YrMonth,
                      family = "nbinom1"(link = "log"),
                      ziformula=~ 1,
                      data = hils4)
check_overdispersion(M4)

#      dispersion ratio =   0.894
# Pearson's Chi-Squared = 229.807
#               p-value =   0.888


# Zero-inflated negative binomial (ZINB2)
M5 <- glmmTMB(Catch ~ Area + Nlength + TripDays + YrMonth,
                      family = "nbinom1"(link = "log"),
                      ziformula =~ Area + YrMonth,
                      data = hils4)
check_overdispersion(M5)

#      dispersion ratio =   0.911
# Pearson's Chi-Squared = 230.599
#               p-value =   0.841

Out <- AIC(M1,M2,M3,M4,M5)
rownames(Out) <- c("Poisson GLM",
                   "NB GLM",
                   "NB GLMM",
                   "ZINB1 GLM",
                   "ZINB2 GLM")
colnames(Out) <- c("df", "AIC")
round(Out,0)

#             df   AIC
# Poisson GLM  7 11939 <- overdispersed
# NB GLM       8  1904 <- best fit
# NB GLMM      6  1911
# ZINB1 GLM    9  1906
# ZINB2 GLM   13  1913

# GLM or GLMM?

################################

#MODEL VALIDATION
Res1 <- simulateResiduals(fittedModel = M2, plot = FALSE)
par(mfrow = c(1,1), mar = c(5,5,2,2))
plotQQunif(Res1, testOutliers = TRUE, testDispersion = TRUE)

# Plot the residuals vs fitted values.
par(mfrow = c(1,1), mar = c(5,5,2,2))
E1 <- resid(M2, type = "pearson")
F1 <- fitted(M2)
plot(x = F1, 
     y = E1,
     xlab = "Fitted values",
     ylab = "Pearson residuals",
     pch = 16, cex.lab = 1.5)
abline(v = 1, h = 1, lty = 2)
# Not pretty...

# Plot the residuals vs parameters in the model
par(mfrow = c(2,2), mar = c(5,5,2,2))

#Year
plot(x = hils4$TripDays, 
     y = E1,
     xlab = "Days fishing",
     ylab = "Pearson residuals", 
     pch = 16, cex.lab = 1.5)
abline(0,0, lty=2)

plot(x = hils4$Nlength, 
     y = E1,
     xlab = "Gillnet length",
     ylab = "Pearson residuals", 
     pch = 16, cex.lab = 1.5)
abline(0,0, lty=2)

boxplot(E1 ~ Area, data = hils4,
        xlab = "Area",
        ylab = "",
        range=0,
        pch = 16, cex.lab = 1.5)
abline(0,0, lty=2)

boxplot(E1 ~ YrMonth, data = hils4,
        xlab = "Month",
        ylab = "",
        range=0,
        pch = 16, cex.lab = 1.5)
abline(0,0, lty=2)

# Possible zero-inflation - simulate data from model parameters
# then plot as a frequency histogram. We expect the observed
# number of zeros in the data should match simulated zeros
par(mfrow = c(1,1), mar = c(5,5,3,3))

# Model M2 (NB GLMM)
testZeroInflation(M2)

# Model M2 (NB GLMM)
testZeroInflation(M3)

#######################################

# Plot model

set_theme(
  axis.linecolor = "black", 
  axis.textcolor = "black",
  axis.tickslen = 0.2,
  axis.title.size = 0.7,
  axis.title.color = "black",
  axis.textsize = 0.8,
  geom.outline.size = 1, 
  base = theme_blank())

# Plot fixed effects
plot_model(M2, vline.color = "red", 
           sort.est = FALSE, 
           show.values = TRUE)

# Plot separately

# Plot year
plot1 <- plot_model(M2, type = "pred", terms = c("TripDays"), show.data = F,
                    title = "", 
                    axis.title = c("Days fishing","Hilsha catch (kg)"))

# Plot log flow
plot2 <- plot_model(M2, type = "pred", terms = c("Nlength"), show.data = F,
                    title = "", 
                    axis.title = c("Gillnet length (m)","Hilsha catch (kg)"))

# Plot area
plot3 <- plot_model(M2, type = "pred", terms = c("Area"), show.data = F,
                    title = "", 
                    axis.title = c("Area","Hilsha catch (kg)"))

# Plot month
plot4 <- plot_model(M2, type = "pred", terms = c("YrMonth"), show.data = F,
                    title = "", 
                    axis.title = c("Month","Hilsha catch (kg)"))

grid.arrange(plot1, plot2, plot3, plot4, nrow = 2)

# Define preferred figure format
My_theme <- theme(panel.background = element_blank(),
                  panel.border = element_rect(fill = NA, size = 1),
                  strip.background = element_rect(fill = "white", 
                                                  color = "white", size = 1),
                  text = element_text(size = 14),
                  panel.grid.major = element_line(colour = "white", size = 0.1),
                  panel.grid.minor = element_line(colour = "white", size = 0.1))

# Plot factors together - days
plot_model(M2,
           type = "pred", 
           terms = c("TripDays", "Area", "YrMonth"),
           colors = c("blue", "firebrick2", "green2"),
           show.data = F,
           pred.type = c("fe"),
           title = "",
           axis.title = c("Days fishing", 
                          "Catch (kg)"),
           show.legend = T,
           show.values = F) + My_theme

# Plot factors together - net length
plot_model(M2,
           type = "pred", 
           terms = c("Nlength", "Area", "YrMonth"),
           colors = c("blue", "firebrick2", "green2"),
           show.data = F,
           pred.type = c("fe"),
           title = "",
           axis.title = c("Gillnet length (m)", 
                          "Catch (kg)"),
           show.legend = T,
           show.values = F) + My_theme


# Model parameters
summary(M2)

# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)      2.491404   0.219632  11.344  <0.001
# AreaLowerMeghna -0.111035   0.188491  -0.589   0.556    
# AreaLowerPadma  -0.438065   0.229137  -1.912   0.056
# YrMonthSep92    -0.075127   0.141731  -0.530   0.596   
# YrMonthOct92     0.382853   0.141948   2.697   0.007
# Nlength          0.001323   0.000974   1.358   0.174   
# TripDays         0.407983   0.040177  10.155  <0.001

tab_model(M2,
          show.zeroinf = F,
          dv.labels = c("Generalised Poisson GLM (Hilsha)"),
          string.pred = "Coeffcient",
          string.ci = "Conf. Int (95%)",
          string.p = "P-value",
          p.style = c("numeric"),
          emph.p = FALSE,
          transform = NULL)

#############################

# MAKE ESTIMATES

# Use model to make estimate of standardised CPUE for bitterling:
# E.g. estimated catch in LowerPadma in Oct92 with 120 m gillnet over 3 days?

# Specify observations and put in a dataframe
newdf = data.frame(Area = "LowerPadma", 
                YrMonth = "Oct92", 
                Nlength = 120, 
               TripDays = 3)

# Use model parameters to predict catch
P1 <- predict(M2, newdf, type = "response")
round(P1,0)
# 46 kg hilsa

# And compare catch in the same month with same gear and duration among each Area
newdf2 = data.frame(Area = c("BayofBengal", "LowerMeghna", "LowerPadma"), 
                 YrMonth = c("Oct92", "Oct92", "Oct92"),
                 Nlength = c(120, 120, 120),
                TripDays = c(3, 3, 3))

P2    <- predict(M2, newdf2, type = "response")
Areas <- c("BayofBengal","LowerMeghna","LowerPadma")
Out   <- data.frame(Areas,round(P2,0))
colnames(Out) <- c("Area", "Catch(kg)")
Out

#          Area  Catch(kg)
# 1 BayofBengal        71
# 2 LowerMeghna        63
# 3  LowerPadma        46

# And plot
# Catch as a function of effort
plot(ggpredict(M2, c("TripDays")))
plot(ggpredict(M2, c("TripDays", "Area")))
plot(ggpredict(M2, c("TripDays", "YrMonth")))
plot(ggpredict(M2, c("TripDays", "Area", "YrMonth")))

# But this model needs more work...
# Perhaps a zero-inflated random slope and intercept model?

###############################END
