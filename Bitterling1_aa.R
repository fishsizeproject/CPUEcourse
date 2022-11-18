######################################

# R code for bitterling CPUE data
# Data from South Moravia electrofishing surveys (1995-1998)

######################################

# Start by loading packages
library(arm)
library(car)
library(ggplot2)
library(lattice)
library(lawstat)
library(ggeffects)
library(outliers)
library(tidyverse)
library(scales)
library(lattice)  
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
library(performance)
library(ggeffects)

######################################

#Import bitterling data
bitt <- read_csv(file = "bitterling.csv")

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

##Asta: this part is great, but quite idealised, because nobody has such perfectly balanced data

#Use 'str' to inspect the dataframe
str(bitt)

# Are there missing values?
colSums(is.na(bitt))

# oxbow   year  season habitat  points   catch 
# 0       0       0       0       0       0    
# No missing data

# Are data balanced among levels of the categorical covariates?
# Balance among lochs?
table(bitt$oxbow)
# 1  2  3  4  5  6  7  8 
# 16 16 16 16 16 16 16 16 

# Among years
table(bitt$year)
# 1995 1996 1997 1998 
# 32   32   32   32 

# between habitats
table(bitt$habitat)
# open  veg 
# 64    64 

######################################

# OUTLIERS

#Order data
bitt <- bitt %>%
  mutate(order = seq(1:nrow(bitt)))

#Select continuous variables to plot
p1 <- multi_dotplot(bitt, order, points)
p2 <- multi_dotplot(bitt, order, catch)

#Plot as a grid
grid.arrange(p1, p2, nrow = 1)
# A clear outlier in catch

# Use Grubbs' test to assess whether a value that is 
# farthest (above or below) the mean is an outlier

#For catch
grubbs.test(bitt$catch, type = 10) 
#Asta: explanation is needed about this test, or perhaps a reference to it 

# data:  bitt$catch
# G = 5.60573, U = 0.75062, p-value = 1.26e-07

######################################

#NORMALITY AND HOMOGENEITY OF DEPENDENT VARIABLES

# Frequency polygon plot for catch
bitt %>% ggplot(aes(catch)) +
  geom_freqpoly(bins = 4) +
  labs(x = "Bitterling caught", y = "Frequency") +
  My_theme +
  theme(panel.border = element_rect(colour = "black", 
                                    fill=NA, size = 1))

#Shapiro-Wilk test for deviation from normality
shapiro.test(bitt$catch)

# Shapiro-Wilk normality test
# 
# data:  bitt$catch
# W = 0.81843, p-value <0.001

# Patterns in the variance? (any lack of homogeneity)
ggplot(bitt, aes(x = points, y = (catch))) +
  ylim(0,250) + xlim(20,100) +
  geom_point(shape = 16, size = 5, alpha = 0.6) +
  theme(panel.background = element_blank()) +
  theme(panel.border = element_rect(fill = NA, size = 1)) +
  theme(strip.background = element_rect(fill = "white", 
                   color = "white", size = 1)) +
  theme(text = element_text(size=13)) +
  xlab("Points") + ylab("Bitterling caught")

# Variance in catch does vary across number of points
# This indicates homogeneity

# Non-linear(?)

######################################

#CALCULATE NUMBER OF ZEROS

# What is the percentage of zeros i the response variable

round(sum(bitt$catch == 0) * 100 / nrow(bitt),0)
#1

######################################

# COLLINEARITY
Coll <- c("year", "season", "habitat", "points")

# Obtain summary using the ggpairs command from the GGally library
ggpairs(bitt[,Coll], ggplot2::aes(alpha = 0.9, colour = habitat))
# No obvious collinearity

#Calculate Variance Inflation Factor (VIF)
round(vif(glm(catch ~ year + season + habitat + points,
                      family = "poisson",
                      data = bitt)),2)

# year  season habitat  points 
#  1       1       1       1 

# No evidence of variance inflation

# ###############################

# Interactions

# Year x season
ggplot(bitt, aes(x = points, y = (catch))) +
  geom_point(shape = 16, size = 3, alpha = 0.7) +
  geom_smooth(method = 'lm', colour = 'red', se = FALSE, size = 1.5) +
  My_theme +
  xlab("Points") + ylab("Catch") +
  facet_grid(year~season)
# No

# Year x habitat
ggplot(bitt, aes(x = points, y = (catch))) +
  geom_point(shape = 16, size = 3, alpha = 0.7) +
  geom_smooth(method = 'lm', colour = 'red', se = FALSE, size = 1.5) +
  My_theme +
  xlab("Points") + ylab("Catch") +
  facet_grid(year~habitat)
# Perhaps

# Season x habitat
ggplot(bitt, aes(x = points, y = (catch))) +
  geom_point(shape = 16, size = 3, alpha = 0.7) +
  geom_smooth(method = 'lm', colour = 'red', se = FALSE, size = 1.5) +
  My_theme +
  xlab("Points") + ylab("Catch") +
  facet_grid(season~habitat)
# Yes

# Oxbow x habitat
ggplot(bitt, aes(x = points, y = (catch))) +
  geom_point(shape = 16, size = 3, alpha = 0.7) +
  geom_smooth(method = 'lm', colour = 'red', se = FALSE, size = 1.5) +
  My_theme +
  xlab("Points") + ylab("Catch") +
  facet_grid(habitat~oxbow)
# Perhaps

#####################################
# 
# The data exploration showed:
#   
# 1.	One significant outlier in catch
# 2.  Deviation from normality in response variable
# 3.	Possible departure from homogeneity
# 4.	Few zeros in the response variable
# 5.	No collinearity
# 6.  No imbalance
# 7.  Possible season x habitat interaction

# Remove outlier from catch
bitt1 <- bitt[which(bitt$catch <200),]
dim(bitt)
dim(bitt1)
# Is it acceptable to drop data?

#####################################

# Aim: Standardise bitterling catch as a function of effort (electrofishing points)
#      among years, seasons, and habitat types

# Designate categories as factors (or they will be treated as numerical)
bitt1$fOxbow   <- as.factor(bitt1$oxbow)
bitt1$fYear    <- as.factor(bitt1$year)
bitt1$fSeason  <- as.factor(bitt1$season)
bitt1$fHabitat <- as.factor(bitt1$habitat)

# Set spring and open habitat as 'baseline' with 'relevel'
bitt1$fSeason  <- relevel(bitt1$fSeason,  ref = "spring")
bitt1$fHabitat <- relevel(bitt1$fHabitat, ref = "open")

# Change label names
bitt1$fSeason <- dplyr::recode(bitt1$fSeason,
                               "spring" = "Spring", 
                               "autumn" = "Autumn")

bitt1$fHabitat <- dplyr::recode(bitt1$fHabitat,
                                "open" = "Unvegetated", 
                                "veg"  = "Vegetated")

# Standardize continuous covariates to avoid numerical problems
# bitt1$points.std <- (bitt1$points-mean(bitt1$points))/sd(bitt1$points)

# Response variable is counts, so start with Poisson - simple but makes assumptions
# Include all covariates(?)
# Include fSeason * fHabitat interaction based on data exploration

M1 <- glmmTMB(catch ~ fYear + fSeason * fHabitat + fOxbow + points,
                      family = "poisson"(link = "log"),
                      data = bitt1)

check_overdispersion(M1)

#      dispersion ratio = 2.280
# Pearson's Chi-Squared = 255.415
#               p-value < 0.001
# The model is overdispersed

# There is dependency (by design) due to oxbow lake, though we are not really interested in differences
# among oxbow lakes. In this case we can include oxbow as a 'random' term in a mixed model (GLMM)
# Here, we pool information from all oxbows to improve our estimates of each individual oxbow.
# This approach is sometimes called partial pooling and enables our conclusions to apply to all
# oxbow lakes.

M2 <- glmmTMB(catch ~ fYear + fSeason * fHabitat + points +
                      (1|fOxbow),
                      family = "poisson"(link = "log"),
                      data = bitt1)

check_overdispersion(M2)

#      dispersion ratio = 2.181
# Pearson's Chi-Squared = 257.392
#               p-value < 0.001
# Also overdispersed

# What causes overdispersion?

# An alternative distribution is negative binomial - like Poisson, but has an extra parameter 
# to model dispersion (it will cost us an extra degree of freedom though)

M3 <- glmmTMB(catch ~ fYear + fSeason * fHabitat + fOxbow + points,
                      family = "nbinom1"(link = "log"),
                      data = bitt1)

check_overdispersion(M3)

#      dispersion ratio = 1.110
# Pearson's Chi-Squared = 123.180
#               p-value = 0.202
# Not significantly overdispersed

# Or negative binomial GLMM
M4 <- glmmTMB(catch ~ fYear + fSeason * fHabitat + points +
                     (1|fOxbow),
                     family = "nbinom1"(link = "log"),
                     data = bitt1)

check_overdispersion(M4)

#      dispersion ratio = 1.003
# Pearson's Chi-Squared = 117.402
#               p-value = 0.472
                
# Not overdispersed

# Compare models with AIC
Out <- AIC(M1,M2,M3,M4)
rownames(Out) <- c("Poisson GLM",
                   "Poisson GLMM",
                   "NB GLM",
                   "NB GLMM")
colnames(Out) <- c("df", "AIC")
round(Out,0)

#              df AIC
# Poisson GLM  15 929
# Poisson GLMM  9 945
# NB GLM       16 889 <- best fit, but...dependency
# NB GLMM      10 898

# Model M4 is the best-fitting model that is not overdispersed
# and that does not ignore dependency in the data

################################

#MODEL VALIDATION

#Plot the residuals vs fitted values.
Res1 <- resid (M4, test = 'pearson')
Fit1 <- fitted(M4)

par(mfrow = c(1,1), mar = c(5,5,2,2))
plot(x = Fit1, y = Res1,
  xlab = "Fitted values",
  ylab = "Pearson residuals",
   pch = 16, cex = 1.5, cex.lab = 1.5)
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)

# Plot the residuals vs parameters in the model

# Points
par(mfrow = c(2,2), mar = c(5,5,2,2))
plot(x = bitt1$points, 
     y = Res1,
     xlab = "Points",
     ylab = "Pearson residuals", 
     pch = 16, cex = 1.5, cex.lab = 1.5)
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)

# Year
boxplot(Res1 ~ fYear, 
        data = bitt1,
        xlab = "Year",
        ylab = "",
        range = 0,
        col = "green2",
        pch = 16, cex.lab = 1.5)
abline(0,0, lty=2)

# Season
boxplot(Res1 ~ fSeason, 
        data = bitt1,
        xlab = "Season",
        ylab = "",
        range = 0,
        col = "blue2",
        pch = 16, cex.lab = 1.5)
abline(0,0, lty=2)

# Habitat
boxplot(Res1 ~ fHabitat, 
        data = bitt1,
        xlab = "Habitat",
        ylab = "",
        range = 0,
        col = "red2",
        pch = 16, cex.lab = 1.5)
abline(0,0, lty=2)
# They are all fine

# Oxbow
par(mfrow = c(1,1), mar = c(5,5,2,2))
boxplot(Res1 ~ fOxbow, 
        data = bitt1,
        xlab = "Oxbow",
        ylab = "",
        range = 0,
        col = "thistle2",
        pch = 16, cex.lab = 1.5)
abline(0,0, lty=2)
# Good


#Normality of residuals - examine plot
ggplot() +
  geom_histogram(colour = "black", fill = "red2", 
                 data = bitt1, aes(Res1), bins = 12) +
  ylab("Frequency") + xlab("Pearson residuals") +
  My_theme
  #Looks fairly normal

###############################

# Assume the model is not misfit

#Obtain model summary
summary(M4)
 
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                1.457111   0.174040   8.372  <0.001
# fYear1996                  0.020593   0.059154   0.348   0.728    
# fYear1997                 -0.153688   0.062462  -2.460   0.014
# fYear1998                 -0.587582   0.069452  -8.460  <0.001
# fSeasonautumn              0.256802   0.096397   2.664   0.008
# fHabitatveg                1.089667   0.084219  12.939  <0.001
# points                     0.022608   0.002723   8.304  <0.001
# fSeasonautumn:fHabitatveg  0.271882   0.109681   2.479   0.013

###############################

# Plot fixed effects
set_theme(
  base = theme_bw(),
  axis.textsize = 1)
plot_model(M4, 
           vline.color = "green4", 
           sort.est = FALSE,
           title = " Bitterling",
           show.zeroinf = F,
           show.values = TRUE)

# Random effects
set_theme(
  base = theme_bw(),
  axis.textsize = 1)
plot_model(M4, 
           vline.color = "green4",
           title = "Oxbow lakes",
           type = "re")

# Visualise the model as a figure
set_theme(
  axis.ticksize.x = 0.5,
  axis.ticksize.y = 0.5,
  axis.textsize.x = 0.5,
  axis.textsize.y = 0.5,
  axis.linecolor = "black", 
  axis.textcolor = "black",
  axis.tickslen = 0.2,
  axis.title.size = 1,
  axis.title.color = "black",
  axis.textsize = 1,
  geom.outline.size = 1,
  legend.pos = "bottom",
  legend.title.face = "italic",
  legend.title.color = "white",
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
                  text = element_text(size = 13),
                  panel.grid.major = element_line(colour = "white", 
                                                    size = 0.1),
                  panel.grid.minor = element_line(colour = "white", 
                                                    size = 0.1))

# And plot model
plot_model(M4,
           type = "pred", 
           terms = c("points", "fSeason", "fHabitat", "fYear"),
           colors = c("blue2", "red2"),
           show.data = T,
           pred.type = c("fe"),
           title = "",
           show.legend = TRUE,
           jitter = 0.05,
           axis.title = c("Number of electrofishing points",
                          "Catch"),
           show.values = F) + #My_theme +
  scale_y_continuous(limits = c(-5, 300)) + 
  scale_x_continuous(limits = c(20, 100)) +
  theme(strip.text.x = element_text(size = 12))

# Summarise neatly in a table
tab_model(M4,
          show.zeroinf = F,
             dv.labels = c("Generalised Poisson GLMM (Bitterling)"),
           string.pred = "Coefficient",
             string.ci = "Conf. Int (95%)",
              string.p = "P-value",
               p.style = c("numeric"),
                emph.p = FALSE,
             transform = NULL)

###############################

# ESTIMATES

# Use model to make estimate of standardised CPUE for bitterling:
# E.g. estimated catch in 1997, in vegetation, in spring, in oxbow 6, with 50 electrofishing points?

# Put specification in a dataframe
newdf = data.frame(fYear = "1997", 
                 fSeason = "Spring", 
                fHabitat = "Vegetated", 
                  points = 50,
                  fOxbow = "6")

# Use model parameters to predict catch
P1 <- predict(M4, newdf, type = "response")
round(P1,0)
# 42 bitterling

# Also tabulate predictions with 'ggpredict' (from 'ggeffects' library)
ggpredict(M4, c("points", "fSeason", "fHabitat"))

# And plot
# Catch as a function of effort
plot(ggpredict(M4, c("points")))
plot(ggpredict(M4, c("points", "fSeason")))
plot(ggpredict(M4, c("points", "fHabitat")))
plot(ggpredict(M4, c("points", "fSeason", "fHabitat"))) #Shows the interaction
plot(ggpredict(M4, c("points", "fSeason", "fYear")))
plot(ggpredict(M4, c("points", "fHabitat", "fYear")))
plot(ggpredict(M4, c("points", "fSeason", "fHabitat", "fYear")))

# Catch per unit effort
plot(ggpredict(M4, c("fYear")))
plot(ggpredict(M4, c("fYear", "fSeason")))
plot(ggpredict(M4, c("fYear", "fHabitat")))
plot(ggpredict(M4, c("fYear", "fSeason", "fHabitat")))
plot(ggpredict(M4, c("fYear", "fHabitat", "fOxbow")))
# plot(ggpredict(M4, c("fYear", "fSeason", "fHabitat", "fOxbow")))

############################### END
