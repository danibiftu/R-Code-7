####### Loading Required Packages ######
# Load libraries necessary for data manipulation and visualization
library(foreign)   # For reading SPSS data files
library(MASS)      # For polr function (Ordinal Logistic Regression)
library(ggplot2)   # For data visualization
library(pscl)      # For logistic regression-related functions
library(bbmle)     # For maximum likelihood estimation
library(VGAM)      # For vector generalized linear models
library(extraDistr) # For extended distributions
library(ggpubr)    # For easier plot management (ggarrange)

# ##############################
# ### Reading and Preparing Data ###
# ##############################

# Read the Anemia dataset and attach to make variables accessible
mydata <- read.spss('F:\\Lenevo File\\Anemia\\Anemia data 2016.sav')
attach(mydata) 
View(mydata)

# Load the education dataset and prepare the plot data
Edu <- read.spss('F:\\Lenevo File\\Anemia\\Plotanemia\\education.sav')
attach(Edu)
data1 <- data.frame(Education, Anemia, Percent, Year)

# Create a scatter plot to visualize education vs anemia, by year
p1 <- ggplot(data = data1, mapping = aes(x = Education, y = Percent, col = Anemia)) +
  geom_point(size = 3) +
  facet_grid(Year ~ .) +
  ylim(0, 95) +
  labs(x = "Education of Mothers", y = "Percentage of Mothers with Anemia", colour = "Levels of Anemia")

# Load the age dataset and prepare the plot data
ag <- read.spss('F:\\Lenevo File\\Anemia\\Plotanemia\\Age.sav')
attach(ag)
data2 <- data.frame(Age, Anemia, Year, Percent)

# Create a scatter plot to visualize age vs anemia, by year
p2 <- ggplot(data = data2, mapping = aes(x = Age, y = Percent, col = Anemia)) +
  geom_point(size = 3) +
  facet_grid(Year ~ .) +
  ylim(0, 95) +
  scale_color_hue(l = 60) +
  labs(x = "Age of Mothers", y = "Percentage of Mothers with Anemia", colour = "Levels of Anemia")

# Arrange p1 and p2 side by side with a common legend at the bottom
ggarrange(p1, p2, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

# ##########################
# ### Regional Variation in Anemia ###
# ##########################

# Load region-specific data and prepare the plot
reg <- read.spss('F:\\Lenevo File\\Anemia\\Plotanemia\\Region.sav')
attach(reg)
data3 <- data.frame(Region, Anemia, Percent, Year)

# Plot region-wise variation in anemia
p3 <- ggplot(data = data3, mapping = aes(x = Region, y = Percent, col = Anemia)) +
  geom_point(size = 5) +
  facet_grid(Year ~ .) +
  ylim(0, 95) +
  labs(x = "Region of Mothers", y = "Percentage of Mothers with Anemia", colour = "Levels of Anemia")

# ##############################
# ### Residence and BMI Analysis ###
# ##############################

# Load residence data and create scatter plot for residence vs anemia
res <- read.spss('F:\\Lenevo File\\Anemia\\Plotanemia\\Residence.sav')
attach(res)
data4 <- data.frame(Residence, Anemia, Percent, Year)
p4 <- ggplot(data = data4, mapping = aes(x = Residence, y = Percent, col = Anemia)) +
  geom_point(size = 3) +
  facet_grid(Year ~ .) +
  ylim(0, 95) +
  labs(x = "Residence of Mothers", y = "Percentage of Mothers with Anemia", colour = "Levels of Anemia")

# Load BMI data and create scatter plot for BMI vs anemia
bmi <- read.spss('F:\\Lenevo File\\Anemia\\Plotanemia\\BMI.sav')
attach(bmi)
data5 <- data.frame(BMI, Anemia, Percent, Year)
p5 <- ggplot(data = data5, mapping = aes(x = BMI, y = Percent, col = Anemia)) +
  geom_point(size = 3) +
  facet_grid(Year ~ .) +
  ylim(0, 95) +
  labs(x = "Mothers BMI", y = "Percentage of Mothers with Anemia", colour = "Levels of Anemia")

# Combine p5 and p4 in a single figure with common legend at the bottom
ggarrange(p5, p4, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

# #####################
# ### Breastfeeding and Wealth ###
# #####################

# Load breastfeeding data and create a scatter plot
Breas <- read.spss('F:\\Lenevo File\\Anemia\\Plotanemia\\Breast.sav')
attach(Breas)
data6 <- data.frame(Breast, Anemia, Percent, Year)
p6 <- ggplot(data = data6, mapping = aes(x = Breast, y = Percent, col = Anemia)) +
  geom_point(size = 3) +
  facet_grid(Year ~ .) +
  ylim(0, 95) +
  labs(x = "Currently Breastfeeding", y = "Percentage of Mothers with Anemia", colour = "Levels of Anemia")

# Load wealth index data and create scatter plot
wel <- read.spss('F:\\Lenevo File\\Anemia\\Plotanemia\\Wealth.sav')
attach(wel)
data7 <- data.frame(WealthI, Anemia, Percent, Year)
p7 <- ggplot(data = data7, mapping = aes(x = WealthI, y = Percent, col = Anemia)) +
  geom_point(size = 3) +
  facet_grid(Year ~ .) +
  ylim(0, 95) +
  labs(x = "Wealth Index", y = "Percentage of Mothers with Anemia", colour = "Levels of Anemia")

# Load marital status data and create scatter plot
mst <- read.spss('F:\\Lenevo File\\Anemia\\Plotanemia\\Marital.sav')
attach(mst)
data8 <- data.frame(Marital, Anemia, Percent, Year)
p8 <- ggplot(data = data8, mapping = aes(x = Marital, y = Percent, col = Anemia)) +
  geom_point(size = 3) +
  facet_grid(Year ~ .) +
  ylim(0, 95) +
  labs(x = "Mothers' Marital Status", y = "Percentage of Mothers with Anemia", colour = "Levels of Anemia")

# Combine p7 and p8 side by side in a single plot
ggarrange(p7, p8, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

# ##########################
# ### Educational Level and Contraceptive Use ###
# ##########################

# Plot the relationship between mother's education level and anemia
educ <- read.spss('F:\\Lenevo File\\Anemia\\Plotanemia\\Education.sav')
attach(educ)
data9 <- data.frame(Education, Anemia, Percent, Year)
p9 <- ggplot(data = data9, mapping = aes(x = Education, y = Percent, col = Anemia)) +
  geom_point(size = 3) +
  facet_grid(Year ~ .) +
  ylim(0, 95) +
  labs(x = "Mothers' Education Level", y = "Percentage of Mothers with Anemia", colour = "Levels of Anemia")

# Plot the relationship between partner's education level and anemia
pedu <- read.spss('F:\\Lenevo File\\Anemia\\Plotanemia\\Peducation.sav')
attach(pedu)
data10 <- data.frame(Peducation, Anemia, Percent, Year)
p10 <- ggplot(data = data10, mapping = aes(x = Peducation, y = Percent, col = Anemia)) +
  geom_point(size = 3, alpha = 1) +
  facet_grid(Year ~ .) +
  ylim(0, 95) +
  labs(x = "Partner Education Level", y = "Percentage of Mothers with Anemia", colour = "Levels of Anemia")

# Combine p10 and p9 side by side in a single plot
ggarrange(p10, p9, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

# ##########################
# ### Contraceptive Use and Pregnancy Status ###
# ##########################

# Plot contraceptive use vs anemia level
Contra <- read.spss('F:\\Lenevo File\\Anemia\\Plotanemia\\Contraceptive.sav')
attach(Contra)
data11 <- data.frame(Contraceptive, Anemia, Percent, Year)
p11 <- ggplot(data = data11, mapping = aes(x = Contraceptive, y = Percent, col = Anemia)) +
  geom_point(size = 3) +
  facet_grid(Year ~ .) +
  ylim(0, 95) +
  labs(x = "Contraceptive Use", y = "Percentage of Mothers with Anemia", colour = "Levels of Anemia")

# Plot pregnancy status vs anemia level
Preg <- read.spss('F:\\Lenevo File\\Anemia\\Plotanemia\\Pregnance.sav')
attach(Preg)
data12 <- data.frame(Pregnance, Anemia, Percent, Year)
p12 <- ggplot(data = data12, mapping = aes(x = Pregnance, y = Percent, col = Anemia)) +
  geom_point(size = 3, alpha = 1) +
  facet_grid(Year ~ .) +
  ylim(0, 95) +
  labs(x = "Pregnancy Status", y = "Percentage of Mothers with Anemia", colour = "Levels of Anemia")

# Combine p11 and p12 side by side in a single plot
ggarrange(p11, p12, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

# ##############################
# ### Ordinal Logistic Regression ###
# ##############################

# Fit an ordinal logistic regression model for Anemia levels based on various predictors
model1 <- polr(Anemialevel ~ PlaceofResidence + WealthIndex + BMI, data = mydata)
summary(model1)  # Print a summary of the model

