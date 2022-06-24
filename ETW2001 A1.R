library(ggplot2)
library(tidyverse)
library(dplyr)
library(Hmisc)
library(GGally)
library(vtable)
library(AER)
library(ggfortify)


attach(Assignment_2_Data)
table(Country,Region)

#Descriptive analysis
summary(Assignment_2_Data)
st(Assignment_2_Data, add.median = TRUE)
#summary statistics for life satisfaction
A2_LS <- select(Assignment_2_Data,`Life Satisfaction (score 1 to 10, higher the happier)`)
A2_LS
st(A2_LS, add.median = TRUE)

#summary statistics for for positive correlated variables
A2_ST_positive <- select(Assignment_2_Data, `Life expectancy`, `Access to electricity (% of population)`, `GDP (constant 2010 US$)`, `Ratio of female to male labor force participation rate (%) (modeled ILO estimate)`)
A2_ST_positive
st(A2_ST_positive, add.median = TRUE, title = "Summary statistics for positive correlated variables")

#summary statistics for negative correlated variables
A2_ST_negative <- select(Assignment_2_Data, `Inflation rate (%)`,`Unemployment rate (%)`,`Infant Mortality (per 1,000 live births)`,`Dependency ratio (% of working population)`)
A2_ST_negative
st(A2_ST_negative, add.median = TRUE, title = "Summary statistics for negative correlated variables")

#summary statistics for regions with life satisfaction
st(Assignment_2_Data, group = 'Region', add.median = TRUE,vars = c("Life Satisfaction (score 1 to 10, higher the happier)"), group.long = TRUE,digits = 2, title = "Life Satisfaction Summary over the Regions")

#Histogram of life satisfaction
ggplot(Assignment_2_Data, aes(`Life Satisfaction (score 1 to 10, higher the happier)`)) +
  geom_vline(xintercept = mean(`Life Satisfaction (score 1 to 10, higher the happier)`), lwd = 2) +
  geom_histogram(color = "black",fill = "grey") +
  labs(title = "Distribution of life satisfaction around the world",
       x = "Life satisfaction scores",
       y = "Count of countries") +
  theme_minimal()


#Correlation analysis
#This plot combines correlation coefficients, correlation tests (via the asterisks next to the coefficients2) and scatter plots for all possible pairs of variables present in a data set.
#Positive correlations
ggpairs(Assignment_2_Data[, c("Life Satisfaction (score 1 to 10, higher the happier)","Life expectancy","Access to electricity (% of population)", "GDP (constant 2010 US$)", "Ratio of female to male labor force participation rate (%) (modeled ILO estimate)")], lower=list(continuous=wrap("smooth", colour="blue")))

#Negative correlations
ggpairs(Assignment_2_Data[, c("Life Satisfaction (score 1 to 10, higher the happier)","Inflation rate (%)", "Unemployment rate (%)", "Infant Mortality (per 1,000 live births)", "Dependency ratio (% of working population)")], lower=list(continuous=wrap("smooth", colour="blue")))

#No correlations
ggpairs(Assignment_2_Data[, c("Life Satisfaction (score 1 to 10, higher the happier)", "Suicide mortality rate (per 100,000 population)","CO2 emissions (kg per 2017 PPP $ of GDP)", "Population", "Population density (people per sq. km of land area)")],lower=list(continuous=wrap("smooth", colour="blue")))


#1) Life satisfaction over the countries/region

#Box plot for life satisfaction in every region
ggplot(Assignment_2_Data, aes(x=`Life Satisfaction (score 1 to 10, higher the happier)`, y=Region)) + 
  geom_boxplot(fill="lightblue")



#Regression Model analysis

#Regression Model (1) (USED)
multireg_LS1 <- lm(`Life Satisfaction (score 1 to 10, higher the happier)` ~ `Life expectancy` + log(`GDP (constant 2010 US$)`) + `Access to electricity (% of population)`+ log(`Unemployment rate (%)`) + `Inflation rate (%)` +`Dependency ratio (% of working population)`+ log(`Infant Mortality (per 1,000 live births)`), data=Assignment_2_Data)
summary(multireg_LS1)
#goodness of fit
summary(multireg_LS1)$r.squared
vifA2 <- 1/(1 - summary(multireg_LS1)$r.squared)
vifA2
#Residual plot
autoplot(multireg_LS1, which = 1:2, nrow = 2)
resid1_A2 <- residuals(multireg_LS1)
ggplot(Assignment_2_Data, aes(x = resid1_A2)) + 
  geom_histogram(fill = "steelblue", color ="white") +
  labs(title="Residual Histogram of LM1", x="Residuals", y="Frequency") #residual histogram

#Regression Model (2) (not used)
multireg_LS2 <- lm(`Life Satisfaction (score 1 to 10, higher the happier)` ~ `Life expectancy` + `Access to electricity (% of population)` + `Dependency ratio (% of working population)`+ log(`Infant Mortality (per 1,000 live births)`)+ log(`Unemployment rate (%)`), data=Assignment_2_Data)
summary(multireg_LS2)
#goodness of fit
summary(multireg_LS2)$r.squared
vif2A2 <- 1/(1 - summary(multireg_LS2)$r.squared)
vif2A2
#Residual plot
autoplot(multireg_LS2, which = 1:2, nrow = 2) #Q-Q plot and residual vs fitted
resid2_A2 <- residuals( )
ggplot(data = Assignment_2_Data, aes(x = resid2_A2)) + 
  geom_histogram(fill = "steelblue", color ="white") +
  labs(title="Residual Histogram of LM2", x="Residuals", y="Frequency") #residual histogram
