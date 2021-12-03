#ASSIGNMENT PART 1

data_assign_1 = read.csv("https://tinyurl.com/ha-dataset1")
View(data_assign_1)

#The following packages were relevant for the first part of the assignment. 

library(tidyverse)
library(psych)
library(car) 
library(lmtest) 
library(sandwich) 
library(boot) 
library(lmboot) 
library(dplyr)
library(gridExtra)
library(lm.beta)


#Started by doing basic descriptive statistics. 

data_assign_1 %>% 
  summary()

data_assign_1 %>% 
  describe()


#Then doing basic histograms. 

data_assign_1 %>%
  ggplot() + aes(x = pain) + geom_histogram()

data_assign_1 %>%
  ggplot() + aes(x = STAI_trait) + geom_histogram()

data_assign_1 %>%
  ggplot() + aes(x = pain_cat) + geom_histogram()

data_assign_1 %>%
  ggplot() + aes(x = mindfulness) + geom_histogram()

data_assign_1 %>%
  ggplot() + aes(x = cortisol_serum) + geom_histogram()

data_assign_1 %>%
  ggplot() + aes(x = cortisol_saliva) + geom_histogram()

data_assign_1 %>%
  ggplot() + aes(x = age) + geom_histogram()

data_assign_1 %>%
  ggplot() + aes(x = IQ) + geom_histogram()

data_assign_1 %>%
  ggplot() + aes(x = weight) + geom_histogram()

data_assign_1 %>%
  ggplot() + aes(x = household_income) + geom_histogram()


#And then scatterplots, as well as scatterplots with a regression line to see if any data is pulling the line.

data_assign_1 %>%
  ggplot() + aes(y = pain, x = STAI_trait) + geom_point()

data_assign_1 %>%
  ggplot() + aes(x = pain, y = STAI_trait) + geom_point() + geom_smooth(method = "lm", se = F)

data_assign_1 %>%
  ggplot() + aes(y = pain, x = pain_cat) + geom_point()

data_assign_1 %>%
  ggplot() + aes(x = pain, y = pain_cat) + geom_point() + geom_smooth(method = "lm", se = F)

data_assign_1 %>%
  ggplot() + aes(y = pain, x = mindfulness) + geom_point()

data_assign_1 %>%
  ggplot() + aes(x = pain, y = mindfulness) + geom_point() + geom_smooth(method = "lm", se = F)

data_assign_1 %>%
  ggplot() + aes(y = pain, x = cortisol_saliva) + geom_point()

data_assign_1 %>%
  ggplot() + aes(x = pain, y = cortisol_saliva) + geom_point() + geom_smooth(method = "lm", se = F)

data_assign_1 %>%
  ggplot() + aes(y = pain, x = cortisol_serum) + geom_point()

data_assign_1 %>%
  ggplot() + aes(x = pain, y = cortisol_serum) + geom_point() + geom_smooth(method = "lm", se = F)

data_assign_1 %>%
  ggplot() + aes(y = age, x = pain) + geom_point(size = 3) + geom_line()


#Looking at the plots, it is clear that there are two outliers in the dataset. However, by just looking at the plots, it is not clear which data it is.
#Either way, we build the first model now. 

model1 <- lm(pain ~ age + sex, data = data_assign_1)

model1


#After building the model, we check if the assumptions are held true for it. 

#For dealing with outliers, we first start off with identifiying extreme cases. 
#This can be identified by visualizing the data and the relationship of the outcome and the predictors one by one.

data_assign_1 %>%
  mutate(rownum = row.names(data_assign_1)) %>%
  ggplot() + aes(x = pain, y = age, label = rownum) + geom_point() + geom_text()

data_assign_1 %>%
  mutate(rownum = row.names(data_assign_1)) %>%
  ggplot() + aes(x = pain, y = sex, label = rownum) + geom_point() + geom_text()


#Then, we want to identify cases with high leverage.

data_assign_1 %>%
  ggplot() + aes(x = pain, y = age) + geom_point() + geom_smooth(method = "lm")

data_assign_1 %>%
  ggplot() + aes(x = pain, y = sex) + geom_point() + geom_smooth(method = "lm")


#This can be done with the Cook's distance as well. 

model1 %>% 
  plot(which = 5) #Here, in model 1, we see that 88, 47 and 120 is marked. 

model1 %>% 
  plot(which = 4) #Here as well, 88, 47 and 120 is marked. 

#ASSUMPTIONS
#Assumptions of normality for model 1

model1 %>% 
  plot(which = 2) #A QQ plot. Rows 88, 47 and 30 are marked. However, 47 and 30 are not deviating a lot from the line and are therefore not excluded from the dataset. Row 88 is.

data_filtered_1 <- data_assign_1 %>%
  slice(-c(88))

#A new model with the filtered dataset is therefore created. We check for normality once again. 

model1.2 <- lm(pain ~ age + sex, data = data_filtered_1)

#Assumptions of normality for model 1.2

model1.2 %>% 
  plot(which = 2) #A QQ plot. Now they are more on the line.

#Assumptions of linearity for model 1.2 

model1.2 %>% 
  residualPlots() #Even though there is a curve, it might be because the graph is zoomed in. However, when looking at the test statistics, it is non-significant, meaning that the assumption of linearity is held true.

#Assumptions of homoscedasticty for model 1.2

model1.2 %>% 
  plot(which = 3) #A plot of the standardized residuals. 

model1.2 %>%
  ncvTest() #A p-value < 0.05 in this test would indicate a violation of the assumption of homoscedasticity. Since the p-value is p = 0.12, it is not violated.

model1.2 %>%
  bptest() #Same logic follows there as for the ncvTest. The p-value is p = 0.21, meaning that the homoscedasticity is not violated.

#Assumptions of no multicollinearity in model 1.2. 

model1.2 %>% 
  vif() #If the VIF is above 3, we have a problem. Our VIF-score is 1 for age, and 1 for sex, meaning that there is no problem of multicollinearity in this model. 

#That concludes the assumption checks for model 1 (with sex and age as predictors). Now we move on to model 2.

model2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_saliva + cortisol_serum, data = data_filtered_1)

model2

#After building the model, we check if the assumptions are held true for it. 

#For dealing with outliers, we first start off with identifiying extreme cases. 
#This can be identified by visualizing the data and the relationship of the outcome and the predictors one by one.

data_filtered_1 %>%
  mutate(rownum = row.names(data_filtered_1)) %>%
  ggplot() + aes(x = pain, y = age, label = rownum) + geom_point() + geom_text()

data_filtered_1 %>%
  mutate(rownum = row.names(data_filtered_1)) %>%
  ggplot() + aes(x = pain, y = sex, label = rownum) + geom_point() + geom_text()

data_filtered_1 %>%
  mutate(rownum = row.names(data_filtered_1)) %>%
  ggplot() + aes(x = pain, y = STAI_trait, label = rownum) + geom_point() + geom_text()  #Here, 34 is an outlier.

data_filtered_1 %>%
  mutate(rownum = row.names(data_filtered_1)) %>%
  ggplot() + aes(x = pain, y = pain_cat, label = rownum) + geom_point() + geom_text()

data_filtered_1 %>%
  mutate(rownum = row.names(data_filtered_1)) %>%
  ggplot() + aes(x = pain, y = mindfulness, label = rownum) + geom_point() + geom_text()

data_filtered_1 %>%
  mutate(rownum = row.names(data_filtered_1)) %>%
  ggplot() + aes(x = pain, y = cortisol_saliva, label = rownum) + geom_point() + geom_text()

data_filtered_1 %>%
  mutate(rownum = row.names(data_filtered_1)) %>%
  ggplot() + aes(x = pain, y = cortisol_serum, label = rownum) + geom_point() + geom_text()

#Then, we want to identify cases with high leverage.

data_filtered_1 %>%
  ggplot() + aes(x = pain, y = age) + geom_point() + geom_smooth(method = "lm")

data_filtered_1 %>%
  ggplot() + aes(x = pain, y = sex) + geom_point() + geom_smooth(method = "lm")

data_filtered_1 %>%
  ggplot() + aes(x = pain, y = STAI_trait) + geom_point() + geom_smooth(method = "lm")

data_filtered_1 %>%
  ggplot() + aes(x = pain, y = pain_cat) + geom_point() + geom_smooth(method = "lm")

data_filtered_1 %>%
  ggplot() + aes(x = pain, y = mindfulness) + geom_point() + geom_smooth(method = "lm")

data_filtered_1 %>%
  ggplot() + aes(x = pain, y = cortisol_saliva) + geom_point() + geom_smooth(method = "lm")

data_filtered_1 %>%
  ggplot() + aes(x = pain, y = cortisol_serum) + geom_point() + geom_smooth(method = "lm")

#Cook's distance.

model2 %>% 
  plot(which = 5) #Here, in model 1, we see that 34 is marked. 

#Therefore, we exclude 34 from the dataset. 

data_filtered_2 <- data_assign_1 %>%
  slice(-c(88, 34))

#A new model with the filtered dataset is therefore created.  

model2.1 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_saliva + cortisol_serum, data = data_filtered_2)

#Since we excluded 34 from the dataset, a new model for model 1 is also created. 

model1.3 <- lm(pain ~ age + sex, data = data_filtered_2)

#Assumptions of normality for model 2.1

model2.1 %>% 
  plot(which = 2) #A QQ plot. Now they are more on the line.

#Assumptions of linearity for model 2.1

model2.1 %>% 
  residualPlots() #The assumptions is not violated. 

#Assumptions of homoscedasticty for model 2.1

model2.1 %>% 
  plot(which = 3) #A plot of the standardized residuals. 

model2.1 %>%
  ncvTest() #Since the p-value is p = 0.87, it is not violated.

model2.1 %>%
  bptest() #The p-value is p = 0.87, meaning that the homoscedasticity is not violated.

#Assumptions of no multicollinearity in model 2.1. 

model2.1 %>% 
  vif() #If the VIF is above 3, we have a problem. 
#Our VIF-score:
  #age: 1.47
  #sex: 1.12
  #anxiety: 1.6
  #pain_cat: 2.3
  #mindfulness: 1.5
  #cortisol_saliva: 5.0
  #cortisol_serum: 4.7

#This means that the assumption of no multicollinearity is violated in model 2. 
#To solve this violation, we can remove highly correlated variables. Since previous studies show that serum cortisol is regarded as more reliably related to stress, the decision has been made to remove cortisol_saliva from the dataset.

data_filtered_2 = select(data_filtered_2, -8)

describe(data_filtered_2)
summary(data_filtered_2)

#Again, we create models based on the new dataset.

MODEL2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_filtered_2)
MODEL1 <- lm(pain ~ age + sex, data = data_filtered_2)

#And we check the VIF-value for model 2 again. 

MODEL2 %>% 
  vif() #Now, there are no VIF-values over 3. 

#However. Since we changed the dataset, we have to re-run model diagnostics for both the models again. 

#Assumptions of normality for model 1

MODEL1 %>% 
  plot(which = 2) #A QQ plot. Now they are more on the line.

#Assumptions of linearity for model 1

MODEL1 %>% 
  residualPlots() #The assumptions is not violated. 

#Assumptions of homoscedasticty for model 1

MODEL1 %>% 
  plot(which = 3) #A plot of the standardized residuals. 

MODEL1 %>%
  ncvTest() #Since the p-value is p = 0.10, it is not violated.

MODEL1 %>%
  bptest() #The p-value is p = 0.19, meaning that the homoscedasticity is not violated.

#Assumptions of no multicollinearity in model 1 

MODEL1 %>% 
  vif()

#Assumptions of normality for model 2

MODEL2 %>% 
  plot(which = 2) #A QQ plot. Now they are more on the line.

#Assumptions of linearity for model 2

MODEL2 %>% 
  residualPlots() #The assumptions is not violated. 

#Assumptions of homoscedasticty for model 2

MODEL2 %>% 
  plot(which = 3) #A plot of the standardized residuals. 

MODEL2 %>%
  ncvTest() #Since the p-value is p = 0.79, it is not violated.

MODEL2 %>%
  bptest() #The p-value is p = 0.89, meaning that the homoscedasticity is not violated.

#Assumptions of no multicollinearity in model 2

MODEL2 %>% 
  vif()

#Now to the model comparison to see if new information was gained about pain. 

summary(MODEL1)
AIC(MODEL1)

summary(MODEL2)
AIC(MODEL2)

confint(MODEL1)
lm.beta(MODEL1)

confint(MODEL2)
lm.beta(MODEL2)

summary(MODEL1)$adj.r.squared
summary(MODEL2)$adj.r.squared

AIC(MODEL1)
AIC(MODEL2)

anova(MODEL1, MODEL2)

#ASSIGNMENT PART 2

full.model <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = data_filtered_2)

#Assumptions of normality for model 3

full.model %>% 
  plot(which = 2) #A QQ plot. Now they are more on the line.

#Assumptions of linearity for model 3

full.model %>% 
  residualPlots() #The assumptions is not violated. 

#Assumptions of homoscedasticty for model 3

full.model %>% 
  plot(which = 3) #A plot of the standardized residuals. 

full.model %>%
  ncvTest() #Since the p-value is p = 0.86, it is not violated.

full.model %>%
  bptest() #The p-value is p = 0.87, meaning that the homoscedasticity is not violated.

#Assumptions of no multicollinearity in model 3

full.model %>% 
  vif() #The values are all under 3. 

#Visualising the data for the new variables. 

data_filtered_2 %>%
  mutate(rownum = row.names(data_filtered_2)) %>%
  ggplot() + aes(x = pain, y = weight, label = rownum) + geom_point() + geom_text()

data_filtered_2 %>%
  mutate(rownum = row.names(data_filtered_2)) %>%
  ggplot() + aes(x = pain, y = IQ, label = rownum) + geom_point() + geom_text()

data_filtered_2 %>%
  mutate(rownum = row.names(data_filtered_2)) %>%
  ggplot() + aes(x = pain, y = household_income, label = rownum) + geom_point() + geom_text()

#Backward regression

full.model <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, 
                 data = data_filtered_2)

full.model %>% 
  step(object = full.model)

backward.model <- lm(formula = pain ~ age + pain_cat + mindfulness + cortisol_serum, 
                     data = data_filtered_2)

summary(backward.model)
confint(backward.model)
lm.beta(backward.model)

theorybased.model <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, 
                         data = data_filtered_2)

AIC(backward.model)
AIC(theorybased.model)

anova(backward.model, theorybased.model)

AIC(backward.model)
AIC(full.model)

data_assignment_part_2 = read.csv("https://tinyurl.com/87v6emky")
View(data_assignment_part_2)
summary(data_assignment_part_2)

backward.model2 <- lm(formula = pain ~ age + pain_cat + mindfulness + cortisol_serum, 
                         data = data_assignment_part_2)

theorybased.model2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, 
                         data = data_assignment_part_2)

deviance(backward.model2)
deviance(theorybased.model2)

sum(resid(backward.model2)^2)
sum(resid(theorybased.model2)^2)

backward.model2 %>% 
  summary()

theorybased.model2 %>% 
  summary()

AIC(backward.model, theorybased.model)
AIC(backward.model2, theorybased.model2)

anova(backward.model, theorybased.model)
anova(backward.model2, theorybased.model2)



