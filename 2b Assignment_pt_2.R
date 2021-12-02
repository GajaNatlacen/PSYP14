# Assignment_pt_2 (Gaja Natlacen)

# First import the data set:
data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")

#------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 1. part: excluding the same participants that I did in the previous exercise: 
data_sample_1 <- data_sample_1 %>% 
  mutate(sex = factor(sex))

summary(data_sample_1) # check min and max for numerical values, if there are any NA's

data_sample_1 %>% #pain cannot go above 10 
  filter(pain > 10) 
data_sample_1 %>% #the lowest number for trait is 20 
  filter(STAI_trait < 20)

# Cutting out the errors from the data set, and making a new data set
data_sample_2 <- data_sample_1 %>% 
  slice(-c(34,88))

# Checking new data frame 
describe (data_sample_2)

#------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 2a part: write  the initial model: 
initial_model <- lm (pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = data_sample_2)

# 2b part: Check the assumptions for regression - initial model:

# a) Outliers within the model - there were no unusual errors, so I decided to leave this participants in model.  

initial_model %>% 
  plot(which = 4)

data_sample_2 %>% 
  slice(c(46,84,85))

# b) Normality - assumptions for normality holds
describe(residuals(initial_model))

# c) Linearity - assumptions for linearity holds 
initial_model%>%
  residualPlots()

# d) Homoscedasticity of variance - the assumption for homoscedasticity holds 
initial_model %>%
  ncvTest()

initial_model %>%
  bptest()

# e) Multicollinearity - every vif value is lower than 3, so we don't have to exclude any predictor
initial_model %>% 
  vif()

#-----------------------------------------------------------------------------------------------------

# 3a part: Run backwards regression: 
backward_model <- step(initial_model, direction = "backward")

# 3b part: check the assumptions for regression - backward_model:
# a) Outliers within the model - there were no unusual errors, so I decided to leave this participants in model.  

backward_model %>% 
  plot(which = 4)

data_sample_2 %>% 
  slice(c(46,102,115))

# b) Normality - assumptions for normality holds
describe(residuals(backward_model))

# c) Linearity - assumptions for linearity holds 
backward_model%>%
  residualPlots()

# d) Homoscedasticity of variance - the assumption for homoscedasticity holds 
backward_model %>%
  ncvTest()

backward_model %>%
  bptest()

# e) Multicollinearity - every vif value is lower than 3, so I don't have to exclude any predictor
backward_model %>% 
  vif()

# 3c part: Report the characteristics of the backward model fitted to data file 1 

# a) getting R2, F, df, and p value
summary(backward_model)

# b) everything put in one table - beta, standardized beta and CI
coef_table(backward_model)
#-----------------------------------------------------------------------------------------------------
# 4. part: Regression equation of the backward model: 
# Y = 1.28 - 0.04 * age + 0.11 * pain_cat - 0.27 * mindfulness + 0.53 * cortisol_serum

#-----------------------------------------------------------------------------------------------------
# 5. part: Take model3 from first part of the assignment and assign it to the theory_based_model 
model3 <- lm (pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_2)
theory_based_model <- model3

# I won't check the assumptions, since I have already done that in previous exercise (Assignment_pt_1) - they all hold true. 

#-----------------------------------------------------------------------------------------------------
# 6a part: Comparing the backward model with initial model - backward_model statistically significant fits our data better than initial_model
AIC (initial_model, backward_model)
anova(initial_model, backward_model) 

# 6b part: Comparing the backward model with theory_based model - backward_model statistically significant fits our data better than theory_based model
AIC (theory_based_model, backward_model)
anova(theory_based_model, backward_model) 

# 6c part: Comparing the model fit of three models with ANOVA - no statistically significant differences
anova(theory_based_model, backward_model, initial_model)

#-----------------------------------------------------------------------------------------------------
# For second part of the exercise, I have to import new data set - data from first set, collected on 160 new participants:

home_sample_2 <- read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_2.csv")

# 7. part: Check the data frame for any unusual errors: 

home_sample_2 <- home_sample_2 %>% 
  mutate(sex = factor(sex))
levels(home_sample_2$sex)

summary(home_sample_2) #here I can see that there is no unusual data (maybe IQ or household_income, but it is hard to make conclusion based on the information we were given, so I do not dare to exclude any of the participants)
describe(home_sample_2) #skewness and kurtosis are in the borders of normality - except for sex which is categorical variable and it doesn't make sense to change it
  
# Checking the distribution of sex variable   
home_sample_2 %>% 
  ggplot()+
  aes(x = sex) + 
  geom_bar() + 
  theme_minimal()

#-----------------------------------------------------------------------------------------------------

# 8. part: Using  two models to make predictions on a new data set: 
pred_test <- predict(theory_based_model, home_sample_2)
RSS_test <- sum((home_sample_2[,"pain"]- pred_test)^2)
RSS_test

pred_test_back <- predict(backward_model, home_sample_2)
RSS_test_back <- sum ((home_sample_2[,"pain"] - pred_test_back)^2)
RSS_test_back

#-----------------------------------------------------------------------------------------------------
# Used table - code 
coef_table = function(model){	
  require(lm.beta)	
  mod_sum = summary(model)	
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))		
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))		
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"		
  
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model), confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])), 2)), mod_sum_p_values)		
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")		
  mod_sum_table["(Intercept)","Std.Beta"] = "0"		
  return(mod_sum_table)	
}	

