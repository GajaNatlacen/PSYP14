# Assignment_pt_1 (Gaja Natlacen)

# First we have to import the data set:
data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_1.csv")

# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 1a part:  Checking the data set 
# Check the variables included in model 2 (age, sex, STAI, pain catastrophizing, mindfulness, and cortisol measures as predictors, and pain as an outcome) for coding errors, and the model itself for influential outliers. 

str(data_sample_1) #checking whole data frame (gives us what class the variables are)

data_sample_1 <- data_sample_1 %>% #define characters as factors, so we can check the levels of characters 
  mutate(sex = factor(sex)) 
levels(data_sample_1$sex) #check levels 

summary(data_sample_1) # check min and max for numerical values, if there are any NA's, 

describe(data_sample_1) #checking normality of distribution as well - skewness and kurtuosis 

# Checking which rows have unusual data:
data_sample_1 %>% #pain cannot go above 10 
  filter(pain > 10) 
data_sample_1 %>% #the lowest number for trait is 20 
  filter(STAI_trait < 20)

# Cutting out the errors from our data set, and making a new data set
data_sample_2 <- data_sample_1 %>% 
  slice(-c(34,88))

# Checking new data frame 
describe (data_sample_2)

# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 1b part: Visualization of data

# Checking for outliers in data_sample_1
data_sample_1 %>% 	
  mutate(rownum = row.names(data_sample_1)) %>% 	
  ggplot() +	
  aes(x = STAI_trait, y = pain, label = rownum) +	
  geom_label()	

# Checking some demographic values, that we are going to use in the first part 
data_sample_2 %>% 
  ggplot()+
  aes(x = sex) + 
  geom_bar() + 
  theme_minimal()

data_sample_2 %>% 
  ggplot()+
  aes(x = age) + 
  geom_bar() + 
  theme_light()

  
data_sample_2 %>% 
  ggplot()+
  aes(x = pain) + 
  geom_bar()+ 
  theme_classic()


# Checking the link between variable we want to predict and the predictors 
data_sample_2 %>% 	
  ggplot() +	
  aes(x = STAI_trait, y = pain) +	
  geom_point() +	
  geom_smooth(method = "lm", se = F)	

data_sample_2 %>% 	
  ggplot() +	
  aes(x = pain_cat, y = pain) +	
  geom_point() +	
  geom_smooth(method = "lm", se = F)	

data_sample_2 %>% 	
  ggplot() +	
  aes(x = cortisol_serum, y = pain) +	
  geom_point() +	
  geom_smooth(method = "lm", se = F)	

data_sample_2 %>% 	
  ggplot() +	
  aes(x = mindfulness, y = pain) +	
  geom_point() +	
  geom_smooth(method = "lm", se = F)


# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 2. part:  Do a hierarchical regression - models 1 and 2 
model1 <- lm (pain ~ age + sex, data = data_sample_2)
model2 <- lm (pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = data_sample_2)

# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 3. part: Check the models to see if the assumptions of linear regression hold true

# a) Outliers within the model - there were no unusual errors, so I decided to leave this participants in model.  

model1 %>% 
  plot(which = 4)

data_sample_2 %>% 
  slice(c(8,23,46))

model2 %>% 	
  plot(which = 4)	

data_sample_2 %>% 
  slice(c(46,73,85))

# b) Normality - three different ways of checking (I used described function to make conclusions)
model1 %>%
  plot(which = 2)

residuals_model1 = enframe(residuals(model1))
residuals_model1 %>%
  ggplot() + aes(x = value) + geom_histogram()

describe(residuals(model1))

model2 %>%
  plot(which = 2)

residuals_model2 = enframe(residuals(model2))
residuals_model2 %>%
  ggplot() + aes(x = value) + geom_histogram()

describe(residuals(model2))

# c) Linearity - we check the p-values
model1%>%
  residualPlots()

model2%>%
  residualPlots()

# d) Homoscedasticity of variance - I tested this assumption with all three tests  (made conclusions based on p-values)
model1 %>%
  plot(which = 3)

model1 %>%
  ncvTest()

model1 %>%
  bptest()

model2 %>%
  plot(which = 3)

model2 %>%
  ncvTest()

model2 %>%
  bptest()

# e) Multicollinearity - checking how correlated predictors are (if vif is above 3, then they are correlated too much)
model1 %>% 
  vif()

model2 %>% 
  vif()

# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 4a part: Because cortisol_serum and cortisol_saliva were correlated to much, I decided (based on previous research) to exclude cortisol_salaiva from my model:
model3 <- lm (pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_2)


# 4b part: Since now we have a model 3, we have to run all the assumptions again: 
# Normality: 
describe(residuals(model3))

# Linearity: 
model3%>%
  residualPlots()

# Homoscedasticity: 
model3 %>%
  bptest()

# Multicollinearity 
model3 %>% 
  vif ()

# Checking for outliers - there were none too significant
model3 %>% 
  plot(which = 4)

data_sample_2 %>% 
  slice(c(46,64,85))

# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 5. part: Report the results of model 1 and model 3 (which is the  final model). 

# a) getting R2, F, df, and p value
summary(model1) 
summary(model3)

# b) information about confidence intervals 
confint(model1)
confint(model3)

# c) information about standardized beta's
lm.beta(model1)
lm.beta(model3)

# d) everything put in one table, for easier interpretation
coef_table(model1)
coef_table(model3)

# ------------------------------------------------------------------------------

# 6. part:   Write up the regression equation of model 3. 
# Y = 1.47 - 0.04 * age + 0.16 * sex - 0.01 * STAI_trait + 0.11 * pain_cat - 0.28 * mindfulness + 0.57 * cortisol_serum 

# ------------------------------------------------------------------------------

# 7. part: 
# Compare the two models in terms of how much variance they explain of pain’s variability in the sample.

# a) adjusted R.squared - checking how much variance we explain with one and another model 
summary(model1)$adj.r.squared
summary(model3)$adj.r.squared

# b) AIC - which model is better fit
AIC (model1, model3)

# c) Checking residual error with ANOVA
anova(model1, model3)


# -------------------------------------------------------------------------------------
# Used table of hierarchical regression - code 
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


