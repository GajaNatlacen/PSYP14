# Assignment_pt_3 (Gaja Natlacen)

# First I have to import the data sets:
home_sample_3 <- read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_3.csv")
home_sample_4 <- read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_4.csv")

# -----------------------------------------------------------------------------------------------------------------------------------------------------
# 1a part: Checking the data set for any unusual data + excluding it 
# Change all character variables to factors 
home_sample_3 <- home_sample_3 %>% 
  mutate(sex = factor(sex))
levels(home_sample_3$sex)

home_sample_3 <- home_sample_3 %>% 
  mutate(hospital = factor(hospital))
levels(home_sample_3$hospital)

# Check data for errors: 
summary(home_sample_3)
describe(home_sample_3)

# SEX --> Changing the value of factor - there can  only be two levels of sex  so I decided to change the woman to female
home_sample_3 %>% 
  filter(sex == "woman")
home_sample_3 <- home_sample_3 %>% 
  mutate(sex=recode(sex,
                    "woman"="female"))


# HOUSEHOLD INCOME --> Since it is not important at all for us, maybe we can just leave it? Or change it to positive value.
home_sample_3 %>% #income cannot go below 0 
  filter(household_income < 0) 
 
home_sample_3 %>% 
  ggplot()+
  aes(x=household_income)+
  geom_histogram()

describe(home_sample_3$household_income) #I will get the mean of household_income 

# That way I will change the negative value of household_income (which is clearly an error, to mean of household_income, since in that way I won't change my data set too much + I will also get read of error value)
home_sample_3$household_income[home_sample_3$household_income < 0] <- 70033.64

# Check the data again: 
summary(home_sample_3)

# -----------------------------------------------------------------------------------------------------------------------------------------------------
# 1b part: Visualizing data 

home_sample_3 %>% 
  ggplot()+
  aes(x = sex) + 
  geom_bar() + 
  theme_minimal()

# -----------------------------------------------------------------------------------------------------------------------------------------------------
# 2 part: Linear mixed model on home_sample_3
mod_rand_int <- lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1|hospital), data = home_sample_3)
mod_rand_int

# -----------------------------------------------------------------------------------------------------------------------------------------------------
# 3a part: Printing coefficients table and CI for random intercept model: 
summary(mod_rand_int) #gives us coefficients and p-values 
confint(mod_rand_int) #gives us confidence intervals 
stdCoef.merMod (mod_rand_int) #gives us standardized beta 


# 3b part: Compare coefficients of random intercept model to model 3, obtained in the assignment 1
data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_1.csv")
data_sample_2 <- data_sample_1 %>% 
  slice(-c(34,88))

model3 <- lm (pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_2)

summary(model3)
coef_table(model3)

# -----------------------------------------------------------------------------------------------------------------------------------------------------

# 4.  part: Compute marginal and conditional R squared values: 
r.squaredGLMM(mod_rand_int)

# -----------------------------------------------------------------------------------------------------------------------------------------------------

#In this part of the assignment, I used another data frame - home_sample_4
# 5a part: Check data frame 4 for any unusual data 
home_sample_4 <- home_sample_4 %>% 
  mutate(sex = factor(sex))

home_sample_4 <- home_sample_4 %>% 
  mutate(hospital = factor(hospital))

summary(home_sample_4)
describe(home_sample_4)

# 5b part: visualization of the data
home_sample_4 %>% 
  ggplot()+
  aes(x = sex) + 
  geom_bar() + 
  theme_minimal()

data_sample_4 %>% 
  ggplot()+
  aes(x = age) + 
  geom_bar() + 
  theme_light()

# -----------------------------------------------------------------------------------------------------------------------------------------------------
# 6. part: Predict pain on data frame 4
prediction <- predict(mod_rand_int, home_sample_4, allow.new.levels = TRUE)
RSS <- sum((home_sample_4[,"pain"]- prediction)^2)
RSS

mod_mean <- lm (pain ~ 1, data=home_sample_4)
TSS <- sum((home_sample_4$pain - predict(mod_mean))^2)
TSS

R2 <- 1 - (RSS/TSS)
R2

# -----------------------------------------------------------------------------------------------------------------------------------------------------
# 7. part: New model on data frame - random slope and intercept model (just  most influential predictor)
stdCoef.merMod(mod_rand_int) #most influential is cortisol_serum 

mod_rand_slope <- lmer(pain ~ cortisol_serum + (cortisol_serum|hospital), data = home_sample_3)

# -----------------------------------------------------------------------------------------------------------------------------------------------------
# 8. part: Visualization of fitted regression line, for each hospital separately: 
home_sample_3 <- home_sample_3 %>% 		
  mutate(pred_slope = predict(mod_rand_slope))

home_sample_3 %>% 		
  ggplot() +		
  aes(y = pain, x = cortisol_serum, group = hospital)+		
  geom_point(aes(color = hospital), size = 3) +		
  geom_line(color='red', aes(y=pred_slope, x=cortisol_serum))+		
  facet_wrap( ~ hospital, ncol = 2)


# -----------------------------------------------------------------------------------------------------------------------------------------------------
# 9. part: Comparing random intercept model with random intercept and random slope model: 
sum(residuals(mod_rand_int)^2)
sum(residuals(mod_rand_slope)^2)

cAIC(mod_rand_int)$caic
cAIC(mod_rand_slope)$caic

anova(mod_rand_int, mod_rand_slope)

# ----------------------------------------------------------------------------
# Used codes for this part of the assignment: 

stdCoef.merMod <- function(object) {	
  sdy <- sd(getME(object,"y"))	
  sdx <- apply(getME(object,"X"), 2, sd)	
  sc <- fixef(object)*sdx/sdy	
  se.fixef <- coef(summary(object))[,"Std. Error"]	
  se <- se.fixef*sdx/sdy	
  return(data.frame(stdcoef=sc, stdse=se))	
}	

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



