library(regclass)
#df <- read.csv("insurance.csv", header = TRUE)

df <- read_csv("Dropbox/STA9750/Final project/insurance.csv")
#display the first 6 rows of the dataset
head(df)

# check if there are any missing values
summary(df)
nrow(df) # 1338 rows

# first check the distribution of the quantitative variables
hist(df$age)
qq(df$age) # not normal

hist(df$bmi)
qq(df$bmi) # not normal 

hist(df$charges)
qq(df$charges) # not normal

all_correlations(data.frame(df), interest = "charges", sorted = "strength")


# Relationship between two quantitative variables
# dfAmount vs. ApplicantIncome (show a higher correlation from the cor matrix)
# shows heteroscatisty
plot(charges~bmi, data = df)


### Testing the association

# not statistically significant
associate(charges~sex, data = df, seed = 9750)

# the p-value of median test is between 0 and 0.007 
associate(charges~smoker, data = df, seed = 9750)

# not statistically significant
associate(charges~region, data = df, seed = 9750)

# the p-value of Spearman's rank correlation is between 0 and 0.007
associate(charges~children, data = df, seed = 9750)

# the p-value of Spearman's rank correlation is between 0 and 0.007
associate(charges~age, data = df, seed = 9750)

# the p-value of Pearson's correlation (r) is between 0 and 0.007 
associate(charges~bmi, data = df, seed = 9750)


### Testing the significant of linear association
BMI <- lm(charges~bmi,data=df)
BMI
summary(BMI)

possible_regressions(BMI)
anova(BMI)

children <- lm(charges~children,data=df)
children
summary(children)

possible_regressions(children)
anova(children)

age <- lm(charges~age,data=df)
age
summary(age)

possible_regressions(age)
anova(age)


#######--- Is the linear model practical? (model fitting performance)---- ####
# Model 1: charges vs. bmi
BMI <- lm(charges~bmi,data=df)
summary(BMI) # RMSE: 11870

children <- lm(charges~children,data=df)
summary(children) #RMSE: 12090

age <- lm(charges~age,data=df)
summary(age) #RMSE: 11560

sd(df$charges) #RMSE: 12110.01

confint(BMI,level=0.95)
confint(age,level=0.95)
confint(children,level=0.95)
