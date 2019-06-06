
################################## LAB.2 ############################################
################### Linear regression with R (part II) #######################


# Data in file Gender Discrimination.csv contain the information about gender, experience and annual salary in $ for some employees of a company. We want to evaluate
# whether the salary differs between males and females, given the experience.
my.data <- read.csv('lab2/Gender_Discrimination.csv', sep=',')

# First look at the data
my.data[1:3,]
dim(my.data)
summary(my.data)

# Variable Gender is a qualitative variable with 2 levels, Female and Male, so..
is.factor(my.data$Gender)
table(my.data$Gender)

# Initial descriptionof the data.
# Boxplot of the salary
boxplot(my.data$Salary, las=2, col='grey', main='Annual Salary')
## las=2 plots the y-labels horizontally, to make them readable

# Gender distribution
pie(table(my.data$Gender), labels=c('Female','Male'))

# Distribution of salary given gender
boxplot(my.data$Salary~my.data$Gender, main='Salary given Gender',
        col=c('pink','blue'), las=2, ylab='Salary', cex.axis=0.7)
## cex.axis: modify the dimension of the labels, default is 1

# Distribution of experience given gender
boxplot(my.data$Experience~my.data$Gender, main='Experience given Gender',
        col=c('pink','blue'), las=2, ylab='Experience')

# Finally, Dispersion plot of salary and experience
plot(my.data$Experience, my.data$Salary, main='Salary vs Experience',
     xlab='Experience', ylab='Salary', las=2, cex.axis=0.7)
# Add a distinguish Gender factor
points(my.data$Experience[my.data$Gender == 'Female'],
       my.data$Salary[my.data$Gender == 'Female'], col='pink', pch=19)
points(my.data$Experience[my.data$Gender == 'Male'],
       my.data$Salary[my.data$Gender == 'Male'], col='blue', pch=19)
legend('topleft', pch=c(19,19), c('Female','Male'),
       col=c('pink','blue'), bty='n')


# Estimate a multiple linear regression model with covariates Gender and Experience.
# Consider that Gender is codified so that it assumes value 0 if Gender=Female and value 1
# if Gender=Male (R follows the alphabetical order; it can be changed). The model is
#
#                     Salary = β0 + β1Gender + β2Experience + ε
#
#if we want to explicit that Gender has an associated binary/indicator variable (dummy
#variable). 
#               Salary = β0 + β1 I(Gender=Male) + β2Experience + ε
#
# Thus, if Gender=Female, the model is
#                     Salary = β0 + β2Experience + ε,
#
# while if Gender=Male, the model is
#                    Salary = β0 + β1 + β2Experience + ε,

model <- lm(Salary ~ Gender + Experience, data=my.data)
summary(model)
# Note that in the summary we have the estimate of β1, the parameter in case gender
# is male. Female level is considered as reference level. The linear regression fit for females is Salary \ = 5.3260001 × 10 \
# 4 + 1744.6288555 ∗ Experience, while that for males is
# Salary = 5.3260001 × 104 + 1.7020585 × 104 + 1744.6288555 ∗ Experience = 7.0280587 ×
# 104 + 1744.6288555 ∗ Experience.


