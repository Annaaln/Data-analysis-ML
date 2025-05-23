install.packages("car")
library("purrr")
library("haven")
library("lmtest")
library("rlms")
library("dplyr")
library("GGally")
library("car")
library("sandwich")
library("naniar")
library("purrr")

data <- readr::read_csv("C:\\Users\\ASUS\\Desktop\\учеба в рту мирэа\\проект 2 сем\\r16i_os26c.csv")
glimpse(data)
data2 <- select(data, lj13.2, l_age, lh5, l_educ, status, lj6.2, l_marst, lj1.1.2, lj23, lj24, lj32.1)

data2 = na.omit(data2)

sal <- as.numeric(data2$lj13.2)
sal1 = as.character(data2$lj13.2)
sal2 = lapply(sal1, as.integer)
sal = as.numeric(unlist(sal2))
data2["salary"] <- (sal - mean(sal)) / sd(sal)
data2["salary"]


age1 = as.character(data2$l_age)
age2 = lapply(age1, as.integer)
age3 = as.numeric(unlist(age2))
data2["age"]<- (age3 - mean(age3)) / sd(age3)
data2["age"]


data2["sex"] <- data2$lh5
data2$sex[which(data2$sex!='1')]<-0
data2$sex[which(data2$sex=='1')]<-1
data2$sex = as.numeric(data2$sex)
data2["sex"]


data2["h_educ"] = data2$l_educ
data2["higher_educ"] = data2$l_educ
data2["higher_educ"] = 0
data2$higher_educ[which(data2$h_educ == '21')] <- 1
data2$higher_educ[which(data2$h_educ == '22')] <- 1
data2$higher_educ[which(data2$h_educ == '23')] <- 1
data2["higher_educ"]


data2["status1"] = data2$status
data2["status2"] = 0
data2$status2[which(data2$status1 == '1')] <- 1
data2$status2[which(data2$status1 == '2')] <- 1
data2$status2 = as.numeric(data2$status2)
data2["status2"]

dur1 = as.character(data2$lj6.2)
dur2 = lapply(dur1, as.integer)
dur3 = as.numeric(unlist(dur2))
data2["dur"] = (dur3 - mean(dur3))/sd(dur3)
data2["dur"]

data2["wed1"] = data2$l_marst
data2wed1 = 0
data2$wed1[which(data2$wed1=='2')] <- 1
data2$wed1[which(data2$wed1=='3')] <- 1
data2$wed1 = as.numeric(data2$wed1)
data2["wed1"]

data2["wed2"] = lapply(data2["wed1"], as.character)
data2wed2 = 0
data2$wed2[which(data2$wed1=='1')] <- 1
data2$wed2 = as.numeric(data2$wed2)
data2["wed2"]

data2["wed3"]=data2$l_marst
data2$wed3 = 0
data2$wed3[which(data2$wed1 == '4')] <- 1
data2$wed3 = as.numeric(data2$wed3)
data2["wed3"]

data2["wed4"]=data2$l_marst
data2$wed4 = 0
data2$wed4[which(data2$wed1 == '5')] <- 1
data2$wed4 = as.numeric(data2$wed4)
data2["wed4"]

data2["sat"] = data2$lj1.1.2
data2["sat"] = lapply(data2["sat"], as.character)
data2["satisfy"] = 0
data2$satisfy[which(data2$sat=='1')] <- 1
data2$satisfy[which(data2$sat=='2')] <- 1
data2$satisfy = as.numeric(data2$satisfy)
data2["satisfy"]

data2["state"] = data2$lj23
data2["state"] = lapply(data2["state"], as.character)
data2["state_owner"] = 0
data2$state_owner[which(data2$state=='1')] <- 1
data2$state_owner = as.numeric(data2$state_owner)
data2["state_owner"]

data2["foreign"] = data2$lj24
data2["foreign"] = lapply(data2["foreign"], as.character)
data2["foreign_owner"] = 0
data2$foreign_owner[which(data2$foreign=='1')] <- 1
data2$foreign_owner = as.numeric(data2$foreign_owner)
data2["foreign_owner"]


data2["sj"] = data2$lj32.1
data2["sj"] = lapply(data2["sj"], as.character)
data2["second_job"] = 0
data2$second_job[which(data2$sj=='1')] <- 1
data2$second_job = as.numeric(data2$second_job)
data2["second_job"]

data2 = na.omit(data2)
data2["salary"] = data2["salary"]*1.034

data3 = select(data2, salary, age, sex, higher_educ, status2, dur, wed1, wed2, wed3, wed4, satisfy, second_job, state_owner, foreign_owner)


#Part 1
base_model <- lm(data=data3, salary~age+sex+higher_educ+status2+dur+wed1+wed2+wed3+wed4+satisfy+second_job+state_owner+foreign_owner)
summary(base_model)
base_model <- lm(data=data3, salary~age+sex+higher_educ+status2+dur+wed1+wed4+satisfy+state_owner+foreign_owner)
summary(base_model)
cor(data3) 
vif(base_model)
#All VIF values are low, no linear relationship

#Part 2

#The function for finding the best logarithm
the_best_log <- function(variable, regressor) {
  result <- 0.1
  max_r2 <- 0.0
  
  for (base in seq(from=0.1, to=5.0, by=0.1)) {
    if (base != 1.0) {
      model = lm(variable ~ I(log(regressor, base)), data)
      r2 <- summary(model)$r.squared
      if (r2 > max_r2) {
        max_r2 <- r2
        result <- base
      }
    }
  }
  
  print(max_r2)
  
  result
}

#The function to find the best exponent
the_best_exp <- function(variable, regressor) {
  result <- 0.1
  max_r2 <- 0.0
  
  for (base in seq(from=0.1, to=3.0, by=0.1)) {
    if (base != 1.0) {
      model = lm(variable ~ I(base^regressor), data)
      r2 <- summary(model)$r.squared
      if (r2 > max_r2) {
        max_r2 <- r2
        result <- base
      }
    }
  }
  
  print(max_r2)
  
  result
}

#Part 3
min_age = min(data2$age)
data2["age_"] = as.numeric(map(data2$age, function(x) x - min_age + 1e-8))

min_salary = min(data2$salary)
data2["salary_"] = as.numeric(map(data2$salary, function(x) x - min_salary + 1e-8))

the_best_log(data2$dur, data2$age_)
the_best_exp(data2$dur, data2$age)
the_best_log(data2$dur, data2$salary_)
the_best_exp(data2$dur, data2$salary)

#exclude insignificant parameters until no dependencies
mod_age <- lm(dur ~ age + I(log(age_, 2.1)) + I(2.5 ^ age), data2)
vif(mod_age)
best_age <- lm(dur ~ I(log(age_, 2.1)) + I(2.5 ^ age), data2)
summary(best_age)
vif(best_age)
#no dependencies

mod_sal <- lm(dur ~ salary + I(log(salary_, 2.1)) + I(2.5 ^ salary_), data2)
vif(mod_sal)
#no dependencies


base_model <- lm(data=data2, dur ~ I(log(age_, 2.1)) + I(2.5 ^ age)+sex+higher_educ+status2+salary+I(log(salary_, 2.1)) + I(2.5 ^ salary_)+wed1+wed4+satisfy+state_owner+foreign_owner)
summary(base_model)
vif(base_model)
#no dependencies

m_model <- lm(data=data2, dur~I(age^2)+I(log(age_, 2.1))+I(2.5^age)+sex+higher_educ+status2+salary+wed1+wed4+satisfy+state_owner+foreign_owner)
summary(m_model)
vif(m_model)
m_model <- lm(data=data2, dur ~ I(age^2) + I(log(age_, 2.1))+sex+higher_educ+status2+salary+wed1+wed4+satisfy+foreign_owner+state_owner)
summary(m_model)
m_model <- lm(data=data2, dur ~ I(age^2)+sex+higher_educ+status2+salary+wed4+satisfy)
summary(m_model)

m_model <- lm(data=data2, dur ~ I(salary ^ 2)+salary+I(age ^ 2)+sex+higher_educ+status2+satisfy+wed4)
summary(m_model)
vif(m_model)

m_model <- lm(data=data2, dur~I(salary^2)+salary+I(age^2)+I(salary*age)+sex+higher_educ+status2+satisfy+wed4)
summary(m_model)
vif(m_model)

#The best model
best_model <- lm(data=data2, dur~I(salary^2)+salary+I(age^2)+I(salary*age)+sex+higher_educ+status2+satisfy+wed4)
summary(best_model)
vif(best_model)

#Part 4

#The function for finding the confidence interval
dinterval <- function(model){
  degrees_of_freedom <- df.residual(model)
  t_crit <- qt(0.975, degrees_of_freedom)
  summary = summary(model)
  std_err <- summary$coefficients[2, 2]
  koef <- summary$coefficients[2, 1]
  c(koef - std_err*t_crit, koef + std_err*t_crit)
}

salary_model <- lm(data=data2, dur~salary)
summary(salary_model)
dinterval(salary_model)

age_model <- lm(data=data2, dur~age)
summary(age_model)
dinterval(age_model)

sex_model <- lm(data=data2, dur~sex)
summary(sex_model)
dinterval(sex_model)

wed4_model <- lm(data=data2, dur~wed4)
summary(wed4_model)
dinterval(wed4_model)

higher_educ_model <- lm(data=data2, dur~higher_educ)
summary(higher_educ_model)
dinterval(higher_educ_model)

status2_model <- lm(data=data2, dur~ status2)
summary(status2_model)
dinterval(status2_model)

satisfy_model <- lm(data=data2, dur~satisfy)
summary(satisfy_model)
dinterval(satisfy_model)

#The research data showed that older men, widowers with higher education, 
#high salaries, and satisfied with their jobs have the longest working hours.

#Part 5
#Unmarried, without higher education; urban 
#married residents
subset1 = subset(data2, wed3 == 1)
subset1 = subset(data2, higher_educ == 0)
subset2 = subset(data2, status2 == 1)
subset2 = subset(data2, wed1 == 1)

sub_model1 <- lm(data=subset1, dur~I(salary^2)+salary+I(age ^ 2)+I(salary*age)+sex+higher_educ+status2+satisfy+wed4)
summary(sub_model1)
#The resulting model gives inaccurate predictions with r^2(adj)= -0.00047
sub_model2 <- lm(data=subset2, dur~I(salary^2)+salary+I(age ^ 2)+I(salary*age)+sex+higher_educ+status2+satisfy+wed4)
summary(sub_model2)
#The resulting model gives inaccurate predictions with r^2(adj)= 0.01875

model2 <- lm(data=subset1, dur ~ I(salary ^ 2))
summary(model2)
dinterval(model2)


model2 <- lm(data=subset1, dur ~ I(age ^ 2))
summary(model2)
dinterval(model2)

model2 <- lm(data=subset1, dur ~ I(salary * age))
summary(model2)
dinterval(model2)

model2 <- lm(data=subset2, dur ~ I(salary ^ 2))
summary(model2)
dinterval(model2)


model2 <- lm(data=subset2, dur ~ I(age ^ 2))
summary(model2)
dinterval(model2)


model2 <- lm(data=subset2, dur ~ I(salary*age))
summary(model2)
dinterval(model2)
