rm(list = ls())
library(readxl)
getwd()

# Loading Data

d1 <- read_excel("LungCancer.xlsx", sheet = "cancer")
str(d1)
View(d1)
attach(d1)
table(Treatment)

summary(Months)



# Kaplan-Meier non-parametric analysis by group
# Group data based on Time and estimate KM survival function based on Event
install.packages("survival")
library(survival)



km1 <- survfit(Surv(Months, Status) ~ Treatment)      #  Here we have used months as time 
summary(km1)
plot(km1, xlab="Time in Months", ylab="Survival Probability", col = "green")

# In given  sample we can say that after 1000 Days everyone in sample experienced the event(Death)
# As per graph for both the Treatments standerad and The test 
# In Starting 180 Days Standard trearment has high survival probability but after 200 days it reduces as compared with Test treatment.
# Standard treatment has less time period to sustain as we obsrve 0% probability of survival at 553 days.



km2 <- survfit(Surv(`Survival in days`, Status) ~ Treatment)      #  Here we have used Days as time 
summary(km2)
plot(km2, xlab="Time in Days", ylab="Survival Probability", col = 1:2) # Balck = treatment 1(Standard)


nrow(d1)



d2 <- d1[ which(d1$Treatment == '1'), ]
summary(d2$`Survival in days`)

# AS per summary  Mean time of Survival for standerd treatment is 115 Days


d3 <- d1[ which(d1$Treatment == '2'), ]
summary(d3$`Survival in days`)

# AS per summary  Mean time of Survival for the Test treatment is 128 Days
summary(Age)



# Nelson-Aalen non-parametric analysis
na <- survfit(coxph(Surv(`Survival in days`, Status) ~ 1), type="aalen")
summary(na)
plot(na, xlab="Time in Months", ylab="Survival Probability")

# Cox proportional hazard model - coefficients and hazard rates
cox <- coxph(Surv(`Survival in days`, Status) ~ Treatment +`Cell Type` + PriorCh + Months + Kar_Score+ Age, method="breslow")
summary(cox)

# Negative coefficent for "Kar_Score" says that people with higher Kar_Score are less likely to experience the event (Death) i.e more time of survival.
# Negative coefficent for "Age" says that Older people are less likely to experience the event (Death) i.e Less time of survival. 

# People with prioprchemotherapy will have 10% lower hazard rate (Experience Death).

 

# Exponential, Weibull, and log-logistic parametric model coefficients
exp <- survreg(Surv(`Survival in days`, Status) ~ `Cell Type` + PriorCh + Kar_Score + Age, dist="exponential")
summary(exp)

weibull <- survreg(Surv(`Survival in days`, Status) ~ `Cell Type` + PriorCh + Kar_Score + Age, dist="weibull")
summary(weibull)

loglogistic <- survreg(Surv(`Survival in days`, Status) ~ `Cell Type` + PriorCh + Kar_Score + Age, dist="loglogistic")
summary(loglogistic)

library(stargazer)
stargazer(cox, exp, weibull, loglogistic, type="text")
