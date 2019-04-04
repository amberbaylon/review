# read in data
tigers <- read.table(text = 
"2004, 235.6, 2, 0.483, 1, 10
 2005, 436.6, 1, 0.488, 4, 10
 2006, 290.9, 3, 0.670, 7, 10
 2007, 305.1, 5, 0.605, 2, 10
 2008, 223.3, 1, 0.500, 4, 11
 2009, 302.1, 5, 0.552, 19, 13
 2010, 260.8, 3, 0.558, 10, 16
 2011, 201.9, 5, 0.533, 15, 13
 2012, 296.2, 3, 0.512, 6, 9
 2013, 374.7, 6, 0.553, 8, 14
 2014, 279.9, 4, 0.582, 17, 12
 2015, 190.4, 4, 0.500, 8, 12
 2016, 122.1, 1, 0.517, 11, 14
 2017, 111.2, 2, 0.448, 7, 11
 2018, 87.3, 1, 0.418, 9, 14", header = FALSE, sep = "," )
names(tigers) <- c("Year", "TVaud", "Tigers", "AtBreak", "DNP", "Bullpen")
mean(tigers$TVaud)
mean(tigers$Tigers)
mean(tigers$AtBreak)

# 3: means, sd, and correlations

# means
mean(tigers$TVaud)
mean(tigers$Tigers)
mean(tigers$DNP)
mean(tigers$Bullpen)
mean(tigers$AtBreak)

# SD
sd(tigers$TVaud)
sd(tigers$Tigers)
sd(tigers$DNP)
sd(tigers$Bullpen)
sd(tigers$AtBreak)

# correlation between explanatory and response variables 
cor(tigers$TVaud, tigers$Tigers)
cor(tigers$TVaud, tigers$AtBreak)
cor(tigers$TVaud, tigers$DNP)
cor(tigers$TVaud, tigers$Bullpen)


# 5: confirm that 2005 has an R studentized residual that exceeds the rule of thumb
# fit the model: TVaud = beta0 + beta1 Tigers + beta2 Atbreak + beta3 DNP + beta4 BullPen 
# + epsilon ~ N(0, sigma2)

tigers.out <- lm(TVaud ~ Tigers + AtBreak + DNP + Bullpen, data = tigers)

# residuals 
tigers.r <- rstudent(tigers.out)
# which one is greater than the rule of thumb?
hist(tigers.r)
abline(v = 3, col = "hot pink", main = "Histogram of R-stdentized Residuals")
subset(tigers, abs(tigers.r) > 3)

# The R-studentized residual for 2005 proves to exceed the rule of thumb due to the fact that it’s 
# R-studentized residual for 2005 is 4.386 and our rule of thumb is 3.  

# remove 2005 to 
tigers <- subset(tigers, tigers$Year != 2005) 

# 6: investigate the validity of the linear model assumptions

# a: comment on the validity of the normality assumption 
tigers.out <- lm(TVaud ~ Tigers + AtBreak + DNP + Bullpen, data = tigers)
tigers.r <- rstudent(tigers.out)
plot(density(tigers.r))
hist(tigers.r)
shapiro.test(tigers.r) # p-value: 0.06679

# b: regression diagnostics

# rule of thumb (for leverage): 2*(p+1)/n
tiger.leverage <- lm.influence(tigers.out)$hat
subset(tigers, tiger.leverage > 2*5/14) # no outliers 

# cook's D (change in parameter estimates with and without obs)
tigers.cd <- cooks.distance(tigers.out)
tigers.cd
subset(tigers, tigers.cd > 4/(14-(5))) # no outliers

plot(tigers.out)

plot(~ Tigers + AtBreak + DNP + Bullpen, data = tigers)

# After using our Regression Diagnostics such as cooks distance and leverage, 
# we can conclude that there aren’t any explanatory variables that exceed the rule of thumb

# c: parameter estimates and standard errors
summary(tigers.out)

# d: what effect does the number of tigers named all-stars have on the Detroit Tv audience?
confint(tigers.out)
library(car)
crPlots(tigers.out)

# f: does how well the tigers are doing have a statistically significant effect 
# Test Ho: no effect on the Detroit TV audience
#     Ha: effect on the Detroit TV audience 
tigers.readuced1 <- lm(TVaud ~ DNP + Bullpen, data = tigers)
anova(tigers.readuced1, tigers.out)

# With a test statistic of 10.38, and a small p-value of 0.005, we reject the null hypothesis and 
# conclude that how well the Tigers play does have a statistically significant effect on the 
# popularity of the game. 


# test boring aspects of the game
tigers.readuced2 <- lm(TVaud ~ Tigers + AtBreak, data = tigers)
anova(tigers.readuced2, tigers.out)

# h: estimates for scenarios:

# scenario 1: successful tigers 
predict(tigers.out, newdata = data.frame(Tigers = 5, AtBreak = 0.6, DNP = 7, Bullpen = 11), type = "response")

# 95% CI on P(Win)
success1 <- predict(tigers.out, newdata = data.frame(Tigers = 5, AtBreak = 0.6, DNP = 7, Bullpen = 11), type = "response", 
                   se.fit = TRUE)

# 95% CI on logit(Win)
tiger.L <- success1$fit - 1.96*success1$se.fit
tiger.L
tiger.U <- success1$fit + 1.96*success1$se.fit
tiger.U

# scenario 2: Tanking Tigers
predict(tigers.out, newdata = data.frame(Tigers = 1, AtBreak = 0.4, DNP = 7, Bullpen = 11), type = "response")

# 95% CI on P(Win)
success2 <- predict(tigers.out, newdata = data.frame(Tigers = 1, AtBreak = 0.4, DNP = 7, Bullpen = 11), type = "response", 
                   se.fit = TRUE)
success2

# 95% CI on logit(Win)
tiger.L <- success2$fit - 1.96*success2$se.fit
tiger.L
tiger.U <- success2$fit + 1.96*success2$se.fit
tiger.U

