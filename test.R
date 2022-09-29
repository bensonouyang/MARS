source("mars.R")
source("anova.R")
source("plot.R")
source("predict.R")
source("print.R")
source("summary.R")
source("fitted.R")
source("residuals.R")

## Example 1

library(ISLR)
data(Wage)
mc <- mars.control(Mmax=10)
mout <- mars(wage ~ age + education, data=Wage, control=mc)
ff <- fitted(mout)
p1 <- predict(mout)
p2 <- predict(mout,newdata=data.frame(age=Wage$age,education=Wage$education))
head(cbind(ff,p1,p2)) # columns should be identical
mout # tests print method
summary(mout) #test summary method
anova(mout) # test anova method
plot(mout) # test plot method


## Example 2

archive = read.csv("archive.csv")
archive = na.omit(archive)
train_data = data.frame(y = archive$Year, x = archive$Carbon.Dioxide..ppm.)
mc <- mars.control(Mmax=2)
mout <- mars(y ~ x, data=train_data, control=mc)
ff <- fitted(mout)
p1 <- predict(mout)
p2 <- predict(mout,newdata=data.frame(x=train_data$x))
head(cbind(ff,p1,p2)) # columns should be identical
mout # tests print method
summary(mout) #test summary method
anova(mout) # test anova method
plot(mout) # test plot method

## Example 3

data = read.csv("params.csv")
train_data2 = as.data.frame(cbind(data$PeakDayNum,data$FirstDayNum))
colnames(train_data2) = c("y","x")
mc <- mars.control(Mmax=2)
mout <- mars(y ~ x, data=train_data2, control=mc)
ff <- fitted(mout)
p1 <- predict(mout)
p2 <- predict(mout,newdata=data.frame(x=train_data2$x))
head(cbind(ff,p1,p2)) # columns should be identical
mout # tests print method
summary(mout) #test summary method
anova(mout) # test anova method
plot(mout) # test plot method
