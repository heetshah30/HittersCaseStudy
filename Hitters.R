install.packages("ISLR", dependencies=T)
library("ISLR")
dat<-Hitters
names(Hitters)
?Hitters
Hitters.new = na.omit(Hitters)
attach(Hitters.new)
summary(Hitters.new)
hist(Hitters.new$AtBat)
hist(Hitters.new$Hits)
hist(Hitters.new$Salary)

install.packages("usdm")
library(usdm)
x <- cbind(AtBat, Hits, HmRun, Runs, RBI, Walks, Years, CAtBat, CHits, CHmRun, CRuns, CRBI, CWalks, League, Division, PutOuts, Assists, Errors, NewLeague)
vifstep(x, th = 1000)
x.new <- cbind(AtBat, HmRun, Runs, RBI, Walks, Years, CHmRun, CWalks, League, Division, PutOuts, Assists, Errors, NewLeague)
cor(x)
vifstep(x.new)
pairs(Salary~AtBat+HmRun+Runs+RBI+Walks+Years+CHmRun+CWalks+PutOuts+Assists+Errors,data=Hitters.new, 
      main="Simple Scatterplot Matrix")
model.hitters = lm(Salary~., data=Hitters.new)
step(model.hitters,direction="both",k=2)
step(model.hitters,direction="both",k=log(263))
###Specify the null (i.e, intercept-only) model
hitters.null<-lm(Salary~1, data=Hitters.new)
##We need to specify the "upper" (the fullest model)
step(hitters.null,scope=list(upper=model.hitters),
     direction="both",k=2)
step(hitters.null,scope=list(upper=model.hitters),
     direction="both",k=log(263))
library(leaps)
x<-model.matrix(~.-Salary, data=Hitters.new)[,-1]
hitresult <- leaps(x=x, y=Salary, int=TRUE, method=c("adjr2"), nbest=2)
hitresult
which.max(hitresult$adjr2)
hitresult$which[101,]
BackAIC <- lm(formula = Salary ~ AtBat + Hits + Walks + CAtBat + CRuns + 
                CRBI + CWalks + Division + PutOuts + Assists, data = Hitters.new)
summary(BackAIC)
anova(BackAIC)
BackAIC1 <- lm(formula = Salary ~ AtBat + Hits + Walks + CAtBat + CRuns + 
                CRBI + CWalks + Division + PutOuts, data = Hitters.new)
summary(BackAIC1)
anova(BackAIC1)
BackAIC2 <- lm(formula = Salary ~ AtBat + Hits + Walks + CRuns + 
                 CRBI + CWalks + Division + PutOuts, data = Hitters.new)
summary(BackAIC2)
anova(BackAIC2)
BackBIC <- lm(formula = Salary ~ AtBat + Hits + Walks + CRuns + CRBI + CWalks + 
                Division + PutOuts, data = Hitters.new)
summary(BackBIC)
ForAIC <- lm(formula = Salary ~ CRBI + Hits + PutOuts + Division + AtBat + 
               Walks + CWalks + CRuns + CAtBat, data = Hitters.new)
summary(ForAIC)
ForBIC <- lm(formula = Salary ~ CRBI + Hits + PutOuts + Division + AtBat + 
               Walks, data = Hitters.new)
summary(ForBIC)

#Partial F-test procedure to test variables
anova(BackAIC2)
anova(BackAIC1)
n = dim(Hitters.new)[1]####Number of observations (rows) in the dataset 
k=9
g=8
d.SSE<-25159234-24814051
numerator<-d.SSE/(k - g)
denominator<- 98079###MSE of the full model
fstat<-numerator/denominator
fstat
pval<-1-pf(fstat,1,253)
pval

#Final model
Finalmodel <- lm(formula = Salary ~ AtBat + Hits + Walks + CRuns + 
                   CRBI + CWalks + Division + PutOuts, data = Hitters.new)
summary(Finalmodel)
anova(Finalmodel)

#Check correlation of variables
x.final <- cbind(Salary, AtBat, Hits, Walks, CRuns, CRBI, CWalks, Division, PutOuts)
cor(x.final)
vifstep(x.final, th = 1000)

#residual plot: e versus x
plot(resid(Finalmodel)~AtBat,xlab="AtBat", ylab="Residuals")
#Doesn't seem like much except few outliers
plot(resid(Finalmodel)~Hits,xlab="Hits", ylab="Residuals")
plot(resid(Finalmodel)~Walks,xlab="Walks", ylab="Residuals")
plot(resid(Finalmodel)~CRuns,xlab="CRuns", ylab="Residuals")
plot(resid(Finalmodel)~CRBI,xlab="CRBI", ylab="Residuals")
plot(resid(Finalmodel)~CWalks,xlab="CWalks", ylab="Residuals")
plot(resid(Finalmodel)~PutOuts,xlab="PutOuts", ylab="Residuals")
plot(resid(Finalmodel)~Division,xlab="Division", ylab="Residuals")

#residual plot: e versus y.hat=
plot(resid(Finalmodel)~fitted(Finalmodel),xlab= "Y.hat", ylab= "Residuals")

#Standardized residuals
n = length(Salary)
mse = sum(resid(Finalmodel)^2)/(n-2)
stand.e = resid(Finalmodel)/sqrt(mse)
plot(stand.e~fitted(Finalmodel),ylab="Standardized Residual", main="Standardized Residual Plot")
plot(resid(model.fit)~fitted(Finalmodel),ylab="Residual", main="Residual Plot")

#Q-Q plot
qqnorm(resid(Finalmodel))
qqnorm(Salary)
qqline(resid(Finalmodel))
qqline(Salary)
#Hist
hist(resid(Finalmodel))
hist(Salary)
#Not far off but outliers are there

#Box-cox transformation
install.packages("MASS")
library(MASS)
boxcox(Finalmodel)
#Lambda is close to zero.
#So log transformation seems appropriate

Hitters.new$S_log = log(Salary)
hist(Hitters.new$S_log)
qqnorm(S_log)
qqline(S_log)
TFinalmodel <- lm(formula = S_log ~ AtBat + Hits + Walks + CRuns + 
                       CRBI + CWalks + Division + PutOuts, data = Hitters.new)
plot(resid(TFinalmodel2)~fitted(TFinalmodel2),xlab= "Y.hat", ylab= "Residuals")
#residual plot: e versus x
plot(resid(TFinalmodel2)~AtBat,xlab="AtBat", ylab="Residuals")
#Much better now
plot(resid(TFinalmodel2)~Hits,xlab="Hits", ylab="Residuals")
plot(resid(TFinalmodel2)~Walks,xlab="Walks", ylab="Residuals")
plot(resid(TFinalmodel2)~CRuns,xlab="CRuns", ylab="Residuals")
plot(resid(TFinalmodel2)~CRBI,xlab="CRBI", ylab="Residuals")
plot(resid(TFinalmodel2)~CWalks,xlab="CWalks", ylab="Residuals")
plot(resid(TFinalmodel2)~PutOuts,xlab="PutOuts", ylab="Residuals")
anova(TFinalmodel)
summary(TFinalmodel)

library(car)
cr.plots(Finalmodel)
cr.plots(TFinalmodel)

which(rstandard(TFinalmodel)>3)
which(hatvalues(TFinalmodel)>2*(9/263))
cooks.distance(TFinalmodel)
which(cooks.distance(TFinalmodel)>0.3)
which(cooks.distance(Finalmodel)>0.3)

TFinalmodel1 <- lm(formula = S_log ~ AtBat + Hits + Walks + CRuns + 
                        PutOuts + Division, data = Hitters.new)
anova(TFinalmodel)
summary(TFinalmodel1)
anova(TFinalmodel1)
n = dim(Hitters.new)[1]####Number of observations (rows) in the dataset 
k=256
g=254
d.SSE<-100.046-98.191
numerator<-d.SSE/(k - g)
denominator<- 0.387###MSE of the full model
fstat<-numerator/denominator
fstat
pval<-1-pf(fstat,2,254)
pval
summary(TFinalmodel1)
summary(TFinalmodel)
plot(resid(TFinalmodel1)~fitted(TFinalmodel1),xlab= "Y.hat", ylab= "Residuals")
x.final1 <- cbind(AtBat, Hits, Walks, CRuns, PutOuts, Division)
cor(x.final1)

Cormodel1 <- lm(formula = AtBat ~ Hits + Walks + CRuns + PutOuts + Division, data = Hitters.new)
Cormodel2 <- lm(formula = Hits ~ AtBat + Walks + CRuns + PutOuts + Division, data = Hitters.new)
Cormodel3 <- lm(formula = Walks ~ Hits + Walks + CRuns + PutOuts + Division, data = Hitters.new)
summary(Cormodel1)
summary(Cormodel2)
summary(Cormodel3)

library(car)
cr.plots(TFinalmodel1)

which(rstandard(TFinalmodel1)>3)
which(hatvalues(TFinalmodel1)>2*(9/263))
cooks.distance(TFinalmodel1)
which(cooks.distance(TFinalmodel1)>0.3)
which(cooks.distance(Finalmodel)>0.3)

TFinalmodel2 <- lm(formula = S_log ~ AtBat + Hits + log(CRuns) + 
                     PutOuts + Hits*log(CRuns) + AtBat*log(CRuns) 
                   + Hitsq*AtBat + Years + Yearsq, data = Hitters.new)
summary(TFinalmodel2)
anova(TFinalmodel2)
cr.plots(TFinalmodel2)
CRunsq <- CRuns^2
AtBatq <- AtBat^2
Hitters.new$Hitsq <- Hits^2
Walksq <- Walks^2
Hitters.new$Yearsq <- Years^2
plot(Salary ~ Hits)
plot(Salary ~ Years)

which(abs(rstandard(TFinalmodel2))>3)
which(hatvalues(TFinalmodel2)>2*(9/263))
which(cooks.distance(TFinalmodel2)>0.5)
Hitters.new1 <- Hitters.new[-c(173,241), ]
TFinalmodel3 <- lm(formula = S_log ~ AtBat + Hits + log(CRuns) + 
                     PutOuts + Hits*log(CRuns) + AtBat*log(CRuns) 
                   + Hitsq*AtBat + Years + Yearsq, data = Hitters.new1)
summary(TFinalmodel3)
anova(TFinalmodel3)
which(abs(rstandard(TFinalmodel3))>3)
which(hatvalues(TFinalmodel3)>2*(9/263))
which(cooks.distance(TFinalmodel3)>0.5)
plot(resid(TFinalmodel3)~fitted(TFinalmodel3),xlab= "Y.hat", ylab= "Residuals")

qqnorm(Salary)
