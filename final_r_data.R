#############################################
# PREDICT 422 Practical Machine Learning    #
# Laura Stotts Gorans, Phil Harm, Tim Kiely #
#############################################

#Load Data
path = 'E:\\Northwestern\\Classes\\pred422\\final'
pathL = 'C:/Users/lstottsg/Desktop/Laura School/Fall 2016/'
fname = 'charity.csv'
charity <- read.csv(file=file.path(path,fname)) # load the "charity.csv" file

View(charity)  # allows us to view the data set

#install packages
install.packages("car")
install.packages("lattice")

#load libraries
library(car)
library(lattice)

##########################################
#REQUIREMENT 1 - Exploratory Data Analyis#
##########################################

charity.t <- charity
attach(charity.t)

names(charity.t)  # names of the variables 
dim(charity.t)  # dimension (number of rows and columns)
str(charity.t)  # structure of the data set
class(charity.t)  # type of data
summary(charity.t)  #summary of data stats

#subset variables into categorical vs continuous
charity.cat <- charity.t[2:10]
names(charity.cat)

charity.cont <- charity.t[11:21]
names(charity.cont)

#Missing Variables

##Correlations

##Explore Non-Binary Categorical

    #Table, Pie, Histogram

          #chld - Separate into two variables - nochld/chld?
          table(chld,donr)
          pie (table(chld))
          hist(chld, prob =T , ylim =c(0 , 1))
          lines( density ( chld , na.rm= TRUE ) , col="red")
          mu <-mean ( chld , na.rm= TRUE )
          sigma <-sd( chld , na.rm= TRUE )
          x <-seq (1 ,3 , length =3)
          y <-dnorm (x , mu , sigma )
          lines(x ,y , lwd =2 , col=" blue ")

          #hinc
          table(hinc,donr)
          pie (table(hinc))
          hist(hinc, prob =T , ylim =c(0 , 1))
          lines( density ( hinc , na.rm= TRUE ) , col="red")
          mu <-mean ( hinc , na.rm= TRUE )
          sigma <-sd( hinc , na.rm= TRUE )
          x <-seq (1 ,3 , length =3)
          y <-dnorm (x , mu , sigma )
          lines(x ,y , lwd =2 , col=" blue ")

          #wrat
          table(wrat,donr)
          pie (table(wrat))
          hist(wrat, prob =T , ylim =c(0 , 1))
          lines( density ( wrat , na.rm= TRUE ) , col="red")
          mu <-mean ( wrat , na.rm= TRUE )
          sigma <-sd( wrat , na.rm= TRUE )
          x <-seq (1 ,3 , length =3)
          y <-dnorm (x , mu , sigma )
          lines(x ,y , lwd =2 , col=" blue ")

    #Kernel Density Plots
        par(mfrow=c(2,2))

          #chld
          d.chld <- density(charity.t$chld)
          plot(d.chld, main = "Kernel density of Children")
          polygon(d.chld, col = "red", border = "blue")

          #hinc
          d.hinc <- density(charity.t$hinc)
          plot(d.hinc, main = "Kernel density of Children")
          polygon(d.hinc, col = "red", border = "blue")

          #wrat
          d.wrat <- density(charity.t$wrat)
          plot(d.wrat, main = "Kernel density of Children")
          polygon(d.wrat, col = "red", border = "blue")


##Explore Categorical Binary

    #Histograms
    par(mfrow=c(2,3))
    
    hist(charity.cat$reg1,
         main = "Histogram of Reg1",
         xlab = "Region 1")
    
    hist(charity.cat$reg2,
         main = "Histogram of Reg2",
         xlab = "Region 2")
    
    hist(charity.cat$reg3,
         main = "Histogram of Reg3",
         xlab = "Region 3")
    
    hist(charity.cat$reg4,
         main = "Histogram of Reg4",
         xlab = "Region 4")
    
    hist(charity.cat$home,
         main = "Histogram of Home",
         xlab = "Home Ownership")
    
    hist(charity.cat$genf,
         main = "Histogram of Gender",
         xlab = "Gender")

##Explore Continuous

    #Scatterplots
        par(mfrow=c(1,1))
        scatterplot.matrix(charity.cont)


    #QQ-Plots for continuous variables

        #need transformations
        qq(donr ~ avhv)
        qq(donr ~ incm)
        qq(donr ~ inca)
        qq(donr ~ plow)

        #npro - no transformation necessary?
        qq(donr ~ npro)

        #normal to 500
        qq(donr ~ tgif)

        #lgif normal to 200
        qq(donr ~ lgif)

        #rgif - normal to 100
        qq(donr ~ rgif)

        #tdon - normal to 25
        qq(donr ~ tdon)

        #tlag - needs transformation
        qq(donr ~ tlag)

        #agif - sparse after 50
        qq(donr ~ agif)


##Transformations

    #Log
    charity.t$avhv <- log(charity.t$avhv)
    
    #Cubic Root
    
    #Squared
    
    #Std Dev
    
    #T Std Dev
    
    #ArcSin
    
    #Binned
    
    #Interaction


#Partition Data
#Training
data.train <- charity.t[charity$part=="train",]
x.train <- data.train[,2:21]
c.train <- data.train[,22] # donr
n.train.c <- length(c.train) # 3984
y.train <- data.train[c.train==1,23] # damt for observations with donr=1
n.train.y <- length(y.train) # 1995

#Validation
data.valid <- charity.t[charity$part=="valid",]
x.valid <- data.valid[,2:21]
c.valid <- data.valid[,22] # donr
n.valid.c <- length(c.valid) # 2018
y.valid <- data.valid[c.valid==1,23] # damt for observations with donr=1
n.valid.y <- length(y.valid) # 999

#Testing
data.test <- charity.t[charity$part=="test",]
n.test <- dim(data.test)[1] # 2007
x.test <- data.test[,2:21]

x.train.mean <- apply(x.train, 2, mean)
x.train.sd <- apply(x.train, 2, sd)
x.train.std <- t((t(x.train)-x.train.mean)/x.train.sd) # standardize to have zero mean and unit sd
apply(x.train.std, 2, mean) # check zero mean
apply(x.train.std, 2, sd) # check unit sd
data.train.std.c <- data.frame(x.train.std, donr=c.train) # to classify donr
data.train.std.y <- data.frame(x.train.std[c.train==1,], damt=y.train) # to predict damt when donr=1

x.valid.std <- t((t(x.valid)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.valid.std.c <- data.frame(x.valid.std, donr=c.valid) # to classify donr
data.valid.std.y <- data.frame(x.valid.std[c.valid==1,], damt=y.valid) # to predict damt when donr=1

x.test.std <- t((t(x.test)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.test.std <- data.frame(x.test.std)


##########################################
#REQUIREMENT 2 - CLASSIFICATION MODELING #
##########################################

#~~~~~~~~~~~~~~
# LINEAR DICSRIMINANT ANALYSIS
#~~~~~~~~~~~~~~
library(MASS)

model.lda1 <- lda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                    avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                  data.train.std.c) # include additional terms on the fly using I()

post.valid.lda1 <- predict(model.lda1, data.valid.std.c)$posterior[,2] # n.valid.c post probs

#calculate ordered profit function using average donation = $14.50 and mailing cost = $2
profit.lda1 <- cumsum(14.5*c.valid[order(post.valid.lda1, decreasing=T)]-2)
plot(profit.lda1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.lda1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.lda1)) # report number of mailings and maximum profit


cutoff.lda1 <- sort(post.valid.lda1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.lda1 <- ifelse(post.valid.lda1>cutoff.lda1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.lda1, c.valid) # classification table

#~~~~~~~~~~~~~~
# Quadratic DISCRIMINANT ANALYSIS
#~~~~~~~~~~~~~~

#example of fitting qda
model.qda1 <- qda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                    avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                  data.train) # include additional terms on the fly using I()

qda1.donr <- predict(model.qda1, data.valid)$class # make predictions using the validation set

table(qda1.donr, c.valid)  # determines the confusion matrix

mean(qda1.donr==c.valid)   # determines the validation set error


#~~~~~~~~~~~~~~
# LOGSITIC REGRESSION
#~~~~~~~~~~~~~~
model.log1 <- glm(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                    avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                  data.train.std.c, family=binomial("logit"))

post.valid.log1 <- predict(model.log1, data.valid.std.c, type="response") # n.valid post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.log1 <- cumsum(14.5*c.valid[order(post.valid.log1, decreasing=T)]-2)
plot(profit.log1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.log1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.log1)) # report number of mailings and maximum profit

cutoff.log1 <- sort(post.valid.log1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.log1 <- ifelse(post.valid.log1>cutoff.log1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.log1, c.valid) # classification table

# select model.log1 since it has maximum profit in the validation sample
post.test <- predict(model.log1, data.test.std, type="response") # post probs for test data

# Oversampling adjustment for calculating number of mailings for test set
n.mail.valid <- which.max(profit.log1)
tr.rate <- .1 # typical response rate is .1
vr.rate <- .5 # whereas validation response rate is .5
adj.test.1 <- (n.mail.valid/n.valid.c)/(vr.rate/tr.rate) # adjustment for mail yes
adj.test.0 <- ((n.valid.c-n.mail.valid)/n.valid.c)/((1-vr.rate)/(1-tr.rate)) # adjustment for mail no
adj.test <- adj.test.1/(adj.test.1+adj.test.0) # scale into a proportion
n.mail.test <- round(n.test*adj.test, 0) # calculate number of mailings for test set

cutoff.test <- sort(post.test, decreasing=T)[n.mail.test+1] # set cutoff based on n.mail.test
chat.test <- ifelse(post.test>cutoff.test, 1, 0) # mail to everyone above the cutoff
table(chat.test)

#~~~~~~~~~~~~~~
# Logistic Regression GAM
#~~~~~~~~~~~~~~
library(gam)
# example of fitting gam
model.gam1 <- lm(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                   avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, data = data.train)

# examples of fitting GAM using smoothing splines
model.ss1 <- smooth.spline(x.valid$avhv, c.valid, cv = TRUE)     # finds d.o.f. using LOOCV
model.ss2 <- smooth.spline(x.valid$incm, c.valid, cv = TRUE)     # finds d.o.f. using LOOCV
model.gamSS1 <- gam(c.valid ~ s(x.valid$avhv, 2) + s(x.valid$incm, 7) + x.valid$npro, data = data.train)

#~~~~~~~~~~~~~~
# K-Nearest Neighbor
#~~~~~~~~~~~~~~
library(class)

#example of fitting knn
knn.pred <- knn(x.train, x.valid, c.train, k = 1)
table(knn.pred, c.valid) # determines the confusion matrix
(527+614)/2018 # 56% of the observations are correctly predicted

##########################################
#REQUIREMENT 3 - PREDICTION MODELING     #
##########################################

#~~~~~~~~~~~~~~
# LEAST SQUARES REGRESSION
#~~~~~~~~~~~~~~
model.ls1 <- lm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
                  avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                data.train.std.y)

pred.valid.ls1 <- predict(model.ls1, newdata = data.valid.std.y) # validation predictions
mean((y.valid - pred.valid.ls1)^2) # mean prediction error
sd((y.valid - pred.valid.ls1)^2)/sqrt(n.valid.y) # std error

# drop wrat for illustrative purposes
model.ls2 <- lm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + 
                  avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                data.train.std.y)

pred.valid.ls2 <- predict(model.ls2, newdata = data.valid.std.y) # validation predictions
mean((y.valid - pred.valid.ls2)^2) # mean prediction error
sd((y.valid - pred.valid.ls2)^2)/sqrt(n.valid.y) # std error


# select model.ls2 since it has minimum mean prediction error in the validation sample
yhat.test <- predict(model.ls2, newdata = data.test.std) # test predictions

#~~~~~~~~~~~~~~
# Best Subset with K-CrossFold
#~~~~~~~~~~~~~

#~~~~~~~~~~~~~~
# RIDGE
#~~~~~~~~~~~~~

#~~~~~~~~~~~~~~
# LASSO
#~~~~~~~~~~~~~
library(glmnet)
#Set-Up Model Matrix
lasso = model.matrix(damt ~ ., data = data.train)[,-1]
x.lasso <- lasso[,2:21]
y.lasso <- lasso[,22]
cv.out = cv.glmnet(x.lasso, y.lasso, alpha = 1)
plot(cv.out)

#Tweak Lambda
lasso.lambda= cv.out$lambda.1se
lasso.train = glmnet(x.lasso, y.lasso,alpha=1,lambda=lasso.lambda)
coef(lasso.train)
lasso.valid=predict(lasso.train,newx=as.matrix(x.valid))

#TEST RESULTS on VALIDATION SAMPLE
summary(lasso.valid)
plot(lasso.valid) 
mean((lasso.valid -c.valid)^2)
sd((lasso.test -n.valid.c)^2)/sqrt(n.train.y)


##########################################
#REQUIREMENT 4 - SAVE FIANL RESULTS      #
##########################################

# Save final results for both classification and regression

length(chat.test) # check length = 2007
length(yhat.test) # check length = 2007
chat.test[1:10] # check this consists of 0s and 1s
yhat.test[1:10] # check this consists of plausible predictions of damt

ip <- data.frame(chat=chat.test, yhat=yhat.test) # data frame with two variables: chat and yhat
write.csv(ip, file="GHK.csv", row.names=FALSE) # use your initials for the file name

##########################################
#REQUIREMENT 5 - NOTES FOR WRITE-UP      #
##########################################
#Use this space for write-ups notes


