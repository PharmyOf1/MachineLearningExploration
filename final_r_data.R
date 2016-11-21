---
title: "Course Project: Practical Machine Learning"
subtitle: "Laura Stotts Gorans, Phil Harm, Tim Kiely "
output: word_document
---


```{r, include = F}
# header section. put knitr options here
knitr::opts_chunk$set(
	echo = TRUE,
	message = FALSE,
	warning = FALSE,
	fig.width=8,
	fig.height=8
)
```


```{r}

#############################################
# PREDICT 422 Practical Machine Learning    #
# Laura Stotts Gorans, Phil Harm, Tim Kiely #
#############################################


# Load packages. Check if they are installed first though
list.of.packages <- c(
  "tidyverse" # tidyverse is a wrapper that contains dplyr, ggplot, tidyr, readr and a bunch of other great Hadley stuff
  ,"corrplot" #correlation plot
  ,"lattice" #quantile plots
  ,"tibble"
  )

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages,library, character.only=T)

# Load Data
rm(list=ls())
charity <- read_csv("charity.csv") # load the "charity.csv" file

# quick view of the data
glimpse(charity)
View(charity)
```


# EDA

```{r}
##########################################
#REQUIREMENT 1 - Exploratory Data Analyis#
##########################################
charity.eda <- charity
attach(charity.eda)

names(charity.eda)  # names of the variables 
dim(charity.eda)  # dimension (number of rows and columns)
str(charity.eda)  # structure of the data set
class(charity.eda)  # type of data

#remove "id", from our list of variables
charity.eda <- charity.eda[2:23]

#create subset of categorical predictor variables
charity.cat <- charity.eda[2:10]
names(charity.cat)

#create subset of continuous predictor variables
charity.cont <- charity.eda[11:20]
names(charity.cont)
class(charity.cont)

```



# Missing data?
```{r}
summary(charity.eda)

#use na.omit() to remove all the rows that have missing values in any variable
charity.eda <- na.omit(charity.eda)
dim(charity.eda)
sum(is.na(charity.eda))

```

Both of our response variables, donr and damt contain 2007 NA's. We might want to interpolate these values or possibly filter them out completely.


## Histogram of each numeric variable
```{r}

num_vars <- names(charity.eda)[sapply(charity.eda, is.numeric)]

charity.eda %>% 
  select(one_of(num_vars)) %>% 
  gather(Var,Value) %>% 
  mutate(Value=as.numeric(Value)) %>% 
  ggplot()+
  aes(x=Value)+
  geom_histogram()+
  facet_wrap(~Var, scales="free")


```



## Explore Non-Binary Categorical
```{r}

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
      d.chld <- density(chld)
      plot(d.chld, main = "Kernel density of Children")
      polygon(d.chld, col = "red", border = "blue")
      
      #hinc
      d.hinc <- density(hinc)
      plot(d.hinc, main = "Kernel density of Household Income")
      polygon(d.hinc, col = "red", border = "blue")
      
      #wrat
      d.wrat <- density(wrat)
      plot(d.wrat, main = "Kernel density of Wealth Rating")
      polygon(d.wrat, col = "red", border = "blue")
```



## Explore Binary Categorical
```{r}      

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


    #Scatterplots
      par(mfrow=c(1,1))
      pairs(~ donr + avhv, charity.eda)
      plot(avhv, donr)

```

We observe the following: 

* AGIF (Average dollar amount of gifts to date): Mostly normal with skew to right
* ID (data identifier): unique ID (remeber that the binwidth is ~30 in the histograms)
* reg1, reg2, reg3, reg4: Binary region indicators. Reg 3 and 4 have about 1/4th what reg 1 and 2 have. May need to overweight/undersample for those regions
* home (Homeowner): 0's are vastly underweighted here. We may need to apply under-sampling techniques
* chld (numebr of children): roughly normally distributed around the value 2, but with many zero values
* hinc (household income in seven categories): Category 4 is by far the most popular, which makes sense. 
* genf (gender): sample is roughly 65% female
* wrat (Wealth Rating, 9 is highest 0 is lowest): Many 8's and 9's. This data is skewed towards wealthier people
* avhv (Average Home Value in $ thousands): Nearly normal with a mean of approx $150K, skewed right
* incm (Median Family Income in neighborhood in $ thousands): Nearly normal with a mean of approx $40K
* inca (Average family income in neighborhood in $ thousands): Nearly normal with a mean of approx $50K
* plow (% categorized as low income in neighborhood): long tailed, highest concentration around 0
* npro (total number of promotions received to date): normally distributed
* tgif (dollar amount total gifts to date): Normally distributed, but with outliers. may need to trim
* lgif (Dollar amount of largest gift to date): Normally distributed, but with outliers. may need to trim
* rgif (Dollar amount of most recent gift): Normally distributed, but with outliers. may need to trim
* tdon (Number of months since last donation): Nearly normal, but with some high concentrations around 15 and 21. Also skewed right
* tlag (Number of months between first and second gift): Skewed right
* agif (Average dollar amount of gifts to date): Nearly normal. Might be a good candidate to create a multiplier vs largest gift
* donr (Donor, binary): Response Variable. Evenly distributed
* damt (Donation in dollars): Response Variable. Zero-inflated distribution with an otherwise normal distribution



# QQ Plots of donr
```{r, fig.width=3,fig.height=3}
ranges <- sapply(charity.eda,max)
binary_vars <- ranges==1
continuous_vars <- ranges>1
cont_select <- continuous_vars[continuous_vars==T]
cont_select <- names(na.omit(cont_select))
cont_select <- cont_select[1:15]


for(i in 1:length(cont_select)){
  var <- charity.eda %>% select(one_of(cont_select[i]))
  name <- names(var)
  mod <- as.formula(paste0("donr~",name))
  a <- qq(mod, data = charity.eda, main = name)
  print(a)
}

```



## Explore Binary Categorical
```{r} 
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

#Boxplots for Continuous Variables
  par(mfrow=c(1,1))
  boxplot(avhv,
          main = toupper("Boxplot of AVHV"),
          ylab = "Age in years",
          col = "blue")

```


## Overlap of Region data?
```{r}
charity.eda %>% 
  group_by(reg1,reg2,reg3,reg4) %>% 
  summarise(count=n())

```

The region data is mutually exclusive


# Cross correlations?
```{r}
M <- cor(charity.eda %>% select(one_of(num_vars)),use = "na.or.complete")
corrplot(M, order = "hclust", type="upper")
```

Some auto-correlations between RGIF (Dollar amount of most recent gift) LGID (Last gift) and AGIF (average gift). Auto-correlations also between Median and Average Income and home prices. Inverse correlation between PLOW and the income variables.


# Do the NA Values in the reponse vars have different distributions?
```{r}
the_nas <- charity.eda %>% filter(is.na(donr))
the_complete <- charity.eda %>% filter(!is.na(donr))


# full set represented
dim(the_nas)
dim(the_complete)
dim(the_nas)[1]+dim(the_complete)[1]==nrow(charity.eda)


na_means <- suppressWarnings(sapply(the_nas,mean))
complete_means <- suppressWarnings(sapply(the_complete,mean))
a<-as.data.frame(na_means)
a$rows <- rownames(a)
b<-as.data.frame(complete_means)
b$rows <- rownames(b)
c <- left_join(a,b)
c <- c %>% mutate(abs_diff = abs(na_means-complete_means))
c %>% arrange(-abs_diff)


# run a t-test, hypothesis is that both means are the same:
t.test(c$na_means,c$complete_means)
# fail to reject
```

The NAs in the repsonse variables have similar population means, therefore, it might be permissible to interpolate the responses.




# TRANSFORMATIONS

```{r}
#Set-up Transformations
obs <- nrow(charity.cont)
for(i in obs){

  signedlog10 = function(x) {
  ifelse(abs(x) <= 1, 0, sign(x)*log10(abs(x)))
  }
  
  cuberoot = function(x) {
  x^(1/3)
  }
  
  square = function(x) {
  x^2
  }
  
  charity.cont <- as_data_frame(charity.cont)

  charity.log <- as_data_frame(apply(charity.cont, 2, log))
  charity.log10 <- apply(charity.cont, 2, signedlog10)
  charity.sqrt <- apply(charity.cont, 2, sqrt)
  charity.sqr <- apply(charity.cont, 2, square)
  charity.cubrt <- apply(charity.cont, 2, cuberoot)
  
  charity.transform4 <- transform(charity.cont, log=charity.log)
  charity.transform3 <- transform(charity.transform4, log10=charity.log10)
  charity.transform2 <- transform(charity.transform3, sqrt=charity.sqrt)
  charity.transform1 <- transform(charity.transform2, sqr=charity.sqr)
  charity.transform <- transform(charity.transform1, cubrt=charity.cubrt)
  
}

View(charity.transform)

    #Binned
    
    #Interaction


```



# PARTITION THE DATA FOR CROSS VALIDATION

```{r}

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


