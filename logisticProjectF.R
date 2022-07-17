#logistic regression

#read datafile
mydata <- read.csv(file.choose(), header=T)
str(mydata)
mydata$gender <- as.factor(mydata$gender)
mydata$ssc_b <- as.factor(mydata$ssc_b)
mydata$hsc_b <- as.factor(mydata$hsc_b)
mydata$hsc_s <- as.factor(mydata$hsc_s)
mydata$degree_t <- as.factor(mydata$degree_t)
mydata$workex <- as.factor(mydata$workex)
mydata$specialisation <- as.factor(mydata$specialisation)
mydata$status <- as.factor(mydata$status)

# 2-way table of factor variables
xtabs(~status + hsc_s, data=mydata)
xtabs(~status + degree_t, data=mydata)
xtabs(~status + workex, data=mydata)
xtabs(~status + specialisation, data=mydata)

#partition data - train(80%) & test(20%)
set.seed(1234)
ind <- sample(2, nrow(mydata), replace=T, prob=c(0.8, 0.2))
train <- mydata[ind==1,]
test <- mydata[ind==2,]

# logistic regression model
mymodel <- glm(status ~ ssc_p + ssc_b + hsc_p + hsc_b + hsc_s + degree_p + degree_t + workex + etest_p + specialisation + mba_p, data=train, family= 'binomial')
summary(mymodel)
mymodel1 <- glm(status ~ ssc_p + hsc_p + degree_p + degree_t + workex + mba_p, data=train, family= 'binomial')
summary(mymodel1)

# univariate model
model1 <- glm(status ~ hsc_s, data= train, family= 'binomial')
summary(model1)
model2 <- glm(status ~ degree_t, data= train, family= 'binomial')
summary(model2)
model3 <- glm(status ~ workex, data= train, family= 'binomial')
summary(model3)
model4 <- glm(status ~ specialisation, data= train, family= 'binomial')
summary(model4)

#prediction
p1 <- predict(mymodel1, train, type='response')
head(p1)
head(train)

# misclasification error- train data
pred1 <- ifelse(p1>0.5, 1, 0)
tab1 <- table(Predicted= pred1, Actual= train$status)
tab1
1-sum(diag(tab1))/sum(tab1)

# misclasification error- test data
p2 <- predict(mymodel1, test, type='response')
pred2 <- ifelse(p2>0.5, 1, 0)
tab2 <- table(Predicted= pred2, Actual= test$status)
tab2
sum(diag(tab2))/sum(tab2)
