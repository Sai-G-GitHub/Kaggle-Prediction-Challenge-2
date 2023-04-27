df <- read.csv('HireRTrain1-1.csv')
test<-read.csv('test_challenge2.csv')
submission <- read.csv("sample_submission2.csv")
# library(rpart)
# # install.packages("rpart.plot")
# library(rpart.plot)
# install.packages("devtools")
# devtools::install_github("devanshagr/CrossValidation")

tree <- rpart(Hired ~ ., data = subset(df, Coding!='Weak'&TikTokFOLLOWERS>7954), method = "class")
rpart.plot(tree)

# pred<-predict(tree, df, type="class")
# head(pred)

# CrossValidation::cross_validate()

m1 <- rpart(Hired ~ ., data = df[df$TikTokFOLLOWERS >= 4710 & df$Coding=='Weak' & df$TikTokFOLLOWERS<7954 & df$TwitterFOLLOWERS<7544,])
m2 <- rpart(Hired ~ ., data = df[df$TikTokFOLLOWERS >= 4710 & df$Coding=='Weak' & df$TikTokFOLLOWERS<7954 & df$TwitterFOLLOWERS>=7544,])
m3 <- rpart(Hired ~ ., data = df[df$TikTokFOLLOWERS >= 4710 & df$Coding=='Weak' & df$TikTokFOLLOWERS>=7954,])
m4 <- rpart(Hired ~ ., data = df[df$TikTokFOLLOWERS >= 4710 & df$Coding!='Weak',])
m5 <- rpart(Hired ~ ., data = df[df$TikTokFOLLOWERS < 4710 & df$TwitterFOLLOWERS<6881,])
m6 <- rpart(Hired ~ ., data = df[df$TikTokFOLLOWERS < 4710 & df$TwitterFOLLOWERS>=6881 & df$TwitterFOLLOWING>=4335 & df$TikTokTFOLLOWING>=2934,])
m7 <- rpart(Hired ~ ., data = df[df$TikTokFOLLOWERS < 4710 & df$TwitterFOLLOWERS>=6881 & df$TwitterFOLLOWING>=4335 & df$TikTokTFOLLOWING<2934,])
m8 <- rpart(Hired ~ ., data = df[df$TikTokFOLLOWERS < 4710 & df$TwitterFOLLOWERS>=6881 & df$TwitterFOLLOWING<4335,])
# m9 <- rpart(Hired ~ ., data = df[df$Coding!='Weak',])
# m10 <- rpart(Hired ~ ., data = df[df$TikTokFOLLOWERS < 8071 & df$Coding=='Weak',])

p1<-predict(m1, newdata = df[df$TikTokFOLLOWERS >= 4710 & df$Coding=='Weak' & df$TikTokFOLLOWERS<7954 & df$TwitterFOLLOWERS<7544,], type="class")
p2<-predict(m2, newdata = df[df$TikTokFOLLOWERS >= 4710 & df$Coding=='Weak' & df$TikTokFOLLOWERS<7954 & df$TwitterFOLLOWERS>=7544,], type="class")
p3<-predict(m3, newdata = df[df$TikTokFOLLOWERS >= 4710 & df$Coding=='Weak' & df$TikTokFOLLOWERS>=7954,], type="class")
p4<-predict(m4, newdata = df[df$TikTokFOLLOWERS >= 4710 & df$Coding!='Weak',], type="class")
p5<-predict(m5, newdata = df[df$TikTokFOLLOWERS < 4710 & df$TwitterFOLLOWERS<6881,], type="class")
p6<-predict(m6, newdata = df[df$TikTokFOLLOWERS < 4710 & df$TwitterFOLLOWERS>=6881 & df$TwitterFOLLOWING>=4335 & df$TikTokTFOLLOWING>=2934,], type="class")
p7<-predict(m7, newdata = df[df$TikTokFOLLOWERS < 4710 & df$TwitterFOLLOWERS>=6881 & df$TwitterFOLLOWING>=4335 & df$TikTokTFOLLOWING<2934,], type="class")
p8<-predict(m8, newdata = df[df$TikTokFOLLOWERS < 4710 & df$TwitterFOLLOWERS>=6881 & df$TwitterFOLLOWING<4335,], type="class")
# p9<-predict(m9, newdata = df[df$Coding!='Weak',], type="class")
# p10<-predict(m10, newdata = df[df$TikTokFOLLOWERS < 8071 & df$Coding=='Weak',], type="class")


mypred<-df
decision<- rep('No', nrow(mypred))
decision[mypred$TikTokFOLLOWERS >= 4710 & mypred$Coding=='Weak' & mypred$TikTokFOLLOWERS<7954 & mypred$TwitterFOLLOWERS<7544] <- as.character(p1)
decision[mypred$TikTokFOLLOWERS >= 4710 & mypred$Coding=='Weak' & mypred$TikTokFOLLOWERS<7954 & mypred$TwitterFOLLOWERS>=7544] <- as.character(p2)
decision[mypred$TikTokFOLLOWERS >= 4710 & mypred$Coding=='Weak' & mypred$TikTokFOLLOWERS>=7954] <- as.character(p3)
decision[mypred$TikTokFOLLOWERS >= 4710 & mypred$Coding!='Weak'] <- as.character(p4)
decision[mypred$TikTokFOLLOWERS < 4710 & mypred$TwitterFOLLOWERS<6881] <- as.character(p5)
decision[mypred$TikTokFOLLOWERS < 4710 & mypred$TwitterFOLLOWERS>=6881 & mypred$TwitterFOLLOWING>=4335 & mypred$TikTokTFOLLOWING>=2934] <- as.character(p6)
decision[mypred$TikTokFOLLOWERS < 4710 & mypred$TwitterFOLLOWERS>=6881 & mypred$TwitterFOLLOWING>=4335 & mypred$TikTokTFOLLOWING<2934] <- as.character(p7)
decision[mypred$TikTokFOLLOWERS < 4710 & mypred$TwitterFOLLOWERS>=6881 & mypred$TwitterFOLLOWING<4335] <- as.character(p8)
# decision[mypred$Coding!='Weak'] <- as.character(p9)
# decision[mypred$TikTokFOLLOWERS < 8071 & mypred$Coding=='Weak'] <- as.character(p10)

mypred$Hired<-decision
error<-mean(df$Hired!=mypred$Hired)
error

CrossValidation::cross_validate(df,m1,5,0.7)
CrossValidation::cross_validate(df,m2,5,0.7)
CrossValidation::cross_validate(df,m3,5,0.7)
CrossValidation::cross_validate(df,m4,5,0.7)
CrossValidation::cross_validate(df,m5,5,0.7)
CrossValidation::cross_validate(df,m6,5,0.7)
CrossValidation::cross_validate(df,m7,5,0.7)
CrossValidation::cross_validate(df,m8,5,0.7)

mypred<-test

p1t <- predict(m1, newdata = test[test$TikTokFOLLOWERS >= 4710 & test$Coding=='Weak' & test$TikTokFOLLOWERS<7954 & test$TwitterFOLLOWERS<7544,], type="class")
p2t <- predict(m2, newdata = test[test$TikTokFOLLOWERS >= 4710 & test$Coding=='Weak' & test$TikTokFOLLOWERS<7954 & test$TwitterFOLLOWERS>=7544,], type="class")
p3t <- predict(m3, newdata = test[test$TikTokFOLLOWERS >= 4710 & test$Coding=='Weak' & test$TikTokFOLLOWERS>=7954,], type="class")
p4t <- predict(m4, newdata = test[test$TikTokFOLLOWERS >= 4710 & test$Coding!='Weak',], type="class")
p5t <- predict(m5, newdata = test[test$TikTokFOLLOWERS < 4710 & test$TwitterFOLLOWERS<6881,], type="class")
p6t <- predict(m6, newdata = test[test$TikTokFOLLOWERS < 4710 & test$TwitterFOLLOWERS>=6881 & test$TwitterFOLLOWING>=4335 & test$TikTokTFOLLOWING>=2934,], type="class")
p7t <- predict(m7, newdata = test[test$TikTokFOLLOWERS < 4710 & test$TwitterFOLLOWERS>=6881 & test$TwitterFOLLOWING>=4335 & test$TikTokTFOLLOWING<2934,], type="class")
p8t <- predict(m8, newdata = test[test$TikTokFOLLOWERS < 4710 & test$TwitterFOLLOWERS>=6881 & test$TwitterFOLLOWING<4335,], type="class")

predtest<-test
decision<- rep('No', nrow(mypred))
decision[predtest$TikTokFOLLOWERS >= 4710 & predtest$Coding=='Weak' & predtest$TikTokFOLLOWERS<7954 & predtest$TwitterFOLLOWERS<7544] <- as.character(p1t)
decision[predtest$TikTokFOLLOWERS >= 4710 & predtest$Coding=='Weak' & predtest$TikTokFOLLOWERS<7954 & predtest$TwitterFOLLOWERS>=7544] <- as.character(p2t)
decision[predtest$TikTokFOLLOWERS >= 4710 & predtest$Coding=='Weak' & predtest$TikTokFOLLOWERS>=7954] <- as.character(p3t)
decision[predtest$TikTokFOLLOWERS >= 4710 & predtest$Coding!='Weak'] <- as.character(p4t)
decision[predtest$TikTokFOLLOWERS < 4710 & predtest$TwitterFOLLOWERS<6881] <- as.character(p5t)
decision[predtest$TikTokFOLLOWERS < 4710 & predtest$TwitterFOLLOWERS>=6881 & predtest$TwitterFOLLOWING>=4335 & predtest$TikTokTFOLLOWING>=2934] <- as.character(p6t)
decision[predtest$TikTokFOLLOWERS < 4710 & predtest$TwitterFOLLOWERS>=6881 & predtest$TwitterFOLLOWING>=4335 & predtest$TikTokTFOLLOWING<2934] <- as.character(p7t)
decision[predtest$TikTokFOLLOWERS < 4710 & predtest$TwitterFOLLOWERS>=6881 & predtest$TwitterFOLLOWING<4335] <- as.character(p8t)

submission$Prediction<-decision
write.csv(submission, 'kagsubmission2.csv', row.names = FALSE)
View(submission)




