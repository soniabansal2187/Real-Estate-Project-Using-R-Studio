data1 <- read.csv("regression data.csv", header= TRUE,sep=",")

View(data1)
# display top 6 obs in the data 
head(data1)
tail(data1)
nrow(data1)
ncol(data1)
dim(data1)
names(data1)
str(data1)

#dividing datainto training and validation

library(caTools)#package

set.seed(1)

# sample the input data with 70% for training and 30% for testing
sample <- sample.split(data1$selling.price,SplitRatio=0.70)
sample
train_data <- subset(data1,sample==TRUE)#split of the data using subset command 
test_data <- subset(data1,sample==FALSE)

#model building
model <- lm(selling.price ~., data = train_data)

summary(model)

#rerun model again using sig var 

model2 <- lm(selling.price ~local.selling.price.in.hundred.of.dollars
             +number.of.bathrooms+size.of.the.living.space+number.of.garages
             +construction.type, data = train_data)


summary(model2)

#rerun model3 again using sig var 

model3 <- lm(selling.price ~local.selling.price.in.hundred.of.dollars
             +number.of.bathrooms+size.of.the.living.space
             +construction.type, data = train_data)


summary(model3)

#prediction  on testing data set 
predtest<-predict(model3,test_data) 
head(predtest)

# attach it with the dataframe
predtest1<- data.frame(predtest)

#to bind the predicted data set with data set by cbind function
final_data<- cbind(test_data,predtest1)

#cal rmse 
sqrt(mean((final_data$selling.price - final_data$predtest)^2))

#export output file
write.csv(final_data,"linear_output.csv")
