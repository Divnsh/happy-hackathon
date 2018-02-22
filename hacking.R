h = read.csv('Happy_train.csv', stringsAsFactors = T)
ht = read.csv('Happy_test.csv', stringsAsFactors = T)
library(zoo)
library(plyr)
library(mice)
library(naivebayes)
library(e1071)
library(rpart)
library(randomForest)
str(h)
summary(h)
names(h)
h = h[,c(-1,-19,-22)]
ht = ht[,c(-1,-19,-22)]
p = na.omit(ddply(h, "workstat", summarise, mean = mean(prestige, na.rm=T)))
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(h,2,pMiss)

md.pattern(h)

h$tvhours = ifelse(is.na(h$tvhours), median(h$tvhours, na.rm=T), h$tvhours)
h$prestige = ifelse(is.na(h$prestige), mean(h$prestige, na.rm=T), h$prestige)

idx <- sample(1:nrow(h),as.integer(0.7*nrow(h)))
trainh <- h[idx,]
testh <- h[-idx,]

# Naive Bayes
nbmodel = naiveBayes(happy~., trainh)
pred = predict(nbmodel, testh[,-35], type="class")
t = table(testh[,35],pred)
sum(diag(t))/sum(t)

# Decision Tree
dtmodel = rpart(happy~., trainh, method='class')
pred2 = predict(dtmodel, testh[,-35], type='class')
t2 = table(testh[,35], pred2)
sum(diag(t2))/sum(t2)

pred2f = predict(dtmodel, ht, type='class')
f = data.frame(1:2621, pred2f)
colnames(f) = c('Id', 'Category')
write.csv(f,'output.csv', row.names=F)


# Random Forest

rfmodel = randomForest(happy ~ . , data =trainh , na.action = na.roughfix, method='class')
pred3 = predict(rfmodel, testh[,-35], type='class')
t3 = table(testh$happy,pred3)
sum(diag(t3))/sum(t3)

x = rbind(trainh[1,c(-35)] , ht)
nht = x[-1,]
apply(nht,2,pMiss)
nht = complete(mice(nht))
pred3f = predict(rfmodel, nht, type='class')
f2 = data.frame(1:2621, pred3f)
colnames(f) = c('Id', 'Category')
write.csv(f,'output2.csv', row.names=F)

## Difference in output : DT vs RF
o=c()
for(i in 1:2621){
if(pred2f[i] == pred3f[i]){
  o=append(o,"f")
} else{
  o=append(o,"y")
}
}
length(o[o='y'])/length(o)
