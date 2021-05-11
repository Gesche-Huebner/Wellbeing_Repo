

#Cross validation

#Run EHS_Wellbeing_Markdown first to have all necessary files in workspace

#do crossvalidation
#calculated accuracy for all models in test and train data
#calculated balanced accuracy, false positive, false negative for LifeSatisfaction_Dummy 
set.seed(2201)

wellbeing_red$LifeSatisfaction_Dummy<-as.factor(wellbeing_red$LifeSatisfaction_Dummy)
wellbeing_red$Worthwhile_Dummy<-as.factor(wellbeing_red$Worthwhile_Dummy)
wellbeing_red$Happy_Dummy<-as.factor(wellbeing_red$Happy_Dummy)
wellbeing_red$Anxious_Dummy<-as.factor(wellbeing_red$Anxious_Dummy)

#for Life Satisfaction 
#set up training and test data
training.sample<-wellbeing_red$LifeSatisfaction_Dummy%>% createDataPartition(p=0.8, list=FALSE)

train.data<-wellbeing_red[training.sample,]
test.data<-wellbeing_red[-training.sample,]

#fit the training model 
mod_fit<-train(formula(model_life_dummy), data=train.data, method="glm", family=binomial(link="logit"))

#get the data to use for testing
a<-prepwb(test.data,new_dummy,'LifeSatisfaction_Dummy',baddies_dummy)
#get predicted values
mod_fit_test<-predict(mod_fit,newdata=a[,-which(colnames(test.data)=="LifeSatisfaction_Dummy")], type="raw")

#function to get accuracy of test data
calc_acc<-function(actual, predicted){
  mean(actual ==predicted)
}

calc_acc(actual=a$LifeSatisfaction_Dummy, predicted=mod_fit_test)
#type mod_fit for accuracy of training data


#look at confusion matrix  to get other other information 
predictions <- mod_fit %>% predict(test.data)

pred = predict(mod_fit, newdata=test.data)
#test data
confusionMatrix(data=pred, test.data$LifeSatisfaction_Dummy)
#train data
caret::confusionMatrix.train(mod_fit)
confusionMatrix(confusionMatrix.train(mod_fit)$table)


#get area under the curve

library(pROC)
test_roc<-roc(test.data$LifeSatisfaction_Dummy, as.numeric(mod_fit_test), plot=T, print.auc=T)




#for Wortwhile 
training.sample<-wellbeing_red$Worthwhile_Dummy%>% createDataPartition(p=0.8, list=FALSE)

train.data<-wellbeing_red[training.sample,]
test.data<-wellbeing_red[-training.sample,]

mod_fit<-train(formula(model_worth_dummy), data=train.data, method="glm", family=binomial(link="logit"))

#get the data to use
a<-prepwb(test.data,new_dummy,'Worthwhile_Dummy',baddies_dummy)
#get predicted values
mod_fit_test<-predict(mod_fit,newdata=a[,-which(colnames(test.data)=="Worthwhile_Dummy")], type="raw")


calc_acc(actual=a$Worthwhile_Dummy, predicted=mod_fit_test)



#for Happy
training.sample<-wellbeing_red$Happy_Dummy%>% createDataPartition(p=0.8, list=FALSE)

train.data<-wellbeing_red[training.sample,]
test.data<-wellbeing_red[-training.sample,]

mod_fit<-train(formula(model_happy_dummy), data=train.data, method="glm", family=binomial(link="logit"))

#get the data to use
a<-prepwb(test.data,new_dummy,'Happy_Dummy',baddies_dummy)
#get predicted values
mod_fit_test<-predict(mod_fit,newdata=a[,-which(colnames(test.data)=="Happy_Dummy")], type="raw")

calc_acc(actual=a$Happy_Dummy, predicted=mod_fit_test)


#for Anxious
training.sample<-wellbeing_red$Anxious_Dummy%>% createDataPartition(p=0.8, list=FALSE)

train.data<-wellbeing_red[training.sample,]
test.data<-wellbeing_red[-training.sample,]

mod_fit<-train(formula(model_anxious_dummy), data=train.data, method="glm", family=binomial(link="logit"))

#get the data to use
a<-prepwb(test.data,new_dummy,'Anxious_Dummy',baddies_dummy)
#get predicted values
mod_fit_test<-predict(mod_fit,newdata=a[,-which(colnames(test.data)=="Anxious_Dummy")], type="raw")

calc_acc(actual=a$Anxious_Dummy, predicted=mod_fit_test)







