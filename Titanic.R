setwd("C:/Users/vsvoboda001/Desktop")


library(ggplot2)
library(dplyr)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(rpart)
library(MLmetrics)


train_df=read.csv("train.csv")
test_df=read.csv("test.csv")
# defining final output file
result_df=data.frame("PassengerId"=test_df[,1],Survived=rep(0,nrow(test_df)))

# name is irelevant, drop it (it can probably be used for deeper analysis  by separating family name from the string)

train_df0=train_df[,-4]
test_df0=test_df[,-3]

###############

summary(train_df0)

# is ticket number useful?

NROW(unique(train_df0[,8]))
NROW((train_df0[,8]))
ggplot(train_df0)+geom_bar(aes(x=Ticket),fill="gray")


a=train_df0 %>% 
  group_by(Ticket) %>%
  summarise(no_rows = length(Ticket))          

a=subset(a,no_rows>2)
a

#681 unique values out of 891 - maybe some group tickets, largest group od size 7 - probably not significant we drop it


train_df1=train_df0[,-8]
test_df1=test_df0[,-7]


########################


## cabin column - most values missing - lets drop it + drop ID


train_df2=train_df1[,-c(1,9)]
test_df2=test_df1[,-c(1,8)]

#######################3

#########################3
####### deal with NAs
###########################
summary(train_df2)
summary(test_df2)
summary(rbind(train_df2[,-1],test_df2))
# Embarked - replaced missing values by most common value
train_df2[,"Embarked"]=as.character(train_df2[,"Embarked"])
train_df2[train_df2[,"Embarked"]=="","Embarked"]="S"
train_df2[,"Embarked"]=as.factor(train_df2[,"Embarked"])
# age - replace by median

train_df2[is.na(train_df2[,"Age"]),"Age"]=median(rbind(train_df2[,-1],test_df2)[,"Age"],na.rm = TRUE)
test_df2[is.na(test_df2[,"Age"]),"Age"]=median(rbind(train_df2[,-1],test_df2)[,"Age"],na.rm = TRUE)

# Fare - replace by mean
test_df2[is.na(test_df2[,"Fare"]),"Fare"]=mean(rbind(train_df2[,-1],test_df2)[,"Fare"],na.rm = TRUE)


######### 
summary(train_df2)
summary(test_df2)



str(train_df2)



#########################3
# discretize Age and Fare (better typically when using decision trees)
#########################


###################33
# Age discretization
########################
range(rbind(train_df2[,-1],test_df2)[,"Age"])
hist(rbind(train_df2[,-1],test_df2)[,"Age"])
train_df2$Age2=0
train_df2$Age2[train_df2$Age<=15]="0-15"
train_df2$Age2[train_df2$Age>15 & train_df2$Age<=30]="15-30"

#train_df2$Age2[train_df2$Age>=18 & train_df2$Age<=30]="18-30"


train_df2$Age2[train_df2$Age>=30]="30+"

ggplot(train_df2) + geom_bar(aes(x=Age2,fill=as.factor(Survived)),position="dodge")

aggregate(Survived ~ Age2, data=train_df2, FUN=function(x) {sum(x)/length(x)})




train_df2$Age=as.factor(train_df2$Age2)
train_df2$Age2=NULL


test_df2$Age2=0

test_df2$Age2[test_df2$Age<=15]="0-15"
test_df2$Age2[test_df2$Age>15 & test_df2$Age<=30]="15-30"

#test_df2$Age2[test_df2$Age>18 & test_df2$Age<=30]="18-30"


test_df2$Age2[test_df2$Age>30]="30+"

test_df2$Age=as.factor(test_df2$Age2)
test_df2$Age2=NULL

######################3
# Fare discretization
####################

range(rbind(train_df2[,-1],test_df2)[,"Fare"])
hist(rbind(train_df2[,-1],test_df2)[,"Fare"])

train_df2$Fare2=0


train_df2$Fare2[train_df2$Fare<=10]="0-10"
train_df2$Fare2[train_df2$Fare>10 & train_df2$Fare<=50]="10-50"



train_df2$Fare2[train_df2$Fare>50]="50+"


ggplot(train_df2) + geom_bar(aes(x=Fare2,fill=as.factor(Survived)),position="dodge")

aggregate(Survived ~ Fare2, data=train_df2, FUN=function(x) {sum(x)/length(x)})

train_df2$Fare=as.factor(train_df2$Fare2)
train_df2$Fare2=NULL

test_df2$Fare2=0


test_df2$Fare2[test_df2$Fare<=10]="0-10"
test_df2$Fare2[test_df2$Fare>10 & test_df2$Fare<=50]="10-50"




test_df2$Fare2[test_df2$Fare>50]="50+"

test_df2$Fare=as.factor(test_df2$Fare2)
test_df2$Fare2=NULL

##########################################33
######## preliminary analysis - examining dependency of individual variables with survival rate
###############################


str(rbind(train_df2[,-1],test_df2))
summary(rbind(train_df2[,-1],test_df2))



## male die much more likely
ggplot(train_df2) + geom_bar(aes(x=Sex,
                                 fill=as.factor(Survived)),
                             position="dodge")
aggregate(Survived ~ Sex, data=train_df2, FUN=function(x) {sum(x)/length(x)})

# Pclass - lower class more likely to die
ggplot(train_df2) + geom_bar(aes(x=Pclass,
                                 fill=as.factor(Survived)),
                             position="dodge")
aggregate(Survived ~ Pclass, data=train_df2, FUN=function(x) {sum(x)/length(x)})

# Age
ggplot(train_df2) + geom_bar(aes(x=Age,
                                 fill=as.factor(Survived)),
                             position="dodge")
aggregate(Survived ~ Age, data=train_df2, FUN=function(x) {sum(x)/length(x)})


# SibSp
ggplot(train_df2) + geom_bar(aes(x=SibSp,
                                 fill=as.factor(Survived)),
                             position="dodge")
aggregate(Survived ~ SibSp, data=train_df2, FUN=function(x) {sum(x)/length(x)})

# Parch
ggplot(train_df2) + geom_bar(aes(x=Parch,
                                 fill=as.factor(Survived)),
                             position="dodge")
aggregate(Survived ~ Parch, data=train_df2, FUN=function(x) {sum(x)/length(x)})

# Fare
ggplot(train_df2) + geom_bar(aes(x=Fare,
                                 fill=as.factor(Survived)),
                             position="dodge")
aggregate(Survived ~ Fare, data=train_df2, FUN=function(x) {sum(x)/length(x)})



# Embarked
ggplot(train_df2) + geom_bar(aes(x=Embarked,
                                 fill=as.factor(Survived)),
                             position="dodge")
aggregate(Survived ~ Embarked, data=train_df2, FUN=function(x) {sum(x)/length(x)})


###################
##################33
# all variables seem to be significant but Parch and SibSp seem to have similar properties and are in nature similar
# so we try to combine the into only one variable
#############################3333
train_df2$relatives=train_df2$SibSp+train_df2$Parch
train_df2$SibSp=NULL
train_df2$Parch=NULL

test_df2$relatives=test_df2$SibSp+test_df2$Parch
test_df2$SibSp=NULL
test_df2$Parch=NULL

# relatives
ggplot(train_df2) + geom_bar(aes(x=relatives,
                                 fill=as.factor(Survived)),
                             position="dodge")
aggregate(Survived ~ relatives, data=train_df2, FUN=function(x) {sum(x)/length(x)})

sum(train_df2$relatives==10)
sum(test_df2$relatives>4)




#################333

# we will discretize the relatives variable as well

# Relatives disretization
###################3

train_df2$Relatives2=0


train_df2$Relatives2[train_df2$relatives==0 | train_df2$relatives>3]="0 & 4+"
train_df2$Relatives2[train_df2$relatives>0 & train_df2$relatives<=3]="1-3"
#train_df2$Relatives2[train_df2$relatives>3]="4+"





ggplot(train_df2) + geom_bar(aes(x=Relatives2,fill=as.factor(Survived)),position="dodge")

aggregate(Survived ~ Relatives2, data=train_df2, FUN=function(x) {sum(x)/length(x)})

train_df2$relatives=as.factor(train_df2$Relatives2)
train_df2$Relatives2=NULL

test_df2$Relatives2=0


test_df2$Relatives2[test_df2$relatives==0 | test_df2$relatives>3]="0 & 4+"
test_df2$Relatives2[test_df2$relatives>0 & test_df2$relatives<=3]="1-3"

#test_df2$Relatives2[test_df2$relatives>3]="4+"




test_df2$relatives=as.factor(test_df2$Relatives2)
test_df2$Relatives2=NULL

###########################
### model training - let try decision tree
###########################

tree_model <- rpart(Survived ~ Age + Pclass + Sex  + relatives  + Fare + Embarked ,
                    data=train_df2,
                    method="class")

printcp(tree_model)	




fancyRpartPlot(tree_model)

est=predict(tree_model,train_df2)

Gini(y_pred=est[,2],y_true=(train_df2[,1]))
Accuracy(ifelse(est[,2]>0.5,1,0),y_true=(train_df2[,1]))

######################
#### control - splitting new test/train s
#######################
# out od sample test
Gtr=c()
Gte=c()
AcTr=c()
AcTe=c()
for (i in 1:10)
{
set.seed(i)
new_df=cbind(train_df2,"ran"=runif(nrow(train_df2)))
new_train=subset(new_df,ran>0.2)[,-8]
new_test=subset(new_df,ran<0.2)[,-8]


tree_model2 <- rpart(Survived ~ Age + Pclass + Sex  + relatives  + Fare + Embarked ,
                     data=new_train,
                     method="class")

#printcp(tree_model2)	




#fancyRpartPlot(tree_model2)

res=predict(tree_model2,new_train)
est=predict(tree_model2,new_test)

Gtr[i]=Gini(y_pred=res[,2],y_true=(new_train[,1]))
Gte[i]=Gini(y_pred=est[,2],y_true=(new_test[,1]))
AcTr[i]=Accuracy(ifelse(res[,2]>0.5,1,0),y_true=(new_train[,1]))
AcTe[i]=Accuracy(ifelse(est[,2]>0.5,1,0),y_true=(new_test[,1]))
}

#############################
##################################33
# visual checks of produced tree, comparison to simple models based on two key variables
###########################
##########################

aggregate(Survived ~ Sex, data=train_df2, FUN=function(x) {sum(x)/length(x)})
sim_model=aggregate(Survived ~ Sex +Pclass, data=train_df2, FUN=function(x) {sum(x)/length(x)})
sim_pred=rep(sim_model[6,3],nrow(train_df2))
sim_pred[train_df2$Sex=="female" & train_df2$Pclass==3]=sim_model[5,3]
sim_pred[train_df2$Sex=="male" & train_df2$Pclass==2]=sim_model[4,3]
sim_pred[train_df2$Sex=="female" & train_df2$Pclass==2]=sim_model[3,3]
sim_pred[train_df2$Sex=="male" & train_df2$Pclass==1]=sim_model[2,3]
sim_pred[train_df2$Sex=="female" & train_df2$Pclass==1]=sim_model[1,3]


Gini(sim_pred,y_true=(train_df2[,1]))
Accuracy(ifelse(sim_pred>0.5,1,0),y_true=(train_df2[,1]))

# out of sample test of simplified model
NGtr=c()
NGte=c()
NAcTr=c()
NAcTe=c()
for(i in 1:10)
{
set.seed(i)
new_df_sim=cbind(train_df2,"ran"=runif(nrow(train_df2)))
new_train_sim=subset(new_df_sim,ran>0.2)[,-8]
new_test_sim=subset(new_df_sim,ran<0.2)[,-8]


sim_pred=rep(sim_model[6,3],nrow(new_train_sim))
sim_pred[new_train_sim$Sex=="female" & new_train_sim$Pclass==3]=sim_model[5,3]
sim_pred[new_train_sim$Sex=="male" & new_train_sim$Pclass==2]=sim_model[4,3]
sim_pred[new_train_sim$Sex=="female" & new_train_sim$Pclass==2]=sim_model[3,3]
sim_pred[new_train_sim$Sex=="male" & new_train_sim$Pclass==1]=sim_model[2,3]
sim_pred[new_train_sim$Sex=="female" & new_train_sim$Pclass==1]=sim_model[1,3]


NGtr[i]=Gini(sim_pred,y_true=(new_train_sim[,1]))
NAcTr[i]=Accuracy(ifelse(sim_pred>0.5,1,0),y_true=(new_train_sim[,1]))


sim_pred=rep(sim_model[6,3],nrow(new_test_sim))
sim_pred[new_test_sim$Sex=="female" & new_test_sim$Pclass==3]=sim_model[5,3]
sim_pred[new_test_sim$Sex=="male" & new_test_sim$Pclass==2]=sim_model[4,3]
sim_pred[new_test_sim$Sex=="female" & new_test_sim$Pclass==2]=sim_model[3,3]
sim_pred[new_test_sim$Sex=="male" & new_test_sim$Pclass==1]=sim_model[2,3]
sim_pred[new_test_sim$Sex=="female" & new_test_sim$Pclass==1]=sim_model[1,3]


NGte[i]=Gini(sim_pred,y_true=(new_test_sim[,1]))
NAcTe[i]=Accuracy(ifelse(sim_pred>0.5,1,0),y_true=(new_test_sim[,1]))
}




#######################
############################
## writing the final output file based on chosen model
########################
#######################
result_df[,2]=predict(tree_model,test_df2)[,2]

result_df$Survived[result_df$Survived>=0.5]=1
result_df$Survived[result_df$Survived<0.5]=0

write.csv(result_df,"prediction.csv",row.names=FALSE)
######################3
#########################

######### noticing the model flaw - it kills too many man

test_df$Survived=result_df[,2]

ggplot(test_df) + geom_bar(aes(x=Sex,
                               fill=as.factor(Survived)),
                           position="dodge")
aggregate(Survived ~ Sex, data=test_df, FUN=function(x) {sum(x)/length(x)})

ggplot(train_df) + geom_bar(aes(x=Sex,
                                fill=as.factor(Survived)),
                            position="dodge")
aggregate(Survived ~ Sex, data=train_df, FUN=function(x) {sum(x)/length(x)})









