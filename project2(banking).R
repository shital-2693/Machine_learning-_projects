setwd("/Users/Admin/Downloads")
ldata_test=read.csv("bank-full_test.csv" ,stringsAsFactors = F)
setwd("/Users/Admin/Downloads")
ldata_train=read.csv("bank-full_train.csv" ,stringsAsFactors = F)
ldata_test$y=NA
ldata_test$data='test'
ldata_train$data='train'
ld_all=rbind(ldata_train,ldata_test)
colnames(ldata_test)
colnames(ldata_train)
str(ld_all)
summary(ld_all)
library(dplyr)
# Convert some of the variables, which have come as characters, to numbers, wherever applicable

ld_all1=ld_all %>% 
  mutate(default = as.numeric(default=="yes"),
         housing = as.numeric(housing=="yes"),
         loan = as.numeric(loan=="yes"),
         y=as.numeric(y=="yes")
         
  )
glimpse(ld_all1)
# dummy var for job
table(ld_all1$job)
round(prop.table(table(ld_all1$job,ld_all1$y),1),2)
# groups= 
# 1.unknown,admin.,self-employed (they have the same response rate)
# 2.unemployed,management
# 3.student
# 4. retired
# 5.technician
# 6.entrepreneur,services  (same)
# 7.housemaid, blue-collar
ld_all2=ld_all1 %>%
  mutate(jb_2=as.numeric(job %in% c("unemployed" , "management" )),
         jb_6=as.numeric(job %in% c("entrepreneur","services")),
         jb_7=as.numeric(job %in% c("housemaid", "blue-collar")),
         jb_3=as.numeric(job=="student"),
         jb_4=as.numeric(job=="retired"),
         jb_5=as.numeric(job=="technician"))%>% 
  select(-job)
  glimpse(ld_all2)
  
# dummy var for marital
table(ld_all2$marital) 
ld_all3=ld_all2 %>% 
  mutate(ms_s=as.numeric(marital=="single"),
         ms_d=as.numeric(marital=="divorced"))%>% 
  select(-marital)
glimpse(ld_all3)
# dummy for education
round(prop.table(table(ld_all3$education,ld_all3$y),1),2)

ld_all4=ld_all3 %>%
  mutate(ed_u=as.numeric(education=="unknown"),
         ed_t=as.numeric(education=="tertiary"),
         ed_s=as.numeric(education=="secondary")) %>%
  select(-education)
glimpse(ld_all4)
# dummy for contact
round(prop.table(table(ld_all4$contact,ld_all4$y),1),2)

ld_all5=ld_all4 %>% 
  mutate(ct_c=as.numeric(contact=="cellular"),
         ct_t=as.numeric(contact=="telephone"))%>% 
  select(-contact)
glimpse(ld_all5)
# dummy for poutcome

round(prop.table(table(ld_all5$poutcome,ld_all5$y),1),2)


ld_all6=ld_all5 %>%
  select(-ID,-poutcome)
glimpse(ld_all6)
str(ld_all6)
# dummy for month
round(prop.table(table(ld_all6$month,ld_all6$y),1),2)

ld=ld_all6 %>% 
  mutate(mt_o=as.numeric(month=="oct"),
         mt_sd=as.numeric(month %in% c("sep","dec")),
         mt_m=as.numeric(month=="mar"),
         mt_ap=as.numeric(month=="apr"),
         mt_f=as.numeric(month=="feb"),
         mt_jan=as.numeric(month %in% c("aug","jan","nov")),
         mt_jj=as.numeric(month %in% c("jul","jun")))%>%

    select(-month)
glimpse(ld)
# dealing with NA values
for(col in names(ld)){
  if(sum(is.na(ld[,col]))>0 & !(col %in% c("data","y"))){
    ld[is.na(ld[,col]),col]=mean(ld[ld$data=='train',col],na.rm=T)
  }
}
lapply(ld,function(x) sum(is.na(x)))
# dealing with outliers
# 1.balance
summary(ld)

boxplot(ld$balance,horizontal = T)
iqr=1428-72
upfen_bal<-1428+1.5*iqr
lowfen_bal<-72-1.5*iqr

ld$balance<-ifelse(ld$balance > 3462, 3462, ld$balance)
boxplot(ld$balance, horizontal = T)

ld$balance[ld$balance < -1962]<-lowfen_bal
boxplot(ld$balance, horizontal = T)

# 2.duration
iqr1=319-103
upfen_bal<-319+1.5*iqr1
lowfen_bal<-103-1.5*iqr1

ld$duration<-ifelse(ld$duration > 643, 643, ld$duration)
boxplot(ld$duration, horizontal = T)

ld$duration[ld$duration < -221]<-lowfen_bal
boxplot(ld$duration, horizontal = T)
summary(ld)


# split the data
ld_train=ld %>% filter(data=="train") %>% select(-data)
ld_test=ld %>% filter(data=="test") %>% select(-data,-y)

set.seed(18)
ld=sample(1:nrow(ld_train),0.8*nrow(ld_train))
#trainset 
ld_train1=ld_train[ld,]
# testset
ld_train2=ld_train[-ld,]

# Building a logistic Regression Model
library(car)

# Check VIF
vif_train=lm(y~.-ID,data=ld_train1)
sort(vif(vif_train),decreasing = T)[1:3] #looks ohk

# Build Logistic Regression model
fit_train=glm(y~.-ID,data=ld_train1,family="binomial")
summary(fit_train)

step(fit_train)
formula(fit_train)

fit_train=glm(formula = y ~ balance + housing + loan + duration + campaign + 
                jb_7 + jb_3 + jb_4 + ms_s + ms_d + ed_t + ed_s + ct_c + ct_t + 
                ptc_s + ptc_o + ptc_f + mt_o + mt_sd + mt_m + mt_ap + mt_f + 
                mt_jan + mt_jj, family = "binomial", data = ld_train1)
summary(fit_train)

# performance of score model on validation data
library(pROC)
score.train=predict(fit_train,ld_train2,type = "response")
auc_sore=auc(roc(ld_train2$y,score.train))
auc_sore

library(ggplot2)
mydata=data.frame(y=ld_train2$y,score.train=score.train)

ggplot(mydata,aes(y=y,x=score.train,color=factor(y)))+geom_point()+geom_jitter()

# Final Model

vif_train=lm(y~.-ID,data=ld_train)
sort(vif(vif_train),decreasing = T)[1:3]

fit_train.final=glm(y~.-ID,data=ld_train,family="binomial")

step(fit_train.final)

fit_train.final=glm(formula = y ~ balance + housing + loan + duration + campaign + 
                      jb_6 + jb_7 + jb_3 + jb_4 + ms_s + ms_d + ed_t + ed_s + ct_c + 
                      ct_t + ptc_s + ptc_o + ptc_f + mt_o + mt_sd + mt_m + mt_ap + 
                      mt_f + mt_jan + mt_jj - mt_jj - jb_6, family = "binomial", data = ld_train)

summary(fit_train.final)

# predict on testdata

test.prob.score=predict(fit_train,newdata = ld_test, type="response")
write.csv(test.prob.score,"shital_shende_p5.csv",row.names = F)

train.score=predict(fit_train.final,newdata = ld_train, type="response")
real=ld_train$y
length(real)
head(train.score)
# cuttoff
cutoff_data = NULL
cutoffs=round(seq(0,1,length=100),3)
cutoffs # A vector of 100 cutoff values

for (cutoff in cutoffs)
{
  predicted=as.numeric(train.score>cutoff) # predicted=ifelse(rg_train$score>cutoff, 1, 0)
  
  TP=sum(predicted==1 & ld_train$y==1)
  FP=sum(predicted==1 & ld_train$y==0)
  FN=sum(predicted==0 & ld_train$y==1)
  TN=sum(predicted==0 & ld_train$y==0)
  cutoff_data=rbind.data.frame(cutoff_data,c(cutoff,TP,FP,FN,TN))
}

colnames(cutoff_data) = c("Cutoff", "TP", "FP", "FN", "TN")
View(cutoff_data)


P = sum(ld_train$y==1) # Total positives or 1s
N = sum(ld_train$y==0) # Total negatives or 0s

cutoff_data=cutoff_data %>%
  mutate(Sn=TP/P, 
         Sp=TN/N,
         dist=sqrt((1-Sn)**2+(1-Sp)**2),
         P=FN+TP,
         N=TN+FP) %>%
  mutate(KS=abs((TP/P)-(FP/N))) %>%
  mutate(Accuracy=(TP+TN)/(P+N)) %>%
  mutate(M=(9*FN+5*FP)/(P+N)) %>% # Custom metric
  select(-P,-N)

View(cutoff_data)

library(tidyr)

cutoff_viz=cutoff_data %>%
  select(Cutoff,Sn,Sp,dist,KS,Accuracy,M) %>%
  gather(Criterion,Value,Sn:M)

View(cutoff_viz) # Data has been changed from wide to long format
# Vizualization
ggplot(cutoff_viz,aes(x=Cutoff,y=Value,color=Criterion))+
  geom_line()


cutoff_viz %>%
  filter(Criterion=="KS") %>%
  ggplot(aes(x=Cutoff,y=Value,color=Criterion))+geom_line()


## ------------------------------------------------------------------------
# Cutoff with max KS
KS_cutoff=cutoff_data$Cutoff[which.max(cutoff_data$KS)][1]
KS_cutoff

# Predict on the test data using the model
test.score=predict(fit_train.final,newdata=ld_test,type="response")

# Classify probability values into 0s and 1s based on KS cutoff
test.predicted=as.numeric(test.prob.score > KS_cutoff)
test.predicted=ifelse(test.predicted==1,"Yes","No")
write.csv(test.predicted,'SHITAL2_SHENDE.csv',row.names = F)

# Confusion Matrix
# error having different length...
# 
# ## ------------------------------------------------------------------------
# # Cutoff with max Accuracy
Acc_cutoff=cutoff_data$Cutoff[which.max(cutoff_data$Accuracy)][1]
 Acc_cutoff
# 
# table(rg_test$Revenue.Grid,as.numeric(rg_test$score>Acc_cutoff))
# 
# ## ------------------------------------------------------------------------
# Cutoff with minimum M ( The hypothetical business criterion)
 M_cutoff=cutoff_data$Cutoff[which.min(cutoff_data$M)][1]
 M_cutoff
# 
table(ld_test,as.numeric(rg_test$score>M_cutoff))
# 
# ## ------------------------------------------------------------------------
# # ROC Curve
# 
library(pROC)
roccurve=roc(ld_train$y,train.score)
plot(roccurve)/
auc(roccurve) # The higher the auc the better the model
# 
roc_data=cutoff_data %>%
  select(cutoff,Sn,Sp) %>%
  mutate(TPR=Sn,FPR=1-Sp) %>%
  select(cutoff,TPR,FPR)
# 
# View(roc_data) # This table will help look for the "cutoff" value once you have identified an appropriate (FPR, TPR)
# 
# ggplot(roc_data,aes(x=FPR,y=TPR))+geom_line()+ggtitle("My ROC Curve")





