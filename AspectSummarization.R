install.packages('tm')
install.packages('dplyr')
install.packages('qdap')
install.packages('topicmodels')
install.packages('ggplot2')
install.packages('knitr')


# Code to load the packages

library(tm)
library(qdap)
library(topicmodels)
library(ggplot2)
library(wordcloud)
library(knitr)
library(SnowballC)
library(data.table)
library(dplyr)

library(tm)
library(rjson)



#setwd("C:/Users/Akhil/Documents/Textmining/AmazonReviews/mobilephone")

# Define the path to the text files. Point it to the directory where files have been kept

filenames <- list.files("C:/Users/Akhil/Documents/Textmining/AmazonReviews/mobilephone", pattern="*.json", full.names=TRUE) # this should give you a character vector, with each file name represented by an entry
#Using Lapply to parse all the files in path into an embedded list
myx_mob <- lapply(filenames, function(x) fromJSON(file=x))

#Initializing data frames for storing product info
prod_mob<-data.frame(Characters=character(),Characters=character(),  stringsAsFactors = FALSE)
name_mob<-data.frame(Characters=character(), stringsAsFactors = FALSE)
products<-NULL

#Using a loop for pulling product ID and product description from the list and storing it as a dataframe
for (i in 1:4471)
{
  
  if (is.null(myx_mob[[i]]$ProductInfo$Name)==TRUE) {myx_mob[[i]]$ProductInfo$Name<-"x"}
  
  products<-cbind(
    as.data.frame(myx_mob[[i]]$ProductInfo$ProductID),
    as.data.frame(myx_mob[[i]]$ProductInfo$Name)
  )
  
  prod_mob<-rbind(prod_mob,products)  
}
#write.csv(prod_mob,file="prod_mob.csv")

#myx_mob[[3]]$ProductInfo$Name
#prod_mob[1,]


#Removing product information from the master list for ease of parsing reviews data
for (i in 1:4471)
{myx_mob[[i]]$ProductInfo<-NULL}  
#Initializing dataframes for storing reviews data
datax_mob<-NULL

datamine_mob<-NULL

datamine_mob<-data.frame(Characters=character(),
                         Characters=character(),
                         Characters=character(),
                         Ints=integer(),
                         Characters=character(),
                         Characters=character(),
                         
                         stringsAsFactors = FALSE)

datax_mob<-data.frame(Characters=character(),
                      Characters=character(),
                      Characters=character(),
                      Ints=integer(),
                      Characters=character(),
                      Characters=character(),
                      
                      
                      stringsAsFactors = FALSE)
prodx_mob<-NULL
prody_mob<-NULL
prodz_mob<-NULL

prodx_mob<-data.frame(Characters=character(), stringsAsFactors = FALSE)
prody_mob<-data.frame(Characters=character(), stringsAsFactors = FALSE)
prodz_mob<-data.frame(Characters=character(), stringsAsFactors = FALSE)

#final dataframe is this. Using try function for overriding the errors which may be encountered if any of the fields in reviewsdata is null


for (i in 1:4471)
  
{ 
  {
    
    
    try(datamine_mob<-do.call("rbind",lapply(myx_mob[[i]]$Reviews,as.data.frame)))
    
    try(lenx<-nrow(datamine_mob))
    
    prodx_mob<-as.data.frame(prod_mob[i,1])
    
    try(prody_mob<- as.data.frame(prodx_mob[rep(row.names(prodx_mob),nrow(datamine_mob)),1]))
    
    
    
    prodz_mob=rbind(prodz_mob,prody_mob)
    
    #datamine<-cbind(datamine,xx)
    datax_mob<-rbind(datax_mob,datamine_mob)  
  }
  
  print(i)
  prody_mob<-NULL
  # print(nrow(prodz))
  #print(nrow(datax))
  
  
}


#getwd()
#setwd("C:/Users/Akhil/Documents/Textmining/AmazonReviews/mobile")
#dt_mob<- read.csv(file="C:/Users/Akhil/Documents/Textmining/AmazonReviews/mobile/dt_mobile.csv")


#data table is much more efficient for large dataframes

dt_mob<-as.data.table(cbind(datax_mob,prodz_mob))

colnames(dt_mob)
#changing column name
names(dt_mob)[7]<-"Product"

#finding out records with same review ID
mob_dup<-count(dt_mob,Author,ReviewID)

#Filtering out all the non-duplicate rows
mob_dup<-filter(mob_dup,n==1)

#Inner joining the main dataframe and the table having unique review ID
dt_mobv2<-merge(dt_mob,mob_dup,
                by.x=c("Author","ReviewID"),by.y=c("Author","ReviewID"))



#Filtering out data for Nokia Lumia 521
senti<-filter(dt_mobv2,Product=="B00COYOAYW")
#Changing the column names of the dataframe havig product ID and product description
names(prod_mob)[1]<-"Product"

names(prod_mob)[2]<-"Productinfo"

#Mapping product description

sentiv1<-merge(senti,prod_mob,by.x =c("Product"),by.y = c("Product") )

sentiv1<-as.data.frame(sentiv1)



#Code for calculating topic polarity
#It runs a loop(for no. of reviews) and checks whether the given set of keywords exist or not. If they do exist, it creates Six-grams of the review and checks for the components having the keywords.It then calculates polarity of those components and assigns it to a new column pertaining to topic polarity
#code for polarity topic software

polx<-data.frame(
  Ints=integer(),
  
  
  stringsAsFactors = FALSE)
for (i in 1:1151)
{
  price<-regexpr("windows|apps|app|android|phone|music|device|map|ios|microsoft|lumia|gps|data|wifi|video|gb|storage|sd|photo|download|memory|os|google|install|web|media|file|email|software|updat",sentiv1[i,"Content"],ignore.case = TRUE, perl = FALSE,
                 fixed = FALSE, useBytes = FALSE)
  if (price==-1) {sentiv1[i,"softwareind"]=0} else {sentiv1[i,"softwareind"]=1}
  
  
  #  x<-as.data.frame(filter(sentiv1,softwareind==1))
  
  if (sentiv1[i,"softwareind"]==1)
  {
    text<-as.vector(sentiv1[i,6])
    ng<-ngram(text,n=6)
    x<-as.list(get.ngrams(ng))
    len<-length(x)
    for (j in 1:len)
    {
      price<-regexpr("windows|apps|app|android|phone|music|device|map|ios|microsoft|lumia|gps|data|wifi|video|gb|storage|sd|photo|download|memory|os|google|install|web|media|file|email|software|updat",x[j],ignore.case = TRUE, perl = FALSE,
                     fixed = FALSE, useBytes = FALSE)
      if (price[1]!=-1){
        pol<-as.data.frame(polarity(x[j]))
        #pol<-pol$all$polarity
      } 
      #else {pol<-as.data.frame(0,0,0,0,0,0)}
      pol<-distinct(select(pol,group.ave.polarity),group.ave.polarity)
      polx=rbind(polx,pol)
      polx<-distinct(polx,group.ave.polarity)
      #polx<-filter(polx,polarity!=0) 
      
    }
    sentiv1[i,"softwareindpolarity"]<-mean(polx$group.ave.polarity,na.rm = TRUE)
    polx<-NULL
  }
  else if (sentiv1[i,"softwareind"]==0) 
  {sentiv1[i,"softwareindpolarity"]<-0
  }
  
}




#topic 4 - Polarity for Hardware

polx<-data.frame(
  Ints=integer(),
  
  
  stringsAsFactors = FALSE)
for (i in 1:1151)
{
  price<-regexpr("battery|day|life|charge|screen|turn|camera|hour|time|charge|phone|week|issue|front|thing|button|touch|receiv|turn on|drop|saver|back|shut|replacement|picture|front face camera|nokia|power|hand|touch screen|plug|front camera|warranty|flash|light|freeze|power|button|night|drain|piece",sentiv1[i,"Content"],ignore.case = TRUE, perl = FALSE,
                 fixed = FALSE, useBytes = FALSE)
  if (price==-1) {sentiv1[i,"hardwareind"]=0} else {sentiv1[i,"hardwareind"]=1}
  
  
  #  x<-as.data.frame(filter(sentiv1,hardwareind==1))
  
  if (sentiv1[i,"hardwareind"]==1)
  {
    text<-as.vector(sentiv1[i,6])
    ng<-ngram(text,n=6)
    x<-as.list(get.ngrams(ng))
    len<-length(x)
    for (j in 1:len)
    {
      price<-regexpr("battery|day|life|charge|screen|turn|camera|hour|time|charge|phone|week|issue|front|thing|button|touch|receiv|turn on|drop|saver|back|shut|replacement|picture|front face camera|nokia|power|hand|touch screen|plug|front camera|warranty|flash|light|freeze|power|button|night|drain|piece",x[j],ignore.case = TRUE, perl = FALSE,
                     fixed = FALSE, useBytes = FALSE)
      if (price[1]!=-1){
        pol<-as.data.frame(polarity(x[j]))
        #pol<-pol$all$polarity
      } 
      #else {pol<-as.data.frame(0,0,0,0,0,0)}
      pol<-distinct(select(pol,group.ave.polarity),group.ave.polarity)
      polx=rbind(polx,pol)
      polx<-distinct(polx,group.ave.polarity)
      #polx<-filter(polx,polarity!=0) 
      
    }
    sentiv1[i,"hardwareindpolarity"]<-mean(polx$group.ave.polarity,na.rm = TRUE)
    polx<-NULL
  }
  else if (sentiv1[i,"hardwareind"]==0) 
  {sentiv1[i,"hardwareindpolarity"]<-0
  }
  
}












#Polarity for carrier


polx<-data.frame(
  Ints=integer(),
  
  
  stringsAsFactors = FALSE)
for (i in 1:1151)
{
  price<-regexpr("card|tons|price|sim|unlock|service|plan|customer|pay|old|month|data|device|return|amazon|activat|prepay|product|tmobile|contract|unlimited|replacement|provider|year|money|transfer|network|micro|store|wifi|cell phone|activation|t-mobile|switch|refund|deal|cost|metropcs|offer|information|country",sentiv1[i,"Content"],ignore.case = TRUE, perl = FALSE,
                 fixed = FALSE, useBytes = FALSE)
  if (price==-1) {sentiv1[i,"carrierind"]=0} else {sentiv1[i,"carrierind"]=1}
  
  
  #  x<-as.data.frame(filter(sentiv1,hardwareind==1))
  
  if (sentiv1[i,"carrierind"]==1)
  {
    text<-as.vector(sentiv1[i,6])
    ng<-ngram(text,n=6)
    x<-as.list(get.ngrams(ng))
    len<-length(x)
    for (j in 1:len)
    {
      price<-regexpr("card|tons|price|sim|unlock|service|plan|customer|pay|old|month|data|device|return|amazon|activat|prepay|product|tmobile|contract|unlimited|replacement|provider|year|money|transfer|network|micro|store|wifi|cell phone|activation|t-mobile|switch|refund|deal|cost|metropcs|offer|information|country",x[j],ignore.case = TRUE, perl = FALSE,
                     fixed = FALSE, useBytes = FALSE)
      if (price[1]!=-1){
        pol<-as.data.frame(polarity(x[j]))
        #pol<-pol$all$polarity
      } 
      #else {pol<-as.data.frame(0,0,0,0,0,0)}
      pol<-distinct(select(pol,group.ave.polarity),group.ave.polarity)
      polx=rbind(polx,pol)
      polx<-distinct(polx,group.ave.polarity)
      #polx<-filter(polx,polarity!=0) 
      
    }
    sentiv1[i,"carrierindpolarity"]<-mean(polx$group.ave.polarity,na.rm = TRUE)
    polx<-NULL
  }
  else if (sentiv1[i,"carrierind"]==0) 
  {sentiv1[i,"carrierindpolarity"]<-0
  }
  
}



#Additional illustration - Below are the snippets of code where software topic is broken down into two components - OS and other apps. This is done to check whether the model's accuracy increases or not if we use more specific topics. These two variables have not been included in the main model

#softwarepart1 - OS


polx<-data.frame(
  Ints=integer(),
  
  
  stringsAsFactors = FALSE)
for (i in 1:1151)
{
  price<-regexpr("windows|apps|app|android|ios|microsoft|os|google|install|web|email|software|updat",sentiv1[i,"Content"],ignore.case = TRUE, perl = FALSE,
                 fixed = FALSE, useBytes = FALSE)
  if (price==-1) {sentiv1[i,"softwarev1ind"]=0} else {sentiv1[i,"softwarev1ind"]=1}
  
  
  #  x<-as.data.frame(filter(sentiv1,softwarev1ind==1))
  
  if (sentiv1[i,"softwarev1ind"]==1)
  {
    text<-as.vector(sentiv1[i,6])
    ng<-ngram(text,n=6)
    x<-as.list(get.ngrams(ng))
    len<-length(x)
    for (j in 1:len)
    {
      price<-regexpr("windows|apps|app|android|ios|microsoft|os|google|install|web|email|software|updat",x[j],ignore.case = TRUE, perl = FALSE,
                     fixed = FALSE, useBytes = FALSE)
      if (price[1]!=-1){
        pol<-as.data.frame(polarity(x[j]))
        #pol<-pol$all$polarity
      } 
      #else {pol<-as.data.frame(0,0,0,0,0,0)}
      pol<-distinct(select(pol,group.ave.polarity),group.ave.polarity)
      polx=rbind(polx,pol)
      polx<-distinct(polx,group.ave.polarity)
      #polx<-filter(polx,polarity!=0) 
      
    }
    sentiv1[i,"softwarev1indpolarity"]<-mean(polx$group.ave.polarity,na.rm = TRUE)
    polx<-NULL
  }
  else if (sentiv1[i,"softwarev1ind"]==0) 
  {sentiv1[i,"softwarev1indpolarity"]<-0
  }
  
}



#softwarepart2 -app



polx<-data.frame(
  Ints=integer(),
  
  
  stringsAsFactors = FALSE)
for (i in 1:1151)
{
  price<-regexpr("phone|music|device|map|lumia|gps|data|wifi|video|gb|storage|sd|photo|download|memory|media|file",sentiv1[i,"Content"],ignore.case = TRUE, perl = FALSE,
                 fixed = FALSE, useBytes = FALSE)
  if (price==-1) {sentiv1[i,"softwarev2ind"]=0} else {sentiv1[i,"softwarev2ind"]=1}
  
  
  #  x<-as.data.frame(filter(sentiv1,softwarev2ind==1))
  
  if (sentiv1[i,"softwarev2ind"]==1)
  {
    text<-as.vector(sentiv1[i,6])
    ng<-ngram(text,n=6)
    x<-as.list(get.ngrams(ng))
    len<-length(x)
    for (j in 1:len)
    {
      price<-regexpr("phone|music|device|map|lumia|gps|data|wifi|video|gb|storage|sd|photo|download|memory|media|file",x[j],ignore.case = TRUE, perl = FALSE,
                     fixed = FALSE, useBytes = FALSE)
      if (price[1]!=-1){
        pol<-as.data.frame(polarity(x[j]))
        #pol<-pol$all$polarity
      } 
      #else {pol<-as.data.frame(0,0,0,0,0,0)}
      pol<-distinct(select(pol,group.ave.polarity),group.ave.polarity)
      polx=rbind(polx,pol)
      polx<-distinct(polx,group.ave.polarity)
      #polx<-filter(polx,polarity!=0) 
      
    }
    sentiv1[i,"softwarev2indpolarity"]<-mean(polx$group.ave.polarity,na.rm = TRUE)
    polx<-NULL
  }
  else if (sentiv1[i,"softwarev2ind"]==0) 
  {sentiv1[i,"softwarev2indpolarity"]<-0
  }
  
}


#Modeling component

install.packages("sampling")
install.packages("tree")
install.packages("ISLR")
install.packages("boot")
install.packages("Metrics")
install.packages("ROCR")
install.packages("caret")
install.packages("ggplot2")
install.packages("e1071")
install.packages("cvTools")
install.packages("dplyr")
install.packages("rpart")				        # Popular decision tree algorithm
install.packages("rattle")					# Fancy tree plot
install.packages("rpart.plot")				# Enhanced tree plots
install.packages("RColorBrewer")				# Color selection for fancy tree plot
install.packages("randomForest")
install.packages("sampling")
install.packages("e1071")
install.packages("plyr")
install.packages("data.table")

library("data.table")

library("sampling")
library("tree")
library("ISLR")
library("boot")
library("Metrics")
library("ROCR")
library("caret")
library("ggplot2")
library("e1071")
library("cvTools")
library("dplyr")
library("rpart")				        # Popular decision tree algorithm
library("rattle")					# Fancy tree plot
library("rpart.plot")				# Enhanced tree plots
library("RColorBrewer")				# Color selection for fancy tree plot
library("randomForest")
library("sampling")
library("e1071")
library("plyr")



sentiv1$Overall<-as.numeric(sentiv1$Overall)
sentiv1$Overallnew <- mapvalues(sentiv1$Overall, from = c("1", "2","3","4","5"), to = c("5", "4","3","2","1"))

sentiv1$target<-ifelse(sentiv1$Overallnew<4,0,1)
table(sentiv1$target)
data1<-as.data.frame(sentiv1)
View(data1)
#data1$lengthchr<-nchar(as.character(data1$Content))
#x<-mean(data1$lengthchr)
data1<-cbind(data1,sapply(gregexpr("\\S+",data1$Content),length))
colnames(data1)
  attach(data1)
names(data1)[22]<-"LengthIndex"

#Base Model - Logistic regression

s=strata(data1,stratanames = "target",size=c(273,273),method = "srswor")
s=getdata(data1,s)

#View(s)
set.seed(1)
#splitting the dataset into train and test
index <- sample(1:nrow(s), size = 0.6*nrow(s))
train <- s[index,]
test <- s[-index,]

#building generalised linear model
LogModel <- glm(target~ softwareind+hardwareindpolarity+carrierindpolarity+LengthIndex, family=binomial, data = train)
#predicting the model on test
LogModel_pred <- predict(LogModel, test)
pred_class <- ifelse(LogModel_pred>=0.1, 1,0)
#computing the accuracy
table(pred_class,test$target)
print(paste("Accuracy usind validation method is: ", 1-sum(pred_class!=test$target)/nrow(test)))
summary(LogModel)
##Code to plot the ROC curve
pred<-prediction(LogModel_pred,test$target)
pref<-performance(pred, measure = "tpr", x.measure = "fpr")
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
roc.data <- data.frame(fpr=unlist(pref@x.values), tpr=unlist(pref@y.values), model="GLM")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) + geom_ribbon(alpha=0.2) +  
  geom_line(aes(y=tpr)) + ggtitle(paste0("ROC Curve w/ AUC=", auc))





#Building Decision Tree Model
formula_R<-as.formula(target~ softwareindpolarity+hardwareindpolarity+carrierindpolarity+LengthIndex)
TreeModelR<- rpart(formula_R, data =train)
prp(TreeModelR, branch = 1,extra = 1, varlen = 0, faclen = 10, box.col = 3,round=0,digits = 4)
TreeModel_predR<-predict(TreeModelR,test)
TreeModelR$variable.importance
pred_classR <- ifelse(TreeModel_predR>=0.30, 1,0)
table(pred_classR,test$target)
1-sum(pred_classR!=test$target)/nrow(test)


##Code to plot the ROC curve
pred<-prediction(TreeModel_predR,test$target)
pref<-performance(pred, measure = "tpr", x.measure = "fpr")
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
roc.data <- data.frame(fpr=unlist(pref@x.values), tpr=unlist(pref@y.values), model="GLM")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) + geom_ribbon(alpha=0.2) +  
  geom_line(aes(y=tpr)) + ggtitle(paste0("ROC Curve w/ AUC=", auc))



#Building the SVM Model
svmmodel<- svm(target~ softwareindpolarity+hardwareindpolarity+carrierindpolarity+LengthIndex,data = train)
SvmModel_pred <- predict(svmmodel, test)
pred_class <- ifelse(SvmModel_pred>=0.4, 1,0)
#computing the accuracy
table(pred_class,test$target)
print(paste("Accuracy usind validation method is: ", 1-sum(pred_class!=test$target)/nrow(test)))
summary(svmmodel)

#ROC curve for SVM model
pred<-prediction(SvmModel_pred,test$target)
pref<-performance(pred, measure = "tpr", x.measure = "fpr")
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
roc.data <- data.frame(fpr=unlist(pref@x.values), tpr=unlist(pref@y.values), model="GLM")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) + geom_ribbon(alpha=0.2) +  
  geom_line(aes(y=tpr)) + ggtitle(paste0("ROC Curve w/ AUC=", auc))









#Opinion spamming - Data processing

#Finging mid-popular products for opinion spamming. Breaking no. of reviews in percentiles and then  selecting only 20-80%ile
midpopproducts_mob<-count(dt_mobv2,Product)
ApplyQuintiles <- function(x) {
  cut(x, breaks=c(quantile(midpopproducts_mob$n, probs = seq(0, 1, by = 0.20))), 
      labels=c("0-20","20-40","40-60","60-80","80-100"))
}


midpopproducts_mob$Quintile <- sapply(midpopproducts_mob$n, ApplyQuintiles)


#Merging quantile mapping data with the main dataframe and filtering out required products
dt_mob_quant<-as.data.table(merge(dt_mobv2,midpopproducts_mob,by=intersect(names(dt_mobv2)<-c("Author","ReviewID","Content","Overall","Date","Product"),midpopproducts_mob=c("Quintile")),by.x=c("Product"),by.y=c("Product"),all.dt=TRUE))
dt_mob_quant$Quintile<-as.character(dt_mob_quant$Quintile)
dt_mob_quant_filtered<-as.data.table(filter(dt_mob_quant,Quintile %in% c("20-40","40-60","60-80")))

#Calculating length of the review
dt_mob_quant_filtered<-cbind(dt_mob_quant_filtered,sapply(gregexpr("\\S+",dt_mob_quant_filtered$Content),length))
View(dt_mob_quant_filtered)
colnames(dt_mob_quant_filtered)

#Total reviews on that particular product

names(dt_mob_quant_filtered)[9]<- "No_of_prodreviews"
#No. of words in the review
names(dt_mob_quant_filtered)[11]<- "Length"

#Adding product description 
dt_mob_quant_filtered<-merge(dt_mob_quant_filtered,prod_mob,by.x = c("Product"),by.y = c("Product"))
colnames(dt_mob_quant_filtered)

#Total author reviews in the mid-range category on Amazon
dt_mob_quant_filteredauthor<-count(dt_mob_quant_filtered,Author)
dt_mob_quant_filtered<-merge(dt_mob_quant_filtered,dt_mob_quant_filteredauthor,by.x=c("Author"),by.y=c("Author"))


colnames(dt_mob_quant_filtered)
names(dt_mob_quant_filtered)[13]<-"Total author reviews"
#Filtering out records where rating given is 1 or 5

dt_mob_quant_filteredv1<-filter(dt_mob_quant_filtered,Overall %in% c("1.0","5.0"))
dt_mob_quant_filteredv1<-filter(dt_mob_quant_filteredv1,Author %in% c(
                            
                            "AJ",
                            "Albert B",
                            "Craig",
                            "Nataliya",
                            "rpv",
                            "Shawn C.",
                            "Yehezkel Yosef",
                            "Alex",
                            "Tessy",
                            "Walter",
                            
                            "andy",
                            "Anonymous",
                            "Carlos",
                            "Christina",
                            "christine",
                            "Cindy",
                            "coco",
                            "Courtney",
                            
                            "Dan",
                            "Dana",
                            "Daniel",
                            "Dave",
                            "david",
                            "DEALHUNTER",
                            "Robert J. Allen"
                            
                            
                          ))      
#Filtering out only samsung products
dt_mob_quant_filteredv2<-filter(dt_mob_quant_filteredv1,Product %in% c("B0002VQDVM",
                                                                       "B000FDVZ2E",
                                                                       "B000ITMSLW",
                                                                       "B000VXIHH4",
                                                                       "B000Y19ZQ0",
                                                                       "B0014DEMSS",
                                                                       "B001897ASC",
                                                                       "B001ANXPOY",
                                                                       "B001DYTVN4",
                                                                       "B001DZG63G",
                                                                       "B001LOGEHM",
                                                                       "B001M5PSR2",
                                                                       "B0026ZI8TC",
                                                                       "B0029ZAF2M",
                                                                       "B002ACONNQ",
                                                                       "B002AS9WEA",
                                                                       "B002ED82PG",
                                                                       "B002EOXKGQ",
                                                                       "B002I9Z4WK",
                                                                       "B002JXP9WA",
                                                                       "B002L152GI",
                                                                       "B002LSHZPW",
                                                                       "B002QMGCMA",
                                                                       "B002RS4GAI",
                                                                       "B002TUTVTA",
                                                                       "B002YIH1IU",
                                                                       "B0031OYCJM",
                                                                       "B0033SFV5A",
                                                                       "B0033SLS0C",
                                                                       "B0036X8EMY",
                                                                       "B003BNY4JG",
                                                                       "B003D3NDCI",
                                                                       "B003D7HW7G",
                                                                       "B003FMVW34",
                                                                       "B003L76N0U",
                                                                       "B003NRZBKQ",
                                                                       "B003QHY0ZU",
                                                                       "B003UXA1GW",
                                                                       "B003WP4SO4",
                                                                       "B003Y297YG",
                                                                       "B003Y2D6WU",
                                                                       "B0041A7QPM",
                                                                       "B0042TT3LM",
                                                                       "B00433SZTI",
                                                                       "B00435DXOS",
                                                                       "B0043V00L6",
                                                                       "B0043VTBD4",
                                                                       "B00452V7V0",
                                                                       "B00456T4X4",
                                                                       "B0045JCCTY",
                                                                       "B0046REOWS",
                                                                       "B00481XZDQ",
                                                                       "B0049UHQQ8",
                                                                       "B004AKOVIS",
                                                                       "B004D37XB4",
                                                                       "B004DKEE46",
                                                                       "B004EBPB00",
                                                                       "B004ESEZKA",
                                                                       "B004F44QO8",
                                                                       "B004FFEBZQ",
                                                                       "B004GJW0HC",
                                                                       "B004GXOTJ0",
                                                                       "B004H1N8IY",
                                                                       "B004HFRFMK",
                                                                       "B004HI6GOU",
                                                                       "B004HXF1TQ",
                                                                       "B004I0JUSG",
                                                                       "B004KEOSIM",
                                                                       "B004MN9MZK",
                                                                       "B004NG6J48",
                                                                       "B004O4UAY4",
                                                                       "B004OQDXRS",
                                                                       "B004QGXX3A",
                                                                       "B004QIYT7M",
                                                                       "B004UBCNSC",
                                                                       "B004UI23QM",
                                                                       "B004UVR9A4",
                                                                       "B004V92C48",
                                                                       "B004VRJ036",
                                                                       "B004W0RZCU",
                                                                       "B004W7I8XS",
                                                                       "B004WOJP4C",
                                                                       "B004XRT2DW",
                                                                       "B004Y0276M",
                                                                       "B004Z1V0WW",
                                                                       "B004ZGH062",
                                                                       "B0051I5I3K",
                                                                       "B0051I6FUU",
                                                                       "B00526RCLC",
                                                                       "B0052N6HMK",
                                                                       "B0053GQO1U",
                                                                       "B0055DSUZY",
                                                                       "B0056ELM3O",
                                                                       "B0056JBKCW",
                                                                       "B00579ONO8",
                                                                       "B0058PN9HI",
                                                                       "B005B3OXWC",
                                                                       "B005BTUG2M",
                                                                       "B005D5IX42",
                                                                       "B005EU077W",
                                                                       "B005H98M36",
                                                                       "B005IDBJG8",
                                                                       "B005IOMSA8",
                                                                       "B005NK75D2",
                                                                       "B005NKC1WW",
                                                                       "B005O756WQ",
                                                                       "B005P3UP8O",
                                                                       "B005Q3ML3U",
                                                                       "B005VIWMCU",
                                                                       "B0060BD93I",
                                                                       "B0060PGI4G",
                                                                       "B00635LYJW",
                                                                       "B0063GMSY6",
                                                                       "B00652MW9E",
                                                                       "B0065UI6AU",
                                                                       "B0067PLZ1U",
                                                                       "B006C58HHK",
                                                                       "B006FA1YG8",
                                                                       "B006IF23YM",
                                                                       "B006J8N9RI",
                                                                       "B006L9G20Q",
                                                                       "B006LRNPZS",
                                                                       "B006VO8ZAQ",
                                                                       "B006XZYTEY",
                                                                       "B006YG7ZMU",
                                                                       "B006ZPD934",
                                                                       "B0072YKDLI",
                                                                       "B0073T4I6I",
                                                                       "B0074SJON0",
                                                                       "B00766BG6S",
                                                                       "B00771CRYC",
                                                                       "B007AHRXUQ",
                                                                       "B007ANY8FS",
                                                                       "B007AODX7Q",
                                                                       "B007COSPJ0",
                                                                       "B007EUSIPI",
                                                                       "B007IK024O",
                                                                       "B007KCW1G2",
                                                                       "B007LU0HU0",
                                                                       "B007N0RAXK",
                                                                       "B007NKUI7U",
                                                                       "B007RQIV42",
                                                                       "B007RZ7CY8",
                                                                       "B007V61WIA",
                                                                       "B007VFSCVG",
                                                                       "B007VQPM4A",
                                                                       "B007W66B08",
                                                                       "B007XWA0JE",
                                                                       "B007Z0OEJG",
                                                                       "B007Z0OEK0",
                                                                       "B007Z0OEP0",
                                                                       "B007Z4XRRC",
                                                                       "B0080GQ30M",
                                                                       "B0081YWKE6",
                                                                       "B008277H40",
                                                                       "B0083OWRWE",
                                                                       "B008475498",
                                                                       "B0084T4D34",
                                                                       "B008AFZ2J6",
                                                                       "B008AFZ2VO",
                                                                       "B008DVFJOK",
                                                                       "B008GX64JY",
                                                                       "B008KA79RE",
                                                                       "B008LQ7DAK",
                                                                       "B008LXDCMQ",
                                                                       "B008MN7JZG",
                                                                       "B008N0GKDK",
                                                                       "B008RKNM5U",
                                                                       "B008XLWDEY",
                                                                       "B008ZE8MMQ",
                                                                       "B0092GJNII",
                                                                       "B0096DWZ82",
                                                                       "B0096DYA8U",
                                                                       "B009FLR18Y",
                                                                       "B009GEORSM",
                                                                       "B009IJE0SM",
                                                                       "B009IJE0SW",
                                                                       "B009IJE31G",
                                                                       "B009ISZQUY",
                                                                       "B009LDPWKU",
                                                                       "B009PJHAS2",
                                                                       "B009SAID7U",
                                                                       "B009VL8G32",
                                                                       "B00A3UOXDC",
                                                                       "B00A9Z4CZU",
                                                                       "B00AAOV9E2",
                                                                       "B00AAPWQKM",
                                                                       "B00AC03P6Y",
                                                                       "B00AH0OLV2",
                                                                       "B00AO83XAW",
                                                                       "B00AQBDBIQ",
                                                                       "B00ATDT83I",
                                                                       "B00ATH2110",
                                                                       "B00B06LP7K",
                                                                       "B00B090QRM",
                                                                       "B00B1TNF7E",
                                                                       "B00B37V3SC",
                                                                       "B00B3HX5Y2",
                                                                       "B00B3I15UM",
                                                                       "B00B43VY84",
                                                                       "B00B472RVS",
                                                                       "B00B5081MI",
                                                                       "B00B7CRR4W",
                                                                       "B00BBZ3ALY",
                                                                       "B00BC5IKEA",
                                                                       "B00BC63TRM",
                                                                       "B00BFY25NU",
                                                                       "B00BK08J7K",
                                                                       "B00BPGZGES",
                                                                       "B00C3JMSRE",
                                                                       "B00C3LTLH2",
                                                                       "B00C9DEYFS",
                                                                       "B00CBMMWVU",
                                                                       "B00CGIULGC",
                                                                       "B00CLMGKKY",
                                                                       "B00CLRM68Y",
                                                                       "B00CNVOM4O",
                                                                       "B00CO2RENI",
                                                                       "B00CPRZWB8",
                                                                       "B00CRNTDII",
                                                                       "B00CRO6QFA",
                                                                       "B00CSYD4M2",
                                                                       "B00CY6U990",
                                                                       "B00D0KVPXI",
                                                                       "B00D359X16",
                                                                       "B00D3LI8OI",
                                                                       "B00D7ZMSZ0",
                                                                       "B00D8T9QZU",
                                                                       "B00D93LOY6",
                                                                       "B00DELEBY8",
                                                                       "B00DIHX0MW",
                                                                       "B00DJ338U4",
                                                                       "B00DSR62KY",
                                                                       "B00DV55U06",
                                                                       "B00DV9LJG6",
                                                                       "B00E0OH4QA",
                                                                       "B00E5WISFS",
                                                                       "B00E6FGHAW",
                                                                       "B00E6FHHY2",
                                                                       "B00E6JQ4OW",
                                                                       "B00E8S8LRE",
                                                                       "B00EAYWESI",
                                                                       "B00ED3T0TM",
                                                                       "B00EKSG4R6",
                                                                       "B00EKXDL0E",
                                                                       "B00ELUTN8U",
                                                                       "B00EMKKW5W",
                                                                       "B00EP2BN00",
                                                                       "B00EPCQN4G",
                                                                       "B00EV54ZZQ",
                                                                       "B00EW1MUII",
                                                                       "B00EYM3EXK",
                                                                       "B00F72JW8M",
                                                                       "B00F9S90RW",
                                                                       "B00FFG45T6",
                                                                       "B00FZJDPLC",
                                                                       "B00G197Q4M",
                                                                       "B00G9FVHMK",
                                                                       "B00G9FVM0C",
                                                                       "B00G9G0OP0",
                                                                       "B00GNK4R8M",
                                                                       "B00GO9IBWA",
                                                                       "B00GQ4QQOI",
                                                                       "B00HWB39IA",
                                                                       "B00HWC05AY",
                                                                       "B00HWEK39K",
                                                                       "B00HX0M7SS",
                                                                       "B00I0ECBG4",
                                                                       "B00INEIZN4",
                                                                       "B00IZ1WUNG",
                                                                       "B00IZ1XA94",
                                                                       "B00IZ1XJ3Q",
                                                                       "B00IZ1Y2XM",
                                                                       "B00J4TK4B8",
                                                                       "B00J4TK4CC",
                                                                       "B00JB6AJDC",
                                                                       "B00JFNDLRC","B0002VQDVM",
                                                                       "B000FDVZ2E",
                                                                       "B000ITMSLW",
                                                                       "B000VXIHH4",
                                                                       "B000Y19ZQ0",
                                                                       "B0014DEMSS",
                                                                       "B001897ASC",
                                                                       "B001ANXPOY",
                                                                       "B001DYTVN4",
                                                                       "B001DZG63G",
                                                                       "B001LOGEHM",
                                                                       "B001M5PSR2",
                                                                       "B0026ZI8TC",
                                                                       "B0029ZAF2M",
                                                                       "B002ACONNQ",
                                                                       "B002AS9WEA",
                                                                       "B002ED82PG",
                                                                       "B002EOXKGQ",
                                                                       "B002I9Z4WK",
                                                                       "B002JXP9WA",
                                                                       "B002L152GI",
                                                                       "B002LSHZPW",
                                                                       "B002QMGCMA",
                                                                       "B002RS4GAI",
                                                                       "B002TUTVTA",
                                                                       "B002YIH1IU",
                                                                       "B0031OYCJM",
                                                                       "B0033SFV5A",
                                                                       "B0033SLS0C",
                                                                       "B0036X8EMY",
                                                                       "B003BNY4JG",
                                                                       "B003D3NDCI",
                                                                       "B003D7HW7G",
                                                                       "B003FMVW34",
                                                                       "B003L76N0U",
                                                                       "B003NRZBKQ",
                                                                       "B003QHY0ZU",
                                                                       "B003UXA1GW",
                                                                       "B003WP4SO4",
                                                                       "B003Y297YG",
                                                                       "B003Y2D6WU",
                                                                       "B0041A7QPM",
                                                                       "B0042TT3LM",
                                                                       "B00433SZTI",
                                                                       "B00435DXOS",
                                                                       "B0043V00L6",
                                                                       "B0043VTBD4",
                                                                       "B00452V7V0",
                                                                       "B00456T4X4",
                                                                       "B0045JCCTY",
                                                                       "B0046REOWS",
                                                                       "B00481XZDQ",
                                                                       "B0049UHQQ8",
                                                                       "B004AKOVIS",
                                                                       "B004D37XB4",
                                                                       "B004DKEE46",
                                                                       "B004EBPB00",
                                                                       "B004ESEZKA",
                                                                       "B004F44QO8",
                                                                       "B004FFEBZQ",
                                                                       "B004GJW0HC",
                                                                       "B004GXOTJ0",
                                                                       "B004H1N8IY",
                                                                       "B004HFRFMK",
                                                                       "B004HI6GOU",
                                                                       "B004HXF1TQ",
                                                                       "B004I0JUSG",
                                                                       "B004KEOSIM",
                                                                       "B004MN9MZK",
                                                                       "B004NG6J48",
                                                                       "B004O4UAY4",
                                                                       "B004OQDXRS",
                                                                       "B004QGXX3A",
                                                                       "B004QIYT7M",
                                                                       "B004UBCNSC",
                                                                       "B004UI23QM",
                                                                       "B004UVR9A4",
                                                                       "B004V92C48",
                                                                       "B004VRJ036",
                                                                       "B004W0RZCU",
                                                                       "B004W7I8XS",
                                                                       "B004WOJP4C",
                                                                       "B004XRT2DW",
                                                                       "B004Y0276M",
                                                                       "B004Z1V0WW",
                                                                       "B004ZGH062",
                                                                       "B0051I5I3K",
                                                                       "B0051I6FUU",
                                                                       "B00526RCLC",
                                                                       "B0052N6HMK",
                                                                       "B0053GQO1U",
                                                                       "B0055DSUZY",
                                                                       "B0056ELM3O",
                                                                       "B0056JBKCW",
                                                                       "B00579ONO8",
                                                                       "B0058PN9HI",
                                                                       "B005B3OXWC",
                                                                       "B005BTUG2M",
                                                                       "B005D5IX42",
                                                                       "B005EU077W",
                                                                       "B005H98M36",
                                                                       "B005IDBJG8",
                                                                       "B005IOMSA8",
                                                                       "B005NK75D2",
                                                                       "B005NKC1WW",
                                                                       "B005O756WQ",
                                                                       "B005P3UP8O",
                                                                       "B005Q3ML3U",
                                                                       "B005VIWMCU",
                                                                       "B0060BD93I",
                                                                       "B0060PGI4G",
                                                                       "B00635LYJW",
                                                                       "B0063GMSY6",
                                                                       "B00652MW9E",
                                                                       "B0065UI6AU",
                                                                       "B0067PLZ1U",
                                                                       "B006C58HHK",
                                                                       "B006FA1YG8",
                                                                       "B006IF23YM",
                                                                       "B006J8N9RI",
                                                                       "B006L9G20Q",
                                                                       "B006LRNPZS",
                                                                       "B006VO8ZAQ",
                                                                       "B006XZYTEY",
                                                                       "B006YG7ZMU",
                                                                       "B006ZPD934",
                                                                       "B0072YKDLI",
                                                                       "B0073T4I6I",
                                                                       "B0074SJON0",
                                                                       "B00766BG6S",
                                                                       "B00771CRYC",
                                                                       "B007AHRXUQ",
                                                                       "B007ANY8FS",
                                                                       "B007AODX7Q",
                                                                       "B007COSPJ0",
                                                                       "B007EUSIPI",
                                                                       "B007IK024O",
                                                                       "B007KCW1G2",
                                                                       "B007LU0HU0",
                                                                       "B007N0RAXK",
                                                                       "B007NKUI7U",
                                                                       "B007RQIV42",
                                                                       "B007RZ7CY8",
                                                                       "B007V61WIA",
                                                                       "B007VFSCVG",
                                                                       "B007VQPM4A",
                                                                       "B007W66B08",
                                                                       "B007XWA0JE",
                                                                       "B007Z0OEJG",
                                                                       "B007Z0OEK0",
                                                                       "B007Z0OEP0",
                                                                       "B007Z4XRRC",
                                                                       "B0080GQ30M",
                                                                       "B0081YWKE6",
                                                                       "B008277H40",
                                                                       "B0083OWRWE",
                                                                       "B008475498",
                                                                       "B0084T4D34",
                                                                       "B008AFZ2J6",
                                                                       "B008AFZ2VO",
                                                                       "B008DVFJOK",
                                                                       "B008GX64JY",
                                                                       "B008KA79RE",
                                                                       "B008LQ7DAK",
                                                                       "B008LXDCMQ",
                                                                       "B008MN7JZG",
                                                                       "B008N0GKDK",
                                                                       "B008RKNM5U",
                                                                       "B008XLWDEY",
                                                                       "B008ZE8MMQ",
                                                                       "B0092GJNII",
                                                                       "B0096DWZ82",
                                                                       "B0096DYA8U",
                                                                       "B009FLR18Y",
                                                                       "B009GEORSM",
                                                                       "B009IJE0SM",
                                                                       "B009IJE0SW",
                                                                       "B009IJE31G",
                                                                       "B009ISZQUY",
                                                                       "B009LDPWKU",
                                                                       "B009PJHAS2",
                                                                       "B009SAID7U",
                                                                       "B009VL8G32",
                                                                       "B00A3UOXDC",
                                                                       "B00A9Z4CZU",
                                                                       "B00AAOV9E2",
                                                                       "B00AAPWQKM",
                                                                       "B00AC03P6Y",
                                                                       "B00AH0OLV2",
                                                                       "B00AO83XAW",
                                                                       "B00AQBDBIQ",
                                                                       "B00ATDT83I",
                                                                       "B00ATH2110",
                                                                       "B00B06LP7K",
                                                                       "B00B090QRM",
                                                                       "B00B1TNF7E",
                                                                       "B00B37V3SC",
                                                                       "B00B3HX5Y2",
                                                                       "B00B3I15UM",
                                                                       "B00B43VY84",
                                                                       "B00B472RVS",
                                                                       "B00B5081MI",
                                                                       "B00B7CRR4W",
                                                                       "B00BBZ3ALY",
                                                                       "B00BC5IKEA",
                                                                       "B00BC63TRM",
                                                                       "B00BFY25NU",
                                                                       "B00BK08J7K",
                                                                       "B00BPGZGES",
                                                                       "B00C3JMSRE",
                                                                       "B00C3LTLH2",
                                                                       "B00C9DEYFS",
                                                                       "B00CBMMWVU",
                                                                       "B00CGIULGC",
                                                                       "B00CLMGKKY",
                                                                       "B00CLRM68Y",
                                                                       "B00CNVOM4O",
                                                                       "B00CO2RENI",
                                                                       "B00CPRZWB8",
                                                                       "B00CRNTDII",
                                                                       "B00CRO6QFA",
                                                                       "B00CSYD4M2",
                                                                       "B00CY6U990",
                                                                       "B00D0KVPXI",
                                                                       "B00D359X16",
                                                                       "B00D3LI8OI",
                                                                       "B00D7ZMSZ0",
                                                                       "B00D8T9QZU",
                                                                       "B00D93LOY6",
                                                                       "B00DELEBY8",
                                                                       "B00DIHX0MW",
                                                                       "B00DJ338U4",
                                                                       "B00DSR62KY",
                                                                       "B00DV55U06",
                                                                       "B00DV9LJG6",
                                                                       "B00E0OH4QA",
                                                                       "B00E5WISFS",
                                                                       "B00E6FGHAW",
                                                                       "B00E6FHHY2",
                                                                       "B00E6JQ4OW",
                                                                       "B00E8S8LRE",
                                                                       "B00EAYWESI",
                                                                       "B00ED3T0TM",
                                                                       "B00EKSG4R6",
                                                                       "B00EKXDL0E",
                                                                       "B00ELUTN8U",
                                                                       "B00EMKKW5W",
                                                                       "B00EP2BN00",
                                                                       "B00EPCQN4G",
                                                                       "B00EV54ZZQ",
                                                                       "B00EW1MUII",
                                                                       "B00EYM3EXK",
                                                                       "B00F72JW8M",
                                                                       "B00F9S90RW",
                                                                       "B00FFG45T6",
                                                                       "B00FZJDPLC",
                                                                       "B00G197Q4M",
                                                                       "B00G9FVHMK",
                                                                       "B00G9FVM0C",
                                                                       "B00G9G0OP0",
                                                                       "B00GNK4R8M",
                                                                       "B00GO9IBWA",
                                                                       "B00GQ4QQOI",
                                                                       "B00HWB39IA",
                                                                       "B00HWC05AY",
                                                                       "B00HWEK39K",
                                                                       "B00HX0M7SS",
                                                                       "B00I0ECBG4",
                                                                       "B00INEIZN4",
                                                                       "B00IZ1WUNG",
                                                                       "B00IZ1XA94",
                                                                       "B00IZ1XJ3Q",
                                                                       "B00IZ1Y2XM",
                                                                       "B00J4TK4B8",
                                                                       "B00J4TK4CC",
                                                                       "B00JB6AJDC",
                                                                       "B00JFNDLRC"))




                                

dt_mob_quant_filteredv2<-arrange(dt_mob_quant_filteredv2,Author)

View(dt_mob_quant_filteredv2)
install.packages("lsa")
library(lsa)
install.packages("tm")
library(tm)
install.packages("qdap")
library(qdap)
# To check the list of stop words

stop_word = c(stopwords("SMART")) 

length(stop_word)                                  

lsa_mobv1<-as.data.frame(select(dt_mob_quant_filteredv2,Content))

lsa_doc<-Corpus(DataframeSource(lsa_mobv1))


# To remove punctuations on the text data
lsa_doc = tm_map(lsa_doc, removePunctuation)   
# To convert all the text data into lower case
lsa_doc = tm_map(lsa_doc, tolower)             
# To remove all the numerical data present in text
lsa_doc = tm_map(lsa_doc, removeNumbers)       
# To remove all the numerical data present in text
lsa_doc = tm_map(lsa_doc,removeWords,stop_word)        
# To remove the white spaces
lsa_doc = tm_map(lsa_doc, stripWhitespace)     
# This stems the document
lsa_doc= tm_map(lsa_doc, stemDocument)        
# Converts the document into plain text document

lsa_doc = tm_map(lsa_doc, PlainTextDocument) 


#word frequency dataframe for term doc matrix
lsamat<-as.wfm(TermDocumentMatrix(lsa_doc))

td.mat.lsa <- lw_bintf(lsamat) * gw_idf(lsamat)  # weighting
lsaSpace <- lsa(td.mat.lsa)  # create LSA space
dist.mat.lsa <- dist(t(as.textmatrix(lsaSpace)))  # compute distance matrix
dist.mat.lsa<-as.matrix(dist.mat.lsa) 


write.csv(dist.mat.lsa,file="distance_MDS.csv")
write.csv(dt_mob_quant_filteredv2,file="Fishy_nonfishy_analysis.csv")







