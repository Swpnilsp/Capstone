library(readxl)
library(tidyverse)
library(factoextra)
library(NbClust)
library(sqldf)
library(Matrix)
library(arules)
library(arulesViz)
library(recommenderlab)
setwd("~/Study/MS-Bana/Capstone")

retail<-read_xlsx('Online Retail.xlsx')
head(retail)

### EDA Part ###
str(retail)
colSums(is.na(retail))
dim(retail)
summary(retail)

### Quantity
summary(retail$Quantity)
hist(retail$Quantity)
plot.ecdf(retail$Quantity)

# Anything that has quantity greater than 10k and less than -10k should be removed
retail<-retail%>%
  filter(Quantity<10000,Quantity>-10000)
table(retail$Description)

## Unit Price
summary(retail$UnitPrice)
hist(retail$UnitPrice)
plot.ecdf(retail$UnitPrice)
#View(retail$UnitPrice)

retail[which(retail$UnitPrice==max(retail$UnitPrice)),]
as.data.frame(retail[which(retail$UnitPrice>10000),])

## Removing cancelled orders
retail<-retail[!startsWith(retail$InvoiceNo,'C'),]

### Date
ts<-retail%>%
  group_by(as.Date(InvoiceDate))%>%
  summarise(orders=n())
colnames(ts)<-c('Day','Orders')

ggplot(data = ts,aes(x = Day,y = Orders))+geom_line()

ts2<-retail%>%
  group_by(as.Date(InvoiceDate))%>%
  summarise(revenue=sum(Quantity*UnitPrice))

colnames(ts2)<-c('Day','Revenue')

ggplot(data = ts2,aes(x = Day,y = Revenue))+geom_line()
## Segmentation ##

## It has been observed that there are missing values in the customerID column. For the purpose of segmentation
# We will remove those observations. But we will consider those observations for MarketBasket Analysis.

### Test 556444

retail<-retail[-which(retail$InvoiceNo==556444),]
### End test
aggregated<-retail%>%
  filter(!is.na(CustomerID))%>%
  group_by(CustomerID)%>%
  summarise(frequency=n(),latest=max(InvoiceDate),monetory=mean(UnitPrice*Quantity))

aggregated<-as.data.frame(aggregated)
head(aggregated)

## we need to express recency in the number of days since the last purchase has been made. 
#The latest date is 9th Dec 2011. 
max(aggregated$latest)
# We can assume that this analysis was done in early 2012 and proceed accordingly.
today<-as.POSIXct("2012-01-02 00:00:00 UTC")
aggregated<-aggregated%>%
  mutate(recency=today-latest)
aggregated$latest<-NULL
aggregated$recency<-as.numeric(aggregated$recency)
head(aggregated)
summary(aggregated)

## there are observations with negative monetory value. These could be because of some errors.
# we can remove those observations

#aggregated<-aggregated%>%
 # filter(monetory>=0)

## for the purpose of analysis, we need to scale the variables
test<-scale(aggregated[,-1])

## We will use following methods to decide optimum number of clusters

# 1. Silhouette method
fviz_nbclust(test, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette Distance Method")

# 2. Elbow method
fviz_nbclust(test, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)+
  labs(subtitle = "Elbow Curve Method")


# 3. Gap statistic
set.seed(22334455)
fviz_nbclust(test, kmeans, nstart = 25,  method = "gap_stat", nboot =100)+
  labs(subtitle = "Gap Statistic Method")


## 2 out of the above three method suggest going for 4 clusters. We will create 4 clusters based on data
test<-aggregated
test$frequency<-scale(test$frequency)
test$recency<-scale(test$recency)
test$monetory<-scale(test$monetory)
head(test)

km<-kmeans(test[,-1],centers = 3,iter.max = 30)
test$segment<-km$cluster

aggregated<-sqldf('select aggregated.*,test.segment from aggregated inner join test
      on aggregated.CustomerID=test.CustomerID')

## Finding out features of the clusters
fviz_cluster(km, geom = "point", data = aggregated[,-c(1,5)]) + ggtitle("Three Clusters")

as.data.frame(aggregated%>%
  group_by(segment)%>%
  summarise(customers=n(),freq=mean(frequency),rec=mean(recency),money=mean(monetory)))



## Writing back in the original data frame
retail<-sqldf('select retail.*,test.segment from retail left join test
      on retail.CustomerID=test.CustomerID')

write.csv(retail,"VizTableau.csv")




#### Market Basket Analysis
head(retail)

## Any order that has been cancelled has Invoice Number starting from 'C'
# We will not consider those orders
test<-retail%>%group_by(InvoiceNo,StockCode)%>%summarise(Value=1)
test<-test[!startsWith(test$InvoiceNo,'C'),]
head(test)
test<-test%>%spread(StockCode,Value,fill = 0)
test<-as.data.frame(test)
head(test)
str(test)
rowSums(test[,-1])
colSums(is.na(test))

### Association Rules
test<-retail%>%group_by(InvoiceNo,Description)%>%summarise(Value=1)
test<-test[!startsWith(test$InvoiceNo,'C'),]
head(test)
test<-test%>%spread(Description,Value,fill = 0)
test<-as.data.frame(test)
head(test)
str(test)
rowSums(test[,-1])
colSums(is.na(test))

### Association Rules
Mat<-as.matrix(test[-1,-1])
dim(Mat)
class(Mat[2,3])
#buckets <- eclat (Mat[,-1], parameter = list(supp = 0.0015, minlen = 2)) 
#inspect(buckets)


### 9 rules 0.02 conf=0.7
s<-as(Mat,"transactions")
rules <- apriori(s, parameter = list(supp = 0.02,conf = 0.7))
plot(rules)
plot(rules, method="graph")
inspect(rules)


###### 3 Recommendations, Collaborative filtering ##
test<-retail%>%group_by(Country,StockCode)%>%summarise(Value=n())
head(test)
test<-test%>%spread(StockCode,Value,fill = 0)
test<-as.data.frame(test)
head(test)
str(test)
rowSumVector<-rowSums(test[,-1])
colSums(is.na(test))

## Now we divide the numbers in the data by rowsums. 

for(r in 1:nrow(test))
{
  for(c in 2:ncol(test))
  {
    test[r,c]<-test[r,c]/rowSumVector[r]
  }
}

for(r in 1:nrow(test))
{
  for(c in 1:ncol(test))
  {
    if(test[r,c]==0)
      {
      test[r,c]<-NA
    }
  }
}

head(test)
df<-test
## Converting to Matrix
test<-sapply(data.frame(test),as.numeric)
test[1:5,]
#test<-as.matrix(test)
#colnames(test)<-colnames(df)
#rownames(test)<-rownames(df)

testRating<- as(test, "realRatingMatrix")
colnames(testRating)<-colnames(df)
rownames(testRating)<-rownames(df)
image(testRating, main = "Raw Ratings")
## Finding out similar users
similarity_users <- similarity(testRating, method =  "cosine", which = "users") 
image(as.matrix(similarity_users), main = "User similarity")
set.seed(222)
scheme <- evaluationScheme(testRating, method="split", train = 0.9,
                           k=5, given=9, goodRating=0.00025)

algorithms <- list("Rnd" = list(name="RANDOM", param=NULL),
                   "Pop" = list(name="POPULAR"),
                   "UBCF_J" = list(name="UBCF",param = list(method = "jaccard")),
                   "UBCF_C" = list(name="UBCF",param = list(method = "cosine")),
                   "UBCF_P" = list(name="UBCF",param = list(method = "pearson")),
                   #"IBCF_J" = list(name="IBCF", pparam = list(method = "jaccard"))
                   #"IBCF_C" = list(name="IBCF", pparam = list(method = "cosine")),
                  #"IBCF_P" = list(name="IBCF", param = list(method = "pearson"))
                   "SVD" = list(name="SVD")
                   )

### Comparing the top recommendations ####
results1 <- evaluate(scheme, algorithms, type = "topNList",n=1:30)
#results1
plot(results1,legend="bottomright")
#plot(results1, "prec/rec", legend="bottomright")

results2 <- evaluate(scheme, algorithms, type = "ratings")
plot(results2,ylim = c(0,0.03),xlim = c(0,10),col=factor(names(results2)))


## we are going with popularity 
recoModelPop<-Recommender(testRating,method = "POPULAR")
popRecom <- predict(recoModelPop, testRating[11,],type="topNList")
popRecomMat<-as(popRecom,"matrix")
popRecoMat1<-t(popRecomMat)
colnames(popRecoMat1)<-df[,1][11]

popRecodf<-as.data.frame(popRecoMat1)
head(popRecodf)
View(popRecodf)
df[,1]
