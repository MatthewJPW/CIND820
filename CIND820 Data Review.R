#Importing the dataset into R
salesdata <- read.csv(file.choose())
salesdata

#Min and max order dates
min(salesdata$QUANTITYORDERED)
max(salesdata$QUANTITYORDERED)

#Min and max of sales sold
min(salesdata$SALES)
max(salesdata$SALES)

#Min and max of sales sold
min(salesdata$PRICEEACH)
max(salesdata$PRICEEACH)

#first 6 rows of dataset
head(salesdata)

#last 6 rows of dataset
tail(salesdata) 

#Seeing unique products the company is selling
unique(salesdata$PRODUCTLINE)
unique(salesdata$ORDERDATE)

#Number of rows and columns
dim(salesdata)

#column names
colnames(salesdata)

#checking for sum of NA
sum(is.na(salesdata))

sum(is.null(salesdata))

#creating backup dataset
salesdata2 <-salesdata

#Removing timestamps from orderdate column
salesdata2$ORDERDATE <- gsub(" .*","",salesdata2$ORDERDATE)

#Seeing the characterisitcs of all the columns in the dataset
sapply(salesdata2, class)

#seeing the number of values in each row
as.data.frame(table(salesdata2$ORDERDATE))
as.data.frame(table(salesdata2$PRODUCTLINE))

#Changing the orderdate class to "Date
library(lubridate)

salesdata2$ORDERDATE <- mdy(salesdata2$ORDERDATE)

class(salesdata2$ORDERDATE)

#Order data by oderdate
salesdata2 <- salesdata2[order(salesdata2$ORDERDATE),]

#Order data by month
salesdata2 <- salesdata2[order(salesdata2$MONTH_ID),]

#Plotting sales by year
plot(salesdata$YEAR_ID,salesdata$SALES)

#plotting sales by order date
plot(salesdata2$ORDERDATE, salesdata2$SALES)

#plotting sales by order dat
plot(x = salesdata2$ORDERDATE, y = salesdata2$SALES, type = "l", main = "type = 'l'")

plot(x = salesdata2$MONTH_ID, y = salesdata2$SALES, type = "l", main = "type = 'l'")

#Plotting with ggplot
library(ggplot2)

ggplot(salesdata2,aes(x = salesdata2$ORDERDATE, y = salesdata2$SALES))+geom_point()+geom_smooth()


#Simple linear regression model (Models 1 - 4)

#Predicting sales by using price sold of each product
model_1 <- lm(salesdata2$SALES ~ salesdata2$PRICEEACH)
summary(model_1)

#Predicting sales useing MSRp
model_2 <- lm(salesdata2$SALES ~ salesdata2$MSRP)
summary(model_2)

#predicting Price sold using MSRP
model_3 <- lm(salesdata2$PRICEEACH ~ salesdata2$MSRP)
summary(model_3) 

#Predicting Manufacturing selling price recommendation using price sold
model_4 <- lm(salesdata2$MSRP ~ salesdata2$PRICEEACH)
summary(model_4)

#Multiple Linear Regression
Multi_Model_1 <- lm(salesdata2$SALES ~ salesdata2$PRICEEACH + salesdata2$MSRP)
summary(Multi_Model_1)

#Pearson correlation between Priceeach & MSRP
cor(salesdata2$PRICEEACH, salesdata2$MSRP, method = "pearson")

#Pearson correlation between Priceeach & MSRP
cor(salesdata2$SALES, salesdata2$PRICEEACH, method = "pearson")

#Plotting the residuals of the model
plot(Multi_Model_1)

