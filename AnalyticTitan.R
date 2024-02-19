file_path<-"D:/myDOC/Global Superstore.csv"
data<-read.csv(file_path)
head(data)
library(codetools, lib.loc = "C:/Program Files/R/R-4.2.2/library")
data$Order.Date<-as.Date(data$Order.Date)
data$Order.Date
frequency<- table(data$Customer.ID)
frequency
customer_profile <- data.frame(Customer.ID = names(frequency), Frequency = as.vector(frequency) )
customer_profile

# Do high frequency customer contribute more revenue
sales_by_customer <- aggregate(Sales ~ Customer.ID, data = data, FUN = sum)
customer_data <- merge(customer_profile, sales_by_customer, by = "Customer.ID", all.x = TRUE)
# Calculating contribution to revenue
customer_data$Contribution <- customer_data$Sales / sum(customer_data$Sales)


# Calculating profit margin
data$ProfitMargin <- data$Profit/data$Sales


# Distribution of customers per country
customer_per_country <- table(data$Country)


# PRODUCT ANALYSIS
library(dplyr)
library(ggplot2)

# Profit Margin By Month using line chart to visualize

data$Month <- format(data$Order.Date, "%m")
profit_by_month<- aggregate(Profit ~ Month, data = data, FUN = sum)
plot(profit_by_month$Month, profit_by_month$Profit, type = "o", xlab = "Month", ylab = "Profit") + ylim(39000, 80000)
#Removing rows with missing values
clean_data <- na.omit(data)
#Fixing missing values with maximum values of the column
data$Month <- ifelse(is.na(data$Month), levels(data$Month)[which.max(table(data$Month))], data$Month)
ggplot(data = data, aes(x = Month, y = Profit)) + geom_line() + ylim(39000, 100000)

# Country with top sale

sales_by_country<- aggregate(Sales~Country, data = data, FUN = sum)
sales_by_country[which.max(sales_by_country$Sales), "Country"]

# Top 5profit-making products per year

data$Order.Date <- as.Date(data$Order.Date)
data$Year <- format(data$Order.Date, "%Y")
product_profit_yearly <- aggregate(Profit~Product.ID + Year, data = data, FUN = sum)
top_profit_products <- by(product_profit_yearly, product_profit_yearly$Year, function(x) head(x[order(-x$Profit),],5))

# How product prices vary with sales






