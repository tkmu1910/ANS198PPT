#Title : Superstore Data Visualization
#Frances Campbell, Minyong Jung, Taekyung Kim
#Data: Kaggle Superstore Data
#Mostly Categorical, Project Key is Visualizing Data to understand 4 year
#Goal: Become Rich

#Restart R without Saving if Packages Crash
########################### All
getwd()
setwd("~/Desktop/ANS198")

install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("plotly")
install.packages("usmap")

library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(usmap)

list.files()
SuperstoreData= read.csv("Sample - Superstore.csv")

ncol(SuperstoreData)
nrow(SuperstoreData)
head(SuperstoreData)
summary(SuperstoreData)
colnames(SuperstoreData)
rownames(SuperstoreData)
###########################
#Find what type of things are sold well to concentrate on as a superstore perspective

###########################
#Sales of each Category
ggplot(data=SuperstoreData, aes(x = Category, y = Sales, fill = Category)) + 
  geom_bar(stat = "Summary") + 
  scale_fill_manual(values = c("#4F91AF" , "#bb7493", "#aae68f" )) +  
  ggtitle("Sales of each Category") 

#Sales of each Sub-Category
ggplot(data=SuperstoreData, aes(x = Sub.Category, y = Sales, fill = Category)) + 
  geom_bar(stat = "Summary") + 
  scale_fill_manual(values = c("#4F91AF" , "#bb7493", "#aae68f" )) +  
  ggtitle("Sales of each Sub-Category") + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1))

ggplot(data=SuperstoreData, aes(x = Sub.Category, y = Sales, fill = Category)) + 
  geom_bar(stat = "Identity") + 
  scale_fill_manual(values = c("#4F91AF" , "#bb7493", "#aae68f" )) +  
  ggtitle("Sales of each Sub-Category") + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1))
###########################


###########################
#VVIP Customer Benefit Possibility
CustomerSales = SuperstoreData %>%
  group_by(Customer.Name) %>%
  summarise(total= sum(Sales))

CustomerSales = CustomerSales[order(CustomerSales$total, decreasing = TRUE), ]

head(CustomerSales)

ggplot(head(CustomerSales), aes(x=Customer.Name, y=total)) +
  geom_bar(stat="identity") +
  labs(title = "6 Most Bought Customers", x = "Name",
       y = "Dollars")

#Figure out where on states are most sold and visualize the whole sale customers
Statesale = SuperstoreData %>%
  group_by(State) %>%
  summarise(total= sum(Sales))

#Map was cool, Data, US  
States_VS_Sales <- data.frame( state = c("Alabama", "Arizona", "Arkansas","California","Colorado","Connecticut","Delaware","District of Columbia","Florida","Georgia","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New york","North Carolina","North Dacota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming"),
                               Sales = c(19511,35282,11678,457688,32108,13384,27451,2865,89474,49096,4382,80166,53555,4579,2914,36592,9217,1271,23706,
                                         28634,76270,29863,10771,22205,5589,7465,16729,7293,35764,4784,310876,55603,920,78258,19683,17431,116512,
                                         22628,8482,1316,30662,170188,11220,8929,70637,138641,1209,32115,1603),
                               stringsAsFactors = FALSE )
plot_usmap(data = States_VS_Sales, values = "Sales", color = "blue") + 
  scale_fill_continuous(low = "yellow", high = "blue", name = "Range of Sales", label = scales::comma) + 
  labs(title = "US States", subtitle = "Amount of Sales in each state") +
  theme(legend.position = "right")
###########################


###########################
#Date Code
SuperstoreData = SuperstoreData %>%
  mutate(Order.Date = lubridate::mdy(Order.Date))

#Want to look and month and year Profit in one graph (Interactive)
MonthlyProfit = SuperstoreData %>%
  mutate(month = month(Order.Date), year = year(Order.Date)) %>%
  group_by(month,year) %>%
  summarise(total= sum(Profit))

MonthLabels = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
Profit2014 = MonthlyProfit$total[1:12]
Profit2015 = MonthlyProfit$total[13:24]
Profit2016 = MonthlyProfit$total[25:36]
Profit2017 = MonthlyProfit$total[37:48]
ProfitBar1417Data = data.frame(MonthLabels, Profit2014, Profit2015, Profit2016, Profit2017)
ProfitBar1417Data$MonthLabels = factor(ProfitBar1417Data$MonthLabels, levels=ProfitBar1417Data[["MonthLabels"]])
ProfitBar1417G = plot_ly(ProfitBar1417Data, x = ~MonthLabels, y = ~Profit2014, type = 'bar', name = '2014 Profit', marker = list(color = 'rgb(50,140,190)'))
ProfitBar1417G = ProfitBar1417G %>% add_trace(y = ~Profit2015, name = '2015 Profit', marker = list(color = 'rgb(195,195,195)'))
ProfitBar1417G = ProfitBar1417G %>% add_trace(y = ~Profit2016, name = '2016 Profit', marker = list(color = 'rgb(255,0,0)'))
ProfitBar1417G = ProfitBar1417G %>% add_trace(y = ~Profit2017, name = '2017 Profit', marker = list(color = 'rgb(0,165,255)'))
ProfitBar1417G = ProfitBar1417G %>% layout(xaxis = list(title = "Months", tickangle = -45),
                                           yaxis = list(title = "Profit Dollar"),
                                           margin = list(b = 100),
                                           barmode = 'group')
ProfitBar1417G

#Monthly Sales Data
MonthlySales = SuperstoreData %>%
  mutate(month = month(Order.Date), year = year(Order.Date)) %>%
  group_by(year,month) %>%
  summarise(total= sum(Sales))

#Yearly Trend Sales
Sales2014 = MonthlySales$total[1:12]
Sales2015 = MonthlySales$total[13:24]
Sales2016 = MonthlySales$total[25:36]
Sales2017 = MonthlySales$total[37:48]
SalesBar1417Data = data.frame(MonthLabels, Sales2014, Sales2015, Sales2016, Sales2017)
SalesBar1417Data$MonthLabels = factor(SalesBar1417Data$MonthLabels, levels=SalesBar1417Data[["MonthLabels"]])
SalesBar1417G = plot_ly(SalesBar1417Data, x = ~MonthLabels, y = ~Sales2014, type = 'bar', name = '2014 Sales', marker = list(color = 'rgb(50,140,190)'))
SalesBar1417G = SalesBar1417G %>% add_trace(y = ~Sales2015, name = '2015 Sales', marker = list(color = 'rgb(195,195,195)'))
SalesBar1417G = SalesBar1417G %>% add_trace(y = ~Sales2016, name = '2016 Sales', marker = list(color = 'rgb(255,0,0)'))
SalesBar1417G = SalesBar1417G %>% add_trace(y = ~Sales2017, name = '2017 Sales', marker = list(color = 'rgb(0,165,255)'))
SalesBar1417G = SalesBar1417G %>% layout(xaxis = list(title = "Months", tickangle = -45),
                                         yaxis = list(title = "Sales Dollar"),
                                         margin = list(b = 100),
                                         barmode = 'group')
SalesBar1417G






