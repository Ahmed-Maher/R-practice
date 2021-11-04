library(dplyr)
library(readr)
library(hablar)
library(tidyr)
library(ggplot2)
library(writexl)
# import the files and combine them into one data frame
df <- list.files(path = "Sales_Data", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 
head(df)

# change "Price Each" & "Quantity Ordered" type to numeric
df <- convert(df, dbl(`Price Each`), num(`Quantity Ordered`))

# add total_price col
df <- mutate(df, Total_Price = `Quantity Ordered` * `Price Each`)
head(df)

# remove NA values
df <- na.omit(df)

# add month col
df <- mutate(df, month = substr(`Order Date`, 1, 2))

# group Total_price for each months
Sales_per_month <- df %>%
  group_by(month) %>%
  summarise(Total_sales = sum(Total_Price))

# make a plot to show total sales per month
ggplot(data = Sales_per_month)+
  geom_col(mapping = aes(x= month, y= Total_sales), fill= "dark blue")+
  labs(title = "Sales per month",x= "Months", y= "Total sales")

# add city col
df1 <- separate(df, `Purchase Address`,into = c("street", "city", "state"), sep = ",")
df1 <- select(df1, c('city',"state"))
df1 <- separate(df1, "state", into = c("num","state"), sep = " ")
df1 <- separate(df1, "city", into = c("sp","city"),sep = " ",extra = "merge")
df1 <- select(df1, -c(num,sp))
df1$state<- with(df1, paste0("(",state,")"))
df1$City<- with(df1, paste(city, state, sep = " "))
df1 <- select(df1, City)
df$city <- df1$City

# group Total_price for each city
Sales_per_city <- df %>%
  group_by(city) %>%
  summarise(Total_sales = sum(Total_Price))
# make a plot to show total sales per City
ggplot(data = Sales_per_city)+
  geom_col(mapping = aes(x= city, y= Total_sales), fill= "dark blue")+
  labs(title = "Sales per city",x= "City", y= "Total sales")+
  theme(axis.text.x = element_text(angle=45, vjust = 1.1, hjust=1.15))

#Create a new workbook for outputs
wb <- list("annual_sales" = df, "Sales_per_month" = Sales_per_month, "Sales_per_city" = Sales_per_city)
write_xlsx(wb, "output\\sales_analysis.xlsx")