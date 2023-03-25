library(lubridate)
library(dplyr)
library(ggplot2)
library(scales)
library(RColorBrewer)

## Read csv file into RSudio
super_mkt <- read.csv("supermarket_sales.csv", stringsAsFactors = FALSE)
View(super_mkt)
             
#check type each column
str(super_mkt)
             
# check missing values in each column
is.na(super_mkt)

# get a summary of missing values for each column
summary(super_mkt)
             
# use sum() to count the number of duplicates in the dataframe
sum(duplicated(super_mkt))
             
# convert the Date column to a date format
super_mkt$Date <- mdy(super_mkt$Date)

# create a new column Month_Name with the month  from the Date column
super_mkt$Month_Name <- month(super_mkt$Date, label = TRUE)
                        
# change column names to lower
colnames(super_mkt) <- tolower(colnames(super_mkt))

# group by productline then sort average rating
group_prod <- super_mkt %>%
  group_by(product.line) %>%
  summarise(mean_rating = mean(rating)) %>%
  arrange(desc(mean_rating))
  
# plot
group_prod$mean_rating <- sprintf("%.2f", group_prod$mean_rating)
group_prod$mean_rating <- as.numeric(group_prod$mean_rating)
group_prod$line_num <- as.numeric(group_prod$product.line)

ggplot(group_prod, aes(x = reorder (product.line , mean_rating), y = mean_rating, fill = mean_rating)) +
  geom_bar(stat = "identity" ) +
  theme_bw() +
  geom_text(aes(label = mean_rating), nudge_x = 0.0, nudge_y = 0.3) + 
  ylim(0, 8) +
  labs(x = "Product Line", y = "Mean Rating", title = "Mean Rating by Product Line") +
  scale_fill_gradient(low = "cyan", high = "blue") +
  coord_flip()
  
 # group by productline then sort average sales
group_total <- super_mkt %>%
  group_by(product.line) %>%
  summarise(mean_sales = mean(total)) %>%
  arrange(desc(mean_sales))
  
  # plot
group_total$mean_sales <- sprintf("%.2f", group_total$mean_sales)
group_total$mean_sales <- as.numeric(group_total$mean_sales)
group_total$line_num <- as.numeric(group_total$product.line)

ggplot(group_total, aes(x = reorder (product.line , mean_sales), y = mean_sales, fill = mean_sales)) +
  geom_bar(stat = "identity" ) +
  theme_bw() +
  geom_text(aes(label = mean_sales), nudge_x = 0.0, nudge_y = 18) + 
  ylim(0, 400) +
  labs(x = "Product Line", y = "Mean Sales", title = "Mean Sales by Product Line") +
  scale_fill_gradient(low = "#EEEAD3", high = "#C6BB6C") +
  coord_flip()
 
 # get unique values
unique(super_mkt$month_name)
                        
# group by month_number then sort sum qty
group_month <- super_mkt %>%
  group_by(month_number) %>%
  summarise(sum_qty = sum(quantity)) %>%
  arrange(desc(sum_qty))
  
ggplot(group_month, aes(x = month_name, y = sum_qty, fill = sum_qty)) +
  geom_bar(stat = "identity", width = 0.5 ) +
  theme_bw() +
  geom_text(aes(label = sum_qty), nudge_x = 0.0, nudge_y = 60) + 
  ylim(0, 2100) +
  labs(x = "Month", y = "Qty", title = "Qty by Month") +
  scale_fill_gradient(low = "#B7E1E0", high = "#52B7B5")
  
 # group by month_name then sort sum total
group_month_sales <- super_mkt %>%
    group_by(month_name) %>%
    summarise(sum_total = sum(total)) %>%
    arrange(desc(sum_total))
 
#plot
ggplot(group_month_sales, aes(x = month_name, y = sum_total, fill = sum_total)) +
  geom_bar(stat = "identity", width = 0.5 ) +
  theme_bw() +
  geom_text(aes(label = sum_total), nudge_x = 0.0, nudge_y = 5000) + 
  ylim(0, 130000) +
  labs(x = "Month", y = "Sales", title = "sales by Month") +
  scale_fill_gradient(low = "#EDD4D4", high = "#BD6161")
  
 # group by customer.type then sort count_customer
group_customer_count <- super_mkt %>%
  group_by(customer.type) %>%
  summarize(count_customer = n()) %>%
  arrange(desc(count_customer))
 
 #plot pie chart
ggplot(group_customer_count, aes(x="", y=count_customer, fill=customer.type)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(fill="Customer Type", title="Customer Distribution") +
  theme_void() +
  geom_text(aes(label = (count_customer*100)/1000), position = position_stack(vjust = 0.5))
  
 #separate hour from time column
super_mkt$hour <- format(as.POSIXct(super_mkt$time, format="%H:%M"),format="%H")
#change character to numeric 
super_mkt$hour <- as.numeric(super_mkt$hour)
summary(super_mkt$hour)
  
# Creating a new column for periods 
super_mkt$periods <- ifelse(super_mkt$hour >=10 & super_mkt$hour <= 12, "Morning", 
                     ifelse(super_mkt$hour >= 13 & super_mkt$hour <= 16, "Afternoon", "Evening"))
                                                                       
# group by periods then sort average gross.income
group_periods <- super_mkt %>%
  group_by(periods) %>%
  summarise(mean_gross = mean(gross.income)) %>%
  arrange(desc(mean_gross))# group by periods then sort average gross.income
group_periods <- super_mkt %>%
  group_by(periods) %>%
  summarise(mean_gross = mean(gross.income)) %>%
  arrange(desc(mean_gross))
  
  # plot
group_periods$mean_gross <- sprintf("%.2f", group_periods$mean_gross)
group_periods$mean_gross <- as.numeric(group_periods$mean_gross)

ggplot(group_periods, aes(x = reorder (periods , mean_gross), y = mean_gross, fill = mean_gross)) +
  geom_bar(stat = "identity", width = 0.7 ) +
  theme_bw() +
  geom_text(aes(label = mean_gross), nudge_x = 0.0, nudge_y = 0.8) + 
  ylim(0, 18) +
  labs(x = "Periods", y = "Mean Gross income", title = "Mean Gross income by Periods") +
  scale_fill_gradient(low = "cyan", high = "blue")
  
  # group by city then sort average gross.income
group_city <- super_mkt %>%
  group_by(city) %>%
  summarise(mean_gross = mean(gross.income)) %>%
  arrange(desc(mean_gross))
  
  # plot
group_city$mean_gross <- as.numeric(group_city$mean_gross)

ggplot(group_city, aes(x = reorder (city , mean_gross), y = mean_gross, fill = mean_gross)) +
  geom_bar(stat = "identity", width = 0.7 ) +
  theme_bw() +
  geom_text(aes(label = mean_gross), nudge_x = 0.0, nudge_y = 0.8) + 
  ylim(0, 18) +
  labs(x = "City", y = "Mean Gross income", title = "Mean Gross income by City") +
  scale_fill_gradient(low = "pink", high = "magenta")
                         
# group by city, periods then average gross.income
group_city_periods <- super_mkt %>%
  group_by(periods, city) %>%
  summarise(mean_gross = mean(gross.income), .groups = "drop")
  
  #plot
ggplot(group_city_periods, aes(x = periods, y = mean_gross, fill = city)) +
  geom_col(position = "dodge") +
  theme_bw() +
  geom_text(aes(label = round(mean_gross, 2)), position = position_dodge(width = 0.9), vjust = -0.5) + 
  ylim(0, 18) +
  labs(x = "Periods", y = "Mean Gross income", title = "Mean Gross Income by Periods in each City") +
  scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3"))#plot

  # group by payment then sort count_payment
group_payment_count <- super_mkt %>%
  group_by(payment) %>%
  summarize(count_payment = n()) %>%
  arrange(desc(count_payment))
  
  #plot pie chart
ggplot(group_payment_count, aes(x="", y=count_payment, fill=payment)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(fill="Payment", title="Payment Distribution") +
  theme_void() +
  geom_text(aes(label = (count_payment*100)/1000), position = position_stack(vjust = 0.5))
  
  #group by city, periods then count_payment
group_payment_gender <- super_mkt %>%
    group_by(payment, gender) %>%
  summarize(count_payment = n(), .groups = "drop")
  
  # plot
ggplot(group_payment_gender, aes(x = gender, y = count_payment, fill = payment)) +
  geom_col(position = "dodge") +
  theme_bw() +
  geom_text(aes(label = round(count_payment, 2)), position = position_dodge(width = 0.9), vjust = -0.5) + 
  ylim(0, 200) +
  labs(x = "gender", y = "count_payment", title = "Payment per gender") +
  scale_fill_manual(values = c("#F7DE71", "#405EF5", "#E44970"))
  
  # group by product.line, customer.type then count_product.line
group_product_customer <- super_mkt %>%
  group_by(product.line, customer.type) %>%
  summarize(count_product = n(), .groups = "drop")
  
  # plot
ggplot(group_product_customer, aes(x = product.line, y = count_product, fill = customer.type)) +
  geom_col(position = "dodge") +
  theme_bw() +
  geom_text(aes(label = round(count_product, 2)), position = position_dodge(width = 0.9), vjust = -0.5) + 
  ylim(0, 110) +
  labs(x = "Product Line", y = "Count_product", title = "Payment per Customer Type") +
  scale_fill_manual(values = c("#739E82", "#F7CC58"))
  
  # group by product.line, gender then count
group_product_gender <- super_mkt %>%
  group_by(product.line, gender) %>%
  summarize(count_product = n(), .groups = "drop")
  
  # plot
ggplot(group_product_gender, aes(x = product.line, y = count_product, fill = gender)) +
  geom_col(position = "dodge") +
  theme_bw() +
  geom_text(aes(label = round(count_product, 2)), position = position_dodge(width = 0.9), vjust = -0.5) + 
  ylim(0, 110) +
  labs(x = "product.line", y = "count_product", title = "Number of sales per gender in each product line") +
  scale_fill_manual(values = c("#E44970", "#405EF5"))
  
  # group by gender then sort sum total
group_gender_sales <- super_mkt %>%
  group_by(gender) %>%
  summarise(sum_total = sum(total)) %>%
  arrange(desc(sum_total))
  
  #plot
ggplot(group_gender_sales, aes(x = gender, y = sum_total, fill = gender)) +
  geom_bar(stat = "identity", width = 0.7 ) +
  theme_bw() +
  geom_text(aes(label = sum_total), nudge_x = 0.0, nudge_y = 10000) + 
  ylim(0, 200000) +
  labs(x = "Gender", y = "Total Sales", title = "Total sales per gender") +
  scale_fill_manual(values = c("#E44970", "#405EF5"))
  
  # group by gender, city then sum total
group_gender_city <- super_mkt %>%
  group_by(gender, city) %>%
  summarize(sum_total= sum(total), .groups = "drop")
  
  #plot
ggplot(group_gender_city, aes(x = city, y = sum_total, fill = gender)) +
  geom_col(position = "dodge") +
  theme_bw() +
  geom_text(aes(label = round(sum_total, 2)), position = position_dodge(width = 0.9), vjust = -0.5) + 
  ylim(0, 70000) +
  labs(x = "City", y = "Total Sales", title = "Total sales per gender in each City") +
  scale_fill_manual(values = c("#E44970", "#405EF5"))
  
  # group by city, product.line then mean total
group_city_product <- super_mkt %>%
  group_by(city, product.line) %>%
  summarize(mean_total= mean(total), .groups = "drop")
  
  #plot
ggplot(group_city_product, aes(x = city, y = mean_total, fill = product.line)) +
  geom_col(position = "dodge") +
  theme_bw() +
  geom_text(aes(label = round(mean_total, 2)), position = position_dodge(width = 0.9), vjust = -0.5) + 
  ylim(0, 400) +
  labs(x = "Product Line", y = "Mean Sales", title = "Sales average per product line in each city") +
  scale_fill_manual(values = c("#F7DE71", "#405EF5", "#E44970", "#2C5530", "#99621E", "#EC3648"))
  
  # group by branch then average total
group_branch <- super_mkt %>%
  group_by(branch) %>%
  summarise(mean_total = mean(total))%>%
  arrange(desc(mean_total))
  
  # plot
ggplot(group_branch, aes(x = branch, y = mean_total, fill = branch)) +
  geom_col(position = "dodge") +
  theme_bw() +
  geom_text(aes(label = round(mean_total, 2)), position = position_dodge(width = 0.9), vjust = -0.5) + 
  ylim(0, 400) +
  labs(x = "Branch", y = "Sales Average", title = "Sales average per branch") +
  scale_fill_manual(values = c("#F7DE71", "#405EF5", "#E44970"))
  
  
  
  
