## 🏕️ Green River Outdoor

this project analyzes customer and transaction data from Green River Outdoor, a retailer of outdoor goods with locations across the U.S. For the current phase of the project, the work focus on five store locations in and around the Boston metro area. Using R, the project aims to use the data to analyze profitability and customer satisfaction for their retail stores.

[R Code File](https://github.com/leticiatca/RetailerAnalysis/blob/main/Analytics_Problem_Set.Rmd)

## The Data

🏢 [stores.csv](https://github.com/leticiatca/RetailerAnalysis/blob/main/stores.csv) (5 records) - Contains data about each of the five retail locations in the region being studied 

👥 [customers.csv](https://github.com/leticiatca/RetailerAnalysis/blob/main/customers.csv) (2,501 records) - Contains demographic data and satisfaction survey data for the store's loyalty program members who have shopped at the five stores

💳 [sales.csv](https://github.com/leticiatca/RetailerAnalysis/blob/main/sales.csv) (5,961 records) - Contains data on all sales transactions in the five stores over the last fiscal quarter (a 3-month time period).

<img width="512" alt="ERD" src="https://github.com/leticiatca/RetailerAnalysis/assets/84414010/a2032cac-b013-44f2-b651-fd927f1cf200">


## Data Manipulation and Wrangling
```
Cleaning steps for 'Customers' dataset

customers <- read.csv('customers.csv')

##Fix inconsistencies in the "State" column##
customers <- customers %>%
  mutate(customer.state = ifelse(str_detect(customer.state, "^Mass"), "MA", customer.state)) %>%
  mutate(customer.state = ifelse(str_detect(customer.state, "^Conn"), "CT", customer.state))

##Fix inconsistencies in the "Birthday Month" calendar##

#Transform any values that do not conform to the acceptable number values
customers <- customers %>%
  mutate(
    birthday.month = case_when(
      str_detect(birthday.month, "^Jan") ~ "1",
      str_detect(birthday.month, "^Feb") ~ "2",
      str_detect(birthday.month, "^Mar") ~ "3",
      str_detect(birthday.month, "^Apr") ~ "4",
      str_detect(birthday.month, "^May") ~ "5",
      str_detect(birthday.month, "^Jun") ~ "6",
      str_detect(birthday.month, "^Jul") ~ "7",
      str_detect(birthday.month, "^Aug") ~ "8",
      str_detect(birthday.month, "^Sep") ~ "9",
      str_detect(birthday.month, "^Oct") ~ "10",
      str_detect(birthday.month, "^Nov") ~ "11",
      str_detect(birthday.month, "^Dec") ~ "12",
      TRUE ~ as.character(birthday.month)
    )
  )

#Check for problematic numerical data in "Age" and "Years as Member" columns
#No problem identified for "years as member"
#Remove rows where age is below 1 or over 100
customers <- customers %>%
  filter(age >= 1, age <= 100)

#Deal with missing values in "In Store Experience" and "Selection Satisfaction" column
customers_clean <- customers %>%
  select(-in.store.exp, -selection)
```

```
Cleaning steps for 'Sales' dataset
# Load 'sales'
data(sales)

# Check the structure of the 'sales' data
str(sales)

# Check for missing values
missing_values <- colSums(is.na(sales))
missing_values

# Convert 'sale.date' to Date format
sales$sale.date <- as.Date(sales$sale.date, origin = "1970-01-01")

# Remove rows with missing values
sales <- sales[complete.cases(sales), ]
```

```
Merging Datsets

#Dataframe with the number of items purchased and average item sale price for each customer.id
customer_summary <- sales %>%
  group_by(customer.id) %>%
  summarise(
    total_items_purchased = n(),
    avg_item_sale_price = mean(sale.amount, na.rm = TRUE))
  
#Merge cleaned customer dataset and average sale to add the customer-level transaction data 
updated_customers <- merge(customers_clean, customer_summary, by = "customer.id")
```

## Summary Statistics and Visualizations
```
sale_amount <- sales$sale.amount

#Compute the mean
mean_sale <- mean(sale_amount)
mean_sale

#Compute the median
median_sale <- median(sale_amount)
median_sale

#Compute the standard deviation
std_dev_sale <- sd(sale_amount)
std_dev_sale

#Compute the skewness coefficient
skewness_coefficient <- skewness(sale_amount)
skewness_coefficient
```

```
#Boxplot for all sale.amount data
boxplot(sale_amount, 
        main = "Total Sale Amount Boxplot", 
        xlab = "Sale Amount",
        horizontal = TRUE,
        col = "dodgerblue")
```
<img width="425" alt="Screenshot 2023-12-08 at 6 00 59 PM" src="https://github.com/leticiatca/RetailerAnalysis/assets/84414010/4d3dcb10-e9c4-42d1-bd1e-779d99af9821">

```
#Boxplots for the four product departments that have the highest dollar sales

#Group Sales by Department
department_sales <- sales %>% 
                    group_by(category) %>%
                    summarise(sales_per_department = sum(sale.amount)) %>%
                    arrange(desc(sales_per_department))

#Find the top 4 selling departments    
highest_sales <- head(department_sales, 4)

#Create boxplots for each department
top_departments <- sales %>%
  filter(category %in% highest_sales$category)

ggplot(top_departments, aes(x = category, y = sale.amount, fill = category)) +
  geom_boxplot() +
  labs(title = "Boxplots for Top 4 Selling Departments",
       x = "Department",
       y = "Sale Amount") +
  scale_fill_manual(values = c("Sleeping Gear" = "dodgerblue",
                               "Backpacks" = "green",
                               "Footwear" = "orange",
                               "Jackets" = "red"))
```
<img width="482" alt="Screenshot 2023-12-08 at 6 01 09 PM" src="https://github.com/leticiatca/RetailerAnalysis/assets/84414010/e687902a-e668-4869-8e53-4c41d54245b8">

```
#Identifying outliers in  'sale.amount' variable using z-score
sales <- sales %>%
  mutate(z_score = (sale.amount - mean_sale) / std_dev_sale)

# Identify outliers with a z-score threshold of 3
z_score_threshold <- 3
outliers <- sales %>%
  filter(abs(z_score) > z_score_threshold)

#Recommend a method for handling the outliers based on your assessment of their causes and frequency.
mean_outliers <- mean(outliers$sale.amount)
mean_outliers

category_outliers <- outliers %>%
  count(category)

#Remove outlier for travel category
sales_clean <- sales %>%
  filter(sale.amount <= 400)

#Updated Boxplot
boxplot(sales_clean$sale.amount, 
        main = "Total Sale Amount Boxplot", 
        xlab = "Sale Amount",
        horizontal = TRUE,
        col = "dodgerblue")

```
Using a z-score threshold of 3, we identified the presence of 75 outliers in the ‘Sales Amount’ column. Among these outliers, 70 are associated with the sleeping gear category, suggesting that they may represent higher-priced products rather than anomalies. However, a single outlier with a sales value of $940 in the travel category is identified as skewing the data significantly. To address this issue, we will selectively remove only this particular outlier to mitigate the impact on our analysis and retain the potential value represented by the sleeping gear category outliers

<img width="470" alt="Screenshot 2023-12-08 at 6 01 16 PM" src="https://github.com/leticiatca/RetailerAnalysis/assets/84414010/67047487-73de-425c-aef4-326e904e5c82">

## Hypothesis Testing
```{r}
##Hypothesis testing
aggregated_data <- sales_clean %>%
  group_by(category) %>%
  summarize(total_gross_margin = sum(gross.margin))

t_test_result <- sales_clean %>%
  filter(category %in% c("Sleeping Gear", "Backpacks")) %>%
  t.test(gross.margin ~ category, data = ., alternative = 'two.sided')

print(t_test_result)
```
<img width="385" alt="Screenshot 2023-12-08 at 6 01 27 PM" src="https://github.com/leticiatca/RetailerAnalysis/assets/84414010/768a1d6f-c63e-4f98-bb42-6d495031d8e3">


## Logistic Regression

```{r}
##Logistic regression

#Create a dummy variable for 'Clearance'
sales_clean$dummy_clearance <- ifelse(sales_clean$price.category == "Clearance", 1, 0)
```


```{r}
#Create a logistic model
logistic.model <- glm(dummy_clearance ~ category + store + unit.cost, family = binomial, data = sales_clean)
summary(logistic.model)
```

```{r}
# Predict the clearance status using the logistic regression model
predicted_clearance <- predict(logistic.model, newdata = sales_clean, type = "response")

# Convert predicted probabilities to binary predictions (0 or 1)
predicted_class <- ifelse(predicted_clearance > 0.5, 1, 0)

# Create a confusion matrix
conf_matrix <- table(Actual = sales_clean$dummy_clearance, Predicted = predicted_class)
print(conf_matrix)
```


```{r}
# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy:", accuracy, "\n")

# Calculate sensitivity (true positive rate)
sensitivity <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
cat("Sensitivity:", sensitivity, "\n")

precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
cat("Precision:", precision, "\n")
```

## Main Takeaways
- Focus on understanding and managing clearance for categories with positive coefficients (e.g., "Camping Misc," "Tops," "Swimwear"). Try to figure out if there are specific trends within these categories that contribute to clearance.
- Consider further investigating the categories with significant negative effects
- Since the store variable and unit.cost also contribute significantly to the prediction of clearance, consider store-specific strategies for managing clearance based on the
positive coefficient for the store variable.
- Evaluate whether certain stores have unique factors influencing clearance and adapt
strategies accordingly.
- Explore discounting strategies for items at different price points.
