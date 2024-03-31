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

| Mean  | Median | Standard Deviation  | Skewness Coefficient |
| ------------- | ------------- | ------------- | ------------- |
| 71.96  | 68 | 38.73  | 2.86  |

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

Findings from the exploratory analysis
The analysis of sales data has unveiled several significant findings. Distribution Characteristics:
- The distribution of sale amounts is positively skewed, indicating a prevalence of transactions with lower sale amounts and a scarcity of those with very high sale amounts.
- The mean sale amount stands at approximately $71.93, surpassing the median of $68.00. This suggests the existence of higher-value transactions that contribute to pulling the mean above the median.
- The standard deviation of sale amounts is approximately $38.74, signaling a moderate spread of transaction values around the mean.
- The skewness coefficient, calculated at 2.86, confirms the positive skew in the distribution of sale amounts.

## Hypothesis Testing
The hypothesis test looks to explore if there is a relevant variation in gross margin performance across departments. The hypothesis test will evaluate the difference in mean gross margin for the category with highest ‘Sale Amount’ (Sleeping Gear) and the category with highest ‘Gross Margin’ (Backpacks)


Null Hypothesis (H0): The mean gross margin (GM$) for Backpacks is equal to the mean GM$ for Sleeping Gear. 

Alternative Hypothesis (H1): The mean GM$ for Backpacks is significantly different from the mean GM$ for Sleeping Gear. 

```
##Hypothesis testing
aggregated_data <- sales_clean %>%
  group_by(category) %>%
  summarize(total_gross_margin = sum(gross.margin))

t_test_result <- sales_clean %>%
  filter(category %in% c("Sleeping Gear", "Backpacks")) %>%
  t.test(gross.margin ~ category, data = ., alternative = 'two.sided')

print(t_test_result)
```
| t-value  | p-value | Lower Bound  | Upper Bound | Mean 'Backpacks'  | Mean 'Sleeping Gear' |
| ------------- | ------------- | ------------- | ------------- | ------------- | ------------- |
| -4.6425 | 3.629e-06 | -0.07207477 | -0.02926846 | 0.5335330 | 0.5842046 |

With such a low p-value, lower than a confidence level of 0.05, we would reject the null hypothesis. In this case, the null hypothesis is that there is no difference in means between "Backpacks" and "Sleeping Gear."
Based on the results, we can conclude that there is a statistically significant difference in gross margin between "Backpacks" and "Sleeping Gear." The negative sign of the confidence interval suggests that the mean gross margin for "Backpacks" is lower than that for "Sleeping Gear."

Some recommendations for them company would be to:
- Evaluate the pricing strategy for both "Backpacks" and "Sleeping Gear." If GRO
wants to increase the profit margin for "Sleeping Gear," they may want to consider
reevaluating the pricing of backpacks to make it more profitable.
- Examine the product mix and assortment within each category. Determine if there are specific products within "Sleeping Gear" that contribute significantly to its higher gross margin. Consider optimizing the product mix in "Backpacks" to include more
profitable items.
- Conduct a detailed cost analysis for both categories. Evaluate if there are cost
differences or operational efficiencies that contribute to the observed gross margin disparity. Identifying cost drivers can guide decisions on cost reduction.

## Logistic Regression
The logistic regression model to predict the variable 'clearance'
We created a new binary variable called ‘Dummy_Clearance’ based on the ‘Price.Category’ variable that returns the value 1 if a product was sold under clearance and 0 otherwise.
The model includes all product categories (bottoms, camping misc, footwear, jackets, sleeping gear, swimwear, tops, travel) with 'backpacks' as the reference category. Additionally, the unit cost and store variable is considered in the model to understand its impact on predicting whether a product falls under clearance.

```
##Logistic regression

#Create a dummy variable for 'Clearance'
sales_clean$dummy_clearance <- ifelse(sales_clean$price.category == "Clearance", 1, 0)
```


```{r}
#Create a logistic model
logistic.model <- glm(dummy_clearance ~ category + store + unit.cost, family = binomial, data = sales_clean)
summary(logistic.model)
```
<img width="385" alt="Screenshot 2023-12-08 at 6 01 27 PM" src="https://github.com/leticiatca/RetailerAnalysis/assets/84414010/768a1d6f-c63e-4f98-bb42-6d495031d8e3">

The reference variable for our model was ‘Backpacks’. The low p-values (α < 0.05) suggest that the corresponding variables are statistically significant in predicting the clearance outcome. The coefficients for each category represent the change in probability of clearance for that category compared to the reference category. For example, for category ‘Bottoms’, the predicted change of an item going to clearance is higher by 0.7779 units compared to the ‘Backpacks’.
Items in these categories are more likely to go on clearance compared to ‘Backpacks’:
- "Camping Misc" (Coefficient: 3.9342)
- "Tops" (Coefficient: 1.0399)
- “Bottoms” (Coefficient: 0.7779)
- "Swimwear" (Coefficient: 0.5313)
Items in these categories are less likely to go on clearance compared to ‘Backpacks’:
- Jackets (Coefficient: -0.0607)
- Footwear (Coefficient: -0.0913)
- Travel (Coefficient: -0.6851)
- Sleeping Gear (Coefficient: -1.2683)

Analyzing the other predictor variables that are not ‘Category’. The coefficient for ‘Store’ is 0.3953, indicating that the store in which an item is located influences the likelihood of clearance. The positive coefficient suggests that items in certain stores are more likely to go on clearance. The coefficient for ‘Unit.Cost’ is 0.0269, indicating that the cost of the item influences the likelihood of clearance. The positive coefficient suggests that more expensive items are more likely to go on clearance.
  
```
# Predict the clearance status using the logistic regression model
predicted_clearance <- predict(logistic.model, newdata = sales_clean, type = "response")

# Convert predicted probabilities to binary predictions (0 or 1)
predicted_class <- ifelse(predicted_clearance > 0.5, 1, 0)

# Create a confusion matrix
conf_matrix <- table(Actual = sales_clean$dummy_clearance, Predicted = predicted_class)
print(conf_matrix)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy:", accuracy, "\n")

# Calculate sensitivity (true positive rate)
sensitivity <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
cat("Sensitivity:", sensitivity, "\n")

precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
cat("Precision:", precision, "\n")
```
<img width="600" alt="Screenshot 2024-03-31 at 7 08 25 PM" src="https://github.com/leticiatca/RetailerAnalysis/assets/84414010/704c71f8-665a-4575-94d3-efc5a5c01455">

The overall accuracy of the model is 83.9%, which indicates the proportion of correctly classified instances (both 0s and 1s). However, accuracy on its own might not be able to assess the model’s ability to predict. The low sensitivity score of 6.67%, indicates that the model struggles to correctly identify positive instances. It is missing a significant portion of actual positive cases. Still, the precision of 75.28% indicates that, when the model predicts positive, it is accurate most of the time. Future analysis could focus on other variables that potentially impact an item going to sale or not to achieve a higher sensitivity.

## Main Takeaways
- Focus on understanding and managing clearance for categories with positive coefficients (e.g., "Camping Misc," "Tops," "Swimwear"). Try to figure out if there are specific trends within these categories that contribute to clearance.
- Consider further investigating the categories with significant negative effects
- Since the store variable and unit.cost also contribute significantly to the prediction of clearance, consider store-specific strategies for managing clearance based on the
positive coefficient for the store variable.
- Evaluate whether certain stores have unique factors influencing clearance and adapt
strategies accordingly.
- Explore discounting strategies for items at different price points.
