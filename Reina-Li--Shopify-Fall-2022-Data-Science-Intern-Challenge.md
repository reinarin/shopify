Shopify Fall 2022 Data Science Intern Challenge
================
Reina Li

-   [Shopify Fall 2022 Data Science Intern
    Challenge](#shopify-fall-2022-data-science-intern-challenge)
    -   [Question 1](#question-1)
        -   [1. Think about what could be going wrong with our
            calculation. Think about a better way to evaluate this
            data.](#1-think-about-what-could-be-going-wrong-with-our-calculation-think-about-a-better-way-to-evaluate-this-data)
        -   [2. What metric would you report for this
            dataset?](#2-what-metric-would-you-report-for-this-dataset)
        -   [3. What is its value?](#3-what-is-its-value)
    -   [Question 2](#question-2)
        -   [1. How many orders were shipped by Speedy Express in
            total?](#1-how-many-orders-were-shipped-by-speedy-express-in-total)
        -   [2. What is the last name of the employee with the most
            orders?](#2-what-is-the-last-name-of-the-employee-with-the-most-orders)
        -   [3. What product was ordered the most by customers in
            Germany?](#3-what-product-was-ordered-the-most-by-customers-in-germany)

------------------------------------------------------------------------

# Shopify Fall 2022 Data Science Intern Challenge

Please complete the following questions, and provide your thought
process/work. You can attach your work in a text file, link, etc. on the
application page. Please ensure answers are easily visible for
reviewers!

------------------------------------------------------------------------

## Question 1

Given some sample data, write a program to answer the following: [click
here to access the required data
set](https://docs.google.com/spreadsheets/d/16i38oonuX1y1g7C_UAmiK9GkY7cS-64DfiDMNiR41LM/edit#gid=0)

On Shopify, we have exactly 100 sneaker shops, and each of these shops
sells only one model of shoe. We want to do some analysis of the average
order value (AOV). When we look at orders data over a 30 day window, we
naively calculate an AOV of $3145.13. Given that we know these shops are
selling sneakers, a relatively affordable item, something seems wrong
with our analysis.

``` r
# Load libraries
library(tidyverse)
library(scales)
```

``` r
# Read in the data set
df <- read.csv("2019 Winter Data Science Intern Challenge Data Set - Sheet1.csv")
```

``` r
# Convert 'created_at' column from chr to datetime
df$created_at <- as.POSIXct(df$created_at, format = "%Y-%m-%d %H:%M:%S")
```

### 1. Think about what could be going wrong with our calculation. Think about a better way to evaluate this data.

``` r
# Calculate initial average order value (AOV) using mean
dollar(mean(df$order_amount))
```

    ## [1] "$3,145.13"

As stated in the question, the calculated naive average order value
(AOV) is $3,145.13. Something seems to be wrong, because the 100 sneaker
shops only sells one model of shoe, and the AOV is a little expensive
for one shoe. Let’s look at the histogram, boxplot, and scatter plot of
the data.

``` r
# Plot histogram
ggplot(df, aes(x = order_amount)) +
  geom_histogram() +
  labs(x = "Order amount",
       y = "Frequency",
       title = "Histogram: Shopify Shoe 30-Day Order Value")
```

![](Reina-Li--Shopify-Fall-2022-Data-Science-Intern-Challenge_files/figure-gfm/initial%20plot-1.png)<!-- -->

``` r
# Plot scatter plot (Q-Q plot)
qqnorm(df$order_amount, main = "Normal Q-Q plot: Shopify Shoe 30-Day Order Value")
```

![](Reina-Li--Shopify-Fall-2022-Data-Science-Intern-Challenge_files/figure-gfm/initial%20plot-2.png)<!-- -->

``` r
# Plot boxplot
ggplot(df, aes(x = created_at, y = order_amount)) +
  geom_boxplot() +
  labs(x = "Date created",
       y = "Order amount",
       title = "Boxplot: Shopify Shoe 30-Day Order Value")
```

![](Reina-Li--Shopify-Fall-2022-Data-Science-Intern-Challenge_files/figure-gfm/initial%20plot-3.png)<!-- -->

Looking at the plots, we can clearly see that there are outliers values,
where the order amount is extremely high compared to the rest of the
values. The outliers are causing the AOV to increase a lot.

I have two suggestions. My ***first suggestion*** would be to determine
the AOV by calculating the median, instead of the mean. The reason why I
suggest using the median is because the median is not sensitive to
outlier values. However, please note that the median does not represent
a true average.

``` r
# Calculate average order value (AOV) using median
dollar(median(df$order_amount))
```

    ## [1] "$284"

If we **determine the AOV by calculating the median**, then the AOV will
be **$284**.

My ***second suggestion*** would be to remove the outliers, and then
determine the AOV by calculating the mean. Use the interquartile range
(IQR) method to remove outliers. We will set up upper and lower fences,
where values that fall outside these fences are considered outliers. The
data points which are 1.5 IQR away from Q1 and Q3 are considered as
outliers.

``` r
# Find and remove outliers using IQR method
quartiles <- quantile(df$order_amount, probs = c(0.25, 0.75), na.rm = FALSE)
iqr_val <- IQR(df$order_amount)
lower_fence <- quartiles[1] - 1.5 * iqr_val
upper_fence <- quartiles[2] + 1.5 * iqr_val
df_no_outlier <- subset(df, df$order_amount > lower_fence & df$order_amount < upper_fence)

# Calculate average order value (AOV) using mean, removed outliers
dollar(mean(df_no_outlier$order_amount))
```

    ## [1] "$293.72"

If we first **remove the outliers and then calculate the average order
amount**, then the AOV would be **$293.72**.

Both of my suggestions give a more reasonable AOV ($284 and $293.72)
than the initially calculated AOV ($3,145.13).

------------------------------------------------------------------------

### 2. What metric would you report for this dataset?

Looking at the data set, we are given values on order ID, shop ID, user
ID, order amount, total items, payment method, and date created. With
these columns, in addition to the average order value (AOV), another
metric I would report for this data set is **total revenue over a 30 day
window**.

------------------------------------------------------------------------

### 3. What is its value?

``` r
# Calculate total revenue over a 30 day window
Revenue <- dollar(sum(df$order_amount))
Revenue
```

    ## [1] "$15,725,640"

To calculate the total revenue over a 30 day window, sum up the order
amount values. The total revenue over a 30 day window is
**$15,725,640**.

------------------------------------------------------------------------

## Question 2

For this question you’ll need to use SQL. [Follow this
link](https://www.w3schools.com/SQL/TRYSQL.ASP?FILENAME=TRYSQL_SELECT_ALL)
to access the data set required for the challenge. Please use queries to
answer the following questions. Paste your queries along with your final
numerical answers below.

------------------------------------------------------------------------

### 1. How many orders were shipped by Speedy Express in total?

    SELECT COUNT(*)
    FROM Orders
    WHERE ShipperID = 1;

**54** orders were shipped by Speedy Express in total.

------------------------------------------------------------------------

### 2. What is the last name of the employee with the most orders?

    SELECT Employees.LastName, COUNT(Orders.OrderID)
    FROM Orders, Employees
    WHERE Orders.EmployeeID = Employees.EmployeeID
    GROUP BY Employees.EmployeeID
    ORDER BY COUNT(*) DESC;

The last name of the employee with the most orders is **Peacock**.

------------------------------------------------------------------------

### 3. What product was ordered the most by customers in Germany?

    SELECT Products.ProductName, SUM(OrderDetails.Quantity)
    FROM Products, OrderDetails, Orders, Customers
    WHERE Products.ProductID = OrderDetails.ProductID
    AND OrderDetails.OrderID = Orders.OrderID
    AND Orders.CustomerID = Customers.CustomerID
    GROUP BY Customers.Country, Products.ProductName
    HAVING Customers.Country = 'Germany'
    ORDER BY SUM(OrderDetails.Quantity) DESC;

The product that was ordered the most by customers in Germany was
**Boston Crab Meat**.

------------------------------------------------------------------------
