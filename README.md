# Sale-prediction-R-to-Python

I convert [R tutorial](https://courses.analyticsvidhya.com/courses/big-mart-sales-prediction-using-r) of the problem [Big Mart Sales](https://datahack.analyticsvidhya.com/contest/practice-problem-big-mart-sales-iii/) into Python. 

Problem Statement:
2013 sales data for 1559 products of Big Mart are collected across 10 stores in different cities. The aim is to build a predictive model to find out the sales of each product at a particular store.

Data format:
The training and test set are Pandas DataFrames (read from CSV files, whose shapes are (8523, 12) and (5681, 11) respectively). Each row contains attributes of a product. Each column is an attribute (Item_Identifier, Item_Weight, Item_Fat_Content, Item_Visibility, etc.). 

Evaluation Metric: Root Mean Square Rrror (RMSE)

I apply generalized linear models (from sklearn): Linear regression, Ridge regression (L2 regularization technique), and Lasso Regression (L1 regularization technique, by setting l1_ratio = 1 in ElasticNet regression model). The best model, Lasso Regression, has RMSE of 1129.63.

Please see the R script in sale_prediction.R and Python script in Sales_prediction.ipynb.




