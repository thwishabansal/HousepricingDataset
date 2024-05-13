# House Price Prediction

## Problem Statement: 
Predict the prices of houses in a certain area based on features such as the number of bedrooms, bathrooms, stories, area, parking facilities, and other related factors using Multiple Linear Regression and Regression Tree models.

## Data Cleaning
No missing values, but outliers were present.
Outliers in the top 1 percentile were detected and cleaned using R Studio.

## Models Created
Supervised Learning Models: Multiple Linear Regression, Regression Tree.
Unsupervised Learning Model: KMeans Clustering.

## Observations and Findings
- Multiple Linear Regression
-- Model 6 achieved the best performance.
-- R^2: 0.529, RMSE: 1,182,739, MAPE: 0.186739.
- Regression Tree
-- RG_4 was identified as the best model.
-- RMSE: 1,214,634, MAPE: 20.22419.

## Final Observations
Model 6 in Linear Regression outperformed RG_4 in the Regression Tree model.
The variables indicating the number of bathrooms, stories, area of the house, parking area, if the house is located on a main road, and the heating system in the house are the most significant.
Parking and hot water heating positively affect house prices, while furnishing has a negative effect.
## Conclusion:
The project successfully predicts house prices based on various factors, highlighting the importance of specific amenities and features in determining housing market values.
