# Regression-Analysis-Statistical-Modelling
Built and evaluated multiple regression models, performing data preprocessing, feature transformations, multicollinearity checks, and diagnostic analysis to produce interpretable insights.

This project develops and evaluates a multiple linear regression model to explain and predict HDB resale prices in Singapore using transaction data from January–June 2021. The analysis focuses on building an interpretable and statistically adequate model through careful preprocessing, diagnostic checks, and model refinement.

The dataset contains over 11,000 HDB resale transactions with both quantitative and categorical predictors, including flat size, storey height, lease information, flat type, flat model, month of sale, and location. Key feature engineering steps include: Converting storey ranges into numeric midpoints, grouping towns into regional categories (CCR, RCR, OCR), constructing a continuous remaining-lease variable, and log-transforming resale prices to address skewness and stabilize variance.

Exploratory analysis revealed strong linear relationships between log(resale price) and floor area, storey height, and lease attributes, while categorical variables such as flat type, flat model, and region showed clear price stratification. Visual diagnostics (histograms, scatter plots, and box plots) guided variable selection and confirmed the suitability of linear regression after transformation.

An initial multivariate regression model was fitted using both categorical and continuous predictors. Model adequacy was assessed through residual diagnostics, normality checks, and influence measures. To address multicollinearity and over-parameterization, Variance Inflation Factors (VIF) and stepwise model selection based on AIC were applied. Redundant variables were removed to improve parsimony without sacrificing explanatory power.

The final model employs a weighted least squares (WLS) approach to correct for heteroscedasticity identified in earlier specifications. The chosen model achieves strong explanatory performance (Adjusted R² ≈ 0.84) while satisfying key regression assumptions, including linearity, normality, homoscedasticity, and low multicollinearity. Results indicate that flat size, storey height, flat model, region, and lease recency are significant determinants of HDB resale prices in Singapore.

However, the analysis is limited to transactions from the first half of 2021 and does not account for external factors such as proximity to amenities, renovations, or macroeconomic conditions. Future work could extend the dataset across multiple years and incorporate additional spatial or economic variables.
