# Employee Attrition
Developed predictive models to analyze employee retention factors and forecast employee turnover. This project used a dataset of 4,654 employees, examining factors such as education level, office location, payment tier, age, gender, and project assignment history to predict if an employee would leave within two years.
#### Objective
To classify employees’ likelihood of turnover, aiding the company in retention strategies by identifying significant variables impacting attrition and evaluating model performance.
Approach
###### •	Data Preparation:
Converted categorical variables like education level, city, gender, and benched status into numerical or dummy variables for analysis.
###### •	Modeling:
o	Logistic Regression: Achieved 73% accuracy with high specificity (90%) but moderate sensitivity (42%).

o	Linear Discriminant Analysis (LDA): Also achieved 73% accuracy, with balanced sensitivity (67%) and specificity (75%).
###### •	Evaluation: 
Assessed each model’s sensitivity, specificity, precision, and AUC to compare their effectiveness in predicting employee turnover.
###### Key Insights:
The models revealed that employees with higher education levels, those in lower payment tiers, and those who had been "benched" from projects were more likely to leave. The decision to use Logistic Regression or LDA would depend on the company's tolerance for misclassification—particularly if false negatives (predicting someone will stay when they might leave) could have greater financial implications.
###### Visuals:
Included odds ratio tables, confusion matrices, and ROC curves to demonstrate model performance and variable significance.

