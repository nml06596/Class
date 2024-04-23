Continued Analysis of the Financial Literacy Levels of St. John Fisher University Students
Nate Lewis
Spring 2024
Background
Financial literacy is often viewed as the ability to understand and effectively use money; this topic incorporates ideas such as personal financial management, budgeting, and investing. Yet, even as financial systems become more prominent due to the increasing access to and influence of the Internet, young people across the globe suffer from low levels of financial literacy (Ramos-Hernández, 2020). Today’s youth, particularly college students, are one of the most vulnerable populations in the financial system and for that reason, it is important to ensure that college students have the tools to become successful “economic citizens” (Amagir, 2017).

The “Financial Knowledge Scale” (FKS) was designed in 2012 by researchers Melissa Knoll and Carrie Houts to accurately measure financial literacy using a 20-item survey (Knoll, 2012) . By utilizing item response theory (IRT), the researchers were able to design an assessment that provides results by analyzing what ability or skill level is needed to secure a certain possibility of answering a question correctly. In 2019, Knoll and Houts revisited their original assessment and improved it using primary data from thousands of households (Knoll, 2019). Using this abundance of new data and a new IRT parameter that analyzes the probability of getting a question correct by chance, the researchers were able to design a more user-friendly 10-item scale without compromising on reliability. The 10-Item Financial Knowledge Scale measures respondents’ financial literacy using a series of multiple-choice and true-false questions. With the researchers’ extensive analysis of previous financial literacy assessments, meticulous focus on evidence-based assessment designing using IRT, and prioritization of user-friendliness, the Financial Knowledge Scale is the best instrument for measuring the financial literacy of college students.

Data Set
In Fall 2022, I worked with various departments of St. John Fisher University to gather responses to the FKS survey from the Fisher student body, along with demographic data. Although 406 unique responses were recorded, some respondents did not fully complete the survey and a small number of observations could not be used due to “Gender” responses; therefore, the remaining 270 fully complete responses were used for analysis purposes. In Spring 2023, basic regression analysis was done to identify certain trends in the data; however, machine learning and advanced regression methods were not utilized. With this project, I am seeking to address the following research question: Using machine learning and advanced regression techniques, what is the best model to accurately predict the financial literacy level of Fisher students? How do these techniques help identify which student populations have low financial literacy scores?



Initial Exploratory Analysis of Data
library(mlbench)
## Warning: package 'mlbench' was built under R version 4.3.3
library(caret)
## Warning: package 'caret' was built under R version 4.3.3
## Loading required package: ggplot2
## Warning: package 'ggplot2' was built under R version 4.3.2
## Loading required package: lattice
library(partykit)
## Warning: package 'partykit' was built under R version 4.3.3
## Loading required package: grid
## Loading required package: libcoin
## Warning: package 'libcoin' was built under R version 4.3.2
## Loading required package: mvtnorm
## Warning: package 'mvtnorm' was built under R version 4.3.2
library(skimr)
## Warning: package 'skimr' was built under R version 4.3.3
library(party)
## Warning: package 'party' was built under R version 4.3.3
## Loading required package: modeltools
## Loading required package: stats4
## Loading required package: strucchange
## Warning: package 'strucchange' was built under R version 4.3.2
## Loading required package: zoo
## Warning: package 'zoo' was built under R version 4.3.2
## 
## Attaching package: 'zoo'
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
## Loading required package: sandwich
## Warning: package 'sandwich' was built under R version 4.3.2
## 
## Attaching package: 'party'
## The following objects are masked from 'package:partykit':
## 
##     cforest, ctree, ctree_control, edge_simple, mob, mob_control,
##     node_barplot, node_bivplot, node_boxplot, node_inner, node_surv,
##     node_terminal, varimp
library(readxl)
## Warning: package 'readxl' was built under R version 4.3.2
library(ggcorrplot)
## Warning: package 'ggcorrplot' was built under R version 4.3.2
Financial literacy scores, along with various demographic characteristics and experience in financial courses, were collected for every survey respondent. The “Score” variable measures how many correct answers a student selected on the 10-question FKS survey. Furthermore, the data set includes dummy variables for age, gender, grade level, academic school at Fisher, and experience with financial courses.

The “read_excel” function is used to import the data set from Excel and the “skim” function verifies that the data set is complete. In addition, the “colnames” function was utilized to rename certain variable and prevent issues with coding and interpretation, as the data set included certain variables with numbers in the title (e.g., the dummy variable for if a student was aged 18 was “18”) and vague names (e.g., the dummy if someone was a male was entitled “Gender”, as opposed to “Male”).

data <- read_excel("C://Users//nml06596//Downloads//Data Set Dummy.xlsx")
skim(data)
Data summary
Name	data
Number of rows	270
Number of columns	19
_______________________	
Column type frequency:	
numeric	19
________________________	
Group variables	None
Variable type: numeric

skim_variable	n_missing	complete_rate	mean	sd	p0	p25	p50	p75	p100	hist
Gender	0	1	0.37	0.48	0	0	0	1.00	1	▇▁▁▁▅
Business	0	1	0.31	0.47	0	0	0	1.00	1	▇▁▁▁▃
Nursing	0	1	0.21	0.41	0	0	0	0.00	1	▇▁▁▁▂
Education	0	1	0.07	0.26	0	0	0	0.00	1	▇▁▁▁▁
Arts & Sciences	0	1	0.30	0.46	0	0	0	1.00	1	▇▁▁▁▃
Pharmacy	0	1	0.10	0.30	0	0	0	0.00	1	▇▁▁▁▁
18	0	1	0.19	0.39	0	0	0	0.00	1	▇▁▁▁▂
19	0	1	0.21	0.41	0	0	0	0.00	1	▇▁▁▁▂
20	0	1	0.17	0.38	0	0	0	0.00	1	▇▁▁▁▂
21	0	1	0.18	0.39	0	0	0	0.00	1	▇▁▁▁▂
22	0	1	0.07	0.26	0	0	0	0.00	1	▇▁▁▁▁
23 or above	0	1	0.18	0.38	0	0	0	0.00	1	▇▁▁▁▂
First Year	0	1	0.20	0.40	0	0	0	0.00	1	▇▁▁▁▂
Sophomore	0	1	0.21	0.41	0	0	0	0.00	1	▇▁▁▁▂
Junior	0	1	0.20	0.40	0	0	0	0.00	1	▇▁▁▁▂
Senior	0	1	0.25	0.43	0	0	0	0.75	1	▇▁▁▁▃
Graduate	0	1	0.14	0.34	0	0	0	0.00	1	▇▁▁▁▁
Course	0	1	0.40	0.49	0	0	0	1.00	1	▇▁▁▁▅
Score	0	1	6.93	1.64	3	6	7	8.00	10	▁▃▇▃▃
colnames(data) <- c('Male','Business','Nursing','Education','ArtsandSciences','Pharmacy',                  'Age18','Age19','Age20','Age21','Age22',"Age23andup",              'FirstYear','Sophomore','Junior','Senior','Graduate',
'Course','Score')
When looking at the Score variable as a whole, the average (=7) and median (=6.93) suggests that the Fisher student body does pass the financial literacy assessment, understanding a majority of basic financial concepts.

summary(data$Score)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    3.00    6.00    7.00    6.93    8.00   10.00
library(dplyr)
## Warning: package 'dplyr' was built under R version 4.3.3
## 
## Attaching package: 'dplyr'
## The following object is masked from 'package:party':
## 
##     where
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
However, my previous research identified that this distribution of is not shared equally throughout all populations at Fisher.

The male population at Fisher tends to answer between one to two more questions correctly than their female counterparts.

data %>%
  group_by(Male) %>%
  summarize(median_score = median(Score), mean_score = mean(Score))
Male
<dbl>
median_score
<dbl>
mean_score
<dbl>
0	6	6.458824
1	8	7.730000
2 rows
The Business major population at Fisher tends to answer about one to two more questions correctly than their non-Business major counterparts.

data %>%
  group_by(Business, Nursing, Education, ArtsandSciences, Pharmacy) %>%
  summarize(median_score = median(Score), mean_score = mean(Score))
## `summarise()` has grouped output by 'Business', 'Nursing', 'Education',
## 'ArtsandSciences'. You can override using the `.groups` argument.
Business
<dbl>
Nursing
<dbl>
Education
<dbl>
ArtsandSciences
<dbl>
Pharmacy
<dbl>
median_score
<dbl>
mean_score
<dbl>
0	0	0	0	1	7.0	6.961538
0	0	0	1	0	6.5	6.560976
0	0	1	0	0	6.0	6.315789
0	1	0	0	0	6.0	6.465517
1	0	0	0	0	8.0	7.729412
5 rows
Those that have taken a financial course tend to answer about one to two more questions correctly than their counterparts that have never taken a financial course.

data %>%
  group_by(Course) %>%
  summarize(median_score = median(Score), mean_score = mean(Score))
Course
<dbl>
median_score
<dbl>
mean_score
<dbl>
0	6.0	6.549383
1	7.5	7.500000
2 rows
Scores tend to increase with age, particularly at age 22 and above.

data %>%
  group_by(Age18, Age19, Age20, Age21, Age22, Age23andup) %>%
  summarize(median_score = median(Score), mean_score = mean(Score))
## `summarise()` has grouped output by 'Age18', 'Age19', 'Age20', 'Age21',
## 'Age22'. You can override using the `.groups` argument.
Age18
<dbl>
Age19
<dbl>
Age20
<dbl>
Age21
<dbl>
Age22
<dbl>
Age23andup
<dbl>
median_score
<dbl>
mean_score
<dbl>
0	0	0	0	0	1	8	7.541667
0	0	0	0	1	0	8	7.850000
0	0	0	1	0	0	7	6.897959
0	0	1	0	0	0	7	6.957447
0	1	0	0	0	0	7	6.607143
1	0	0	0	0	0	6	6.340000
6 rows
Scores tend to increase with each subsequent grade level.

data %>%
  group_by(FirstYear, Sophomore, Junior, Senior) %>%
  summarize(median_score = median(Score), mean_score = mean(Score))
## `summarise()` has grouped output by 'FirstYear', 'Sophomore', 'Junior'. You can
## override using the `.groups` argument.
FirstYear
<dbl>
Sophomore
<dbl>
Junior
<dbl>
Senior
<dbl>
median_score
<dbl>
mean_score
<dbl>
0	0	0	0	8	7.729730
0	0	0	1	7	7.147059
0	0	1	0	7	6.909091
0	1	0	0	7	6.578947
1	0	0	0	6	6.490566
5 rows
Graduate students tend to answer one more questions correctly than their undergraduate counterparts.

data %>%
  group_by(Graduate) %>%
  summarize(median_score = median(Score), mean_score = mean(Score))
Graduate
<dbl>
median_score
<dbl>
mean_score
<dbl>
0	7	6.802575
1	8	7.729730
2 rows
There are strong differences across numerous variables, allowing me to use the following model in my previous research to best predict financial literacy scores:

Score = 7.4 + 0.l5 if Aged 19, 20, or 21 + 1.1 if Aged 22 or above - 0.7 if non-Business - 0.9 if Female

The model previously identified was able to explain 21.17% of the variance in the sample, with all variable significant at an alpha level of 0.05.

Principal Component Analysis
data <- read_excel("C://Users//nml06596//Downloads//Data Set Dummy.xlsx")
colnames(data) <- c('Male','Business','Nursing','Education','ArtsandSciences','Pharmacy',                  'Age18','Age19','Age20','Age21','Age22',"Age23andup",              'FirstYear','Sophomore','Junior','Senior','Graduate',
'Course','Score')
library(FactoMineR)
## Warning: package 'FactoMineR' was built under R version 4.3.3
library(factoextra)
## Warning: package 'factoextra' was built under R version 4.3.3
## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa
The data set was analyzed for multicollinearity using a correlation matrix and PCA analysis. The only variables that showed excessive correlation were those related to age and grade level. This makes intuitive sense: a majority of Fisher students start college around age 18, right after high school graduation. As a result, being First Year is highly correlated with being aged 18. This trend continues for every subsequent year of age and undergraduate grade level.

corr_matrix <- cor(data)
ggcorrplot(corr_matrix, type='lower')


data.pca <- princomp(corr_matrix)
fviz_pca_var(data.pca, col.var = "black")


In order to reduce issues associated with multicollinearity, the decision was made to remove either the Age or Grade Level variables. It was decided that it would be better to remove the Grade Level variable as it’s meaning is vaguer in it’s interpretation: when respondents answered the survey, did they consider “First Year” to mean their first year at Fisher or at any higher education institution? Given that it may introducing unreliability into the data set, it was removed. Note that Graduate was kept because it is not necessarily correlated with age; there are many students that start graduate programs directly after undergraduate, but there is also a large population of students who return to school for graduate programs later in life.

data <- subset(data, select = -c(FirstYear, Sophomore, Junior, Senior))
After removing the variables related to undergraduate grade levels, the updated correlation matrix reveals that multicollinearity issues have been properly addressed.

ggcorrplot(cor(data), type='lower')


Data Splitting & Resampling
The data is split to allow for training data, which can than be evaluated using the testing data set. With the proportion set to 0.9, 243 observations are used to set to estimate models and the remaining 27 observations allow for the testing of the models. Comparing the average scores in both groups implies that both the training and testing groups are truly random, with negligible differences between groups.

set.seed(1000)
trainIndex <- createDataPartition(data$Score, p = .9, list = FALSE, times = 1)
train_data <- data[ trainIndex,]
test_data  <- data[-trainIndex,]
ctrl <- trainControl(method="cv", n=4)

mean(train_data$Score)
## [1] 6.930328
mean(test_data$Score)
## [1] 6.923077
Predicting Scores - Regression Analysis
library(earth) # for MARS
## Warning: package 'earth' was built under R version 4.3.3
## Loading required package: Formula
## Loading required package: plotmo
## Warning: package 'plotmo' was built under R version 4.3.2
## Loading required package: plotrix
## Warning: package 'plotrix' was built under R version 4.3.2
## Loading required package: TeachingDemos
## Warning: package 'TeachingDemos' was built under R version 4.3.2
library(glmnet) # for LARS and LASSO
## Warning: package 'glmnet' was built under R version 4.3.3
## Loading required package: Matrix
## Warning: package 'Matrix' was built under R version 4.3.3
## Loaded glmnet 4.1-8
library(gridExtra)  # for arranging plot
## Warning: package 'gridExtra' was built under R version 4.3.3
## 
## Attaching package: 'gridExtra'
## The following object is masked from 'package:dplyr':
## 
##     combine
library(dplyr) # for cleaning the data set
library(ggplot2) # for graph below
Various techniques were utilized to develop regression models that help estimate scores. Although interaction effects were briefly considered, they greatly over-complicated models (more than tripling the number of predictors in some models) while also reducing the R-squared values of models by around 0.10.

 ctrl <- trainControl(method="cv", number=5)

models <- list(
  m1_lm <- train(Score ~ ., data = train_data, method = "lm", trControl = ctrl),
  m2_ss <- train(Score ~ ., data=train_data, method="glmStepAIC", direction="both",  trControl=ctrl),
  m3_mars <- train(Score ~ . , data=train_data, method="earth", tuneGrid = expand.grid(degree = 1:3, nprune = 1:10), trControl=ctrl),
  m4_elastic <- train(Score ~ . , data=train_data, method="glmnet", tuneLength=10, degree = 2,  trControl=ctrl),
  m5_rf <- train(Score ~ . , data=train_data, method="rf",  trControl=ctrl),
  m6_rpart <- train(Score ~ . , data=train_data, method="rpart",  trControl=ctrl)
)
## Warning in nominalTrainWorkflow(x = x, y = y, wts = weights, info = trainInfo,
## : There were missing values in resampled performance measures.

## Warning in nominalTrainWorkflow(x = x, y = y, wts = weights, info = trainInfo,
## : There were missing values in resampled performance measures.
After resampling, all models had R-squared values between 0.10 and 0.20, with one model being able to explain almost 20% of the variation in training data scores (Model 4 = Elastic).

results <-resamples(models)
summary(results)
## 
## Call:
## summary.resamples(object = results)
## 
## Models: Model1, Model2, Model3, Model4, Model5, Model6 
## Number of resamples: 5 
## 
## MAE 
##             Min.  1st Qu.   Median     Mean  3rd Qu.     Max. NA's
## Model1 1.1243868 1.166264 1.234381 1.221738 1.275655 1.308003    0
## Model2 1.0864893 1.099396 1.236886 1.205460 1.293370 1.311157    0
## Model3 1.0582401 1.064720 1.191740 1.240730 1.397418 1.491530    0
## Model4 0.9715566 1.178455 1.260655 1.194793 1.274889 1.288409    0
## Model5 1.0274552 1.147590 1.174847 1.207635 1.336139 1.352146    0
## Model6 1.1736035 1.174232 1.198890 1.216147 1.220527 1.313481    0
## 
## RMSE 
##            Min.  1st Qu.   Median     Mean  3rd Qu.     Max. NA's
## Model1 1.418460 1.433521 1.527708 1.534353 1.573419 1.718655    0
## Model2 1.376603 1.461174 1.475597 1.529444 1.565572 1.768274    0
## Model3 1.253501 1.307670 1.443299 1.506142 1.642143 1.884094    0
## Model4 1.249380 1.463646 1.504467 1.484348 1.535115 1.669132    0
## Model5 1.254873 1.474622 1.525488 1.501129 1.614164 1.636500    0
## Model6 1.461318 1.474471 1.500146 1.534347 1.611691 1.624108    0
## 
## Rsquared 
##               Min.    1st Qu.    Median      Mean   3rd Qu.      Max. NA's
## Model1 0.022574539 0.08814079 0.1130907 0.1552064 0.1687579 0.3834679    0
## Model2 0.077060441 0.07842037 0.1818473 0.1522965 0.1826711 0.2414834    0
## Model3 0.009487396 0.17452659 0.1904795 0.1728252 0.2003145 0.2893180    0
## Model4 0.127760270 0.16230043 0.1803789 0.1879405 0.1984925 0.2707704    0
## Model5 0.035771999 0.10112239 0.1017128 0.1720552 0.2659325 0.3557363    0
## Model6 0.056995437 0.10602818 0.1274770 0.1367531 0.1886894 0.2045755    0
After running the models against the testing data, R-squared values generally increased for every model, suggesting that the models were truly understanding consistent patterns in the data, as opposed to overfitting to the testing data. Three models (Linear Regression, Stepwise, and Elastic) had similar R-squared values; these models are able to explain more than a quarter of the differences between financial literacy levels (R-squared values > 0.25).

# Predictions based on the test_data

preds_lm <- predict(m1_lm, test_data)
preds_ss <- predict(m2_ss, test_data)
preds_mars <- predict(m3_mars, test_data)
preds_lasso <- predict(m4_elastic, test_data)
preds_rf <- predict(m5_rf, test_data)
preds_rpart <- predict(m6_rpart, test_data)

plot1 <- ggplot(test_data, aes(x = Score, y = preds_lm)) +
  geom_point(color = "yellow") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") + 
  labs(title = "LM Predictions", x = "Actual Score", y = "Predicted Score") +
  theme_bw()

plot2 <- ggplot(test_data, aes(x = Score, y = preds_ss)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") + 
  labs(title = "Stepwise Predictions", x = "Actual Score", y = "Predicted Score") +
  theme_bw()

plot3 <- ggplot(test_data, aes(x = Score, y = preds_mars)) +
  geom_point(color = "green") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") + 
  labs(title = "MARS Predictions", x = "Actual Score", y = "Predicted Score") +
  theme_bw()

plot4 <- ggplot(test_data, aes(x = Score, y = preds_lasso)) +
  geom_point(color = "red") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(title = "Elastic Predictions", x = "Actual score", y = "Predicted Score") +
  theme_bw()

plot5 <- ggplot(test_data, aes(x = Score, y = preds_lm)) +
  geom_point(color = "red") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(title = "Random Forest Predictions", x = "Actual score", y = "Predicted Score") +
  theme_bw()

plot6 <- ggplot(test_data, aes(x = Score, y = preds_rpart)) +
  geom_point(color = "red") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(title = "Rpart Predictions", x = "Actual score", y = "Predicted Score") +
  theme_bw()

# Arrange the plots in a 3-column grid
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol = 3)


# How well did we predict in the testing data? 

r2_lm <- round(summary(lm(test_data$Score ~ preds_lm))$r.squared,2)
r2_ss <- round(summary(lm(test_data$Score ~ preds_ss))$r.squared,2)
r2_mars <- round(summary(lm(test_data$Score ~ preds_mars))$r.squared,2)
r2_elastic <- round(summary(lm(test_data$Score ~ preds_lasso))$r.squared,2)
r2_rf <- round(summary(lm(test_data$Score ~ preds_rf))$r.squared,2)
r2_rpart <- round(summary(lm(test_data$Score ~ preds_rpart))$r.squared,2)

print(paste0("R-squared LM:", r2_lm))
## [1] "R-squared LM:0.28"
print(paste0("R-squared STEPWISE:", r2_ss))
## [1] "R-squared STEPWISE:0.31"
print(paste0("R-squared MARS:", r2_mars))
## [1] "R-squared MARS:0.14"
print(paste0("R-squared ELASTIC:", r2_elastic))
## [1] "R-squared ELASTIC:0.3"
print(paste0("R-squared RF:", r2_rf))
## [1] "R-squared RF:0.21"
print(paste0("R-squared rpart:", r2_rpart))
## [1] "R-squared rpart:0.14"
I selected the stepwise regression as the best model to estimate specific financial literacy scores as it had the highest prediction power (R-squared value = 0.31) and easy interpretability.

The model was estimated as:

Score = 6.8 + 0.9 if Male + 0.5 if Business - 1.0 if Age 18 - 0.7 if Age 19 - 0.4 if Age 20 - 0.5 if Age 21 + 0.4 if Course

m2_ss
## Generalized Linear Model with Stepwise Feature Selection 
## 
## 244 samples
##  14 predictor
## 
## No pre-processing
## Resampling: Cross-Validated (5 fold) 
## Summary of sample sizes: 196, 196, 195, 194, 195 
## Resampling results:
## 
##   RMSE      Rsquared   MAE    
##   1.529444  0.1522965  1.20546
summary(m2_ss$finalModel)
## 
## Call:
## NULL
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   6.8071     0.2203  30.904  < 2e-16 ***
## Male          0.8691     0.2201   3.949 0.000104 ***
## Business      0.4412     0.2547   1.732 0.084515 .  
## Age18        -1.0113     0.2894  -3.495 0.000567 ***
## Age19        -0.6892     0.2858  -2.412 0.016647 *  
## Age20        -0.4267     0.2869  -1.487 0.138259    
## Age21        -0.5303     0.2893  -1.833 0.068040 .  
## Course        0.3502     0.2309   1.516 0.130795    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 2.141005)
## 
##     Null deviance: 651.82  on 243  degrees of freedom
## Residual deviance: 505.28  on 236  degrees of freedom
## AIC: 888.06
## 
## Number of Fisher Scoring iterations: 2
plot(m2_ss$finalModel)


Futhermore, this model suffers from minimal heteroskedasticity concerns.

Predicting Scores - Neural Net Analysis
When I began my project of utilizing machine learning to improve my findings related to the financial literacy of Fisher students, I found the use of neural nets to be a promising idea to help uncover patterns hidden in my data. In specific, I attempted some analysis using the “neuralnet” package, which can be tuned to include specific amounts of nodes in each subsequent layer of analysis. Despite the allure of this package, it was not able to yield any substantial insights for my data set for a few reasons:

Running neural nets requires a high amount of computational power and time that was frankly not necessary given the small size of my data set (n < 300)

The accuracy of neural nets paled in comparison to those of much simpler models

Predicting Scores - Decision Tree Analysis
Using decision tree analysis from both the “ctree” and “ctree2” methods indicates that the best predictor of financial literacy scores is gender. Although the decision trees are easy to interpret and emphasize the importance of gender, the fact that the r-squared values hover around 0.15 means that the ctree method explains almost half as much data as the regression models above. As a result, it appears the decision trees are sacrificing some complexity that may in fact be relevant.

models <- list(
  m1_ctree <- train(Score ~ . , data=train_data, method="ctree", trControl = ctrl),
  m2_ctree2 <- train(Score ~ . , data=train_data, method="ctree2", trControl = ctrl)
  )

#comparing the models
results <- resamples(models)
summary(results)
## 
## Call:
## summary.resamples(object = results)
## 
## Models: Model1, Model2 
## Number of resamples: 5 
## 
## MAE 
##            Min.  1st Qu.  Median     Mean  3rd Qu.    Max. NA's
## Model1 1.228505 1.232000 1.25979 1.255002 1.271894 1.28282    0
## Model2 1.189736 1.207673 1.21658 1.240743 1.271525 1.31820    0
## 
## RMSE 
##            Min.  1st Qu.   Median     Mean  3rd Qu.     Max. NA's
## Model1 1.500753 1.516527 1.528005 1.553005 1.604875 1.614865    0
## Model2 1.384765 1.471308 1.481810 1.520844 1.626086 1.640250    0
## 
## Rsquared 
##              Min.    1st Qu.    Median      Mean   3rd Qu.      Max. NA's
## Model1 0.01893222 0.09191583 0.1049585 0.1184998 0.1772996 0.1993931    0
## Model2 0.02658807 0.11477378 0.2051773 0.1584910 0.2128223 0.2330935    0
difs <- diff(results)
summary(difs)
## 
## Call:
## summary.diff.resamples(object = difs)
## 
## p-value adjustment: bonferroni 
## Upper diagonal: estimates of the difference
## Lower diagonal: p-value for H0: difference = 0
## 
## MAE 
##        Model1 Model2 
## Model1        0.01426
## Model2 0.6886        
## 
## RMSE 
##        Model1 Model2 
## Model1        0.03216
## Model2 0.6312        
## 
## Rsquared 
##        Model1 Model2  
## Model1        -0.03999
## Model2 0.5953
Accuracy and Kappa matrices assert that the the Ctree2 tree is more reliable than the regular Ctree tree. Looking at the generated Ctree2 tree allows for many interesting findings:

Gender is the primary tool to identify Fisher populations that may be at risk of having low financial literacy levels. Overall, male students tended to have higher scores than their female counterparts.
Two populations have a very large range in the scores. The first is non-Business female students above the age of 18. The second is non-Business male students that are not in graduate programs. This implies that one should not lump all non-Business majors together and expect to be able to accurately predict scores based on that characteristic alone.
plot((m2_ctree2$finalModel), main = "Ctree2 Model")


Limitations of Predicting Scores
Despite the promising allure of being able to accurately predict students’ exact financial literacy levels, the current data on hand is limited by the fact that there is not enough information on the students surveyed. Although some models were able to predict nearly 30% of differences between scores, 70% of the data is still unexplained. Some of my other research done at Fisher indicates that habits and attitudes developed during childhood play an important role in financial literacy, but without this data collected for participants (as well as factoring in the effect of students merely guessing on questions), the ability of my models to precisely estimate exact scores may be limited.

Predicting Groups - Cluster Analysis
Even if exact scores cannot be predicted, it is useful to understand which student populations at Fisher can be considered “at-risk” of not having adequate financial literacy levels. The second half of my analysis aims to understand which groups of students typically “fail” the financial literacy assessment.

Note: the idea of a specific number of questions that must be answered correctly to be considered as “passing scores” is entirely subjective. For purposes of this research, I consider a score below 7 as “failing”, with grades of 65% or below typically being considered a failing grade in higher education.

I first looked to group students based on similar characteristics utilizing the clustering tools from the “factoextra” package.

library(factoextra)
library(caret)
library(dplyr)
library(skimr)
Using the “elbow method” and automated selection from the “gap_stat” method, I was able to determine the proper number of clusters to be 2.

fviz_nbclust(data[,1:15], kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2)


After generating 2 clusters, the following subpopulation of Fisher students were identified:

“At-Risk Population” with a Score of 5.3: Younger females that have not taken financial courses
“Average Students” with a Score of 8.1: Typical students at Fisher, weighted towards older Business students that have taken a financial course.
score_clusters <- kmeans(data[,1:15], centers=2)
data$cluster <- score_clusters$cluster
score_clusters$size
## [1] 160 110
score_clusters$centers
##     Male  Business   Nursing  Education ArtsandSciences   Pharmacy     Age18
## 1 0.4875 0.4125000 0.1750000 0.05625000       0.2562500 0.10000000 0.1187500
## 2 0.2000 0.1727273 0.2727273 0.09090909       0.3727273 0.09090909 0.2818182
##       Age19     Age20   Age21      Age22 Age23andup   Graduate    Course
## 1 0.1875000 0.2000000 0.16875 0.10625000  0.2187500 0.16875000 0.5062500
## 2 0.2363636 0.1363636 0.20000 0.02727273  0.1181818 0.09090909 0.2454545
##      Score
## 1 8.056250
## 2 5.290909
It is important to note that the clusters are not entirely accurate, as some students may fit into both clusters.

fviz_cluster(score_clusters, data=data)


For example, although young females that have not taken a course are considered “At-Risk”, not all in the cluster failed the assessment.

data %>%
  group_by(Male, Age18, Course) %>%
  summarise(max(Score))
## `summarise()` has grouped output by 'Male', 'Age18'. You can override using the
## `.groups` argument.
Male
<dbl>
Age18
<dbl>
Course
<dbl>
max(Score)
<dbl>
0	0	0	10
0	0	1	9
0	1	0	9
0	1	1	7
1	0	0	10
1	0	1	10
1	1	0	9
1	1	1	9
8 rows
Predicting Groups - Decision Tree Analysis
The decision tree analysis was also brought back to identify “at-risk” groups of students, as opposed to predicting specific scores.

data <- read_excel("C://Users//nml06596//Downloads//Data Set Dummy.xlsx")
colnames(data) <- c('Male','Business','Nursing','Education','ArtsandSciences','Pharmacy',                  'Age18','Age19','Age20','Age21','Age22',"Age23andup",              'FirstYear','Sophomore','Junior','Senior','Graduate',
'Course','Score')
data <- subset(data, select = -c(FirstYear, Sophomore, Junior, Senior))
data$Score <- cut(data$Score, breaks = c(0, 6, 10), labels = c('Fail', 'Pass'))
set.seed(270)
trainIndex <- createDataPartition(data$Score, p = .9, list = FALSE, times = 1)
train_data <- data[ trainIndex,]
test_data  <- data[-trainIndex,]
ctrl <- trainControl(method="cv", n=5)
models <- list(
  m1_ctree <- train(Score ~ ., data=train_data, method="ctree", trControl = ctrl),
  m2_ctree2 <- train(Score ~ ., data=train_data, method="ctree2", trControl = ctrl)
  )

results <- resamples(models)
summary(results)
## 
## Call:
## summary.resamples(object = results)
## 
## Models: Model1, Model2 
## Number of resamples: 5 
## 
## Accuracy 
##             Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
## Model1 0.6326531 0.6326531 0.6875000 0.6751701 0.6938776 0.7291667    0
## Model2 0.5416667 0.5918367 0.6666667 0.6375850 0.6734694 0.7142857    0
## 
## Kappa 
##              Min.   1st Qu.    Median     Mean   3rd Qu.      Max. NA's
## Model1  0.2025316 0.2840909 0.3188137 0.317668 0.3283582 0.4545455    0
## Model2 -0.0134357 0.1680815 0.3134851 0.242017 0.3333333 0.4086207    0
difs <- diff(results)
summary(difs)
## 
## Call:
## summary.diff.resamples(object = difs)
## 
## p-value adjustment: bonferroni 
## Upper diagonal: estimates of the difference
## Lower diagonal: p-value for H0: difference = 0
## 
## Accuracy 
##        Model1 Model2 
## Model1        0.03759
## Model2 0.1118        
## 
## Kappa 
##        Model1 Model2 
## Model1        0.07565
## Model2 0.3203
Accuracy and Kappa matrices assert that the the Ctree2 tree is more reliable than the regular Ctree tree. Looking at the generated Ctree2 tree allows for many intersting findings:

Gender is the primary tool to identify Fisher populations that may be at risk of having low financial literacy levels. Whereas a majority of each male subpopulation passed the assessments, a minority of women passed the assessment in each female subpopulation except for one.
There is a one female group that had high passing rates: females above the age of 18 that have taken a financial course. This indicates that involvement in a financial course may be relevant for reducing the risk of female populations having low financial literacy levels.
In both male and female populations, being aged 18 was significant enough to generate specific tree pathways with lower pass rates. This indicates that for both genders, being of a young age puts students at a higher risk of having lower financial literacy.
plot((m2_ctree2$finalModel), main = "Ctree2 Model")


Conclusion and Next Steps
In many ways, this extension of my original research was promising in some ways, but unfruitful in others. In terms of findings, it confirms that the included explanatory variables are relevant for understanding the financial literacy of students at St. John Fisher University. This is particularly true when looking at the gender and age variables, with machine learning confirming that findings of female students and younger students having lower financial literacy are accurate and not a result of researcher bias. In addition, tools such as decision trees are incredibly powerful tools for illustrating effects of various variables on financial literacy levels.

From a policy perspective at Fisher, this research lends support for establishing initiatives for improving financial literacy levels of certain student populations, specifically young female students typically in the School of Nursing. Potential programs could include embedding a financial literacy “crash course” into the SJF Nursing program. Although some will look at my analysis to claim that financial courses had a negligible effect on financial literacy courses, this may be attributed to the fact that these effective courses are seldom offered to and participated in by those in “at-risk” populations.

However, my research makes it clear that machine learning and advanced regression techniques cannot solve a basic issue with the data set: not enough variables. The fact that even the best models can only explain 30% of the differences in financial literacy scores implies that better models can only be developed with the inclusion of more information about participants. I suspect that further research that incorporates variables about participants’ socioeconomic status and habits reinforced during their upbringing may be far more accurate at predicting financial literacy levels and detecting “at-risk” populations within the Fisher community.

Works Cited
Ramos-Hernández, Jésica Josefina, et al. (2020, March 5). Financial Literacy Level on College Students: A Comparative Descriptive Analysis between Mexico and Colombia. European Journal of Contemporary Education, vol. 9, no. 1. 10.13187/ejced.2020.1.126.

Amagir, Aisa, et al. (2017, July 13). A Review of Financial-Literacy Education Programs for Children and Adolescents. Citizenship, Social, and Economics Education, vol. 17, no. 1, Pages 56–80. 10.1177/2047173417719555.

Knoll, Melissa A. Z., and Carrie R. Houts. (2012, August 30). The Financial Knowledge Scale: An Application of Item Response Theory to the Assessment of Financial Literacy. Journal of Consumer Affairs, vol. 46, no. 3, Pages 381-410. 10.1111/j.1745-6606.2012.01241.x.

Knoll, Melissa A. Z., and Carrie R. Houts. (2019, December 3). The Financial Knowledge Scale: New Analyses, Finding, and Development of a Short Form. Journal of Consumer Affairs, vol. 54, no. 2. 10.1111/joca.12288.
