# STAT628-Module2-Group5-2019
# Summary of Project
Our project focuses on the measure of percentage of body fat. In this module, we will come up with a simple, robust, accurate and precise “rule-of-thumb” method to estimate percentage of body fat.
Our project can be divided into 6 steps:

1.Data Pre-processing

2.Feature Selection

3.Model Construction & Evaluation

4.Model Diagnosis

5.Conclusions

6.Application on Shiny

Firstly, we get a general idea of the dataset and start to clean up the data by using scatterplot, leverage, cooks distance, outliertest and diffits. Finally, we recovery one mismeasure point, and delete 6 points as well.

Secondly, we need to select variables for our model. We also use various methods, such as mallow's cp, adjusted R^2, AIC, BIC and VIF. Then, we get different subsets of variables under different methods.

After that, we need to evaluate our model and choose the best to construct our final model. In this part, we use 2 methods to evaluate each model with different choice of variables: R^2 and MSE. By trading off between performance and cost.
For R^2, we can see the the model with 2 variables ABDOMEN and WRIST, its R^2 is 0.7058, not much smaller than the model with all variables, but on the other hand it just needs 2 variables, and costs much less than other models. In this way, we choose ABDOMEN and WRIST to be model variables and construct our model.
Also, for MSE, we can conclude that MSE of 2 variables model is 18.53%, not much larger than that of 3 variables model. So still trade off between cost and accuracy, we decide to choose the 2 variable model: BODYFAT ~ ABDOMEN + WRIST.

Next, we need model diagnosis. We need to check whether our model satisfies the assumptions of normality, equal variance, independence.

Finally, we can put our model into application and we achieve this on shiny.
