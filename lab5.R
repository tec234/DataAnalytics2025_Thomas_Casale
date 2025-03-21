> library(e1071)
Warning message:
package ‘e1071’ was built under R version 4.4.3 
> library(caret)
Loading required package: ggplot2
Loading required package: lattice
> data(wime)
Warning message:
In data(wime) : data set ‘wime’ not found
> data(wine)
Warning message:
In data(wine) : data set ‘wine’ not found
> View(wine)
> features <- wine[, -ncol(wine)]
> target <- wine$Class
> set.seed(123)
> trainIndex <- createDataPartition(target, p = 0.8, list = FALSE)
> trainData <- features[trainIndex, ]
> trainLabels <- target[trainIndex]
> testData <- features[-trainIndex, ]
> testLabels <- target[-trainIndex]
> trainDataScaled <- scale(trainData)
> testDataScaled <- scale(testData)
> svmLinear <- svm(trainDataScaled, as.factor(trainLabels), kernel = "linear")
> 
> tuneLinear <- tune.svm(trainDataScaled, as.factor(trainLabels), kernel = "linear",
+                        ranges = list(cost = 2^(2:9))) 
Error in tune("svm", train.x = x, train.y = y, ranges = ranges, ...) : 
  formal argument "ranges" matched by multiple actual arguments
> tuneLinear <- tune.svm(trainDataScaled, as.factor(trainLabels), kernel = "linear",
+                        cost = 2^(2:9))
> bestLinearModel <- tuneLinear$best.model
> cat("Best C for linear SVM:", tuneLinear$best.parameters$cost, "\n")
Best C for linear SVM: 4 
> svmRBF <- svm(trainDataScaled, as.factor(trainLabels), kernel = "radial")
> 
> tuneRBF <- tune.svm(trainDataScaled, as.factor(trainLabels), kernel = "radial",
+                     cost = 2^(2:9), gamma = 2^(-7:4))
> bestRBFModel <- tuneRBF$best.model
> cat("Best C for RBF SVM:", tuneRBF$best.parameters$cost, "\n")
Best C for RBF SVM: 4 
> cat("Best Gamma for RBF SVM:", tuneRBF$best.parameters$gamma, "\n")
Best Gamma for RBF SVM: 0.0078125 
> predLinear <- predict(bestLinearModel, testDataScaled)
> confMatrixLinear <- confusionMatrix(predLinear, as.factor(testLabels))
> cat("Linear SVM Confusion Matrix:\n")
Linear SVM Confusion Matrix:
> print(confMatrixLinear)
Confusion Matrix and Statistics

          Reference
Prediction  1  2  3
         1 12  0  0
         2  0 14  0
         3  0  0  9

Overall Statistics
                                   
               Accuracy : 1        
                 95% CI : (0.9, 1) 
    No Information Rate : 0.4      
    P-Value [Acc > NIR] : 1.181e-14
                                   
                  Kappa : 1        
                                   
 Mcnemar's Test P-Value : NA       

Statistics by Class:

                     Class: 1 Class: 2 Class: 3
Sensitivity            1.0000      1.0   1.0000
Specificity            1.0000      1.0   1.0000
Pos Pred Value         1.0000      1.0   1.0000
Neg Pred Value         1.0000      1.0   1.0000
Prevalence             0.3429      0.4   0.2571
Detection Rate         0.3429      0.4   0.2571
Detection Prevalence   0.3429      0.4   0.2571
Balanced Accuracy      1.0000      1.0   1.0000
> predRBF <- predict(bestRBFModel, testDataScaled)
> confMatrixRBF <- confusionMatrix(predRBF, as.factor(testLabels))
> cat("RBF SVM Confusion Matrix:\n")
RBF SVM Confusion Matrix:
> print(confMatrixRBF)
Confusion Matrix and Statistics

          Reference
Prediction  1  2  3
         1 12  0  0
         2  0 14  0
         3  0  0  9

Overall Statistics
                                   
               Accuracy : 1        
                 95% CI : (0.9, 1) 
    No Information Rate : 0.4      
    P-Value [Acc > NIR] : 1.181e-14
                                   
                  Kappa : 1        
                                   
 Mcnemar's Test P-Value : NA       

Statistics by Class:

                     Class: 1 Class: 2 Class: 3
Sensitivity            1.0000      1.0   1.0000
Specificity            1.0000      1.0   1.0000
Pos Pred Value         1.0000      1.0   1.0000
Neg Pred Value         1.0000      1.0   1.0000
Prevalence             0.3429      0.4   0.2571
Detection Rate         0.3429      0.4   0.2571
Detection Prevalence   0.3429      0.4   0.2571
Balanced Accuracy      1.0000      1.0   1.0000
> library(class)
> predKNN <- knn(trainDataScaled, testDataScaled, trainLabels, k = 3)
> confMatrixLinear <- confusionMatrix(predLinear, as.factor(testLabels))
> confMatrixRBF <- confusionMatrix(predRBF, as.factor(testLabels))
> confMatrixKNN <- confusionMatrix(predKNN, as.factor(testLabels))
> cat("Linear SVM Performance:\n")
Linear SVM Performance:
> print(confMatrixLinear$byClass[, c("Precision", "Recall", "F1")])
         Precision Recall F1
Class: 1         1      1  1
Class: 2         1      1  1
Class: 3         1      1  1
> 
> cat("\nRBF SVM Performance:\n")

RBF SVM Performance:
> print(confMatrixRBF$byClass[, c("Precision", "Recall", "F1")])
         Precision Recall F1
Class: 1         1      1  1
Class: 2         1      1  1
Class: 3         1      1  1
> 
> cat("\nkNN Performance:\n")

kNN Performance:
> print(confMatrixKNN$byClass[, c("Precision", "Recall", "F1")])
         Precision Recall F1
Class: 1         1      1  1
Class: 2         1      1  1
Class: 3         1      1  1
> library(readr)
> NY_House_Dataset <- read_csv("C:/Users/Thomas/Dropbox/My PC (DESKTOP-2A9JECI)/Downloads/NY-House-Dataset.csv")
Rows: 4801 Columns: 17                                                                          
── Column specification ────────────────────────────────────────────────────────────────────────
Delimiter: ","
chr (11): BROKERTITLE, TYPE, ADDRESS, STATE, MAIN_ADDRESS, ADMINISTRATIVE_AREA_LEVEL_2, LOCA...
dbl  (6): PRICE, BEDS, BATH, PROPERTYSQFT, LATITUDE, LONGITUDE

ℹ Use `spec()` to retrieve the full column specification for this data.
ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
> View(NY_House_Dataset)
> library(ggplot2) 
> set.seed(123)
> n <- 100
> square_footage <- runif(n, 800, 5000)  
> price <- square_footage * 200 + rnorm(n, mean = 0, sd = 50000)  
> 
> housing_data <- data.frame(SquareFootage = square_footage, Price = price)
> trainIndex <- createDataPartition(housing_data$Price, p = 0.8, list = FALSE)
> trainData <- housing_data[trainIndex, ]
> testData <- housing_data[-trainIndex, ]
> svm_model <- svm(Price ~ SquareFootage, data = trainData)
> svm_pred <- predict(svm_model, testData)
> lm_model <- lm(Price ~ SquareFootage, data = trainData)
> lm_pred <- predict(lm_model, testData)
> 
> ggplot(testData, aes(x = Price, y = svm_pred)) +
+     geom_point(color = 'blue') +
+     geom_abline(slope = 1, intercept = 0, color = 'red') +
+     labs(title = "SVM: Predicted vs Real Price", x = "Real Price", y = "Predicted Price")
> ggplot(testData, aes(x = Price, y = lm_pred)) +
+     geom_point(color = 'green') +
+     geom_abline(slope = 1, intercept = 0, color = 'red') +
+     labs(title = "Linear Model: Predicted vs Real Price", x = "Real Price", y = "Predicted Price")
> svm_residuals <- testData$Price - svm_pred
> lm_residuals <- testData$Price - lm_pred
> ggplot(data.frame(Residuals = svm_residuals), aes(x = Residuals)) +
+     geom_histogram(binwidth = 50000, fill = 'blue', color = 'black', alpha = 0.7) +
+     labs(title = "Residuals for SVM Regression", x = "Residuals", y = "Frequency")
> ggplot(data.frame(Residuals = lm_residuals), aes(x = Residuals)) +
+     geom_histogram(binwidth = 50000, fill = 'green', color = 'black', alpha = 0.7) +
+     labs(title = "Residuals for Linear Regression", x = "Residuals", y = "Frequency")
> 