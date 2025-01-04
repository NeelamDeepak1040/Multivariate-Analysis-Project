library(MASS)
library(caret)
library(readxl)
library(pROC)
library(MVTests)
library(car) 
library(ggplot2) 
library(glmnet) 
library(FactoMineR) 
library(factoextra) 
library(boot) 
library(corrplot) 


## Read in Data
setwd("C:/Users/Ntatk/Downloads") 
sleep <- read_excel("Sleep_health_and_lifestyle_dataset.xlsx") 


##############################################
########## Data Preprocessing & EDA ##########
##############################################

sleep <- sleep[, -which(names(sleep) == "Person ID")]
sleep$Sleep_Disorder_Binary <- ifelse(sleep$`Sleep Disorder` %in% c("Insomnia", "Sleep Apnea"), 1, 0) 
table(sleep$Sleep_Disorder_Binary) 
sleep$Sleep_Disorder_Binary <- as.factor(sleep$Sleep_Disorder_Binary)
sleep$BMI_Binary <- ifelse(sleep$`BMI Category` %in% c("Obese", "Overweight"), 1, 0) 
table(sleep$BMI_Binary) 

sleep$Systolic <- as.numeric(sapply(strsplit(as.character(sleep$`Blood Pressure`), "/"), `[`, 1))
sleep$Diastolic <- as.numeric(sapply(strsplit(as.character(sleep$`Blood Pressure`), "/"), `[`, 2))
sleep$Blood_Pressure_Binary <- with(sleep, ifelse(Systolic >= 130 | Diastolic >= 80, 1, 0)) 

table(sleep$Blood_Pressure_Binary) 


#Group the occupations  
sleep$Occupation_Group <- ifelse(sleep$Occupation %in% c("Doctor", "Nurse"), "Health-related", 
                                 
                                 ifelse(sleep$Occupation %in% c("Software Engineer", "Scientist", "Engineer"), "IT/Tech", 
                                        
                                        ifelse(sleep$Occupation %in% c("Sales Representative", "Manager", "Sales Person", "Accountant"), "Sales/Finance", 
                                               
                                               "Other"))) 

table(sleep$Occupation_Group) 
sleep$Occupation_Group <- as.factor(sleep$Occupation_Group) 

#Plot occupation group vs sleep disorder 
ggplot(sleep, aes(x = Occupation_Group, fill = as.factor(Sleep_Disorder_Binary))) + 
  geom_bar(position = "fill") + 
  labs(x = "Occupation Group", y = "Proportion", fill = "Sleep Disorder (0 = None, 1 = Disorder)") + 
  theme_minimal() 

ggplot(sleep, aes(x = Occupation, fill = as.factor(Sleep_Disorder_Binary))) + 
  geom_bar(position = "fill") + 
  labs(x = "Occupation Group", y = "Proportion", fill = "Sleep Disorder (0 = None, 1 = Disorder)") + 
  theme_minimal() 

#Convert variables to factor 
sleep$Gender <- as.factor(sleep$Gender) 
sleep$BMI_Binary <- as.factor(sleep$BMI_Binary) 
sleep$Blood_Pressure_Binary <- as.factor(sleep$Blood_Pressure_Binary) 
sleep$Occupation_Group <- relevel(sleep$Occupation_Group, ref = "Other") 

#Correlation matrix 
cor_matrix <- cor(sleep[, c("Age", "Stress Level")]) 
cor_matrix 
corr_data <- sleep[, c("Age","Sleep Duration","Quality of Sleep", "Physical Activity Level", "Stress Level",
                       "Heart Rate", "Daily Steps", "Systolic", "Diastolic")] 

cor_matrix <- cor(corr_data) 
cor_matrix 
corrplot(cor_matrix, method = "circle", type = "lower", tl.col = "black", tl.cex = 0.8, title = "Correlation Heatmap") 

#What Effect Does Occupation Have on Physical Activity Level and How Does This, in turn, Affect Sleep Duration? 
# box plot before categorizing 
ggplot(sleep, aes(x = Occupation, y = `Physical Activity Level`, fill = Occupation)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2, width = 0.6) +
  labs( 
    title = "Physical Activity Levels by Occupation", 
    x = "Occupation", 
    y = "Physical Activity (minutes/day)" 
  ) + 
  theme_minimal() +  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.title = element_text(size = 11, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5), 
    plot.subtitle = element_text(size = 10, face = "italic", hjust = 0.5)
  ) + 
  scale_fill_manual(values = c( 
    "#FF9999", "#99CCFF", "#FFCC99", "#CCFF99", "#CCCCFF", 
    "#FF6666", "#66CCFF", "#FF9966", "#66FF66", "#9966FF",  
    "#FFCC66", "#6699FF" 
  ))  

# box plot after grouping 
names(sleep) 

ggplot(sleep, aes(x = Occupation_Group, y = `Physical Activity Level`, fill = Occupation_Group)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2, width = 0.6) +
  labs( 
    title = "Physical Activity Levels by Occupation Group", 
    x = "Occupation Group", 
    y = "Physical Activity (minutes/day)" 
  ) + 
  theme_minimal() +  
  theme( 
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.title = element_text(size = 11, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, face = "italic", hjust = 0.5) 
  ) +
  scale_fill_manual(values = c( 
    "#FF9999", "#99CCFF", "#FFCC99", "#CCFF99", "#CCCCFF", 
    "#FF6666", "#66CCFF", "#FF9966", "#66FF66", "#9966FF",  
    "#FFCC66", "#6699FF" 
  ))  

#########################
########## PCA ##########
#########################

# Variables for PCA 
pca_data <- sleep[, c("Age", "Physical Activity Level", "Stress Level", "Heart Rate", "Daily Steps")] 

# Handle missing values if any 
pca_data <- na.omit(pca_data) 

# Perform PCA using FactoMineR 
pca_result <- PCA(pca_data, scale.unit = TRUE, graph = FALSE) 
pca_result$eig 

# Scree Plot 
fviz_screeplot(pca_result, addlabels = TRUE, ylim = c(0, 50), ggtheme = theme_minimal()) + 
  labs(title = "Scree Plot", x = "Dimensions", y = "Percentage of Explained Variance") 

# Biplot with cleaner aesthetics 
fviz_pca_biplot(pca_result, repel = TRUE, 
                geom.ind = "point",  
                pointsize = 2,       
                col.ind = sleep$`Quality of Sleep`,  
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),  
                col.var = "black",  
                arrowsize = 1,      
                labelsize = 4,      
                ggtheme = theme_minimal()) + 
  labs(title = "PCA - Biplot", x = "PC1 (38.4%)", y = "PC2 (36%)") + 
  theme(legend.position = "right")  



#################################################
########## Logistic Regression & Lasso ##########
#################################################

#Fit logistic regression model with all predictors 

full_logistic_model <- glm(Sleep_Disorder_Binary ~ Gender + Age + Occupation_Group + sleep$`Sleep Duration` + sleep$`Quality of Sleep` + sleep$`Physical Activity Level` + sleep$`Stress Level` + sleep$`Heart Rate` + sleep$`Daily Steps` + BMI_Binary + Blood_Pressure_Binary, data = sleep, family = "binomial") 
summary(full_logistic_model) 

#Splitting data into training and testing sets 
set.seed(123) 
train_index <- createDataPartition(sleep$Sleep_Disorder_Binary, p = 0.8, list = FALSE) 
train_data <- sleep[train_index, ] 
test_data <- sleep[-train_index, ] 

#Lasso Model
x_train <- model.matrix(Sleep_Disorder_Binary ~ Gender + Age + Occupation_Group + train_data$`Sleep Duration` + train_data$`Quality of Sleep` + train_data$`Physical Activity Level` + train_data$`Stress Level` + train_data$`Heart Rate` + train_data$`Daily Steps` + BMI_Binary + Blood_Pressure_Binary, data = train_data)[, -1] 
y_train <- train_data$Sleep_Disorder_Binary 

x_test <- model.matrix(Sleep_Disorder_Binary ~ Gender + Age + Occupation_Group + test_data$`Sleep Duration` + test_data$`Quality of Sleep` + test_data$`Physical Activity Level` + test_data$`Stress Level` + test_data$`Heart Rate` + test_data$`Daily Steps` + BMI_Binary + Blood_Pressure_Binary, data = test_data)[, -1] 
y_test <- test_data$Sleep_Disorder_Binary 

#Fit Lasso model 
lasso_model <- cv.glmnet(x_train, y_train, alpha = 1, family = "binomial") 
plot(lasso_model) 
coef(lasso_model, s = "lambda.min") 
lasso_model$lambda.min 
pred_probs <- predict(lasso_model, newx = x_test, s = "lambda.min", type = "response")

#Convert probabilities to binary predictions 
pred_class <- ifelse(pred_probs > 0.5, 1, 0) 
confusion_matrix <- table(Predicted = pred_class, Actual = y_test) 
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix) 
TP <- confusion_matrix[2, 2] # True Positives 
FP <- confusion_matrix[2, 1] # False Positives 
precision_full <- TP / (TP + FP) 
print(paste("Precision (Full Lasso):", round(precision_full, 4))) 
pred_probs_lasso_all <- as.vector(predict(lasso_model, newx = x_test, s = "lambda.min", type = "response")) 

#ROC curve for Lasso model 
roc_lasso_all <- roc(y_test, pred_probs_lasso_all) 

#AUC for Lasso model 
auc_lasso_all <- auc(roc_lasso_all) 
print(paste("AUC (Full Lasso):", round(auc_lasso_all, 4))) 
plot(roc_lasso_all, main = "ROC Curve for Full Lasso Model") 

#Lasso with significant predictors 
x_train_simplified <- model.matrix(Sleep_Disorder_Binary ~ Age + BMI_Binary + train_data$'Quality of Sleep' + Occupation_Group , data = train_data)[, -1]
y_train_simplified <- train_data$Sleep_Disorder_Binary 

x_test_simplified <- model.matrix(Sleep_Disorder_Binary ~ Age + BMI_Binary + test_data$'Quality of Sleep' + Occupation_Group, data = test_data)[, -1]
y_test_simplified <- test_data$Sleep_Disorder_Binary 

lasso_model_simplified <- cv.glmnet(x_train_simplified, y_train_simplified, alpha = 1, family = "binomial")
plot(lasso_model_simplified) 
lasso_coefficients <- coef(lasso_model_simplified, s = "lambda.min") 
print(lasso_coefficients) 
lasso_model_simplified$lambda.min 

pred_probs_lasso_simplified <- predict(lasso_model_simplified, newx = x_test_simplified, s = "lambda.min", type = "response") 

pred_class_lasso_simplified <- ifelse(pred_probs_lasso_simplified > 0.5, 1, 0) 
confusion_matrix_lasso_simplified <- table(Predicted = pred_class_lasso_simplified, Actual = y_test_simplified) 
accuracy_lasso_simplified <- sum(diag(confusion_matrix_lasso_simplified)) / sum(confusion_matrix_lasso_simplified) 
TP_simplified <- confusion_matrix_lasso_simplified[2, 2] #true positives 
FP_simplified <- confusion_matrix_lasso_simplified[2, 1] #false positives 

#Precision 
precision_simplified <- TP_simplified / (TP_simplified + FP_simplified) 
print(paste("Precision (Simplified Lasso):", round(precision_simplified, 4))) 


#AUC for simplified Lasso model 
roc_lasso_simplified <- roc(y_test_simplified, pred_probs_lasso_simplified) 
auc_lasso_simplified <- auc(roc_lasso_simplified) 
print(paste("AUC (Simplified Lasso):", round(auc_lasso_simplified, 4))) 

plot(roc_lasso_simplified, main = "ROC Curve for Simplified Lasso Model") 
pred_probs_lasso_all <- predict(lasso_model, newx = x_test, s = "lambda.min", type = "response") 
roc_lasso_all <- roc(y_test, pred_probs_lasso_all) 
auc_lasso_all <- auc(roc_lasso_all) 
print(paste("AUC (Full Lasso):", round(auc_lasso_all, 4))) 

plot(roc_lasso_all, col = "blue", main = "ROC Curve Comparison", lwd = 2) 
plot(roc_lasso_simplified, col = "red", add = TRUE, lwd = 2) 
legend("bottomright", legend = c("Full Lasso", "Simplified Lasso"), col = c("blue", "red"), lwd = 2) 



#########################################
########## Comparison of means ##########
#########################################

## Box M Test
BoxM(cbind(sleep$Age,sleep$'Sleep Duration', sleep$'Quality of Sleep', sleep$'Physical Activity Level', sleep$'Stress Level', sleep$'Heart Rate', sleep$'Daily Steps'), sleep$Sleep_Disorder_Binary)
BoxM(cbind(sleep$'Physical Activity Level', sleep$'Daily Steps'), sleep$Sleep_Disorder_Binary)

### All Continuous Variables
alpha = 0.05
sleep_y <- sleep[sleep$Sleep_Disorder_Binary == 1,c('Age', 'Sleep Duration', 'Quality of Sleep', 'Physical Activity Level', 'Stress Level', 'Heart Rate', 'Daily Steps')]
sleep_n <- sleep[sleep$Sleep_Disorder_Binary == 0,c('Age', 'Sleep Duration', 'Quality of Sleep', 'Physical Activity Level', 'Stress Level', 'Heart Rate', 'Daily Steps')]
n1 <- length(sleep_y)
n2 <- length(sleep_n)
p=6
xbar1 <- colMeans(sleep_y)
xbar2 <- colMeans(sleep_n)
S1 <- cov(sleep_y)
S2 <- cov(sleep_n)

Se = S1/n1+S2/n2
T2 = t(xbar1-xbar2)%*%solve(Se)%*%(xbar1-xbar2)
c5 = qchisq(1-alpha,p)
c(T2, c5)

### Physical Fitness Variables
alpha = 0.05
sleep_y <- sleep[sleep$Sleep_Disorder_Binary == 1,c('Physical Activity Level', 'Daily Steps')]
sleep_n <- sleep[sleep$Sleep_Disorder_Binary == 0,c('Physical Activity Level', 'Daily Steps')]
n1 <- length(sleep_y)
n2 <- length(sleep_n)
p=2
xbar1 <- colMeans(sleep_y)
xbar2 <- colMeans(sleep_n)
S1 <- cov(sleep_y)
S2 <- cov(sleep_n)

Se = S1/n1+S2/n2
T2 = t(xbar1-xbar2)%*%solve(Se)%*%(xbar1-xbar2)
c5 = qchisq(1-alpha,p)
c(T2, c5)


#############################
########## LDA/QDA ##########
#############################

train_indices <- createDataPartition(sleep$'Sleep Disorder', list = FALSE, p = 0.7, times = 1)

train <- sleep[train_indices, ]
names(train)<-make.names(names(train),unique = TRUE)
test <- sleep[-train_indices, ]
names(test)<-make.names(names(test),unique = TRUE)

## LDA
lda = lda(Sleep_Disorder_Binary ~ Age + Sleep.Duration + Quality.of.Sleep + Physical.Activity.Level + Stress.Level + Heart.Rate + Daily.Steps, data=train, prior=c(1,1)/2)

lda_pred = predict(lda,test)

confusionMatrix(data = lda_pred$class, reference = test$Sleep_Disorder_Binary)

lda_roc = roc(test$Sleep_Disorder_Binary, lda_pred$posterior[,1])
plot(lda_roc)
auc(lda_roc)

## QDA
qda = qda(Sleep_Disorder_Binary ~ Age + Sleep.Duration + Quality.of.Sleep + Physical.Activity.Level + Stress.Level + Heart.Rate + Daily.Steps, data=train, prior=c(1,1)/2)

qda_pred = predict(qda,test)

confusionMatrix(data = qda_pred$class, reference = test$Sleep_Disorder_Binary)
qda_roc = roc(test$Sleep_Disorder_Binary, qda_pred$posterior[,1])
plot(qda_roc)
auc(qda_roc)















