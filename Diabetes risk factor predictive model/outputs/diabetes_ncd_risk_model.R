## =============================================================
## NCD Risk Prediction Model
## Predicting elevated risk of diabetes (a leading NCD) from
## routinely collected clinical/metabolic markers
##
## Dataset: Pima Indians Diabetes Dataset
##   - 768 female patients (Pima Heritage, Arizona, USA), age 21+
##   - Outcome: diabetes diagnosis (binary).
##   - Source: UCI Machine Learning Repository / National Institute
##     of Diabetes and Digestive and Kidney Diseases
## =============================================================

set.seed(123)

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(caret)
  library(randomForest)
  library(ranger)
  library(glmnet)
  library(pROC)
  library(corrplot)
})

dir.create("outputs", showWarnings = FALSE)

## Load data

col_names <- c("Pregnancies", "Glucose", "BloodPressure", "SkinThickness",
               "Insulin", "BMI", "DiabetesPedigreeFunction", "Age", "Outcome")

df <- read.csv("diabetes_raw.csv", header = FALSE, col.names = col_names)

cat("Rows:", nrow(df), " Cols:", ncol(df), "\n")
str(df)

## CLEANING DATA --------------

## In this dataset, biologically implausible zeros (0 glucose, 0 BMI, 0 blood pressure, etc.) are actually missing values encoded as 0. I recode them as NA, then impute

zero_as_na_cols <- c("Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI")

df_clean <- df
df_clean[zero_as_na_cols] <- lapply(df_clean[zero_as_na_cols], function(x) {
  x[x == 0] <- NA
  x
})

missing_summary <- sapply(df_clean, function(x) sum(is.na(x)))
cat("\nMissing values per column:\n")
print(missing_summary)

## Median imputation

for (col in zero_as_na_cols) {
  med <- median(df_clean[[col]], na.rm = TRUE)
  df_clean[[col]][is.na(df_clean[[col]])] <- med
}

## Outcome as a proper factor for classification

df_clean$Outcome <- factor(df_clean$Outcome, levels = c(0, 1),
                            labels = c("NoDiabetes", "Diabetes"))

cat("\nClass balance:\n")
print(table(df_clean$Outcome))
print(round(prop.table(table(df_clean$Outcome)) * 100, 1))

## EXPLORATORY DATA ANALYSIS -----------------

## Correlation matrix among numeric predictors

png("outputs/01_correlation_matrix.png", width = 900, height = 900, res = 130)
corr_mat <- cor(df_clean %>% select(-Outcome))
corrplot(corr_mat, method = "color", type = "upper", addCoef.col = "black",
         number.cex = 0.7, tl.col = "black", tl.srt = 45,
         title = "Correlation Matrix of NCD Risk Predictors", mar = c(0,0,2,0))
dev.off()

## Key predictor distributions by outcome

p1 <- ggplot(df_clean, aes(x = Glucose, fill = Outcome)) +
  geom_density(alpha = 0.5) +
  labs(title = "Glucose Distribution by Diabetes Status",
       x = "Plasma Glucose (mg/dL)", y = "Density") +
  theme_minimal()
ggsave("outputs/02_glucose_distribution.png", p1, width = 7, height = 5)

p2 <- ggplot(df_clean, aes(x = Outcome, y = BMI, fill = Outcome)) +
  geom_boxplot() +
  labs(title = "BMI by Diabetes Status", x = "", y = "BMI") +
  theme_minimal() + theme(legend.position = "none")
ggsave("outputs/03_bmi_boxplot.png", p2, width = 6, height = 5)

p3 <- ggplot(df_clean, aes(x = Age, fill = Outcome)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
  labs(title = "Age Distribution by Diabetes Status", x = "Age (years)", y = "Count") +
  theme_minimal()
ggsave("outputs/04_age_distribution.png", p3, width = 7, height = 5)

## 4. TRAIN / TEST SPLIT ----------------

train_index <- createDataPartition(df_clean$Outcome, p = 0.8, list = FALSE)
train_data <- df_clean[train_index, ]
test_data  <- df_clean[-train_index, ]

cat("\nTrain n =", nrow(train_data), " Test n =", nrow(test_data), "\n")

## 3 MODEL TRAINING ---------------

ctrl <- trainControl(method = "cv", number = 5,
                      classProbs = TRUE,
                      summaryFunction = twoClassSummary,
                      savePredictions = "final")

## 1. Baseline: Logistic Regression (clinically standard, interpretable)
set.seed(123)
model_glm <- train(Outcome ~ ., data = train_data,
                    method = "glm", family = "binomial",
                    trControl = ctrl, metric = "ROC")

## 2. Elastic Net logistic regression (regularized, handles correlated predictors)
set.seed(123)
model_glmnet <- train(Outcome ~ ., data = train_data,
                       method = "glmnet",
                       trControl = ctrl, metric = "ROC",
                       tuneLength = 5)

## 3. Random Forest (captures non-linear interactions)
set.seed(123)
model_rf <- train(Outcome ~ ., data = train_data,
                   method = "rf",
                   trControl = ctrl, metric = "ROC",
                   tuneLength = 3, importance = TRUE)

results_cv <- resamples(list(Logistic = model_glm,
                              ElasticNet = model_glmnet,
                              RandomForest = model_rf))
print(summary(results_cv))

png("outputs/05_cv_model_comparison.png", width = 800, height = 500, res = 120)
bwplot(results_cv, main = "Cross-Validated Model Comparison (ROC-AUC)")
dev.off()


## TEST SET EVALUATION -----------

evaluate_model <- function(model, test_data, model_name) {
  probs <- predict(model, newdata = test_data, type = "prob")[, "Diabetes"]
  preds <- predict(model, newdata = test_data)

  cm <- confusionMatrix(preds, test_data$Outcome, positive = "Diabetes")
  roc_obj <- roc(response = test_data$Outcome, predictor = probs,
                  levels = c("NoDiabetes", "Diabetes"), quiet = TRUE)

  list(
    name = model_name,
    confusion_matrix = cm,
    auc = as.numeric(auc(roc_obj)),
    roc_obj = roc_obj,
    sensitivity = cm$byClass["Sensitivity"],
    specificity = cm$byClass["Specificity"],
    ppv = cm$byClass["Pos Pred Value"],
    npv = cm$byClass["Neg Pred Value"],
    f1 = cm$byClass["F1"]
  )
}

eval_glm    <- evaluate_model(model_glm, test_data, "Logistic Regression")
eval_glmnet <- evaluate_model(model_glmnet, test_data, "Elastic Net")
eval_rf     <- evaluate_model(model_rf, test_data, "Random Forest")

summary_table <- data.frame(
  Model = c(eval_glm$name, eval_glmnet$name, eval_rf$name),
  AUC = round(c(eval_glm$auc, eval_glmnet$auc, eval_rf$auc), 3),
  Sensitivity = round(c(eval_glm$sensitivity, eval_glmnet$sensitivity, eval_rf$sensitivity), 3),
  Specificity = round(c(eval_glm$specificity, eval_glmnet$specificity, eval_rf$specificity), 3),
  PPV = round(c(eval_glm$ppv, eval_glmnet$ppv, eval_rf$ppv), 3),
  NPV = round(c(eval_glm$npv, eval_glmnet$npv, eval_rf$npv), 3),
  F1 = round(c(eval_glm$f1, eval_glmnet$f1, eval_rf$f1), 3)
)

print(summary_table)
write.csv(summary_table, "outputs/06_test_performance_summary.csv", row.names = FALSE)

## ROC curve comparison plot

png("outputs/07_roc_curves.png", width = 800, height = 700, res = 120)
plot(eval_glm$roc_obj, col = "#1f77b4", lwd = 2,
     main = "ROC Curves: NCD (Diabetes) Risk Prediction Models")
plot(eval_glmnet$roc_obj, col = "#ff7f0e", lwd = 2, add = TRUE)
plot(eval_rf$roc_obj, col = "#2ca02c", lwd = 2, add = TRUE)
legend("bottomright",
       legend = c(sprintf("Logistic (AUC = %.3f)", eval_glm$auc),
                  sprintf("Elastic Net (AUC = %.3f)", eval_glmnet$auc),
                  sprintf("Random Forest (AUC = %.3f)", eval_rf$auc)),
       col = c("#1f77b4", "#ff7f0e", "#2ca02c"), lwd = 2)
dev.off()

## INTERPRETABILITY -------------

## Logistic regression coefficients (odds ratios) 

final_glm <- model_glm$finalModel
or_table <- data.frame(
  Predictor = names(coef(final_glm)),
  Coefficient = round(coef(final_glm), 4),
  OddsRatio = round(exp(coef(final_glm)), 3)
)

print(or_table)
write.csv(or_table, "outputs/08_logistic_odds_ratios.csv", row.names = FALSE)

## Random Forest variable importance

rf_imp <- varImp(model_rf)$importance
if (!"Overall" %in% names(rf_imp)) {
  rf_imp$Overall <- rf_imp[["Diabetes"]]
}
rf_imp$Predictor <- rownames(rf_imp)
rf_imp <- rf_imp[order(-rf_imp$Overall), ]

png("outputs/09_rf_variable_importance.png", width = 800, height = 500, res = 120)
print(plot(varImp(model_rf), main = "Random Forest: Variable Importance"))
dev.off()

print(rf_imp)

## SELECT AND SAVE FINAL MODEL -------------

best_idx <- which.max(summary_table$AUC)
best_model_name <- summary_table$Model[best_idx]
cat("\nBest performing model on held-out test set:", best_model_name,
    "(AUC =", summary_table$AUC[best_idx], ")\n")

final_model_object <- switch(best_model_name,
                              "Logistic Regression" = model_glm,
                              "Elastic Net" = model_glmnet,
                              "Random Forest" = model_rf)

saveRDS(final_model_object, "outputs/final_ncd_risk_model.rds")
saveRDS(list(zero_as_na_cols = zero_as_na_cols,
             medians = sapply(zero_as_na_cols, function(c) median(df[[c]][df[[c]] != 0]))),
        "outputs/preprocessing_params.rds")

