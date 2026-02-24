# Loading Required Libraries
library(tidyverse)
library(janitor)
library(glue)
library(VIM)           # kNN imputation using Gower distance (numeric + categorical)
library(ggplot2)
library(ggcorrplot)    # pretty correlation heatmaps
library(car)           # VIF
library(rsample)       # splits
library(rpart)         # decision tree
library(randomForest)  # random forest
library(FNN)           # kNN regression

# Configuration 
data_path      <- 'used_device_data.csv'
knn_k          <- 5

# Naming the Ouctome Variable
main_outcome      <- "normalized_used_price"

# Expected Column names from the dataset
expected_cols <- c(
  "device_brand","os","screen_size","4g","5g",
  "rear_camera_mp","front_camera_mp","internal_memory","ram",
  "battery","weight","release_year","days_used",
  "normalized_used_price","normalized_new_price"
)

# Outliers Rules
rules <- list(
  screen_size            = c(min=5,     max=20),
  rear_camera_mp         = c(min=0,     max=200),
  front_camera_mp        = c(min=0,     max=100), # 0 allowed (no front cam)
  internal_memory        = c(min=0.5,   max=1024),
  ram                    = c(min=0.5,   max=32),
  battery                = c(min=800,   max=12000),
  weight                 = c(min=80,    max=250),
  release_year           = c(min=2005,  max=as.numeric(format(Sys.Date(), "%Y"))),
  days_used              = c(min=0,     max=4000),
  normalized_used_price  = c(min=0,     max=10),
  normalized_new_price   = c(min=0,     max=10)
)

# Loading dataset by checking column names
raw <- read.csv(data_path, stringsAsFactors = FALSE, check.names = FALSE) |>
  clean_names()

# Check columns (cleaned names)
missing_expected <- setdiff(make_clean_names(expected_cols), names(raw))
if (length(missing_expected)) stop(glue("Missing expected columns: {paste(missing_expected, collapse=', ')}"))

cat("\n Dimensions (rows, cols): \n"); print(dim(raw))
cat("\n TOP 5 ROWS: \n"); print(utils::head(raw, 5))

# Type Normalization
df <- raw

# Trim character
char_cols <- names(df)[sapply(df, is.character)]
df[char_cols] <- lapply(df[char_cols], \(x) trimws(x))

# Locate the 4G/5G columns and rename them into x4g/x5g if required
col_4g <- if ("x4g" %in% names(df)) "x4g" else if ("4g" %in% names(df)) "4g" else stop("4G column not found")
col_5g <- if ("x5g" %in% names(df)) "x5g" else if ("5g" %in% names(df)) "5g" else stop("5G column not found")

# Set factors
df[[col_4g]] <- factor(tolower(df[[col_4g]]), levels = c("no","yes"))
df[[col_5g]] <- factor(tolower(df[[col_5g]]), levels = c("no","yes"))
df[["device_brand"]] <- factor(df[["device_brand"]])
df[["os"]] <- factor(df[["os"]])

# Print Summary Statistics
print(summary(df))

# Checking for duplicates
dup_count <- sum(duplicated(df))
cat("\nDuplicate rows:", dup_count, "\n")
df <- df |> distinct()

# Missing  Values
na_per_col_original <- sapply(df, \(x) sum(is.na(x)))
cat("\n Missing Values Per Column: \n"); print(na_per_col_original)
cat("Total missing Values:", sum(na_per_col_original), "\n")

# Zero Values
num_cols <- names(df)[sapply(df, is.numeric)]
zero_candidates <- intersect(num_cols,
                             c("screen_size","rear_camera_mp","front_camera_mp","internal_memory","ram","battery","weight","release_year","days_used"))
zero_counts <- sapply(zero_candidates, \(col) sum(df[[col]] == 0, na.rm = TRUE))
cat("\n Zero Values per Column: \n"); print(zero_counts)
cat("Total zeros across columns:", sum(zero_counts), "\n")

# Listing Out of Range values
apply_oor <- function(d, rule_list){
  viol <- list()
  for (nm in names(rule_list)) {
    if (nm %in% names(d)) {
      rng <- rule_list[[nm]]
      bad <- d[[nm]] < rng["min"] | d[[nm]] > rng["max"]
      bad[is.na(bad)] <- FALSE
      viol[[nm]] <- sum(bad, na.rm = TRUE)
      d[[nm]][bad] <- NA
    }
  }
  list(df = d, violations = unlist(viol))
}
oor <- apply_oor(df, rules)
df  <- oor$df
violations_per_col <- oor$violations

cat("\n Out-of-Range Values per Column: \n"); print(violations_per_col)
cat("Total Out-of-Range Set to NA:", sum(violations_per_col), "\n")

na_mid <- sapply(df, \(x) sum(is.na(x)))
cat("\n Missing After Out-of-Range->NA ===\n"); print(na_mid)
cat("Total missing (mid):", sum(na_mid), "\n")

# Imputing Missing, Zero and Out of Range Values
cat("\n KNN Imputation Method: \n" )

# Use VIM::kNN (Gower distance) so numeric + categorical are imputed together.
df_before <- df
t_knn <- system.time({
  df_imp <- VIM::kNN(
    data      = df,
    k         = knn_k,
    imp_var   = TRUE,    # adds *_imp flags
    numFun    = median,  # numeric aggregation from neighbors
    catFun    = mode,    # categorical aggregation (mode)
    weightDist= FALSE
  )
})
message(glue("KNN imputation completed in {round(t_knn['elapsed'], 2)} sec."))
  
# Extract imputed counts
imp_flags <- names(df_imp)[endsWith(names(df_imp), "_imp")]
imputed_counts <- sapply(imp_flags, \(nm) sum(df_imp[[nm]] == TRUE))
names(imputed_counts) <- gsub("_imp$", "", names(imputed_counts))

# Keep both: df_with_flags (audit) and df (clean)
df_with_flags <- df_imp
df <- df_imp[, !endsWith(names(df_imp), "_imp"), drop=FALSE]

# Printing NA values after imputation
na_final <- sapply(df, \(x) sum(is.na(x)))
cat("\n Missing values per column after imputation: \n"); print(na_final)
cat("Total missing values:", sum(na_final), "\n")

cat("\n Imputed values per column: \n")
print(imputed_counts[imputed_counts > 0])

# Visualizations
theme_clean <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face="bold"),
    panel.grid.minor = element_blank()
  )

# Histograms of all numeric features
num_vars <- names(df)[sapply(df, is.numeric)]
for (v in num_vars) {
  p <- ggplot(df, aes(x = .data[[v]])) +
    geom_histogram(bins = 30, fill = "#bdbdbd", color = "white") +
    labs(title = glue("Distribution of {v}"), x = v, y = "Count") +
    theme_clean
  print(p)
}

# Bar plots of categorical features
cat_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
for (v in cat_vars) {
  p <- ggplot(df, aes(x = .data[[v]])) +
    geom_bar(fill = "#9ecae1", color = "white") +
    labs(title = glue("Distribution of {v}"), x = v, y = "Count") +
    theme_clean +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
  print(p)
  
}

# Pearson Correlation heatmap
cm_pearson  <- suppressWarnings(cor(df[num_vars], use="pairwise.complete.obs", method="pearson"))

p_corr_p <- ggcorrplot(cm_pearson, hc.order=TRUE, type="lower", lab=FALSE,
                       title="Pearson Correlation (numeric)", outline.color="white")

print(p_corr_p)

# Scatterplots — numeric predictors vs normalized_used_price
predictor_nums <- setdiff(num_vars, c(main_outcome))
for (x in predictor_nums) {
  p <- ggplot(df, aes(.data[[x]], .data[[main_outcome]])) +
    geom_point(alpha=0.35, size=1.2, color="#2b8cbe") +
    geom_smooth(method="lm", se=FALSE, color="#cb181d", linewidth=1) +
    labs(title=glue("{x} vs {main_outcome}"), x=x, y=main_outcome) + theme_clean
  print(p)
}

#Predictor Relevancy
is_cat <- function(x) is.factor(x) || is.character(x)
cat_vars <- names(df)[sapply(df, is_cat)]
num_predictors <- setdiff(num_vars, c(main_outcome))
cat_predictors <- setdiff(cat_vars, c(main_outcome))

# Numeric predictors vs normalized_used_price (Pearson)
relev_num_vs_used <- map_dfr(num_predictors, \(x){
  ok <- complete.cases(df[[x]], df[[main_outcome]])
  if (sum(ok) > 2) tibble(
    predictor = x,
    pearson_r = suppressWarnings(cor(df[[x]][ok], df[[main_outcome]][ok], method = "pearson"))
  )
}) |>
  arrange(desc(abs(pearson_r)))
cat("\n Top numeric predictors vs normalized_used_price (Pearson |r|): \n")
print(head(relev_num_vs_used, 10))

# Categorical predictors vs normalized_used_price (ANOVA p-values)
relev_cat_vs_used <- map_dfr(cat_predictors, \(x){
  if (length(unique(na.omit(df[[x]]))) < 2) return(NULL)
  fit <- tryCatch(aov(df[[main_outcome]] ~ as.factor(df[[x]]), data=df), error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  tibble(predictor = x, p_value = summary(fit)[[1]][["Pr(>F)"]][1])
}) |>
  arrange(p_value)
cat("\n Top categorical predictors vs normalized_used_price (ANOVA p): \n")
print(head(relev_cat_vs_used, 10))

# VIF for multi-collinearity among numeric predictors
vif_fit <- lm(as.formula(glue("{main_outcome} ~ {paste(num_predictors, collapse=' + ')}")), data = df)
vif_table <- tibble(
  predictor = names(car::vif(vif_fit)),
  VIF = as.numeric(car::vif(vif_fit))
) |>
  arrange(desc(VIF))
cat("\n VIF for numeric predictors: \n")
print(vif_table)


# For reproducibility
set.seed(123)  

# Data Partioning (70/20/10)
# First: 90% analysis, 10% holdout
split_1  <- initial_split(df, prop = 0.9)
analysis <- training(split_1)
holdout  <- testing(split_1)

# Second: from 90% analysis, carve 70% train / 20% test overall -> 70/90 ≈ 0.7778
split_2 <- initial_split(analysis, prop = 0.7/0.9)
train   <- training(split_2)
test    <- testing(split_2)

cat("\n Partition Sizes: \n")
cat("Train:", nrow(train), "| Test:", nrow(test), "| Holdout:", nrow(holdout), "\n")

# Use all columns except the outcome as predictors (keep both numeric + factors, including normalized_new_price)
regressors <- setdiff(names(df), main_outcome)
reg_formula <- as.formula(glue("{main_outcome} ~ {paste(regressors, collapse=' + ')}"))

# Metrics
rmse <- function(y, yhat) sqrt(mean((y - yhat)^2))
mae  <- function(y, yhat) mean(abs(y - yhat))
r2   <- function(y, yhat) 1 - sum((y - yhat)^2) / sum((y - mean(y))^2)

# Train models
cat("\n Regression Models Training: \n")

# Linear Regression
lm_fit <- lm(reg_formula, data = train)

# Decision Tree
tree_fit <- rpart(reg_formula, data = train, method = "anova",
                  control = rpart.control(cp = 0.01, minsplit = 20))

# Random Forest
# mtry heuristic: sqrt(# numeric + # factor dummies); randomForest handles factors directly
rf_fit <- randomForest(
  reg_formula,
  data = train,
  ntree = 400,
  mtry  = max(2, floor(sqrt(ncol(train) - 1))),
  importance = TRUE
)

# kNN Regression (numeric-only with one-hot + standardization using TRAIN stats)
make_mm <- function(d, predictors, center_scale = NULL){
  mm <- model.matrix(~ . - 1, data = d[, predictors, drop = FALSE])  # dummies
  if (is.null(center_scale)) {
    mu  <- apply(mm, 2, mean)
    sdv <- apply(mm, 2, sd); sdv[sdv == 0] <- 1
    list(X = scale(mm, center = mu, scale = sdv), center = mu, scale = sdv)
  } else {
    list(X = scale(mm, center = center_scale$center, scale = center_scale$scale))
  }
}
# build matrices
train_mm <- make_mm(train, regressors)
test_mm  <- make_mm(test,  regressors, center_scale = list(center = train_mm$center, scale = train_mm$scale))
hold_mm  <- make_mm(holdout, regressors, center_scale = list(center = train_mm$center, scale = train_mm$scale))

# choose k by quick CV on Test (simple sweep)
ks <- c(3,5,7,9,11,15)
knn_rmse <- sapply(ks, function(k) {
  pred <- FNN::knn.reg(train = train_mm$X, test = test_mm$X, y = train[[main_outcome]], k = k)$pred
  rmse(test[[main_outcome]], pred)
})
best_k <- ks[which.min(knn_rmse)]
cat(glue("kNN: chosen k = {best_k} (by Test RMSE over {paste(ks, collapse=', ')})\n"))

# Models Evaluation
predict_and_metrics <- function(fit, test_df, hold_df, type = c("lm","tree","rf","knn")){
  type <- match.arg(type)
  if (type == "knn") {
    pred_test    <- FNN::knn.reg(train = train_mm$X, test = test_mm$X,  y = train[[main_outcome]], k = best_k)$pred
    pred_holdout <- FNN::knn.reg(train = train_mm$X, test = hold_mm$X,  y = train[[main_outcome]], k = best_k)$pred
  } else {
    pred_test    <- predict(fit, newdata = test_df)
    pred_holdout <- predict(fit, newdata = hold_df)
  }
  list(
    test = c(RMSE = rmse(test_df[[main_outcome]], pred_test),
             MAE  = mae(test_df[[main_outcome]],  pred_test),
             R2   = r2(test_df[[main_outcome]],   pred_test)),
    hold = c(RMSE = rmse(hold_df[[main_outcome]], pred_holdout),
             MAE  = mae(hold_df[[main_outcome]],  pred_holdout),
             R2   = r2(hold_df[[main_outcome]],   pred_holdout)),
    preds = list(test = pred_test, hold = pred_holdout)
  )
}

res_lm   <- predict_and_metrics(lm_fit,   test, holdout, "lm")
res_tree <- predict_and_metrics(tree_fit, test, holdout, "tree")
res_rf   <- predict_and_metrics(rf_fit,   test, holdout, "rf")
res_knn  <- predict_and_metrics(NULL,     test, holdout, "knn")

reg_results <- bind_rows(
  tibble(model="Linear Regression", dataset="Test",    RMSE=res_lm$test["RMSE"],   MAE=res_lm$test["MAE"],   R2=res_lm$test["R2"]),
  tibble(model="Linear Regression", dataset="Holdout", RMSE=res_lm$hold["RMSE"],   MAE=res_lm$hold["MAE"],   R2=res_lm$hold["R2"]),
  tibble(model="Decision Tree",     dataset="Test",    RMSE=res_tree$test["RMSE"], MAE=res_tree$test["MAE"], R2=res_tree$test["R2"]),
  tibble(model="Decision Tree",     dataset="Holdout", RMSE=res_tree$hold["RMSE"], MAE=res_tree$hold["MAE"], R2=res_tree$hold["R2"]),
  tibble(model="Random Forest",     dataset="Test",    RMSE=res_rf$test["RMSE"],   MAE=res_rf$test["MAE"],   R2=res_rf$test["R2"]),
  tibble(model="Random Forest",     dataset="Holdout", RMSE=res_rf$hold["RMSE"],   MAE=res_rf$hold["MAE"],   R2=res_rf$hold["R2"]),
  tibble(model="kNN Regression",    dataset="Test",    RMSE=res_knn$test["RMSE"],  MAE=res_knn$test["MAE"],  R2=res_knn$test["R2"]),
  tibble(model="kNN Regression",    dataset="Holdout", RMSE=res_knn$hold["RMSE"],  MAE=res_knn$hold["MAE"],  R2=res_knn$hold["R2"])
) |>
  mutate(across(c(RMSE, MAE, R2), as.numeric))

cat("\n=== REGRESSION METRICS (Test & Holdout) ===\n")
print(reg_results)

# Plots
theme_clean <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face="bold"), panel.grid.minor = element_blank())

# Predicted vs Actual (Holdout) for each model
plot_pred_vs_actual <- function(y_true, y_hat, title){
  ggplot(tibble(Actual = y_true, Predicted = y_hat),
         aes(Actual, Predicted)) +
    geom_point(alpha=0.4) +
    geom_abline(slope=1, intercept=0, linetype="dashed") +
    labs(title = title, x="Actual normalized_used_price", y="Predicted") +
    theme_clean
}

p_lm_hold   <- plot_pred_vs_actual(holdout[[main_outcome]], res_lm$preds$hold,   "Linear Regression: Predicted vs Actual (Holdout)")
p_tree_hold <- plot_pred_vs_actual(holdout[[main_outcome]], res_tree$preds$hold, "Decision Tree: Predicted vs Actual (Holdout)")
p_rf_hold   <- plot_pred_vs_actual(holdout[[main_outcome]], res_rf$preds$hold,   "Random Forest: Predicted vs Actual (Holdout)")
p_knn_hold  <- plot_pred_vs_actual(holdout[[main_outcome]], res_knn$preds$hold,  "kNN Regression: Predicted vs Actual (Holdout)")

print(p_lm_hold); print(p_tree_hold); print(p_rf_hold); print(p_knn_hold)

# Residuals (Test) for Linear Regression
lm_resid_test <- tibble(
  Fitted = predict(lm_fit, newdata = test),
  Resid  = test[[main_outcome]] - predict(lm_fit, newdata = test)
)

p_resid_scatter <- ggplot(lm_resid_test, aes(Fitted, Resid)) +
  geom_point(alpha=0.4) +
  geom_hline(yintercept = 0, linetype="dashed") +
  labs(title="Linear Regression: Residuals vs Fitted (Test)",
       x="Fitted", y="Residual") + theme_clean
print(p_resid_scatter)

# Random Forest variable importance
rf_imp <- as.data.frame(importance(rf_fit))
rf_imp$feature <- rownames(rf_imp)
rf_imp_long <- rf_imp |>
  select(feature, IncNodePurity) |>
  arrange(desc(IncNodePurity)) |>
  head(20)

p_rf_imp <- ggplot(rf_imp_long, aes(x = reorder(feature, IncNodePurity), y = IncNodePurity)) +
  geom_col() + coord_flip() +
  labs(title="Random Forest: Top Variable Importance",
       x=NULL, y="IncNodePurity") +
  theme_clean
print(p_rf_imp)

# Metric comparison bars (Holdout)
reg_results_long <- reg_results |>
  filter(dataset == "Holdout") |>
  pivot_longer(cols = c(RMSE, MAE, R2), names_to = "Metric", values_to = "Value")

p_metrics <- ggplot(reg_results_long, aes(x = reorder(model, Value), y = Value)) +
  geom_col() +
  facet_wrap(~ Metric, scales = "free_y") +
  coord_flip() +
  labs(title = "Holdout Performance by Model", x = NULL, y = NULL) +
  theme_clean 
print(p_metrics)


