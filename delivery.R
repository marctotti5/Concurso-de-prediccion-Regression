rm(list = ls())
set.seed(12345)

pacman::p_load(
  caret,       # For nearZeroVar
  mice,        # For imputation
  mgcv,        # For the GAM model
  tidyverse,   # For dplyr, ggplot2, magrittr pipes
  openxlsx     # For excel files
)

data_pisos_train <- read.csv(
  "./data/train.csv",
  encoding = "UTF-8",
  colClasses = c("character", rep(NA, 80)),
  stringsAsFactors = TRUE
)

data_pisos_test <- read.csv(
  "./data/test.csv",
  encoding = "UTF-8",
  colClasses = c("character", rep(NA, 80)),
  stringsAsFactors = TRUE
) %>% mutate(SalePrice = NA)

data_pisos <- rbind(data_pisos_train, data_pisos_test)

# We don't need these anymore.
rm(data_pisos_test, data_pisos_train)

# Imputation 
data_pisos <- data_pisos %>%
  mutate(
    # Variables ordinales convertidas a factor con orden
    MSSubClass = factor(as.character(MSSubClass)),
    MoSold = factor(as.character(MoSold)),
    YrSold = factor(as.character(YrSold)),
    OverallQual = factor(as.character(OverallQual), levels = 1:10),
    OverallCond = factor(as.character(OverallCond), levels = 1:10),

    # Variables con NA significativo: "No acceso" o "No existe"
    Alley = factor(ifelse(
      is.na(Alley),
      "No alley access",
      Alley
    )),

    MasVnrType = factor(
      MasVnrType,
      levels = c("None", "BrkCmn", "BrkFace", "CBlock", "Stone")
    ),

    # Variables de calidad (ordinales: Po < Fa < TA < Gd < Ex)
    ExterQual = factor(ExterQual, levels = c("Po", "Fa", "TA", "Gd", "Ex")),
    ExterCond = factor(ExterCond, levels = c("Po", "Fa", "TA", "Gd", "Ex")),

    # Sótano: imputar "No Basement" cuando falta
    BsmtQual = factor(
      ifelse(is.na(BsmtQual), "No Basement", as.character(BsmtQual)),
      levels = c("No Basement", "Po", "Fa", "TA", "Gd", "Ex")
    ),
    BsmtCond = factor(
      ifelse(is.na(BsmtCond), "No Basement", as.character(BsmtCond)),
      levels = c("No Basement", "Po", "Fa", "TA", "Gd", "Ex")
    ),
    BsmtExposure = factor(
      ifelse(is.na(BsmtExposure), "No Basement", as.character(BsmtExposure)),
      levels = c("No Basement", "No", "Mn", "Av", "Gd")
    ),
    BsmtFinType1 = factor(
      ifelse(is.na(BsmtFinType1), "No Basement", as.character(BsmtFinType1)),
      levels = c("No Basement", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ")
    ),
    BsmtFinType2 = factor(
      ifelse(is.na(BsmtFinType2), "No Basement", as.character(BsmtFinType2)),
      levels = c("No Basement", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ")
    ),

    # Calefacción
    HeatingQC = factor(HeatingQC, levels = c("Po", "Fa", "TA", "Gd", "Ex")),
    KitchenQual = factor(KitchenQual, levels = c("Po", "Fa", "TA", "Gd", "Ex")),

    # Chimenea: imputar "No Fireplace"
    FireplaceQu = factor(
      ifelse(is.na(FireplaceQu), "No Fireplace", as.character(FireplaceQu)),
      levels = c("No Fireplace", "Po", "Fa", "TA", "Gd", "Ex")
    ),

    # Garaje: imputar "No Garage"
    GarageType = factor(ifelse(
      is.na(GarageType),
      "No Garage",
      as.character(GarageType)
    )),
    GarageFinish = factor(
      ifelse(is.na(GarageFinish), "No Garage", as.character(GarageFinish)),
      levels = c("No Garage", "Unf", "RFn", "Fin")
    ),
    GarageQual = factor(
      ifelse(is.na(GarageQual), "No Garage", as.character(GarageQual)),
      levels = c("No Garage", "Po", "Fa", "TA", "Gd", "Ex")
    ),
    GarageCond = factor(
      ifelse(is.na(GarageCond), "No Garage", as.character(GarageCond)),
      levels = c("No Garage", "Po", "Fa", "TA", "Gd", "Ex")
    ),

    # Piscina: imputar "No Pool"
    PoolQC = factor(
      ifelse(is.na(PoolQC), "No Pool", as.character(PoolQC)),
      levels = c("No Pool", "Po", "Fa", "TA", "Gd", "Ex")
    ),

    # Cerca: imputar "No Fence"
    Fence = factor(
      ifelse(is.na(Fence), "No Fence", as.character(Fence)),
      levels = c("No Fence", "MnWw", "GdWo", "MnPrv", "GdPrv")
    ),

    # Características misceláneas
    MiscFeature = factor(ifelse(is.na(MiscFeature), "None", MiscFeature))
  )

data_pisos <- data_pisos %>%
  mutate(
    # Agregación de baños: combinar todos los tipos
    # 1 full bath = 1, 1 half bath = 0.5
    TotalBath = FullBath + HalfBath * 0.5 + BsmtFullBath + BsmtHalfBath * 0.5,

    # Edad de la vivienda en años (al momento de venta)
    HouseAge = as.numeric(as.character(YrSold)) - YearBuilt,

    # Tiempo transcurrido desde la última remodelación
    YearsSinceRemod = as.numeric(as.character(YrSold)) - YearRemodAdd,

    # Superficie total: sótano + área habitable
    TotalSF = TotalBsmtSF + GrLivArea,

    # Superficie total de porches y terrazas
    TotalPorchSF = WoodDeckSF +
      OpenPorchSF +
      EnclosedPorch +
      X3SsnPorch +
      ScreenPorch,

    # Indicador binario: presencia de segundo piso
    Has2ndFloor = ifelse(X2ndFlrSF > 0, "Yes", "No"),

    # Indicador binario: si fue remodelado después de construcción original
    IsRemodeled = ifelse(YearRemodAdd > YearBuilt, "Yes", "No"),

    LogSalePrice = log(SalePrice)
  )

data_for_nzv <- data_pisos |>
  dplyr::select(-Id, -SalePrice) |>
  mutate(across(where(is.character), as.factor))

nzv_info <- nearZeroVar(data_for_nzv, saveMetrics = TRUE)

nzv_vars <- nzv_info |>
  rownames_to_column("variable") |>
  filter(nzv == TRUE | zeroVar == TRUE)

variables_eliminar_sinvariabilidad <- nzv_vars$variable

data_pisos <- data_pisos |>
  # Remove Near Zero Variance variables first
  dplyr::select(-all_of(variables_eliminar_sinvariabilidad)) %>%
  
  # Remove redundant variables safely using any_of()
  dplyr::select(-any_of(c(
      # Baños individuales (reemplazados por TotalBath)
      "FullBath",
      "HalfBath",
      "BsmtFullBath",
      "BsmtHalfBath",

      # Variables de área (reemplazadas por TotalSF)
      "GrLivArea",
      "TotalBsmtSF",

      # Variables de año (reemplazadas por HouseAge/YearsSinceRemod)
      "YearBuilt",
      "YearRemodAdd",

      # Garaje (mantener GarageCars, eliminar GarageArea)
      "GarageArea",

      # Año de construcción del garaje (muy correlacionado con YearBuilt)
      "GarageYrBlt",

      # Variables de porches individuales (reemplazadas por TotalPorchSF)
      "WoodDeckSF",
      "OpenPorchSF", 
      "EnclosedPorch",
      "X3SsnPorch",
      "ScreenPorch",

      # Componentes de área de piso (ya están en TotalSF)
      "X1stFlrSF",
      "X2ndFlrSF",

      # Variables de sótano muy específicas (mantener las más importantes)
      "BsmtFinSF1",
      "BsmtFinSF2",
      "BsmtUnfSF",

      "GarageCond",
      "ExterCond",
      "Exterior2nd",

      # Seccion 16
      "MSSubClass",

      # Seccion 17
      "MoSold"
    )))

# -----------------------------------------------------------------------------
# 12. ROBUST IMPUTATION (MICE)
# -----------------------------------------------------------------------------

# Prepare data for imputation
# We separate the target variable to avoid bias, but keep structure aligned
data_pisos_to_impute <- data_pisos %>%
  mutate(is_test = ifelse(is.na(SalePrice), TRUE, FALSE))

# Define ignore vector: TRUE for test rows (impute them based on train, but don't use them to build the mice model)
ignore_vec <- data_pisos_to_impute$is_test

# Run MICE
# We exclude Id and is_test from the predictor matrix to avoid noise
impute_unified <- mice(
  data_pisos_to_impute %>% dplyr::select(-is_test, -Id), 
  m = 1, 
  method = "rf", # Random Forest is robust for mixed data
  ignore = ignore_vec,
  maxit = 5, 
  seed = 123, 
  printFlag = TRUE
)

# Get completed data
data_pisos_imputed <- complete(impute_unified, 1)

# Restore ID and Split back into Train/Test
data_pisos_imputed$Id <- data_pisos$Id
data_pisos_imputed$is_test <- data_pisos_to_impute$is_test

data_train <- data_pisos_imputed %>% filter(is_test == FALSE) %>% dplyr::select(-is_test)
data_test  <- data_pisos_imputed %>% filter(is_test == TRUE) %>% dplyr::select(-is_test)

# -----------------------------------------------------------------------------
# 13. DIAGNOSTICS & OUTLIER REMOVAL
# -----------------------------------------------------------------------------

# Fit a simple linear model to check for outliers on the clean data
# We select only numeric vars for the diagnostic model to avoid factor level issues
numeric_data_diag <- data_train %>% dplyr::select(where(is.numeric))
model_diag <- lm(log(SalePrice) ~ ., data = numeric_data_diag)

# Calculate diagnostics
diagnostics <- data_train %>%
  mutate(
    std_resid = rstandard(model_diag),
    cooks_d = cooks.distance(model_diag)
  )

# Define threshold for Cook's distance (4/n)
n <- nrow(data_train)
k <- length(coef(model_diag))
cooks_threshold <- 4 / (n - k - 1)

# Remove influential points (Optional but recommended)
data_train_clean <- diagnostics %>% 
  filter(cooks_d <= cooks_threshold) %>%
  dplyr::select(-std_resid, -cooks_d)

print(paste("Removed", nrow(data_train) - nrow(data_train_clean), "outliers."))

# -----------------------------------------------------------------------------
# 14. MODELING (GAM - Generalized Additive Model)
# -----------------------------------------------------------------------------

# Define the GAM formula
# We smooth the key continuous variables found in your EDA
# We keep categorical variables as standard linear terms
# Note: bs="cr" uses cubic regression splines (smooth but robust)
gam_formula_interacciones <- as.formula(
  paste(
    "LogSalePrice",
    "~",
    "s(TotalSF,  bs='cr', k=40) +", # Tamaño total (curva)
    "s(HouseAge, bs='cr', by = OverallCond, k=29) +", # Antigüedad (curva)
    "s(YearsSinceRemod, bs='re', by = Neighborhood, k=40) +", # Tiempo desde reforma (curva)
    "s(LotArea, bs='cr', k=15) +", # Área parcela (curva)
    "OverallQual +", # Factores lineales
    "OverallCond +",
    "s(Neighborhood, bs = 're') +",
    "GarageCars +",
    "Fireplaces +",
    "s(TotalBath, bs = 'cr', k = 3) +",
    "BsmtQual +",
    "KitchenQual +",
    "GarageFinish"
  )
)

print("Training GAM...")

# Fit the model using REML (Restricted Maximum Likelihood) for better smoothness selection
model_gam <- gam(
  gam_formula,
  data = data_train_clean,
  method = "REML",
)

# -----------------------------------------------------------------------------
# 15. PREDICTION & SUBMISSION
# -----------------------------------------------------------------------------

# Predict on Test Data
# GAM predicts on the scale of the linear predictor (log price), so we exp() it
pred_log_gam <- predict(model_gam, newdata = data_test, type = "response")
pred_final <- exp(pred_log_gam)

# Create Submission File
submission <- data.frame(
  Id = data_test$Id,
  SalePrice = as.numeric(pred_final)
)

# Write the file to a 
write.xlsx(submission, "predicted_prices.xlsx")


# 1. Generate predictions for the Validation Set (Log Scale)
preds_log_val <- predict(model_gam, newdata = data_train)

# 2. Extract the actual log prices
actual_log_val <- data_train$LogSalePrice

# 3. Calculate RMSE (Root Mean Squared Error)
rmse_gam <- sqrt(mean((preds_log_val - actual_log_val)^2))

# Print the result
print(paste("The Validation RMSE for the GAM is:", round(rmse_gam, 5)))


# Visual check of the errors
gam.check(model_gam)