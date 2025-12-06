## ----------------------------------------------------------------------------------------------------------
## ----------------------------------------------- LIBRERÍAS ------------------------------------------------
## ----------------------------------------------------------------------------------------------------------
library(tidyverse)
library(gridExtra)
library(DataExplorer)
`%notin%` <- Negate("%in%")
library(labelled)
library(janitor)
library(gtsummary)
library(gt)
library(corrplot)
library(ggcorrplot)
library(GGally)
library(FactoMineR)
library(factoextra)
library(leaflet)
library(pheatmap)
library(sf)
library(ggpubr)
library(naniar)
library(mice)

## ----------------------------------------------------------------------------------------------------------
## -------------------------------------------------- BBDD --------------------------------------------------
## ----------------------------------------------------------------------------------------------------------
rm(list = ls())

## Leemos el id como character, que por defecto lo leía como numeric, el resto por defecto
data_pisos_train <- read.csv(
  "./data/train.csv",
  encoding = "UTF-8",
  colClasses = c("character", rep(NA, 80)),
  stringsAsFactors = TRUE
) %>%
  mutate(
    conjunto = "train"
  )

data_pisos_test <- read.csv(
  "./data/test.csv",
  encoding = "UTF-8",
  colClasses = c("character", rep(NA, 80)),
  stringsAsFactors = TRUE
) %>%
  mutate(
    SalePrice = NA,
    conjunto = "test"
  )

data_pisos <- rbind(data_pisos_train, data_pisos_test)

## ----------------------------------------------------------------------------------------------------------
## --------------------------------------------- DATA CLEANING ----------------------------------------------
## ----------------------------------------------------------------------------------------------------------

## Cambiamos los NA's de aquellas variables para las que conocemos su valor (véase el data dictionary)
data_pisos <- data_pisos %>%
  mutate(
    MSSubClass = factor(as.character(MSSubClass)),
    Alley = factor(ifelse(
      is.na(Alley),
      "No alley access",
      Alley
    )),
    OverallQual = factor(as.character(OverallQual), levels = 1:10),
    OverallCond = factor(as.character(OverallCond), levels = 1:10),
    MasVnrType = factor(
      MasVnrType,
      levels = c("None", "BrkCmn", "BrkFace", "CBlock", "Stone")
    ),
    ExterQual = factor(ExterQual, levels = c("Po", "Fa", "TA", "Gd", "Ex")),
    ExterCond = factor(ExterCond, levels = c("Po", "Fa", "TA", "Gd", "Ex")),
    BsmtQual = factor(
      ifelse(
        is.na(BsmtQual),
        "No Basement",
        as.character(BsmtQual)
      ),
      levels = c("No Basement", "Po", "Fa", "TA", "Gd", "Ex")
    ),
    BsmtCond = factor(
      ifelse(
        is.na(BsmtCond),
        "No Basement",
        as.character(BsmtCond)
      ),
      levels = c("No Basement", "Po", "Fa", "TA", "Gd", "Ex")
    ),
    BsmtExposure = factor(
      ifelse(
        is.na(BsmtExposure),
        "No Basement",
        as.character(BsmtExposure)
      ),
      levels = c("No Basement", "No", "Mn", "Av", "Gd")
    ),
    BsmtFinType1 = factor(
      ifelse(
        is.na(BsmtFinType1),
        "No Basement",
        as.character(BsmtFinType1)
      ),
      levels = c("No Basement", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ")
    ),
    BsmtFinType2 = factor(
      ifelse(
        is.na(BsmtFinType2),
        "No Basement",
        as.character(BsmtFinType2)
      ),
      levels = c("No Basement", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ")
    ),
    HeatingQC = factor(HeatingQC, levels = c("Po", "Fa", "TA", "Gd", "Ex")),
    KitchenQual = factor(KitchenQual, levels = c("Po", "Fa", "TA", "Gd", "Ex")),
    FireplaceQu = factor(
      ifelse(
        is.na(FireplaceQu),
        "No Fireplace",
        as.character(FireplaceQu)
      ),
      levels = c("No Fireplace", "Po", "Fa", "TA", "Gd", "Ex")
    ),
    GarageType = factor(ifelse(
      is.na(GarageType),
      "No Garage",
      as.character(GarageType)
    )),
    GarageFinish = factor(
      ifelse(
        is.na(GarageFinish),
        "No Garage",
        as.character(GarageFinish)
      ),
      levels = c("No Garage", "Unf", "RFn", "Fin")
    ),
    GarageQual = factor(
      ifelse(
        is.na(GarageQual),
        "No Garage",
        as.character(GarageQual)
      ),
      levels = c("No Garage", "Po", "Fa", "TA", "Gd", "Ex")
    ),
    GarageCond = factor(
      ifelse(
        is.na(GarageCond),
        "No Garage",
        as.character(GarageCond)
      ),
      levels = c("No Garage", "Po", "Fa", "TA", "Gd", "Ex")
    ),
    PoolQC = factor(
      ifelse(
        is.na(PoolQC),
        "No Pool",
        as.character(PoolQC)
      ),
      levels = c("No Pool", "Po", "Fa", "TA", "Gd", "Ex")
    ),
    Fence = factor(
      ifelse(
        is.na(Fence),
        "No Fence",
        as.character(Fence)
      ),
      levels = c("No Fence", "MnWw", "GdWo", "MnPrv", "GdPrv")
    ),
    MiscFeature = factor(ifelse(is.na(MiscFeature), "None", MiscFeature)),

    # CREAMOS VARIABLES TRANSFORMADAS (ELIMINAMOS REDUNDANCIAS)
    # Total de baños (combinando todos)
    TotalBath = FullBath + HalfBath * 0.5 + BsmtFullBath + BsmtHalfBath * 0.5,

    # Edad de la casa al momento de la venta
    HouseAge = YrSold - YearBuilt,

    # Años desde la última remodelación
    YearsSinceRemod = YrSold - YearRemodAdd,

    # Área total de superficie (sótano + área habitable)
    TotalSF = TotalBsmtSF + GrLivArea,

    # Área total de porches
    TotalPorchSF = WoodDeckSF +
      OpenPorchSF +
      EnclosedPorch +
      X3SsnPorch +
      ScreenPorch,

    # Indicador de si tiene segundo piso
    Has2ndFloor = ifelse(X2ndFlrSF > 0, 1, 0),

    # Indicador de si fue remodelado
    IsRemodeled = ifelse(YearRemodAdd > YearBuilt, 1, 0)
  )

# Analizar correlación de las nuevas variables con SalePrice
new_vars <- c(
  "TotalBath",
  "HouseAge",
  "YearsSinceRemod",
  "TotalSF",
  "TotalPorchSF",
  "Has2ndFloor",
  "IsRemodeled"
)

cor_new_vars <- data_pisos |>
  select(all_of(new_vars), SalePrice) |>
  cor(use = "pairwise.complete.obs") |>
  as.data.frame() |>
  select(SalePrice) |>
  arrange(desc(abs(SalePrice)))

# Preparar datos para visualización
plot_data <- data_pisos |>
  select(TotalBath, TotalSF, HouseAge, SalePrice)

# Gráfico 1: TotalSF vs SalePrice
p1 <- ggplot(plot_data, aes(x = TotalSF, y = SalePrice)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "TotalSF vs SalePrice", subtitle = "Correlación: 0.78") +
  theme_minimal()

# Gráfico 2: TotalBath vs SalePrice
p2 <- ggplot(plot_data, aes(x = TotalBath, y = SalePrice)) +
  geom_point(alpha = 0.4, color = "darkgreen") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "TotalBath vs SalePrice", subtitle = "Correlación: 0.63") +
  theme_minimal()

# Gráfico 3: HouseAge vs SalePrice
p3 <- ggplot(plot_data, aes(x = HouseAge, y = SalePrice)) +
  geom_point(alpha = 0.4, color = "purple") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "HouseAge vs SalePrice", subtitle = "Correlación: -0.52") +
  theme_minimal()

# Comparación: GrLivArea original vs TotalSF
p4 <- data_pisos |>
  ggplot(aes(x = GrLivArea, y = SalePrice)) +
  geom_point(alpha = 0.4, color = "orange") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "GrLivArea vs SalePrice (Original)",
    subtitle = "Correlación: 0.71"
  ) +
  theme_minimal()

grid.arrange(p1, p4, p2, p3, ncol = 2)

# Crear dataset final eliminando variables redundantes
data_pisos_final <- data_pisos |>
  # Eliminar variables redundantes
  select(
    -c(
      # Baños individuales (reemplazados por TotalBath)
      FullBath,
      HalfBath,
      BsmtFullBath,
      BsmtHalfBath,

      # Variables de área (reemplazadas por TotalSF)
      GrLivArea,
      TotalBsmtSF,

      # Variables de año (reemplazadas por HouseAge/YearsSinceRemod)
      YearBuilt,
      YearRemodAdd,

      # Garaje (mantener GarageCars, eliminar GarageArea)
      GarageArea,

      # Año de construcción del garaje (muy correlacionado con YearBuilt)
      GarageYrBlt,

      # Variables de porches individuales (reemplazadas por TotalPorchSF)
      WoodDeckSF,
      OpenPorchSF,
      EnclosedPorch,
      X3SsnPorch,
      ScreenPorch,

      # Componentes de área de piso (ya están en TotalSF)
      X1stFlrSF,
      X2ndFlrSF,

      # Variables de sótano muy específicas (mantener las más importantes)
      BsmtFinSF1,
      BsmtFinSF2,
      BsmtUnfSF,
    )
  )

## Volvemos a separar en train y test, y eliminamos la variable respuesta de test
data_pisos_train <- data_pisos_final %>%
  filter(conjunto == "train") %>%
  select(-conjunto)

data_pisos_test <- data_pisos_final %>%
  filter(conjunto == "test") %>%
  select(-conjunto, -SalePrice)

## ----------------------------------------------------------------------------------------------------------
## ------------------------------------------------- EDA (SOLO DE TRAIN) ------------------------------------
## ----------------------------------------------------------------------------------------------------------

## TODO: HACER ANALISIS EXPLORATORIO GENERAL DE TRAIN, AHORA SOLO HE MIRADO MISSINGS

## Análisis de missings
plot_missing(data_pisos_train)

# 1. Overall missing pattern visualization
vis_miss(data_pisos_train, warn_large_data = FALSE) +
  labs(
    title = "Missing Value Pattern in Training Set",
    subtitle = "Visual overview of missing data across all variables"
  )

# 2. Missing pattern intersections (co-occurrence of NAs)
gg_miss_upset(data_pisos_train, nsets = 5, nintersects = 10)

# 3. LotFrontage missingness by Neighborhood
data_pisos_train |>
  mutate(LotFrontage_missing = is.na(LotFrontage)) |>
  ggplot(aes(x = Neighborhood, fill = LotFrontage_missing)) +
  geom_bar(position = "fill") +
  coord_flip() +
  scale_fill_manual(
    values = c("gray70", "red3"),
    name = "LotFrontage NA",
    labels = c("Present", "Missing")
  ) +
  labs(
    title = "Proportion of Missing LotFrontage by Neighborhood",
    subtitle = "Strong geographic pattern suggests MAR (Missing At Random)",
    y = "Proportion",
    x = "Neighborhood"
  ) +
  theme_minimal()


# 4. LotFrontage missingness by LotShape
data_pisos_train |>
  mutate(LotFrontage_missing = is.na(LotFrontage)) |>
  count(LotShape, LotFrontage_missing) |>
  group_by(LotShape) |>
  mutate(prop = n / sum(n)) |>
  ggplot(aes(x = LotShape, y = prop, fill = LotFrontage_missing)) +
  geom_col() +
  geom_text(
    aes(label = scales::percent(prop, accuracy = 1)),
    position = position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c("gray70", "red3"),
    name = "LotFrontage",
    labels = c("Present", "Missing")
  ) +
  labs(
    title = "Proportion of Missing LotFrontage by Lot Shape",
    subtitle = "Irregular lots have much higher missingness (~35% vs ~7%)",
    y = "Proportion",
    x = "Lot Shape"
  ) +
  theme_minimal()

# 5. LotArea distribution by LotFrontage missingness
data_pisos_train |>
  mutate(
    LotFrontage_status = ifelse(is.na(LotFrontage), "Missing", "Present")
  ) |>
  ggplot(aes(x = LotFrontage_status, y = LotArea, fill = LotFrontage_status)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("red3", "gray70")) +
  scale_y_log10(labels = scales::comma) +
  labs(
    title = "LotArea Distribution by LotFrontage Availability",
    subtitle = "Similar distributions → no size bias in missingness",
    y = "LotArea (log scale)",
    x = "LotFrontage Status"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# 6. SalePrice distribution by LotFrontage missingness
data_pisos_train |>
  mutate(
    LotFrontage_status = ifelse(is.na(LotFrontage), "Missing", "Present")
  ) |>
  ggplot(aes(
    x = LotFrontage_status,
    y = SalePrice,
    fill = LotFrontage_status
  )) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
  stat_summary(
    fun = median,
    geom = "text",
    aes(label = paste0("$", scales::comma(..y.., accuracy = 1))),
    position = position_nudge(y = 0.15),
    size = 4,
    fontface = "bold"
  ) +
  scale_fill_manual(values = c("red3", "gray70")) +
  scale_y_log10(labels = scales::dollar) +
  labs(
    title = "SalePrice Distribution by LotFrontage Availability",
    subtitle = "Missing LotFrontage has slightly HIGHER median price → not ignorable",
    y = "SalePrice (log scale)",
    x = "LotFrontage Status"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


## ----------------------------------------------------------------------------------------------------------
## ----------------------------------------- MISSING IMPUTATION----------------------------------------------
## ----------------------------------------------------------------------------------------------------------
# Step 1: Preparar datos (sin target)
X_train <- data_pisos_train |> select(-SalePrice)
y_train <- data_pisos_train$SalePrice
X_test <- data_pisos_test

cat("Missing values in train:\n")
colSums(is.na(X_train)) |>
  (\(x) x[x > 0])() |>
  print()

cat("\nMissing values in test:\n")
colSums(is.na(X_test)) |>
  (\(x) x[x > 0])() |>
  print()

# Step 2: Entrenar modelo de imputación SOLO en train
set.seed(123)
cat("\n=== Training imputation model on TRAIN data only ===\n")
start_time <- Sys.time()

# Configurar métodos de imputación
# pmm = predictive mean matching (robusto para numéricos)
# logreg/polyreg = para categóricos
impute_model <- mice(
  X_train,
  m = 1, # Una sola imputación (puedes usar m=5 para múltiple)
  method = "rf", # Random forest para todas las variables
  ntree = 100,
  maxit = 5, # Iteraciones
  seed = 123,
  printFlag = TRUE
)

end_time <- Sys.time()
cat(
  "\nTrain imputation time:",
  round(difftime(end_time, start_time, units = "secs"), 1),
  "secs\n"
)

# Step 3: Extraer datos imputados de train
X_train_imputed <- complete(impute_model, 1)

cat(
  "\nMissing values in train after imputation:",
  sum(is.na(X_train_imputed)),
  "\n"
)

# Step 4: Aplicar el MISMO modelo a test
cat("\n=== Applying trained model to TEST data ===\n")
start_time <- Sys.time()

# Crear objeto mice vacío para test con la misma estructura
impute_test <- mice(
  X_test,
  m = 1,
  maxit = 0, # No entrenar, solo preparar estructura
  seed = 123,
  printFlag = FALSE
)

# Usar los parámetros aprendidos de train para imputar test
# Esto usa los modelos entrenados en train
X_test_imputed <- complete(
  mice.mids(impute_test, newdata = X_test, printFlag = TRUE),
  1
)

end_time <- Sys.time()
cat(
  "\nTest imputation time:",
  round(difftime(end_time, start_time, units = "secs"), 1),
  "secs\n"
)

cat(
  "\nMissing values in test after imputation:",
  sum(is.na(X_test_imputed)),
  "\n"
)

# Step 5: Reconstruir datasets finales
data_pisos_train_imputed <- X_train_imputed %>%
  mutate(SalePrice = y_train) %>%
  as_tibble()

data_pisos_test_imputed <- X_test_imputed %>%
  as_tibble()

## ----------------------------------------------------------------------------------------------------------
## --------------------------------- ANÁLISIS DE CORRELACIONES Y MULTICOLINEALIDAD --------------------------
## ----------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------
## ------------------------------------------- PCA EXPLORATORIO ---------------------------------------------
## ----------------------------------------------------------------------------------------------------------

## ----------------------------------------------------------------------------------------------------------
## -------------------------------------------------- MODELADO ----------------------------------------------
## ----------------------------------------------------------------------------------------------------------
