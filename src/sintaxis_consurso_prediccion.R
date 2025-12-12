## ----------------------------------------------------------------------------------------------------------
## ----------------------------------------------- LIBRERÍAS ------------------------------------------------
## ----------------------------------------------------------------------------------------------------------

# Pacman is a package to better handle the missing packages, when trying to load a package that is not present, it automatically installs it.
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse,
  gridExtra,
  DataExplorer,
  labelled,
  janitor,
  gtsummary,
  gt,
  corrplot,
  ggcorrplot,
  GGally,
  FactoMineR,
  factoextra,
  leaflet,
  pheatmap,
  sf,
  ggpubr,
  naniar,
  mice,
  car, 
  caret,
  DescTools
)

`%notin%` <- Negate("%in%")

## ----------------------------------------------------------------------------------------------------------
## -------------------------------------------------- BBDD --------------------------------------------------
## ----------------------------------------------------------------------------------------------------------

# Ensuring reproducibility
rm(list = ls())
set.seed(12345)


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
    Has2ndFloor = ifelse(X2ndFlrSF > 0, "Yes", "No"),

    # Indicador de si fue remodelado
    IsRemodeled = ifelse(YearRemodAdd > YearBuilt, "Yes", "No")
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
  select_if(is.numeric) %>%
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


## Variables sin variabilidad -> no aportan nada al modelo -> eliminar
data_for_nzv <- data_pisos |>
  select(-Id, -SalePrice) |>
  mutate(across(where(is.character), as.factor))

# Detectar variables con near-zero variance
nzv_info <- nearZeroVar(data_for_nzv, saveMetrics = TRUE)

nzv_vars <- nzv_info |>
  rownames_to_column("variable") |>
  filter(nzv == TRUE | zeroVar == TRUE)

# Visualización de las variables más problemáticas
p1 <- data_pisos |>
  count(KitchenAbvGr) |>
  mutate(prop = n / sum(n)) |>
  ggplot(aes(x = factor(KitchenAbvGr), y = n)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_text(
    aes(label = scales::percent(prop, accuracy = 0.1)),
    vjust = -0.5,
    size = 3
  ) +
  labs(
    title = "KitchenAbvGr Distribution",
    subtitle = paste("95.6% have value 1 → Low variability"),
    x = "KitchenAbvGr",
    y = "Count"
  ) +
  theme_minimal()

p2 <- data_pisos |>
  count(Utilities) |>
  mutate(prop = n / sum(n)) |>
  ggplot(aes(x = Utilities, y = n)) +
  geom_col(fill = "coral", alpha = 0.7) +
  geom_text(
    aes(label = scales::percent(prop, accuracy = 0.1)),
    vjust = -0.5,
    size = 3
  ) +
  labs(
    title = "Utilities Distribution",
    subtitle = "99.9% have 'AllPub' → Near-zero variance",
    x = "Utilities",
    y = "Count"
  ) +
  theme_minimal()

p3 <- data_pisos |>
  count(Street) |>
  mutate(prop = n / sum(n)) |>
  ggplot(aes(x = Street, y = n)) +
  geom_col(fill = "darkgreen", alpha = 0.7) +
  geom_text(
    aes(label = scales::percent(prop, accuracy = 0.1)),
    vjust = -0.5,
    size = 3
  ) +
  labs(
    title = "Street Distribution",
    subtitle = "99.6% have 'Pave' → Near-zero variance",
    x = "Street",
    y = "Count"
  ) +
  theme_minimal()

p4 <- data_pisos |>
  count(PoolArea > 0) |>
  mutate(
    prop = n / sum(n),
    has_pool = ifelse(`PoolArea > 0`, "Has Pool", "No Pool")
  ) |>
  ggplot(aes(x = has_pool, y = n)) +
  geom_col(fill = "purple", alpha = 0.7) +
  geom_text(
    aes(label = scales::percent(prop, accuracy = 0.1)),
    vjust = -0.5,
    size = 3
  ) +
  labs(
    title = "PoolArea Distribution",
    subtitle = "99.9% have no pool → Near-zero variance",
    x = "Pool Status",
    y = "Count"
  ) +
  theme_minimal()

graficos_variables_eliminar_sinvariabilidad <- grid.arrange(
  p1,
  p2,
  p3,
  p4,
  ncol = 2
)
variables_eliminar_sinvariabilidad <- nzv_vars$variable


## Variables categóricas redundantes

# Crear dataset final eliminando variables redundantes
data_pisos_final <- data_pisos |>
  select(-all_of(variables_eliminar_sinvariabilidad)) %>%
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
      #OpenPorchSF,
      #EnclosedPorch,
      #X3SsnPorch,
      #ScreenPorch,

      # Componentes de área de piso (ya están en TotalSF)
      X1stFlrSF,
      X2ndFlrSF,

      # Variables de sótano muy específicas (mantener las más importantes)
      BsmtFinSF1,
      #BsmtFinSF2,
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
## --------------------------------------- Variable respuesta ----------------------------------------------
# Análisis normalidad respuesta
p1 <- data_pisos_train |>
  ggplot(aes(x = SalePrice)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7, color = "white") +
  scale_x_continuous(labels = scales::dollar) +
  labs(
    title = "SalePrice Distribution",
    subtitle = "Right-skewed distribution",
    x = "Sale Price",
    y = "Count"
  ) +
  theme_minimal()

p2 <- data_pisos_train |>
  ggplot(aes(x = SalePrice)) +
  geom_histogram(bins = 50, fill = "coral", alpha = 0.7, color = "white") +
  scale_x_log10(labels = scales::dollar) +
  labs(
    title = "SalePrice Distribution (log scale)",
    subtitle = "More normal after log transformation",
    x = "Sale Price (log)",
    y = "Count"
  ) +
  theme_minimal()

p3 <- data_pisos_train |>
  ggplot(aes(sample = SalePrice)) +
  stat_qq(color = "steelblue", alpha = 0.7) +
  stat_qq_line(color = "red", linewidth = 1) +
  labs(title = "Q-Q Plot: SalePrice", subtitle = "Heavy right tail visible") +
  theme_minimal()

p4 <- data_pisos_train |>
  ggplot(aes(sample = log(SalePrice))) +
  stat_qq(color = "coral", alpha = 0.7) +
  stat_qq_line(color = "red", linewidth = 1) +
  labs(
    title = "Q-Q Plot: log(SalePrice)",
    subtitle = "Closer to normal distribution"
  ) +
  theme_minimal()

grafico_exploratorio_respuesta <- grid.arrange(p1, p2, p3, p4, ncol = 2)

## Correlaciones de la respuesta con variables numéricas
correlations <- data_pisos_train |>
  select(where(is.numeric), -Id) |>
  cor(use = "complete.obs") |>
  as.data.frame() |>
  rownames_to_column("variable") |>
  select(variable, SalePrice) |>
  filter(variable != "SalePrice") |>
  arrange(desc(abs(SalePrice))) |>
  mutate(abs_cor = abs(SalePrice))

top10 <- correlations |> head(10)


grafico_correlaciones_precio_predictores <- ggplot(
  top10,
  aes(x = reorder(variable, abs_cor), y = SalePrice)
) +
  geom_col(aes(fill = SalePrice > 0), alpha = 0.7) +
  geom_text(
    aes(label = round(SalePrice, 3)),
    hjust = ifelse(top10$SalePrice > 0, -0.1, 1.1),
    size = 3.5
  ) +
  scale_fill_manual(values = c("coral", "steelblue"), guide = "none") +
  coord_flip() +
  labs(
    title = "Top 10 Numeric Variables Correlated with SalePrice",
    subtitle = "Strong positive correlations with quality and size metrics",
    x = NULL,
    y = "Correlation with SalePrice"
  ) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank())

## Análisis de variables categóricas
p1 <- data_pisos_train |>
  ggplot(aes(x = factor(OverallQual), y = SalePrice)) +
  geom_boxplot(
    aes(fill = factor(OverallQual)),
    alpha = 0.7,
    show.legend = FALSE
  ) +
  scale_y_continuous(labels = scales::dollar) +
  scale_fill_viridis_d() +
  labs(
    title = "SalePrice by Overall Quality",
    subtitle = "Clear positive relationship",
    x = "Overall Quality (1-10)",
    y = "Sale Price"
  ) +
  theme_minimal()

median_by_neighborhood <- data_pisos_train |>
  group_by(Neighborhood) |>
  summarise(median_price = median(SalePrice, na.rm = TRUE), n = n()) |>
  arrange(desc(median_price))

p2 <- data_pisos_train |>
  mutate(Neighborhood = fct_reorder(Neighborhood, SalePrice, .fun = median)) |>
  ggplot(aes(x = Neighborhood, y = SalePrice)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  scale_y_continuous(labels = scales::dollar) +
  coord_flip() +
  labs(
    title = "SalePrice by Neighborhood",
    subtitle = "High variability across locations",
    x = NULL,
    y = "Sale Price"
  ) +
  theme_minimal()

p3 <- data_pisos_train |>
  mutate(ExterQual = fct_reorder(ExterQual, SalePrice, .fun = median)) |>
  ggplot(aes(x = ExterQual, y = SalePrice)) +
  geom_boxplot(fill = "coral", alpha = 0.7) +
  scale_y_continuous(labels = scales::dollar) +
  labs(
    title = "SalePrice by Exterior Quality",
    x = "Exterior Quality",
    y = "Sale Price"
  ) +
  theme_minimal()

p4 <- data_pisos_train |>
  mutate(KitchenQual = fct_reorder(KitchenQual, SalePrice, .fun = median)) |>
  ggplot(aes(x = KitchenQual, y = SalePrice)) +
  geom_boxplot(fill = "darkgreen", alpha = 0.7) +
  scale_y_continuous(labels = scales::dollar) +
  labs(
    title = "SalePrice by Kitchen Quality",
    x = "Kitchen Quality",
    y = "Sale Price"
  ) +
  theme_minimal()

grid.arrange(p1, p2, p3, p4, ncol = 2)

## --------------------------------------- Predictores: variables categóricas -------------------------------
# Identificar pares de categóricas altamente correlacionados
# Variables de calidad
quality_vars <- c(
  "ExterQual",
  "ExterCond",
  "BsmtQual",
  "BsmtExposure",
  "HeatingQC",
  "KitchenQual",
  "FireplaceQu",
  "GarageQual",
  "GarageCond"
)

quality_numeric <- data_pisos_train |>
  select(all_of(quality_vars), SalePrice) |>
  mutate(across(
    all_of(quality_vars),
    ~ case_when(
      . %in% c("No Basement", "No Fireplace", "No Garage") ~ 0,
      . == "Po" ~ 1,
      . == "Fa" ~ 2,
      . == "TA" ~ 3,
      . == "Gd" ~ 4,
      . == "Ex" ~ 5,
      . == "No" ~ 1,
      . == "Mn" ~ 2,
      . == "Av" ~ 3,
      . == "GLQ" ~ 4,
      TRUE ~ 3
    )
  ))

# Correlaciones entre variables de calidad
cor_quality <- cor(quality_numeric, use = "complete.obs")

pheatmap(
  cor_quality,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  display_numbers = TRUE,
  number_format = "%.2f",
  fontsize_number = 10,
  color = colorRampPalette(c("blue", "white", "red"))(100),
  main = "Correlations: Categorical variables related to quality",
  fontsize_row = 8,
  breaks = seq(-1, 1, length.out = 101),
  legend_breaks = c(-1, -0.5, 0, 0.5, 1)
)


p1 <- data_pisos_train |>
  count(ExterQual, KitchenQual) |>
  ggplot(aes(x = ExterQual, y = KitchenQual, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n), color = "white", fontface = "bold") +
  scale_fill_gradient(low = "steelblue", high = "darkred") +
  labs(
    title = "ExterQual vs KitchenQual",
    subtitle = "r = 0.72 → High correlation",
    x = "Exterior Quality",
    y = "Kitchen Quality"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

p2 <- data_pisos_train |>
  count(GarageQual, GarageCond) |>
  ggplot(aes(x = GarageQual, y = GarageCond, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n), color = "white", fontface = "bold", size = 3) +
  scale_fill_gradient(low = "steelblue", high = "darkred") +
  labs(
    title = "GarageQual vs GarageCond",
    subtitle = "r = 0.96 → Highly redundant!",
    x = "Garage Quality",
    y = "Garage Condition"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

p3 <- data_pisos_train |>
  ggplot(aes(x = ExterQual, y = SalePrice, fill = ExterQual)) +
  geom_violin(alpha = 0.7, show.legend = FALSE) +
  geom_boxplot(width = 0.2, alpha = 0.5, show.legend = FALSE) +
  scale_y_continuous(labels = scales::dollar) +
  scale_fill_viridis_d() +
  labs(
    title = "ExterQual → SalePrice",
    subtitle = "r = 0.68 (strong predictor)",
    x = "Exterior Quality",
    y = "Sale Price"
  ) +
  theme_minimal()

p4 <- data_pisos_train |>
  ggplot(aes(x = ExterCond, y = SalePrice, fill = ExterCond)) +
  geom_violin(alpha = 0.7, show.legend = FALSE) +
  geom_boxplot(width = 0.2, alpha = 0.5, show.legend = FALSE) +
  scale_y_continuous(labels = scales::dollar) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "ExterCond → SalePrice",
    subtitle = "r = 0.02 (weak predictor)",
    x = "Exterior Condition",
    y = "Sale Price"
  ) +
  theme_minimal()

grid.arrange(p1, p2, p3, p4, ncol = 2)

top_categorical_vars <- c(
  "OverallQual",
  "Neighborhood",
  "ExterQual",
  "KitchenQual",
  "BsmtQual",
  "FireplaceQu"
)

# Calcular median price por nivel de cada variable
median_prices <- lapply(top_categorical_vars, function(var) {
  data_pisos_train[complete.cases(data_pisos_train), ] |>
    group_by(!!sym(var)) |>
    summarise(
      median_price = median(SalePrice, na.rm = TRUE),
      n = n()
    ) |>
    arrange(median_price) |>
    mutate(
      variable = var,
      level = as.character(!!sym(var))
    )
}) |>
  bind_rows()

# Plot: Median price by level for top categorical variables
ggplot(
  median_prices,
  aes(x = reorder(level, median_price), y = median_price, fill = variable)
) +
  geom_col(alpha = 0.8) +
  geom_text(
    aes(label = scales::dollar(median_price, accuracy = 1)),
    hjust = -0.1,
    size = 2.5
  ) +
  scale_y_continuous(labels = scales::dollar, limits = c(0, 350000)) +
  scale_fill_viridis_d(name = "Variable") +
  coord_flip() +
  facet_wrap(~variable, scales = "free_y", ncol = 2) +
  labs(
    title = "Top Categorical Predictors: Median SalePrice by Level",
    subtitle = "Clear monotonic relationships indicate strong predictive power",
    x = NULL,
    y = "Median Sale Price"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 10),
    panel.spacing = unit(1, "lines")
  )

##TODO: PENDIENTE HACER UN ANALISIS EN PROFUNDIDAD DE LAS VARIABLES CATEGORICAS (TODAS) PARA DESCARTAR LAS REDUNDANTES
##TODO: PENDIENTE HACER UN ANALISIS EN PROFUNDIDAD DE LAS VARIABLES NUMÉRICAS (TODAS) PARA DESCARTAR LAS REDUNDANTES

## Eliminación de variables categóricas redundantes
variables_categoricas_redundantes <- c("GarageCond", "ExterCond", "Exterior2nd")
data_pisos_train <- data_pisos_train %>%
  select(-all_of(variables_categoricas_redundantes))
data_pisos_test <- data_pisos_test %>%
  select(-all_of(variables_categoricas_redundantes))

## --------------------------------------- Identificación de missings ----------------------------------------------
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
# Step 1: Preparar datos combinados
# Mantener SalePrice en train para que ayude a imputar, pero asegurar que en test sea NA
data_full_imputation <- bind_rows(
  data_pisos_train %>% mutate(is_test = FALSE),
  data_pisos_test %>% mutate(SalePrice = NA, is_test = TRUE)
)

# Step 2: Configurar vector 'ignore'
# TRUE = La fila NO se usa para entrenar el modelo (pero SÍ se imputa)
# Usaremos solo las filas de train (is_test == FALSE) para aprender los patrones
ignore_vec <- data_full_imputation$is_test

cat("\n=== Running unified imputation (Training on TRAIN rows only) ===\n")
start_time <- Sys.time()

# Step 3: Imputar usando 'ignore'
# Excluimos variables auxiliares como 'is_test' o IDs si los hubiera en la fórmula del mice,
# pero mice por defecto usa todo. Aseguramos method='rf'.
impute_unified <- mice(
  data_full_imputation %>% select(-is_test),
  m = 1,
  method = "rf",
  ignore = ignore_vec,
  maxit = 5,
  seed = 123,
  printFlag = TRUE
)

end_time <- Sys.time()
cat(
  "\nImputation time:",
  round(difftime(end_time, start_time, units = "secs"), 1),
  "secs\n"
)

# Step 4: Extraer datos completos y separar
data_full_completed <- complete(impute_unified, 1)


# Recuperar Train
data_pisos_train_imputed <- data_full_completed %>%
  slice(which(!ignore_vec)) %>%
  as_tibble()

# Recuperar Test (y quitar SalePrice que era NA/falso)
data_pisos_test_imputed <- data_full_completed %>%
  slice(which(ignore_vec)) %>%
  select(-SalePrice) %>%
  as_tibble()

sum(is.na(data_pisos_train_imputed))
sum(is.na(data_pisos_test_imputed))

## ----------------------------------------------------------------------------------------------------------
## ----------------------- DIAGNÓSTICO DE INFLUENTIAL POINTS --------------------------------
## ----------------------------------------------------------------------------------------------------------
# Ajustar modelo preliminar para diagnóstico (usando solo numéricas)
numeric_data_diag <- data_pisos_train_imputed %>% select(where(is.numeric))
model_diag <- lm(SalePrice ~ ., data = numeric_data_diag)

diagnostics <- data_pisos_train_imputed %>%
  mutate(
    # Residuos Estandarizados: Detectan outliers en la variable respuesta (Y).
    # Regla: Valor absoluto > 3 indica outlier.
    std_resid = rstandard(model_diag),
    
    # Distancia de Cook: Detecta observaciones influyentes (combinación de outlier + leverage).
    # Mide cuánto cambiaría el modelo si se elimina la observación.
    cooks_d = cooks.distance(model_diag),
    
    # Leverage (Hat values): Detecta valores inusuales en los predictores (X).
    hat_val = hatvalues(model_diag)
  )

# Rules of Thumb
n <- nrow(numeric_data_diag)
k <- length(coef(model_diag)) - 1
cooks_threshold <- 4 / (n - k - 1)      # Cook
leverage_threshold <- 2 * (k + 1) / n   # Leverage

# Visualización
# Gráfico de Influencia vs Residuos: Los puntos rojos son los "peligrosos" (Alta influencia + Mal ajuste).
p_diag <- ggplot(diagnostics, aes(x = hat_val, y = std_resid)) +
  geom_point(aes(color = cooks_d > cooks_threshold), alpha = 0.6) +
  geom_hline(yintercept = c(-3, 3), linetype = "dashed", color = "red") +
  geom_vline(xintercept = leverage_threshold, linetype = "dashed", color = "blue") +
  scale_color_manual(values = c("black", "red"), labels = c("Secure", "Influential")) +
  labs(title = "Diagnostic: Cook's Distance & Influence",
       subtitle = "Red points: Cook's D > 4/(n-k-1). Y axis: Residuals > 3.",
       x = "Leverage (Hat Values)", y = "Standardized Residuals", color = "State") +
  theme_minimal()

# Validación (TotalSF vs Precio)
# Para confirmar que los outliers matemáticos coinciden con los casos "raros" (casas enormes y baratas).
p_val <- ggplot(diagnostics, aes(x = TotalSF, y = SalePrice)) +
  geom_point(aes(color = cooks_d > cooks_threshold), alpha = 0.6) +
  scale_color_manual(values = c("steelblue", "red")) +
  labs(title = "Visual Validation: TotalSF vs Price", 
       subtitle = "Influential points are visually distinguishable") +
  theme_minimal()

grid.arrange(p_diag, p_val, ncol = 2)

# Identificar candidatos a eliminar
# Nos centramos en los que son Influyentes (Cook alto) Y además tienen residuo alto.
outliers_to_remove <- diagnostics %>% 
  filter(cooks_d > cooks_threshold & abs(std_resid) > 3) %>% 
  select(Id, SalePrice, TotalSF, cooks_d, std_resid) %>% 
  arrange(desc(cooks_d))

print(outliers_to_remove)

## ----------------------------------------------------------------------------------------------------------
## ----------------------- VALIDATION: BASE R DIAGNOSTICS ------------------------------
## ----------------------------------------------------------------------------------------------------------
# We perform a double-check using the standard R functions described in class to ensure 
# our previous filtering makes sense.

# 1. Standard Diagnostic Plot
par(mfrow = c(2, 2))
plot(model_diag, main = "Base R Diagnostics")
par(mfrow = c(1, 1)) # Reset layout

# 2. Influence Measures Table
# This function automatically flags points based on internal R thresholds.
inf_measures <- influence.measures(model_diag)
summary_inf <- summary(inf_measures)

# We filter to show only those that R considers highly influential in Cook's D ('cook.d')
print(head(summary_inf))

ids_to_remove <- outliers_to_remove$Id

# --- VERIFICACIÓN DE MAGNITUD (Para decidir si quitamos 17 o menos) ---
print(
  outliers_to_remove %>% 
    select(Id, SalePrice, TotalSF, cooks_d) %>%
    mutate(
      # Cuántas veces supera el umbral (Ratio de severidad)
      Severity = round(cooks_d / cooks_threshold, 1) 
    )
)

# Gráfico de codo para ver el salto
ggplot(outliers_to_remove, aes(x = reorder(Id, -cooks_d), y = cooks_d)) +
  geom_col(fill = "darkred") +
  geom_hline(yintercept = cooks_threshold, linetype = "dashed", color = "blue") +
  labs(title = "Influential points severity",
       x = "ID", y = "Cook's distance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

# Solo eliminamos observaciones que son físicamente anómalas (Casas > 7500 sqft Total con precio bajo).
# Esto corresponde a los IDs 1299 y 524 que vemos extremos en el gráfico de severidad.

ids_final_removal <- outliers_to_remove %>% 
  filter(TotalSF > 7500) %>% 
  pull(Id)

# Creamos 'data_pisos_train_clean' para el modelado.
if(length(ids_final_removal) > 0) {
  data_pisos_train_clean <- data_pisos_train_imputed %>% filter(!Id %in% ids_final_removal)
} else {
  data_pisos_train_clean <- data_pisos_train_imputed
}

## ----------------------------------------------------------------------------------------------------------
## ----------------------- NORMALITY ANALYSIS (TARGET VARIABLE) -------------------------------------
## ----------------------------------------------------------------------------------------------------------

# ESTO LO HACEMOS DOS VECES, MARK LO HACE EN LAS SECCIONES INICIALES


# 1. Visual Inspection
target_df <- data_pisos_train_clean

p1 <- ggplot(target_df, aes(x = SalePrice)) + geom_density(fill = "skyblue", alpha=0.5) + labs(title = "Original")
p2 <- ggplot(target_df, aes(sample = SalePrice)) + stat_qq() + stat_qq_line(color = "red")
p3 <- ggplot(target_df, aes(x = log(SalePrice))) + geom_density(fill = "lightgreen", alpha=0.5) + labs(title = "Log Transformed")
p4 <- ggplot(target_df, aes(sample = log(SalePrice))) + stat_qq() + stat_qq_line(color = "darkgreen")

grid.arrange(p1, p2, p3, p4, ncol = 2)

# 2. Skewness Check & Transformation
skew_val <- e1071::skewness(target_df$SalePrice, na.rm = TRUE)

if(abs(skew_val) > 0.75) {
  data_pisos_train_clean$log_SalePrice <- log(data_pisos_train_clean$SalePrice)
  target_var <- "log_SalePrice"
  print("-> Log transformation applied due to high skewness.")
} else {
  target_var <- "SalePrice"
  print("-> No transformation needed.")
}

## ----------------------------------------------------------------------------------------------------------
## ----------------------- ANÁLISIS CATEGÓRICO (V de Cramer) ------------------------------
## ----------------------------------------------------------------------------------------------------------

# Preparación de variables (Factores con >1 nivel)
data_cat_clean <- data_pisos_train_clean %>%
  select(where(is.factor)) %>%
  select(-any_of("Id")) %>%
  mutate(across(everything(), droplevels))

cat_vars <- names(data_cat_clean)

# A.1. Matriz de Redundancia (V de Cramer)
cramer_matrix <- matrix(0, nrow = length(cat_vars), ncol = length(cat_vars))
rownames(cramer_matrix) <- colnames(cramer_matrix) <- cat_vars

# Bucle optimizado usando la función estándar CramerV
cramer_matrix <- PairApply(data_cat_clean, FUN = CramerV, symmetric = TRUE)

# Visualizar redundancias altas (> 0.8)
redundant_idx <- which(cramer_matrix > 0.8 & cramer_matrix < 1, arr.ind = TRUE)

  redundant_df <- data.frame(
    Var1 = rownames(cramer_matrix)[redundant_idx[,1]],
    Var2 = colnames(cramer_matrix)[redundant_idx[,2]],
    CramerV = cramer_matrix[redundant_idx]
  ) %>% 
    distinct(CramerV, .keep_all = TRUE) %>% 
    arrange(desc(CramerV))
  
  print("--- REDUNDANT PAIRS ---")
  print(redundant_df)

# We eliminate the variable MSSubClass (as for a complicated definition, see dictionary, and redundancy)
data_pisos_train_clean <- data_pisos_train_clean %>% select(-MSSubClass)

## ----------------------------------------------------------------------------------------------------------
## ----------------------- RELEVANCIA CATEGÓRICAS (Kruskal-Wallis) ----------------------------------
## ----------------------------------------------------------------------------------------------------------
# Definir Target y Variables
target_col <- "log_SalePrice"

# Aseguramos que usamos las variables del dataset limpio
current_cat_vars <- names(select(data_pisos_train_clean, where(is.factor)))
current_cat_vars <- setdiff(current_cat_vars, "Id") # Excluir ID

# 2. Ejecutar Test de Kruskal-Wallis
# H0: La mediana del precio es igual en todos los niveles del factor.
# Si P > 0.05, aceptamos H0 -> La variable NO importa para el precio.

relevance_cat <- data.frame(Variable = character(), P_Value = numeric())

for(var in current_cat_vars){
  try({
    # Formula dinámica: log_SalePrice ~ Variable
    f <- as.formula(paste(target_col, "~", var))
    kt <- kruskal.test(f, data = data_pisos_train_clean)
    
    relevance_cat <- rbind(relevance_cat, data.frame(Variable = var, P_Value = kt$p.value))
  }, silent = TRUE)
}

# 3. Listar las irrelevantes
vars_irrelevant <- relevance_cat %>% 
  filter(P_Value > 0.05) %>% 
  arrange(desc(P_Value))
print(vars_irrelevant)

# no hay irrelevants


## ----------------------------------------------------------------------------------------------------------
## ------------------ ANÁLISIS DE CORRELACIONES Y MULTICOLINEALIDAD (VARIABLES NUMERICAS) -------------------
## ----------------------------------------------------------------------------------------------------------

# Correlation plot

# Correlation matrix con SalePrice al final
numeric_vars <- data_pisos_train_clean %>%
  select(where(is.numeric), -Id, -log_SalePrice) %>%
  relocate(SalePrice, .after = last_col())

cor_matrix <- cor(numeric_vars, use = "pairwise.complete.obs")

# Heatmap completo (triangulo inferior y superior)
correlation_plot <- ggcorrplot(
  cor_matrix,
  method = "square",
  type = "full",
  lab = TRUE,
  lab_size = 2.5,
  tl.cex = 9,
  title = "Correlation Matrix: All Numeric Variables",
  colors = c("darkblue", "white", "darkred")
)
correlation_plot


# ------
# Condition Number (Kappa)
# Medida global de multicolinealidad.
# Regla general: < 10 (Bien), 10-30 (Moderada), > 30 (Severa/Grave)

# Seleccionar solo numéricas y ESCALAR (fundamental para kappa)
X_matrix <- data_pisos_train_clean %>%
  select(where(is.numeric), -Id, -contains("SalePrice")) %>% 
  scale()

# Calcular numero de condicion
cond_val <- kappa(na.omit(X_matrix))

cond_val
#-------


# TotRmsAbvGrd correlacion con otras 2 variables y redundante en significado
# MoSold y YrSold no aportan info relevante para predecir el precio de venta: (r=-0.03 y r=-0.05 con SalePrice)
# variables_a_eliminar <- c("TotRmsAbvGrd", "MoSold", "YrSold")
# data_pisos_train_clean <- data_pisos_train_clean %>%
#   select(-all_of(variables_a_eliminar))


# ------
# Volvemos a probar el condition number de kappa tras eliminar estas variables
X_matrix <- data_pisos_train_clean %>%
  select(where(is.numeric), -Id, -contains("SalePrice")) %>% 
  scale()

# Calcular numero de condicion
cond_val <- kappa(na.omit(X_matrix))

cond_val

# Ha subido ligeramente asi que no las eliminamos
#-------

# VIF (Variance Inflation Factor)
# Identifica que variable especifica causa la inflacion de varianza.
# VIF = 1 (Sin correlacion) | VIF > 5 (Alta) | VIF > 10 (Muy grave)

# 1. Ajustamos un modelo auxiliar con todas las numericas
model_vif <- lm(
  log_SalePrice ~ ., 
  data = data_pisos_train_clean %>% select(where(is.numeric), -Id, -SalePrice)
)

# 2. Calculamos los valores VIF
vif_values <- vif(model_vif)

# 3. Tabla ordenada para ver los culpables
vif_df <- data.frame(Variable = names(vif_values), VIF = vif_values) %>%
  arrange(desc(VIF))

# 4. Visualizacion rapida de las variables peligrosas (VIF > 5)
vif_graph <- vif_df %>%
  ggplot(aes(x = reorder(Variable, VIF), y = VIF)) +
  geom_bar(stat = "identity", fill = "firebrick") +
  geom_hline(yintercept = 10, linetype = "dashed", color = "black") +
  coord_flip() +
  labs(
    title = "Variables con Multicolinealidad Alta (VIF > 10)",
    subtitle = "Estas variables estan 'inflando' la varianza del modelo",
    x = "Variable",
    y = "VIF"
  ) +
  theme_minimal()

print(vif_graph)

# Diagnostico Matematico (Determinante y Autovalores)
# Confirma matematicamente la severidad detectada por el VIF.

# A) Determinante de la matriz de correlacion
# 1 = Independencia total | 0 = Multicolinealidad perfecta
det_val <- det(cor(numeric_vars))
cat("DETERMINANTE DE LA MATRIZ:", format(det_val, scientific = FALSE), "\n")
if (det_val < 0.01) {
  cat(" ALERTA: Determinante cercano a 0. Redundancia muy alta.\n")
}
# B) Autovalores (Eigenvalues)
# Valores cercanos a 0 indican dimensiones redundantes
eigen_val <- eigen(cor(numeric_vars))$values
cat("Autovalores mas pequeños (cercanos a 0 son el problema):\n")
print(tail(round(eigen_val, 5), 5))


## ----------------------------------------------------------------------------------------------------------
## ------------------------------------------- PCA EXPLORATORIO ---------------------------------------------
## ----------------------------------------------------------------------------------------------------------

# 1. Preparación correcta de los datos
# Seleccionamos solo las predictoras numéricas (excluimos el target SalePrice)
# El PCA es muy sensible a escalas, por lo que el escalado es OBLIGATORIO (scale.unit = TRUE)
X_pca <- data_pisos_train_imputed %>%
  select(where(is.numeric), -SalePrice)

# Contar número de variables numéricas que quedan
num_numeric_vars <- data_pisos_train_clean %>%
  select(where(is.numeric), -Id, -SalePrice, -log_SalePrice) %>%
  ncol()

num_numeric_vars

# 2. Ejecución del PCA con FactoMineR
# ncp = 10: Guardamos las primeras 10 dimensiones para analizar
res_pca <- PCA(X_pca, scale.unit = TRUE, ncp = 10, graph = FALSE)

# 3. Scree Plot (Gráfico de sedimentación)
# FUNDAMENTAL: ¿Cuánta información retenemos?
# Buscamos el "codo" o componentes con eigenvalue > 1 (Criterio de Kaiser)
pca_screeplot_eigenvalues <- fviz_screeplot(
  res_pca,
  addlabels = T,
  choice = "eigenvalue"
) +
  geom_abline(
    intercept = 1,
    slope = 0,
    colour = "red",
    linetype = "dashed",
    linewidth = 0.5
  ) +
  annotate(
    "text",
    x = Inf,
    y = 1,
    label = "Kaiser's criterion",
    hjust = 1.05,
    vjust = -0.5,
    color = "red",
    size = 3.5
  ) +
  geom_abline(
    intercept = 0.7,
    slope = 0,
    colour = "orange",
    linetype = "dotted",
    linewidth = 0.5
  ) +
  annotate(
    "text",
    x = Inf,
    y = 0.7,
    label = "Jollife's criterion",
    hjust = 1.05,
    vjust = -0.5,
    color = "orange",
    size = 3.5
  )

pca_screeplot_eigenvalues

pca_screeplot_variance <- fviz_screeplot(
  res_pca,
  addlabels = T,
  choice = "variance"
)

pca_screeplot_variance

# 4. Círculo de Correlaciones (Variables)
# Nos dice CÓMO se relacionan las variables originales con las nuevas dimensiones.
# Coloreamos por 'contrib': Las que más pesan en la definición de los ejes.
var_plot <- fviz_pca_var(
  res_pca,
  col.var = "contrib", # Variables importantes en rojo
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE, # Evita solapamiento de texto
  select.var = list(contrib = 20), # Mostrar solo las 20 Top para no saturar
  alpha.var = 0.8
) +
  labs(
    title = "Mapa de Variables (Top 20 Contribuciones)",
    subtitle = "Vectores cercanos = Alta correlación positiva | Opuestos = Negativa | 90º = No relacionadas"
  )
print(var_plot)

## Contribution plots
pca_contribution_dim1 <- fviz_contrib(
  res_pca,
  choice = "var",
  axes = 1,
  top = 10
)
pca_contribution_dim2 <- fviz_contrib(
  res_pca,
  choice = "var",
  axes = 2,
  top = 10
)
pca_contribution_dim3 <- fviz_contrib(
  res_pca,
  choice = "var",
  axes = 3,
  top = 10
)

grid.arrange(
  pca_contribution_dim1,
  pca_contribution_dim2,
  pca_contribution_dim3,
  ncol = 3
)

## Correlation between original variables and PC's
cor_matrix_pca <- res_pca$var$coord[, 1:6]
rownames(cor_matrix_pca) <- unlist(var_label(data_pisos_train_imputed)[rownames(
  cor_matrix_pca
)])
cor_matrix_pca

heatmap_correlations_PCA_originalvariables <- pheatmap(
  cor_matrix_pca,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  display_numbers = TRUE,
  number_format = "%.2f",
  fontsize_number = 8,
  fontsize_row = 9,
  fontsize_col = 10,
  color = colorRampPalette(c("blue", "white", "red"))(100),
  main = "Correlations between original variables and PCs (1-6)",
  cellwidth = 50,
  cellheight = 30,
  margins = c(2, 4)
)

heatmap_correlations_PCA_originalvariables

# 5. Tabla de Contribuciones (Interpretación analítica)

fviz_pca_var(
  res_pca,
  col.var = "contrib", # Variables importantes en rojo
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE, # Evita solapamiento de texto
  select.var = list(contrib = 20), # Mostrar solo las 20 Top para no saturar
  alpha.var = 0.8
) +
  labs(
    title = "Mapa de Variables (Top 20 Contribuciones)",
    subtitle = "Vectores cercanos = Alta correlación positiva | Opuestos = Negativa | 90º = No relacionadas"
  )

# ¿Qué define a la Dimensión 1 y 2? (Esencial para dar sentido de negocio)
cat("\n--- Top 10 variables que definen la Dimensión 1 (Eje X) ---\n")
print(head(sort(res_pca$var$contrib[, 1], decreasing = TRUE), 10))

cat("\n--- Top 10 variables que definen la Dimensión 2 (Eje Y) ---\n")
print(head(sort(res_pca$var$contrib[, 2], decreasing = TRUE), 10))

## Individuals plots
pca_individuals_neighbourhood <- fviz_pca_ind(
  res_pca,
  label = "none",
  axes = c(1, 2),
  geom = "point",
  habillage = data_pisos_train_imputed$Neighborhood,
  pointshape = 19
) +
  xlim(c(-3, 5))

pca_individuals_neighbourhood


## Rotación varimax para ver patrones ocultos en los datos
# r
# ...existing code...
# Ejemplo: rotación varimax y oblimin para PCA exploratorio

# Requiere: install.packages(c("psych","GPArotation","pheatmap"))
library(psych)
library(GPArotation)
library(pheatmap)

# 1) Preparar matriz numérica (usar dataset ya imputado)
X <- data_pisos_train_imputed |>
  select(where(is.numeric), -Id, -SalePrice)

# 2) Estandarizar
X_scaled <- scale(X)

# 3) Eigenvalues para decidir nº de factores (criterio Kaiser > 1)
eig_vals <- eigen(cor(X_scaled))$values
nfactors <- sum(eig_vals > 1) # puedes ajustar manualmente
# opcional: limitar a un máximo razonable
nfactors <- min(nfactors, 8)

# 4) PCA + rotación Varimax (ortogonal) y Oblimin (oblicua)
set.seed(123)
pca_varimax <- principal(
  r = X_scaled,
  nfactors = nfactors,
  rotate = "varimax",
  scores = TRUE,
  method = "pc" # componentes principales
)

pca_oblimin <- principal(
  r = X_scaled,
  nfactors = nfactors,
  rotate = "oblimin", # oblimin oblicuo (GPArotation backend)
  scores = TRUE,
  method = "pc"
)

# 5) Extraer cargas y scores
varimax_loadings <- pca_varimax$loadings |> as.matrix()
oblimin_loadings <- pca_oblimin$loadings |> as.matrix()

varimax_scores <- as.data.frame(pca_varimax$scores)
oblimin_scores <- as.data.frame(pca_oblimin$scores)

# 6) Heatmaps rápidos de cargas (abs para ver intensidad)
varimax_cor_matrix <- varimax_loadings

# Heatmap para VARIMAX
heatmap_varimax <- pheatmap(
  varimax_cor_matrix,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  display_numbers = TRUE,
  number_format = "%.2f",
  fontsize_number = 10,
  color = colorRampPalette(c("blue", "white", "red"))(100),
  main = "Correlations: Original Variables vs Varimax Rotated Components",
  fontsize_row = 8,
  breaks = seq(-1, 1, length.out = 101),
  legend_breaks = c(-1, -0.5, 0, 0.5, 1)
)

heatmap_varimax

# OBLIMIN: Para rotación oblicua, usar la estructura de patrones (pattern matrix)
# En oblimin, las cargas no son directamente correlaciones por la oblicuidad
# Usamos la "structure matrix" que sí representa correlaciones
oblimin_cor_matrix <- oblimin_loadings

pheatmap(
  oblimin_cor_matrix,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  display_numbers = TRUE,
  number_format = "%.2f",
  fontsize_number = 10,
  color = colorRampPalette(c("blue", "white", "red"))(100),
  main = "Correlations: Original Variables vs Oblimin Rotated Components",
  fontsize_row = 8,
  breaks = seq(-1, 1, length.out = 101),
  legend_breaks = c(-1, -0.5, 0, 0.5, 1)
)

# 7) Resultado final utilizable
rotated_solutions <- list(
  nfactors = nfactors,
  eigenvalues = eig_vals,
  varimax = list(
    model = pca_varimax,
    loadings = varimax_loadings,
    scores = varimax_scores
  ),
  oblimin = list(
    model = pca_oblimin,
    loadings = oblimin_loadings,
    scores = oblimin_scores
  )
)

rotated_solutions
# ...existing code...

## ----------------------------------------------------------------------------------------------------------
## -------------------------------------------------- MODELADO ----------------------------------------------
## ----------------------------------------------------------------------------------------------------------


pacman::p_load(dplyr, tibble, glmnet, MASS, car, readr)

# 0) Target: trabajamos con log(SalePrice)
target_used <- "log_SalePrice"
if (!target_used %in% names(data_pisos_train_clean)) {
  data_pisos_train_clean <- data_pisos_train_clean |>
    dplyr::mutate(log_SalePrice = log(SalePrice))
}

rmse <- function(a, b) sqrt(mean((a - b)^2))

# 1) LASSO (solo para descartar variables)
model_df <- data_pisos_train_clean |>
  dplyr::mutate(y = log(SalePrice)) |>
  dplyr::select(-SalePrice, -log_SalePrice, -bc_SalePrice)

X <- model.matrix(y ~ . - 1, data = model_df |> dplyr::select(-Id))
y <- model_df$y

set.seed(123)
cv_lasso <- cv.glmnet(
  x = X, y = y,
  alpha = 1, family = "gaussian",
  nfolds = 10, standardize = TRUE
)

lasso_fit_1se <- glmnet(
  x = X, y = y,
  alpha = 1, family = "gaussian",
  lambda = cv_lasso$lambda.1se, standardize = TRUE
)

coef_1se <- coef(lasso_fit_1se)
selected_1se <- tibble::tibble(
  feature = rownames(coef_1se),
  coef = as.numeric(coef_1se)
) |>
  dplyr::filter(feature != "(Intercept)", coef != 0)

feature_map <- tibble::tibble(feature = colnames(X)) |>
  dplyr::mutate(variable_original = sub(":.*$", "", feature))

vars_keep_1se <- feature_map |>
  dplyr::filter(feature %in% selected_1se$feature) |>
  dplyr::distinct(variable_original) |>
  dplyr::pull(variable_original)

lasso_results <- list(
  target = target_used,
  lambda_1se = cv_lasso$lambda.1se,
  n_keep = length(vars_keep_1se),
  vars_keep = vars_keep_1se
)

# 2) Dataset post-LASSO (para comparar modelos)
train_reduced <- data_pisos_train_clean |>
  dplyr::select(dplyr::any_of(c("Id", "SalePrice", "log_SalePrice", vars_keep_1se))) |>
  dplyr::mutate(y = log_SalePrice)

# Split fijo para comparación 
set.seed(123)
idx_tr <- sample(seq_len(nrow(train_reduced)), size = floor(0.8 * nrow(train_reduced)))
train_tr <- train_reduced[idx_tr, ]
train_va <- train_reduced[-idx_tr, ]

train_tr2 <- train_tr |> dplyr::mutate(Id = as.character(Id)) |> dplyr::select(-Id)
train_va2 <- train_va |> dplyr::mutate(Id = as.character(Id)) |> dplyr::select(-Id)

# 3) OLS base (post-LASSO) + diagnósticos básicos
ols_base <- lm(y ~ . - SalePrice - log_SalePrice, data = train_tr2)
pred_ols <- predict(ols_base, newdata = train_va2)
rmse_ols <- rmse(train_va$y, pred_ols)

vif_vals <- car::vif(ols_base)
cond_num <- kappa(scale(model.matrix(ols_base)[, -1, drop = FALSE]))

diag_summary <- list(
  rmse_ols = rmse_ols,
  vif_top = sort(vif_vals, decreasing = TRUE) |> head(10),
  condition_number = cond_num
)

# 4) Stepwise AIC (post-LASSO) y RMSE en validación
ols_aic <- MASS::stepAIC(ols_base, direction = "both", trace = FALSE)
pred_aic <- predict(ols_aic, newdata = train_va2)
rmse_aic <- rmse(train_va$y, pred_aic)

# 5) Stepwise BIC (post-LASSO) y RMSE en validación
ols_bic <- step(ols_base, direction = "both", k = log(nrow(train_tr2)), trace = 0)
pred_bic <- predict(ols_bic, newdata = train_va2)
rmse_bic <- rmse(train_va$y, pred_bic)

# 6) Ridge (post-LASSO) y RMSE comparable
x_tr <- model.matrix(y ~ . - 1 - Id - SalePrice - log_SalePrice, data = train_tr)
x_va <- model.matrix(y ~ . - 1 - Id - SalePrice - log_SalePrice, data = train_va)

set.seed(123)
cv_ridge <- cv.glmnet(
  x = x_tr, y = train_tr$y,
  alpha = 0, family = "gaussian",
  nfolds = 10, standardize = TRUE
)

ridge_fit <- glmnet(
  x = x_tr, y = train_tr$y,
  alpha = 0, family = "gaussian",
  lambda = cv_ridge$lambda.min, standardize = TRUE
)

pred_ridge <- as.numeric(predict(ridge_fit, newx = x_va))
rmse_ridge <- rmse(train_va$y, pred_ridge)

# 7) Tabla de comparación (storytelling)
results_compare <- tibble::tibble(
  model = c("OLS_postLASSO", "OLS_stepAIC", "OLS_stepBIC", "Ridge_CV"),
  rmse = c(rmse_ols, rmse_aic, rmse_bic, rmse_ridge)
) |>
  dplyr::arrange(rmse)

# Decisión automática: eligir el menor RMSE; si empatan, prioriza OLS_base por simplicidad
best_model_name <- results_compare$model[1]
ols_selected <- dplyr::case_when(
  best_model_name == "OLS_stepAIC" ~ "AIC",
  best_model_name == "OLS_stepBIC" ~ "BIC",
  TRUE ~ "BASE"
)

# 8) MODELO FINAL (entrenar en TODO TRAIN limpio, post-LASSO) + predicción TEST + export

# 0) Limpiar lista de variables seleccionadas (evitar target/leakage)
vars_keep_final <- setdiff(vars_keep_1se, c("SalePrice", "log_SalePrice", "bc_SalePrice"))

# 1) TRAIN final (log) con predictores limpios
train_final_model_log <- data_pisos_train_clean |>
  dplyr::select(dplyr::any_of(vars_keep_final), SalePrice) |>
  dplyr::mutate(y = log(SalePrice)) |>
  dplyr::select(-SalePrice)

ols_final_log <- lm(y ~ ., data = train_final_model_log)

names(model.frame(ols_final_log))

# 2) TEST con los mismos predictores
test_reduced_log <- data_pisos_test_imputed |>
  dplyr::select(dplyr::any_of(vars_keep_final))

pred_log_test <- as.numeric(predict(ols_final_log, newdata = test_reduced_log))

export_test <- tibble::tibble(
  `Predicted.price` = exp(pred_log_test)
)

# readr::write_csv(export_test, "results/predicted_price_test.csv")
# export_test
