# -----------------------------------------------------------------
# SECCIÓN 1: CONFIGURACIÓN INICIAL Y CARGA DE LIBRERÍAS ------
# -----------------------------------------------------------------

# Usamos pacman para los paquetes
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  car,
  caret,
  corrplot,
  DataExplorer,
  DescTools,
  dplyr,
  e1071,
  factoextra,
  FactoMineR,
  GGally,
  ggcorrplot,
  ggpubr,
  glmnet,
  GPArotation,
  gridExtra,
  gt,
  gtsummary,
  janitor,
  labelled,
  leaflet,
  MASS,
  Matrix,
  mgcv,
  mice,
  naniar,
  pheatmap,
  psych,
  sf,
  tidyverse
)

## 1.2. Definir operador personalizado
`%notin%` <- Negate("%in%")

## 1.3. Configurar reproducibilidad
# Eliminamos variables previas y fijamos la semilla
rm(list = ls())
set.seed(12345)


# -----------------------------------------------------------------
# SECCIÓN 2: CARGA Y PREPARACIÓN INICIAL DE DATOS ------
# -----------------------------------------------------------------
## 2.1. Lectura de datos
# Cargamos dataset de entrenamiento y prueba
# El Id se lee como character (por defecto viene numérico)

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

data_pisos_test11 <- read.csv(
  "./data/test.csv",
  encoding = "UTF-8",
  colClasses = c("character", rep(NA, 80)),
  stringsAsFactors = TRUE
)


## 2.2. Combinar train y test
# Reunimos ambos datasets para procesamiento conjunto
data_pisos <- rbind(data_pisos_train, data_pisos_test)

# -----------------------------------------------------------------
# SECCIÓN 3: LIMPIEZA DE DATOS - GESTIÓN DE VALORES FALTANTES ------
# -----------------------------------------------------------------

## 3.1. Imputación de valores faltantes según diccionario de datos
# Para variables categóricas donde NA tiene significado específico (ej: "No Garage", "No Pool")
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

# -----------------------------------------------------------------
# SECCIÓN 4: CREACIÓN DE VARIABLES TRANSFORMADAS ------
# -----------------------------------------------------------------

## 4.1. Agrupar y sintetizar variables redundantes
# Combinamos variables relacionadas para mejorar interpretabilidad y reducir multicolinealidad

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
    IsRemodeled = ifelse(YearRemodAdd > YearBuilt, "Yes", "No")
  )

# -----------------------------------------------------------------
# SECCIÓN 5: ANÁLISIS DE VARIABLES TRANSFORMADAS NUEVAS ------
# -----------------------------------------------------------------

## 5.1. Analizar correlación de las nuevas variables con SalePrice

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
  dplyr::select(all_of(new_vars), SalePrice) |>
  dplyr::select_if(is.numeric) %>%
  cor(use = "pairwise.complete.obs") |>
  as.data.frame() |>
  dplyr::select(SalePrice) |>
  arrange(desc(abs(SalePrice)))

## 5.2. Visualización de nuevas variables vs SalePrice

plot_data <- data_pisos |>
  dplyr::select(TotalBath, TotalSF, HouseAge, SalePrice)

# Gráfico 1: TotalSF vs SalePrice
p1 <- ggplot(plot_data, aes(x = TotalSF, y = SalePrice)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "TotalSF vs SalePrice", subtitle = "Correlation: 0.78") +
  theme_minimal()

# Gráfico 2: TotalBath vs SalePrice
p2 <- ggplot(plot_data, aes(x = TotalBath, y = SalePrice)) +
  geom_point(alpha = 0.4, color = "darkgreen") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "TotalBath vs SalePrice", subtitle = "Correlation: 0.63") +
  theme_minimal()

# Gráfico 3: HouseAge vs SalePrice
p3 <- ggplot(plot_data, aes(x = HouseAge, y = SalePrice)) +
  geom_point(alpha = 0.4, color = "purple") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "HouseAge vs SalePrice", subtitle = "Correlation: -0.52") +
  theme_minimal()

# Comparación: GrLivArea original vs TotalSF
p4 <- data_pisos |>
  ggplot(aes(x = GrLivArea, y = SalePrice)) +
  geom_point(alpha = 0.4, color = "orange") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "GrLivArea vs SalePrice (Original)",
    subtitle = "Correlation: 0.71"
  ) +
  theme_minimal()

plot_correlation_newvars = grid.arrange(p1, p4, p2, p3, ncol = 2)


# ----------------------------------------------------------------------------
# SECCIÓN 6: IDENTIFICACIÓN Y ELIMINACIÓN DE VARIABLES SIN VARIABILIDAD ------
# ----------------------------------------------------------------------------

## Variables sin variabilidad -> no aportan nada al modelo -> eliminar
data_for_nzv <- data_pisos |>
  dplyr::select(-Id, -SalePrice) |>
  mutate(across(where(is.character), as.factor))

# Detectar variables con near-zero variance
nzv_info <- nearZeroVar(data_for_nzv, saveMetrics = TRUE)

nzv_vars <- nzv_info |>
  rownames_to_column("variable") |>
  filter(nzv == TRUE | zeroVar == TRUE)

## 6.2. Visualizar variables problemáticas (baja variabilidad)

# Gráfico 1: Número de cocinas (casi todas tienen 1)
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

# Gráfico 2: Servicios disponibles (casi todos "AllPub")
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


# Gráfico 3: Tipo de calle (casi todos "Pave")
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


# Gráfico 4: Presencia de piscina (muy pocas)
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
    subtitle = "99.6% have no pool → Near-zero variance",
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


# -----------------------------------------------------------------
# SECCIÓN 7: ELIMINACIÓN DE VARIABLES REDUNDANTES ------
# -----------------------------------------------------------------

## 7.1. Crear dataset final eliminando variables sin variabilidad y redundantes
data_pisos_final <- data_pisos |>
  dplyr::select(-all_of(variables_eliminar_sinvariabilidad)) %>%
  # Eliminar variables redundantes
  dplyr::select(
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

## 7.2. Separar de nuevo en train y test
## eliminamos la variable respuesta de test
data_pisos_train <- data_pisos_final %>%
  filter(conjunto == "train") %>%
  dplyr::select(-conjunto)

data_pisos_test <- data_pisos_final %>%
  filter(conjunto == "test") %>%
  dplyr::select(-conjunto, -SalePrice)

# -----------------------------------------------------------------
# SECCIÓN 8: ANÁLISIS EXPLORATORIO (EDA) - VARIABLE RESPUESTA ------
# -----------------------------------------------------------------

## 8.1. Análisis de la variable respuesta (SalePrice)

# Gráfico 1: Histograma escala normal (distribución sesgada)
p1 <- data_pisos_train |>
  ggplot(aes(x = SalePrice)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7, color = "white") +
  scale_x_continuous(labels = scales::dollar) +
  labs(
    # title = "SalePrice Distribution",
    # subtitle = "Right-skewed distribution",
    x = "SalePrice",
    y = "Absolute Frequency"
  ) +
  theme_minimal()+ theme(rect = element_rect(fill = "transparent"))

# Gráfico 2: Q-Q plot escala normal
p2 <- data_pisos_train |>
  ggplot(aes(sample = SalePrice)) +
  stat_qq(color = "steelblue", alpha = 0.7) +
  stat_qq_line(color = "black", linewidth = .5) +
  labs(
    # title = "Q-Q Plot: SalePrice",
    # subtitle = "Heavy right tail visible",
    x = "Theoretical Quantiles",
    y = "SalePrice" 
  ) +
  theme_minimal() + theme(rect = element_rect(fill = "transparent"))

# Gráfico 3: Histograma escala logarítmica (más normal)
p3 <- data_pisos_train |>
  ggplot(aes(x = SalePrice)) +
  geom_histogram(bins = 50, fill = "coral", alpha = 0.7, color = "white") +
  scale_x_log10(labels = scales::dollar) +
  labs(
    # title = "log(SalePrice) Disrtibution",
    # subtitle = "More normal after log transformation",
    x = "log(SalePrice)",
    y = "Absolute Freqency"
  ) +
  theme_minimal()+ theme(rect = element_rect(fill = "transparent"))

# Gráfico 4: Q-Q plot escala logarítmica
p4 <- data_pisos_train |>
  ggplot(aes(sample = log(SalePrice))) +
  stat_qq(color = "coral", alpha = 0.7) +
  stat_qq_line(color = "black", linewidth = .5) +
  labs(
    # title = "Q-Q Plot: log(SalePrice)",
    # subtitle = "Closer to normal distribution",
    x = "Theoretical Quantiles",
    y = "log(SalePrice)" 
  ) +
  theme_minimal()+ theme(rect = element_rect(fill = "transparent"))

grafico_exploratorio_respuesta <- grid.arrange(p1, p2, p3, p4, ncol = 2)

## 8.2. Correlaciones: variables numéricas con SalePrice

correlations <- data_pisos_train |>
  dplyr::select(where(is.numeric), -Id) |>
  cor(use = "complete.obs", method = "spearman") |>
  as.data.frame() |>
  rownames_to_column("variable") |>
  dplyr::select(variable, SalePrice) |>
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



# -----------------------------------------------------------------
# SECCIÓN 9: ANÁLISIS EXPLORATORIO - VARIABLES CATEGÓRICAS ------
# -----------------------------------------------------------------

## 9.1. Relación de variables categóricas con el precio

# Gráfico 1: Calidad general vs Precio
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
    # subtitle = "Clear positive relationship",
    x = "Overall Quality (1-10)",
    y = "Sale Price"
  ) +
  theme_minimal()

# Gráfico 2: Vecindario vs Precio (ordenado por mediana)
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
    # subtitle = "High variability across locations",
    x = NULL,
    y = "Sale Price"
  ) +
  theme_minimal()

# Gráfico 3: Calidad de fachada vs Precio
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

# Gráfico 4: Calidad de cocina vs Precio
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

grafico_correlaciones_categoricas = grid.arrange(p1, p2, p3, p4, ncol = 2)

## 9.2. Identificar variables categóricas redundantes mediante correlación

# Convertir a numérico para calcular correlaciones
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
  dplyr::select(all_of(quality_vars), SalePrice) |>
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

# Matriz de correlaciones entre variables de calidad
cor_quality <- cor(quality_numeric, use = "complete.obs")

graficos_heatmap_correlation = pheatmap(
  cor_quality,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  display_numbers = TRUE,
  number_format = "%.2f",
  fontsize_number = 10,
  color = colorRampPalette(c("steelblue", "white", "coral"))(100),
  main = "Correlations: Categorical variables related to quality",
  fontsize_row = 8,
  breaks = seq(-1, 1, length.out = 101),
  legend_breaks = c(-1, -0.5, 0, 0.5, 1)
)


## 9.3. Visualizar pares altamente correlacionados

# Gráfico 1: ExterQual vs KitchenQual
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

# Gráfico 2: GarageQual vs GarageCond (REDUNDANCIA ALTA)
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

# Gráfico 3: ExterQual → Precio (buen predictor)
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

# Gráfico 4: ExterCond → Precio (predictor débil)
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

graficos_pares_altamente_correlacionados = grid.arrange(p1, p2, p3, p4, ncol = 2)

## 9.4. Top variables categóricas por correlación con precio
top_categorical_vars <- c(
  "OverallQual",
  "Neighborhood",
  "ExterQual",
  "KitchenQual",
  "BsmtQual",
  "FireplaceQu"
)

# Calcular median price por nivel de cada variable (level se guarda como character para poder bind_rows)
median_prices <- purrr::map_dfr(top_categorical_vars, function(var) {
  data_pisos_train |>
    filter(complete.cases(data_pisos_train)) |>
    group_by(!!rlang::sym(var)) |>
    summarise(
      median_price = median(SalePrice, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    ) |>
    mutate(
      variable = var,
      level = as.character(!!rlang::sym(var))
    )
})

# Orden por variable:
# - FireplaceQu: orden fijo de "mejor a peor"
# - Resto: orden por mediana (dentro de cada facet)
fireplace_levels_desc <- c("Ex", "Gd", "TA", "Fa", "Po", "No Fireplace")

median_prices <- median_prices |>
  group_by(variable) |>
  mutate(
    level = dplyr::if_else(
      variable == "FireplaceQu",
      as.character(level),
      as.character(forcats::fct_reorder(level, median_price, .desc = FALSE))
    )
  ) |>
  ungroup() |>
  mutate(
    level = ifelse(variable == "FireplaceQu", level, level),
    level = as.character(level)
  )

# Para que ggplot respete el orden FIXED solo en FireplaceQu sin romper el resto,
# creamos una columna específica para el eje (x) con el factor correcto por fila.

# Orden fijo FireplaceQu (mejor -> peor) y resto por mediana (mayor -> menor)
fireplace_levels_desc <- c("Ex", "Gd", "TA", "Fa", "Po", "No Fireplace")

median_prices <- median_prices |>
  mutate(
    level_plot = dplyr::case_when(
      variable == "FireplaceQu" ~ factor(level, levels = fireplace_levels_desc),
      TRUE ~ forcats::fct_reorder(level, median_price, .desc = TRUE)
    )
  )

# Plot: Median price by level for top categorical variables
grafico_median_price_by_level = ggplot(
  median_prices,
  aes(x = level_plot, y = median_price, fill = variable)
) +
  geom_col(alpha = 0.8) +
  geom_text(
    aes(label = scales::dollar(median_price, accuracy = 1)),
    hjust = -0.1,
    size = 2.5
  ) +
  scale_y_continuous(labels = scales::dollar, limits = c(0, 450000)) +
  scale_fill_viridis_d(name = "Variable") +
  coord_flip() +
  scale_x_discrete(limits = rev) +
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


# -----------------------------------------------------------------
# SECCIÓN 10: ELIMINACIÓN DE VARIABLES CATEGÓRICAS REDUNDANTES ------
# -----------------------------------------------------------------

## 10.1. Identificar y eliminar variables altamente correlacionadas

variables_categoricas_redundantes <- c("GarageCond", "ExterCond", "Exterior2nd")
data_pisos_train <- data_pisos_train %>%
  dplyr::select(-all_of(variables_categoricas_redundantes))
data_pisos_test <- data_pisos_test %>%
  dplyr::select(-all_of(variables_categoricas_redundantes))


# -----------------------------------------------------------------
# SECCIÓN 11: ANÁLISIS DETALLADO DE VALORES FALTANTES ------
# -----------------------------------------------------------------





library(ggview)

## 11.1. Visualización general de missings
missing_values = plot_missing(data_pisos_train, missing_only = TRUE)

# Patrón de valores faltantes
graficos_vis_miss = vis_miss(data_pisos_train, warn_large_data = FALSE) +
  labs(
    title = "Missing Value Pattern in Training Set",
    subtitle = "Visual overview of missing data across all variables"
  )

# Intersecciones de patrones de faltantes
graficos_missing_upset = gg_miss_upset(data_pisos_train, nsets = 5, nintersects = 10)

# LotFrontage missingness by Neighborhood
graficos_missing_neighborhood = data_pisos_train |>
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


# LotFrontage missingness by LotShape
graficos_missing_lotshape = data_pisos_train |>
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

# LotArea distribution by LotFrontage missingness
graficos_missing_lotfrontage = data_pisos_train |>
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


# SalePrice distribution by LotFrontage missingness
graficos_missing_price = data_pisos_train |>
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
    position = position_nudge(y = 0.04),
    size = 4,
    fontface = "bold"
  ) +
  scale_fill_manual(values = c("coral", "steelblue")) +
  scale_y_log10(labels = scales::dollar) +
  labs(
    # title = "SalePrice Distribution by LotFrontage Availability",
    # subtitle = "Missing LotFrontage has slightly HIGHER median price → not ignorable",
    y = "SalePrice (log scale)",
    x = "LotFrontage Status"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# -----------------------------------------------------------------------------
# SECCIÓN 12: IMPUTACIÓN ROBUSTA ------
# -----------------------------------------------------------------------------

# 1. LIMPIEZA PREVIA DE OUTLIERS (IMPORTANTE)
# Quitamos las casas "trampa" del Train ANTES de juntar nada.
# Si las dejamos, MICE podría aprender patrones erróneos de esas casas raras.
# data_pisos_train_clean_rows <- data_pisos_train %>%
#  filter(!(GrLivArea > 4000 & SalePrice < 450000))

# print(paste("Filas de train tras quitar outliers:", nrow(data_pisos_train_clean_rows)))

# 2. PREPARAR DATOS COMBINADOS
# -----------------------------------------------------------------------------
data_full_imputation <- bind_rows(
  data_pisos_train %>% mutate(is_test = FALSE),
  data_pisos_test %>% mutate(SalePrice = NA, is_test = TRUE)
)

# Convertir caracteres a factores para que MICE funcione bien
data_full_imputation <- data_full_imputation %>%
  mutate_if(is.character, as.factor)

# 3. CONFIGURAR VECTOR IGNORE
# -----------------------------------------------------------------------------
# TRUE = Fila de Test (se imputa, pero NO se usa para entrenar el modelo de imputación)
ignore_vec <- data_full_imputation$is_test

cat("\n=== Ejecutando Imputación Unificada (Entrenando solo con TRAIN) ===\n")

# 4. EJECUTAR MICE
# -----------------------------------------------------------------------------
# Excluimos 'is_test' y 'Id' de la matriz de predicción para no confundir
# 'quickpred' ayuda a seleccionar solo variables correlacionadas si va lento
impute_unified <- mice(
  data_full_imputation %>% dplyr::select(-is_test, -Id),
  m = 1,
  method = "rf", # Random Forest es robusto para esto
  ignore = ignore_vec,
  maxit = 5,
  seed = 123,
  printFlag = TRUE
)

# 5. RECUPERAR DATOS SEPARADOS
# -----------------------------------------------------------------------------
data_full_completed <- complete(impute_unified, 1)

# Añadimos de nuevo el ID que quitamos temporalmente en el mice
data_full_completed$Id <- data_full_imputation$Id

# Recuperar Train Imputado
data_pisos_train_imputed <- data_full_completed %>%
  filter(!ignore_vec) %>%
  as_tibble()

# Recuperar Test Imputado (sin SalePrice)
data_pisos_test_imputed <- data_full_completed %>%
  filter(ignore_vec) %>%
  dplyr::select(-SalePrice) %>%
  as_tibble()

print("¡Imputación finalizada sin Data Leakage!")
print(paste("NAs en Train:", sum(is.na(data_pisos_train_imputed))))
print(paste("NAs en Test:", sum(is.na(data_pisos_test_imputed))))

## ----------------------------------------------------------------------------------------------------------
# SECCIÓN 13: DIAGNÓSTICO DE INFLUENTIAL POINTS ------
## ----------------------------------------------------------------------------------------------------------

# Ajustar modelo preliminar para diagnóstico (usando solo numéricas)
numeric_data_diag <- data_pisos_train_imputed %>%
  dplyr::select(where(is.numeric))
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
cooks_threshold <- 4 / (n - k - 1) # Cook
leverage_threshold <- 2 * (k + 1) / n # Leverage

# Visualización
# Gráfico de Influencia vs Residuos: Los puntos rojos son los "peligrosos" (Alta influencia + Mal ajuste).
p_diag <- ggplot(diagnostics, aes(x = hat_val, y = std_resid)) +
  geom_point(aes(color = cooks_d > cooks_threshold), alpha = 0.6) +
  geom_hline(yintercept = c(-3, 3), linetype = "dashed", color = "black") +
  geom_vline(
    xintercept = leverage_threshold,
    linetype = "dotted",
    color = "black"
  ) +
  scale_color_manual(
    values = c("steelblue", "coral"),
    labels = c("Secure", "Influential")
  ) +
  labs(
    title = "Diagnostic: Cook's Distance & Influence",
    subtitle = "Red points: Cook's D > 4/(n-k-1). Y axis: Residuals > 3.",
    x = "Leverage (Hat Values)",
    y = "Standardized Residuals",
    color = "State"
  ) +
  theme_minimal()

# Validación (TotalSF vs Precio)
# Para confirmar que los outliers matemáticos coinciden con los casos "raros" (casas enormes y baratas).

# Comparativa: recta con todos vs recta sin puntos de high leverage (Ids 1299 y 524)
ids_high_leverage <- c("1299", "524")

diagnostics_no_hl <- diagnostics |>
  mutate(Id = as.character(Id)) |>
  filter(!Id %in% ids_high_leverage)


p_val <- ggplot(diagnostics, aes(x = TotalSF, y = SalePrice)) +
  geom_point(aes(color = cooks_d > cooks_threshold), alpha = 0.6) +

  # Línea 1: LM con todos los puntos
  geom_smooth(
    aes(linetype = "LM (all points)"),
    method = "lm",
    se = TRUE,
    color = "goldenrod",
    linewidth = 0.9
  ) +

  # Línea 2: LM excluyendo high leverage 1299 y 524
  geom_smooth(
    data = diagnostics_no_hl,
    mapping = aes(
      x = TotalSF,
      y = SalePrice,
      linetype = "LM (excluding Id 1299 & 524)"
    ),
    method = "lm",
    se = TRUE,
    fullrange = TRUE,
    color = "forestgreen",
    linewidth = 0.9
  ) +

  # Leyenda puntos (Cook's D)
  scale_color_manual(
    values = c("FALSE" = "steelblue", "TRUE" = "coral"),
    name = "Cook's D > threshold"
  ) +

  # Leyenda líneas (comparativa)
  scale_linetype_manual(
    values = c(
      "LM (all points)" = "solid",
      "LM (excluding Id 1299 & 524)" = "dashed"
    ),
    name = "Trend lines"
  ) +

  labs(
    title = "Visual Validation: TotalSF vs Price",
    x = "TotalSF",
    y = "SalePrice"
  ) +
  theme_minimal()


influential_points = grid.arrange(p_diag, p_val, ncol = 2)

# Identificar candidatos a eliminar
# Nos centramos en los que son Influyentes (Cook alto) Y además tienen residuo alto.
outliers_to_remove <- diagnostics %>%
  filter(cooks_d > cooks_threshold & abs(std_resid) > 3) %>%
  dplyr::select(Id, SalePrice, TotalSF, cooks_d, std_resid) %>%
  arrange(desc(cooks_d))

print(outliers_to_remove)


## ----------------------------------------------------------------------------------------------------------
# SECCIÓN 14: VALIDATION: BASE R DIAGNOSTICS ------
## ----------------------------------------------------------------------------------------------------------
# We perform a double-check using the standard R functions described in class to ensure
# our previous filtering makes sense.

# 1. Standard Diagnostic Plot
pdf(file = "/home/edugambin/Documents/reg/Concurso-de-prediccion-Regression/results/images/diagnostics.pdf", width = 8, height = 6)
par(mfrow = c(2, 2))
plot(model_diag)
dev.off()
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
    dplyr::select(Id, SalePrice, TotalSF, cooks_d) %>%
    mutate(
      # Cuántas veces supera el umbral (Ratio de severidad)
      Severity = round(cooks_d / cooks_threshold, 1)
    )
)

# Gráfico de codo para ver el salto
ggplot(outliers_to_remove, aes(x = reorder(Id, -cooks_d), y = cooks_d)) +
  geom_col(fill = "darkred") +
  geom_hline(
    yintercept = cooks_threshold,
    linetype = "dashed",
    color = "blue"
  ) +
  labs(title = "Influential points severity", x = "ID", y = "Cook's distance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

# Solo eliminamos observaciones que son físicamente anómalas (Casas > 7500 sqft Total con precio bajo).
# Esto corresponde a los IDs 1299 y 524 que vemos extremos en el gráfico de severidad.

ids_final_removal <- outliers_to_remove %>%
  filter(TotalSF > 7500) %>%
  pull(Id)

# Creamos 'data_pisos_train_clean' para el modelado.
if (length(ids_final_removal) > 0) {
  data_pisos_train_clean <- data_pisos_train_imputed %>%
    filter(!Id %in% ids_final_removal)
} else {
  data_pisos_train_clean <- data_pisos_train_imputed
}


## ----------------------------------------------------------------------------------------------------------
# SECCIÓN 15: NORMALITY ANALYSIS (TARGET VARIABLE) ------
## ----------------------------------------------------------------------------------------------------------

# ESTO LO HACEMOS DOS VECES, MARC LO HACE EN LAS SECCIONES INICIALES

# 1. Visual Inspection
target_df <- data_pisos_train_clean

p1 <- ggplot(target_df, aes(x = SalePrice)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(title = "Original") +
  theme_minimal()
p2 <- ggplot(target_df, aes(sample = SalePrice)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  theme_minimal()
p3 <- ggplot(target_df, aes(x = log(SalePrice))) +
  geom_density(fill = "lightgreen", alpha = 0.5) +
  labs(title = "Log Transformed") +
  theme_minimal()
p4 <- ggplot(target_df, aes(sample = log(SalePrice))) +
  stat_qq() +
  stat_qq_line(color = "darkgreen") +
  theme_minimal()

grid.arrange(p1, p2, p3, p4, ncol = 2)

# 2. Skewness Check & Transformation
skew_val <- e1071::skewness(target_df$SalePrice, na.rm = TRUE)

if (abs(skew_val) > 0.75) {
  data_pisos_train_clean$log_SalePrice <- log(data_pisos_train_clean$SalePrice)
  target_var <- "log_SalePrice"
  print("-> Log transformation applied due to high skewness.")
} else {
  target_var <- "SalePrice"
  print("-> No transformation needed.")
}

## ----------------------------------------------------------------------------------------------------------
# SECCION 16: ANÁLISIS CATEGÓRICO (V de Cramer) ------------------------------
## ----------------------------------------------------------------------------------------------------------

# Preparación de variables (Factores con >1 nivel)
data_cat_clean <- data_pisos_train_clean %>%
  dplyr::select(where(is.factor)) %>%
  dplyr::select(-any_of("Id")) %>%
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
  Var1 = rownames(cramer_matrix)[redundant_idx[, 1]],
  Var2 = colnames(cramer_matrix)[redundant_idx[, 2]],
  CramerV = cramer_matrix[redundant_idx]
) %>%
  distinct(CramerV, .keep_all = TRUE) %>%
  arrange(desc(CramerV))

print("--- REDUNDANT PAIRS ---")
print(redundant_df)

# We eliminate the variable MSSubClass (as for a complicated definition, see dictionary, and redundancy)
data_pisos_train_clean <- data_pisos_train_clean %>% dplyr::select(-MSSubClass)

## ----------------------------------------------------------------------------------------------------------
# SECCION 17: RELEVANCIA CATEGÓRICAS (Kruskal-Wallis) ----------------------------------
## ----------------------------------------------------------------------------------------------------------
# Definir Target y Variables
target_col <- "log_SalePrice"

# Aseguramos que usamos las variables del dataset limpio
current_cat_vars <- names(dplyr::select(
  data_pisos_train_clean,
  where(is.factor)
))
current_cat_vars <- setdiff(current_cat_vars, "Id") # Excluir ID

# 2. Ejecutar Test de Kruskal-Wallis
# H0: La mediana del precio es igual en todos los niveles del factor.
# Si P > 0.05, aceptamos H0 -> La variable NO importa para el precio.

relevance_cat <- data.frame(Variable = character(), P_Value = numeric())

for (var in current_cat_vars) {
  try(
    {
      # Formula dinámica: log_SalePrice ~ Variable
      f <- as.formula(paste(target_col, "~", var))
      kt <- kruskal.test(f, data = data_pisos_train_clean)

      relevance_cat <- rbind(
        relevance_cat,
        data.frame(Variable = var, P_Value = kt$p.value)
      )
    },
    silent = TRUE
  )
}

# 3. Listar las irrelevantes
vars_irrelevant <- relevance_cat %>%
  filter(P_Value > 0.05) %>%
  arrange(desc(P_Value))
print(vars_irrelevant)

# MoSold eliminada por irrelevante + MoYear

## ----------------------------------------------------------------------------------------------------------
# SECCION 18: ANÁLISIS DE CORRELACIONES Y MULTICOLINEALIDAD (VARIABLES NUMERICAS) -------------------
## ----------------------------------------------------------------------------------------------------------

# Correlation plot

# Correlation matrix con SalePrice al final
numeric_vars <- data_pisos_train_clean %>%
  dplyr::select(where(is.numeric), -Id, -log_SalePrice) %>%
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
  dplyr::select(where(is.numeric)) %>% # Solo numéricas
  dplyr::select(-any_of("Id")) %>% # Quitar Id si existe
  dplyr::select(-contains("SalePrice")) %>% # Quitar Target
  scale() # <--- FALTABA ESTO (Escalar)

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
# X_matrix <- data_pisos_train_clean %>%
#   dplyr::select(where(is.numeric), -Id, -contains("SalePrice")) %>%
#   scale()

# Calcular numero de condicion
# cond_val <- kappa(na.omit(X_matrix))

# cond_val

# Ha subido ligeramente asi que no las eliminamos
#-------

# VIF (Variance Inflation Factor)
# Identifica que variable especifica causa la inflacion de varianza.
# VIF = 1 (Sin correlacion) | VIF > 5 (Alta) | VIF > 10 (Muy grave)

# 1. Ajustamos un modelo auxiliar con todas las numericas
model_vif <- lm(
  log_SalePrice ~ .,
  data = data_pisos_train_clean %>%
    dplyr::select(where(is.numeric), -Id, -SalePrice)
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


## Eliminamos variables innecesarias/redundantes/correladas con otros predictores
variables_descartar <- c(unique(vars_irrelevant$Variable))
data_pisos_train_clean <- data_pisos_train_clean %>%
  dplyr::select(-all_of(variables_descartar))

data_pisos_test_imputed <- data_pisos_test_imputed %>%
  dplyr::select(-all_of(variables_descartar))

## ----------------------------------------------------------------------------------------------------------
# SECCION 19: PCA EXPLORATORIO ---------------------------------------------
## ----------------------------------------------------------------------------------------------------------

# 1. Preparación correcta de los datos
# Seleccionamos solo las predictoras numéricas (excluimos el target SalePrice)
# El PCA es muy sensible a escalas, por lo que el escalado es OBLIGATORIO (scale.unit = TRUE)
X_pca <- data_pisos_train_clean %>%
  dplyr::select(where(is.numeric), -SalePrice)

# Contar número de variables numéricas que quedan
num_numeric_vars <- data_pisos_train_clean %>%
  dplyr::select(where(is.numeric), -Id, -SalePrice, -log_SalePrice) %>%
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
rownames(cor_matrix_pca) <- unlist(var_label(data_pisos_train_clean)[rownames(
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
  habillage = data_pisos_train_clean$Neighborhood,
  pointshape = 19
) +
  xlim(c(-3, 5))

pca_individuals_neighbourhood


## Rotación varimax para ver patrones ocultos en los datos
# r
# ...existing code...
# Ejemplo: rotación varimax y oblimin para PCA exploratorio

# 1) Preparar matriz numérica (usar dataset ya imputado)
X <- data_pisos_train_clean |>
  dplyr::select(where(is.numeric), -Id, -SalePrice)

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
# SECCION 20: MODELADO ----------------------------------------------
## ----------------------------------------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# MODELO 1: LASSO REGRESSION (L1 Regularization)
# -----------------------------------------------------------------------------

# 1. PREPARACIÓN DE DATOS (MATRICES)
# -----------------------------------------------------------------------------
# Definimos variable respuesta y predictores
# Eliminamos Id y las versiones originales de SalePrice para evitar data leakage
target_var <- "log_SalePrice"
unused_vars <- c("Id", "SalePrice", "log_SalePrice")

# Creamos la partición de Validación (como pide el PDF para el benchmark)
set.seed(123) # Semilla para reproducibilidad
train_index <- createDataPartition(
  data_pisos_train_clean[[target_var]],
  p = 0.8,
  list = FALSE
)

data_train <- data_pisos_train_clean[train_index, ]
data_val <- data_pisos_train_clean[-train_index, ]

# glmnet necesita matrices numéricas, 'model.matrix' convierte factores a dummies automáticamente
# [,-1] elimina la columna del intercepto que model.matrix crea por defecto
X_train_mat <- model.matrix(
  as.formula(paste(target_var, "~ .")),
  data = data_train[, !names(data_train) %in% setdiff(unused_vars, target_var)]
)[, -1]
y_train_vec <- data_train[[target_var]]

X_val_mat <- model.matrix(
  as.formula(paste(target_var, "~ .")),
  data = data_val[, !names(data_val) %in% setdiff(unused_vars, target_var)]
)[, -1]
y_val_vec <- data_val[[target_var]]

# 2. ENTRENAMIENTO CON CROSS-VALIDATION
# -----------------------------------------------------------------------------
# alpha = 1 significa LASSO
# alpha = 0 significa RIDGE
set.seed(123)
cv_lasso <- cv.glmnet(
  x = X_train_mat,
  y = y_train_vec,
  alpha = 1, # LASSO puro
  family = "gaussian", # Regresión lineal
  nfolds = 10, # 10-fold CV
  standardize = TRUE # Importante: estandarizar variables para penalización justa
)

# 3. RESULTADOS Y SELECCIÓN DE LAMBDA
# -----------------------------------------------------------------------------
# Lambda.min: el que minimiza el error en CV
# Lambda.1se: el modelo más sencillo (más penalizado) dentro de 1 error estándar del mínimo (más robusto)
best_lambda <- cv_lasso$lambda.1se

print(paste("Mejor Lambda (1se):", round(best_lambda, 5)))

# Gráfico de la evolución de coeficientes vs Lambda
plot(cv_lasso)
title("LASSO: Mean-Squared Error vs Log(Lambda)", line = 2.5)

# 4. EVALUACIÓN EN SET DE VALIDACIÓN
# -----------------------------------------------------------------------------
preds_lasso <- predict(cv_lasso, newx = X_val_mat, s = "lambda.1se")

# Calculamos RMSE (Root Mean Squared Error)
rmse_lasso <- sqrt(mean((preds_lasso - y_val_vec)^2))

print(paste("RMSE Lasso (Validation Set):", round(rmse_lasso, 4)))

# 5. EXTRACCIÓN DE VARIABLES (FEATURE SELECTION)
# -----------------------------------------------------------------------------
# Extraemos coeficientes y quitamos los que son CERO (los eliminados por Lasso)
coef_matrix <- coef(cv_lasso, s = "lambda.1se")
df_coefs <- data.frame(
  feature = rownames(coef_matrix),
  coefficient = as.numeric(coef_matrix)
) %>%
  filter(coefficient != 0 & feature != "(Intercept)") %>%
  arrange(desc(abs(coefficient)))

print("Top 10 variables más importantes según Lasso:")
print(head(df_coefs, 10))

print(paste("Número total de variables seleccionadas:", nrow(df_coefs)))

# -----------------------------------------------------------------------------
# INTERPRETACIÓN RESULTADOS LASSO
# -----------------------------------------------------------------------------
# 1. Rendimiento (RMSE): solia salir 0.109 ahora sale 0.1127 al quitar YrSold y MoSold (Escala Logarítmica)
#    - El modelo tiene un error medio aproximado del ~11% en el precio de venta.
#    - Es un benchmark muy competitivo para empezar.
#
# 2. Selección de Variables:
#    - Lasso ha seleccionado 75 (antes 85) variables relevantes, reduciendo la dimensionalidad
#      y eliminando ruido. Esto cumple con el objetivo de identificar variables
#      redundantes .
#
# 3. Drivers Principales (Top 10):
#    - Calidad y Condición: Las variables 'OverallQual' y 'OverallCond' dominan.
#      Niveles bajos (Cond3, Cond2) penalizan fuertemente el precio (coef negativos),
#      mientras que calidades altas (Qual9, Qual10) lo disparan.
#    - Ubicación: 'Crawford' aparece como un barrio con premium positivo significativo.
#    - Garage: 'GarageQualEx' es un diferenciador clave de valor.

# -----------------------------------------------------------------------------
# MODELO 2: RIDGE REGRESSION (L2 Regularization)
# -----------------------------------------------------------------------------
# Nota: Usamos las mismas matrices X_train_mat, y_train_vec creadas en Lasso

# 1. ENTRENAMIENTO (Alpha = 0)
# -----------------------------------------------------------------------------
set.seed(123)
cv_ridge <- cv.glmnet(
  x = X_train_mat,
  y = y_train_vec,
  alpha = 0, # RIDGE puro
  family = "gaussian",
  nfolds = 10,
  standardize = TRUE
)

# 2. SELECCIÓN DE LAMBDA
# -----------------------------------------------------------------------------
best_lambda_ridge <- cv_ridge$lambda.min
# En Ridge solemos usar lambda.min para maximizar predicción,
# ya que no buscamos eliminar variables como en Lasso.

print(paste("Mejor Lambda Ridge (min):", round(best_lambda_ridge, 5)))

# Gráfico de evolución
plot(cv_ridge)
title("RIDGE: MSE vs Log(Lambda)", line = 2.5)

# 3. EVALUACIÓN (VALIDATION SET)
# -----------------------------------------------------------------------------
preds_ridge <- predict(cv_ridge, newx = X_val_mat, s = "lambda.min")
rmse_ridge <- sqrt(mean((preds_ridge - y_val_vec)^2))

print(paste("RMSE Ridge (Validation Set):", round(rmse_ridge, 4)))

# 4. ANÁLISIS DE COEFICIENTES
# -----------------------------------------------------------------------------
# En Ridge NO habrá coeficientes a cero, pero veremos cuáles tienen mayor peso
coef_ridge <- coef(cv_ridge, s = "lambda.min")
df_coefs_ridge <- data.frame(
  feature = rownames(coef_ridge),
  coefficient = as.numeric(coef_ridge)
) %>%
  filter(feature != "(Intercept)") %>%
  arrange(desc(abs(coefficient)))

print("Top 10 variables más importantes según Ridge:")
print(head(df_coefs_ridge, 10))

# Comparativa rápida en consola
print(paste(
  "Diferencia RMSE (Ridge - Lasso):",
  round(rmse_ridge - rmse_lasso, 5)
))
# Si es negativo, Ridge gana. Si es positivo, Lasso gana.

# -----------------------------------------------------------------------------
# INTERPRETACIÓN RESULTADOS RIDGE
# -----------------------------------------------------------------------------
# 1. Rendimiento (RMSE): 0.1123 (antes 0.1114)
#    - El error es ligeramente INFERIOR al de Lasso (-0.0004).
#    - En este caso, Ridge (que mantiene todas las variables) funciona peor que
#      Lasso (que eliminó variables).
#
#
# 2. Coeficientes:
#    - Ridge penaliza pero no elimina. Vemos que 'OverallQual' y 'GarageQual'
#      siguen siendo los reyes del precio, manteniendo la consistencia con Lasso.

# -----------------------------------------------------------------------------
# MODELO 3: ELASTIC NET (Mezcla Lasso + Ridge)
# -----------------------------------------------------------------------------
# ElasticNet requiere optimizar DOS parámetros:
# 1. Lambda (penalización) -> glmnet lo hace solo con cv.glmnet
# 2. Alpha (mezcla): 0 = Ridge, 1 = Lasso. Nosotros debemos buscar el mejor alpha.

# Definimos una rejilla de alphas para probar (ej: 0.1, 0.2 ... 0.9)
alpha_grid <- seq(0.1, 0.9, by = 0.1)

# Variables para guardar el mejor resultado encontrado
best_enet_rmse <- Inf
best_enet_model <- NULL
best_enet_alpha <- 0

print("Iniciando búsqueda de hiperparámetros para ElasticNet...")

set.seed(123)
for (i in alpha_grid) {
  # Entrenamos con Cross-Validation para este alpha específico
  cv_fit <- cv.glmnet(
    x = X_train_mat,
    y = y_train_vec,
    alpha = i,
    family = "gaussian",
    nfolds = 10,
    standardize = TRUE
  )

  # Predecimos en validación
  pred_val <- predict(cv_fit, newx = X_val_mat, s = "lambda.1se")
  rmse_val <- sqrt(mean((pred_val - y_val_vec)^2))

  # Si este alpha mejora lo que teníamos, lo guardamos como el "campeón"
  if (rmse_val < best_enet_rmse) {
    best_enet_rmse <- rmse_val
    best_enet_model <- cv_fit
    best_enet_alpha <- i
  }
}

# 3. RESULTADOS ELASTIC NET
# -----------------------------------------------------------------------------
print(paste("Mejor Alpha encontrado:", best_enet_alpha))
print(paste("RMSE ElasticNet (Validation):", round(best_enet_rmse, 4)))

# Comparamos con los anteriores
print(paste("Comparativa RMSE:"))
print(paste("Lasso:", round(rmse_lasso, 4)))
print(paste("Ridge:", round(rmse_ridge, 4)))
print(paste("ElasticNet:", round(best_enet_rmse, 4)))

# Si ElasticNet gana, ¿qué variables seleccionó?
coef_enet <- coef(best_enet_model, s = "lambda.1se")
df_coefs_enet <- data.frame(
  feature = rownames(coef_enet),
  coefficient = as.numeric(coef_enet)
) %>%
  filter(coefficient != 0 & feature != "(Intercept)") %>%
  arrange(desc(abs(coefficient)))

print(paste(
  "Variables seleccionadas por el mejor ElasticNet:",
  nrow(df_coefs_enet)
))
print(head(df_coefs_enet, 5))


# -----------------------------------------------------------------------------
# INTERPRETACIÓN RESULTADOS ELASTIC NET
# -----------------------------------------------------------------------------
# 1. Rendimiento (RMSE): 0.1102 (GANADOR ACTUAL)
#    - Ha superado tanto a Lasso (0.1127) como a Ridge (0.1123).
#    - Alpha = 0.4 indica que el modelo óptimo es un híbrido:
#      40% Lasso (selección) + 60% Ridge (contracción).
#
# 2. Selección de Variables:
#    - Se ha quedado con 86 variables. Curiosamente, casi el mismo número que Lasso (75).
#    - Esto confirma firmemente que el "núcleo duro" de información para predecir
#      precios en Ames reside en unas ~75 variables transformadas.
#
# 3. Conclusión de Negocio:
#    - Si tuvieras que poner un modelo en producción hoy, este sería el elegido.
#    - Combina la capacidad de ignorar características irrelevantes con la robustez
#      frente a la multicolinealidad de las variables de calidad (OverallQual/Cond).

# -----------------------------------------------------------------------------
# MODELO 4: PCA REGRESSION (PCR)
# -----------------------------------------------------------------------------

# Definimos variables
target_var <- "log_SalePrice"
unused_vars <- c("Id", "SalePrice", "log_SalePrice")

# Aseguramos partición (semilla 123 para consistencia)
set.seed(123)
train_index <- createDataPartition(
  data_pisos_train_clean[[target_var]],
  p = 0.8,
  list = FALSE
)
data_train <- data_pisos_train_clean[train_index, ]
data_val <- data_pisos_train_clean[-train_index, ]

# Creamos matrices numéricas (dummies)
X_train_mat <- model.matrix(
  as.formula(paste(target_var, "~ .")),
  data = data_train[, !names(data_train) %in% setdiff(unused_vars, target_var)]
)[, -1]
y_train_vec <- data_train[[target_var]]

X_val_mat <- model.matrix(
  as.formula(paste(target_var, "~ .")),
  data = data_val[, !names(data_val) %in% setdiff(unused_vars, target_var)]
)[, -1]
y_val_vec <- data_val[[target_var]]

print("2. Limpiando columnas con varianza cero (usando nearZeroVar)...")

# USAMOS caret::nearZeroVar EN LUGAR DE apply
# Esto detecta columnas con varianza cero de forma segura
nzv_cols_indices <- nearZeroVar(X_train_mat)

if (length(nzv_cols_indices) > 0) {
  print(paste("Eliminando", length(nzv_cols_indices), "columnas constantes..."))
  X_train_clean <- X_train_mat[, -nzv_cols_indices]
  X_val_clean <- X_val_mat[, -nzv_cols_indices]
} else {
  print("No se encontraron columnas constantes.")
  X_train_clean <- X_train_mat
  X_val_clean <- X_val_mat
}

print("3. Ejecutando PCA y optimizando componentes...")

# PCA sobre datos limpios
pca_model <- prcomp(X_train_clean, center = TRUE, scale. = TRUE)

# Proyectar
X_train_pca <- pca_model$x
X_val_pca <- predict(pca_model, newdata = X_val_clean)

# Loop para encontrar el mejor número de componentes
# Probamos hasta 150 componentes (o el máximo disponible si es menor)
max_comps <- min(150, ncol(X_train_pca))
n_components_to_test <- seq(10, max_comps, by = 10)

best_pcr_rmse <- Inf
best_n_comp <- 0
results_pcr <- data.frame(n_comp = integer(), rmse = numeric())

for (k in n_components_to_test) {
  # Crear dataset temporal con k componentes
  train_data_k <- data.frame(y = y_train_vec, X_train_pca[, 1:k])
  val_data_k <- data.frame(y = y_val_vec, X_val_pca[, 1:k])

  # Modelo lineal sobre componentes
  lm_pcr <- lm(y ~ ., data = train_data_k)
  preds_pcr <- predict(lm_pcr, newdata = val_data_k)

  # RMSE
  rmse_k <- sqrt(mean((preds_pcr - y_val_vec)^2))
  results_pcr <- rbind(results_pcr, data.frame(n_comp = k, rmse = rmse_k))

  if (rmse_k < best_pcr_rmse) {
    best_pcr_rmse <- rmse_k
    best_n_comp <- k
  }
}

# 4. RESULTADOS FINALES PCR
# -----------------------------------------------------------------------------
print(paste("Mejor número de componentes:", best_n_comp))
print(paste("RMSE PCR (Validation):", round(best_pcr_rmse, 4)))

# Gráfico
plot(
  results_pcr$n_comp,
  results_pcr$rmse,
  type = "b",
  col = "blue",
  pch = 19,
  xlab = "Nº Componentes",
  ylab = "RMSE",
  main = "PCR Performance"
)
legend("topright", legend = c("PCR"), col = c("blue"), lty = 1)

# -----------------------------------------------------------------------------
# INTERPRETACIÓN RESULTADOS PCR (Principal Component Regression)
# -----------------------------------------------------------------------------
# 1. Rendimiento (RMSE): 0.1244
#    - Este modelo NO ha logrado superar a ElasticNet (0.1102) ni a Lasso/Ridge.
#    - Se sitúa como el de peor desempeño hasta el momento.
#
# 2. Análisis de Componentes:
#    - El modelo necesitó 100 componentes principales para alcanzar su óptimo.
#    - Esto indica que la información del precio está muy dispersa y no se deja
#      comprimir fácilmente en unos pocos "super-factores" lineales sin perder
#      poder predictivo.
#
# 3. ¿Por qué falló?
#    - El PCA es "no supervisado": busca resumir varianza (X) sin mirar el precio (Y).
#    - Es probable que los primeros componentes explicaran mucha varianza geométrica
#      de las casas, pero que la información clave para el precio estuviera en
#      componentes menores que el modelo descartó o diluyó.

# -----------------------------------------------------------------------------
# MODELO 5: REGRESIÓN LOCAL (K-NEAREST NEIGHBORS - KNN)
# -----------------------------------------------------------------------------
# Nota: La función 'loess' estándar no soporta >4 variables.
# Usamos KNN como la implementación robusta de regresión local para alta dimensión.

# 1. PREPARACIÓN
# -----------------------------------------------------------------------------
# KNN es MUY sensible a la escala (distancias). Es obligatorio centrar y escalar.
# Usamos train() de caret que lo hace automáticamente con 'preProcess'.

# Definimos la rejilla de 'k' (vecinos) a probar
# Probamos k impares de 3 a 25
knn_grid <- expand.grid(k = seq(3, 25, by = 2))

# 2. ENTRENAMIENTO CON CROSS-VALIDATION
# -----------------------------------------------------------------------------
set.seed(123)
knn_model <- train(
  x = X_train_clean, # Usamos la matriz limpia sin columnas constantes
  y = y_train_vec,
  method = "knn",
  preProcess = c("center", "scale"), # Estandarización automática
  tuneGrid = knn_grid,
  trControl = trainControl(method = "cv", number = 5) # 5-fold CV interno para elegir k
)

# 3. RESULTADOS
# -----------------------------------------------------------------------------
print(paste("Mejor K seleccionado:", knn_model$bestTune$k))

# Gráfico de RMSE vs Vecinos (K)
plot(knn_model)

# 4. EVALUACIÓN (VALIDATION SET)
# -----------------------------------------------------------------------------
# caret aplica automáticamente la media y desviación estándar de train al validation set
preds_knn <- predict(knn_model, newdata = X_val_clean)

rmse_knn <- sqrt(mean((preds_knn - y_val_vec)^2))

print(paste("RMSE KNN (Validation):", round(rmse_knn, 4)))

# 5. ACTUALIZACIÓN DE TABLA DE LÍDERES
# -----------------------------------------------------------------------------
print("--- TABLA DE LÍDERES (RMSE) ---")
print(paste("1. ElasticNet:", round(best_enet_rmse, 4)))
print(paste("2. Lasso:     ", round(rmse_lasso, 4)))
print(paste("3. Ridge:     ", round(rmse_ridge, 4)))
print(paste("4. KNN:       ", round(rmse_knn, 4)))
print(paste("5. PCR:       ", round(best_pcr_rmse, 4)))


# -----------------------------------------------------------------------------
# INTERPRETACIÓN RESULTADOS KNN (Regresión Local)
# -----------------------------------------------------------------------------
# 1. Rendimiento (RMSE): 0.1784 (EL PEOR)
#    - El error es significativamente más alto que los modelos lineales (~0.108).
#    - KNN ha fallado estrepitosamente.
#
# 2. La Maldición de la Dimensionalidad:
#    - KNN basa sus predicciones en la distancia euclídea entre casas.
#    - Al tener ~90-200 dimensiones (variables), los puntos se vuelven "equidistantes"
#      en el espacio, haciendo que el concepto de "vecino cercano" pierda sentido.
#    - Además, KNN trata todas las variables por igual. El ruido de las variables
#      irrelevantes ha ahogado la señal de las importantes.

# -----------------------------------------------------------------------------
# MODELO 6: GAM (Generalized Additive Models)
# -----------------------------------------------------------------------------

print("Entrenando GAM con Splines Cúbicos...")

# 1. SELECCIÓN DE VARIABLES (Usando las disponibles tras limpieza)
data_train_gam <- data_pisos_train_clean[train_index, ]
data_val_gam <- data_pisos_train_clean[-train_index, ]

# 1. SELECCIONAR CANDIDATAS A SUAVIZAR
# -----------------------------------------------------------------------------
# Solo numéricas (excluimos target e ID)
numeric_predictors <- data_train_gam %>%
  dplyr::select(where(is.numeric), -Id, -SalePrice, -log_SalePrice) %>%
  names()

target_var <- "log_SalePrice"

# 2. FUNCIÓN PARA GENERAR SCATTERPLOT + RECTA + LOESS
# -----------------------------------------------------------------------------
# El objetivo: Si la línea roja (suavizado local) se aleja MUCHO de la azul (lineal),
# necesitamos spline.

plot_linearity_check <- function(data, var_name, target) {
  # Si la variable tiene menos de 10 valores únicos, es casi categórica
  ggplot(data, aes_string(x = var_name, y = target)) +
    geom_point(alpha = 0.3, color = "gray50") +
    geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 1) + # Lineal
    geom_smooth(method = "loess", se = FALSE, color = "red", linewidth = 1) + # Curva local
    labs(
      title = paste("Linearity", var_name),
      subtitle = "Blue=Linear| Red=LOESS",
      x = var_name,
      y = target
    ) +
    theme_minimal() +
    theme(plot.title = element_text(size = 9, face = "bold"))
}

# 3. GENERAR GRID DE GRÁFICOS
# -----------------------------------------------------------------------------
plots_list <- lapply(numeric_predictors, function(v) {
  tryCatch(
    {
      plot_linearity_check(data_train_gam, v, target_var)
    },
    error = function(e) NULL
  )
})

# Eliminar NULLs (variables categóricas numéricas)
plots_list <- plots_list[!sapply(plots_list, is.null)]

# Mostrar en bloques de 6 gráficos por página
n_plots <- length(plots_list)
plots_per_page <- 6

for (i in seq(1, n_plots, by = plots_per_page)) {
  end_idx <- min(i + plots_per_page - 1, n_plots)
  do.call(grid.arrange, c(plots_list[i:end_idx], ncol = 2))
}

## Analisis de variables categoricas relacionadas con la respuesta
cat_vars_gam <- data_train_gam |>
  select_if(~ is.factor(.) | is.character(.)) |>
  names()

cat_vars_gam <- cat_vars_gam[!cat_vars_gam %in% c("Id", "conjunto")]

# Inicializar resultados
results <- data.frame(
  variable = character(),
  n_levels = integer(),
  f_statistic = numeric(),
  p_value = numeric(),
  eta_squared = numeric(),
  median_range = numeric(),
  stringsAsFactors = FALSE
)

for (var in cat_vars_gam) {
  # ANOVA
  formula_str <- paste("log_SalePrice ~", var)
  anova_result <- aov(as.formula(formula_str), data = data_train_gam)
  anova_summary <- summary(anova_result)

  f_stat <- anova_summary[[1]]$`F value`[1]
  p_val <- anova_summary[[1]]$`Pr(>F)`[1]

  # Eta-squared
  ss_between <- anova_summary[[1]]$`Sum Sq`[1]
  ss_total <- sum(anova_summary[[1]]$`Sum Sq`)
  eta_sq <- ss_between / ss_total

  # Rango de precio por nivel
  price_by_level <- data_train_gam |>
    group_by(across(all_of(var))) |>
    summarise(
      median_price = median(log_SalePrice, na.rm = TRUE),
      .groups = "drop"
    )

  median_range <- max(price_by_level$median_price) -
    min(price_by_level$median_price)
  n_levels <- n_distinct(data_train_gam[[var]])

  results <- rbind(
    results,
    data.frame(
      variable = var,
      n_levels = n_levels,
      f_statistic = f_stat,
      p_value = p_val,
      eta_squared = eta_sq,
      median_range = median_range
    )
  )
}

# Ordenar y clasificar
results <- results |>
  arrange(desc(eta_squared)) |>
  mutate(
    sig = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ "ns"
    ),
    effect = case_when(
      eta_squared > 0.14 ~ "Large",
      eta_squared > 0.06 ~ "Medium",
      eta_squared > 0.01 ~ "Small",
      TRUE ~ "Negligible"
    )
  )

variables_categoricas_relacion_respuesta <- ggplot(
  results,
  aes(x = reorder(variable, eta_squared), y = eta_squared, fill = effect)
) +
  geom_col(alpha = 0.8) +
  geom_hline(
    yintercept = c(0.01, 0.06, 0.14),
    linetype = "dashed",
    alpha = 0.5
  ) +
  scale_fill_manual(
    values = c(
      "Large" = "darkgreen",
      "Medium" = "steelblue",
      "Small" = "orange",
      "Negligible" = "red"
    )
  ) +
  coord_flip() +
  labs(
    title = "Categorical Variables: Effect Size (η²) on log_SalePrice",
    subtitle = "η² > 0.14 = Large, 0.06-0.14 = Medium, 0.01-0.06 = Small",
    x = NULL,
    y = "Eta-squared (η²)",
    fill = "Effect Size"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


## INTERACCIONES VARIABLES NUMERICAS Y CATEGORICAS
# -----------------------------------------------------------------------------
numeric_vars <- data_train_gam %>%
  dplyr::select(where(is.numeric), -Id, -SalePrice, -log_SalePrice) %>%
  names()

categorical_vars <- data_train_gam %>%
  dplyr::select(where(is.factor)) %>%
  names()

print(paste("Variables numéricas:", length(numeric_vars)))
print(paste("Variables categóricas:", length(categorical_vars)))
print(paste(
  "Total de combinaciones a evaluar:",
  length(numeric_vars) * length(categorical_vars)
))

# 2. FUNCIÓN OPTIMIZADA PARA GENERAR GRÁFICOS (SIN LEYENDA)
# -----------------------------------------------------------------------------
plot_interaction_fast <- function(data, var_num, var_cat, target) {
  # Filtrar niveles con pocos datos
  data_filtered <- data %>%
    group_by(!!sym(var_cat)) %>%
    filter(n() >= 10) %>%
    ungroup()

  # Si la categórica tiene > 10 niveles, quedarse con los 10 más frecuentes
  if (is.factor(data_filtered[[var_cat]])) {
    n_levels <- length(unique(data_filtered[[var_cat]]))
    if (n_levels > 10) {
      top_levels <- names(sort(
        table(data_filtered[[var_cat]]),
        decreasing = TRUE
      )[1:10])
      data_filtered <- data_filtered %>% filter(!!sym(var_cat) %in% top_levels)
    }
  }

  # Si quedan muy pocos datos, skip
  if (nrow(data_filtered) < 50) {
    return(NULL)
  }

  ggplot(data_filtered, aes_string(x = var_num, y = target, color = var_cat)) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) + # Solo líneas, sin puntos
    scale_color_viridis_d(option = "plasma") +
    labs(
      title = paste(var_num, "×", var_cat),
      x = NULL,
      y = NULL
    ) +
    theme_minimal() +
    theme(
      legend.position = "none", # SIN LEYENDA
      plot.title = element_text(size = 8, face = "bold"),
      axis.text = element_text(size = 6)
    )
}

# 3. GENERAR TODOS LOS GRÁFICOS
# -----------------------------------------------------------------------------
print("Generando gráficos de interacción...")

all_interactions <- expand.grid(
  num = numeric_vars,
  cat = categorical_vars,
  stringsAsFactors = FALSE
)

plots_all <- lapply(1:nrow(all_interactions), function(i) {
  tryCatch(
    {
      plot_interaction_fast(
        data_train_gam,
        all_interactions$num[i],
        all_interactions$cat[i],
        target_var
      )
    },
    error = function(e) NULL
  )
})

# Eliminar NULLs
plots_all <- plots_all[!sapply(plots_all, is.null)]

print(paste("Gráficos generados exitosamente:", length(plots_all)))

# 4. MOSTRAR EN BLOQUES DE 9 (3x3)
# -----------------------------------------------------------------------------
n_plots <- length(plots_all)
plots_per_page <- 9

for (i in seq(1, n_plots, by = plots_per_page)) {
  end_idx <- min(i + plots_per_page - 1, n_plots)
  do.call(grid.arrange, c(plots_all[i:end_idx], ncol = 3))
}

## INTERACCIONES VARIABLES CATEGORICAS
cat_vars <- data_train_gam %>%
  dplyr::select(where(is.factor)) %>%
  names()

cat_vars <- setdiff(cat_vars, c("Id", "conjunto")) # Excluir variables irrelevantes

print(paste("Variables categóricas a evaluar:", length(cat_vars)))
print(paste("Total de combinaciones:", choose(length(cat_vars), 2)))

# 2. FUNCIÓN PARA EVALUAR INTERACCIÓN (ANOVA DOS VÍAS)
# -----------------------------------------------------------------------------
test_interaction <- function(data, var1, var2, target) {
  # Fórmula: Target ~ Var1 + Var2 + Var1:Var2
  formula_str <- paste(target, "~", var1, "+", var2, "+", var1, ":", var2)

  tryCatch(
    {
      # ANOVA de dos vías
      model_full <- aov(as.formula(formula_str), data = data)
      anova_result <- anova(model_full)

      # Extraer F-statistic y p-value del término de INTERACCIÓN (última fila)
      interaction_row <- nrow(anova_result) - 1 # Penúltima fila (antes de Residuals)
      f_stat <- anova_result$`F value`[interaction_row]
      p_val <- anova_result$`Pr(>F)`[interaction_row]

      # Eta-squared de la interacción
      ss_interaction <- anova_result$`Sum Sq`[interaction_row]
      ss_total <- sum(anova_result$`Sum Sq`)
      eta_sq <- ss_interaction / ss_total

      return(data.frame(
        Var1 = var1,
        Var2 = var2,
        F_Statistic = f_stat,
        P_Value = p_val,
        Eta_Squared = eta_sq,
        Significant = ifelse(p_val < 0.05, "Yes", "No")
      ))
    },
    error = function(e) {
      return(NULL)
    }
  )
}

# 3. GENERAR TODAS LAS COMBINACIONES Y EVALUAR
# -----------------------------------------------------------------------------
print("Ejecutando ANOVA de dos vías para todas las combinaciones...")

# Crear grid de pares (evitando duplicados: A-B es lo mismo que B-A)
cat_pairs <- combn(cat_vars, 2, simplify = FALSE)

# Aplicar test a cada par
results_interactions <- lapply(cat_pairs, function(pair) {
  test_interaction(data_train_gam, pair[1], pair[2], target_var)
}) %>%
  bind_rows() %>%
  arrange(desc(Eta_Squared)) %>%
  filter(!is.na(F_Statistic)) # Eliminar fallos

print(paste(
  "Interacciones evaluadas exitosamente:",
  nrow(results_interactions)
))

# 4. FILTRAR LAS SIGNIFICATIVAS Y RELEVANTES
# -----------------------------------------------------------------------------
# Criterio: P-value < 0.05 (significativas) Y Eta² > 0.01 (efecto no trivial)
interactions_relevant <- results_interactions %>%
  filter(P_Value < 0.05 & Eta_Squared > 0.01) %>%
  arrange(desc(Eta_Squared))

print("--- TOP 15 INTERACCIONES CATEGÓRICAS MÁS IMPORTANTES ---")
print(head(interactions_relevant, 15))

# 5. VISUALIZACIÓN: HEATMAP DE P-VALUES
# -----------------------------------------------------------------------------
# Convertir a matriz para pheatmap
library(tidyr)
library(pheatmap)

interaction_matrix <- results_interactions %>%
  dplyr::select(Var1, Var2, P_Value) %>%
  pivot_wider(names_from = Var2, values_from = P_Value, values_fill = 1) %>%
  column_to_rownames("Var1") %>%
  as.matrix()

# Solo mostrar las top variables (limitar a 20x20 para legibilidad)
top_vars <- unique(c(
  head(interactions_relevant$Var1, 15),
  head(interactions_relevant$Var2, 15)
))
interaction_matrix_top <- interaction_matrix[
  rownames(interaction_matrix) %in% top_vars,
  colnames(interaction_matrix) %in% top_vars
]

pheatmap(
  -log10(interaction_matrix_top + 1e-10), # -log10(p) para mejor visualización
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  color = colorRampPalette(c("white", "orange", "red"))(100),
  main = "Interacciones Categóricas: -log10(P-Value)",
  fontsize_row = 8,
  fontsize_col = 8,
  breaks = seq(
    0,
    max(-log10(interaction_matrix_top + 1e-10), na.rm = TRUE),
    length.out = 101
  )
)


# Fórmula corregida:
# - Usamos 'HouseAge' en lugar de 'YearBuilt'
# - Usamos 'YearsSinceRemod' en lugar de 'YearRemodAdd'
# - Mantenemos 'TotalSF' (que agrupa sótanos + area habitable)

gam_formula <- as.formula(
  paste(
    target_var,
    "~",
    "s(TotalSF, bs='cr', k=10) +", # Tamaño total (curva)
    "s(HouseAge, bs='cr', k=10) +", # Antigüedad (curva)
    "s(YearsSinceRemod, bs='cr', k=10) +", # Tiempo desde reforma (curva)
    "s(LotArea, bs='cr', k=10) +", # Área parcela (curva)
    "OverallQual +", # Factores lineales
    "OverallCond +",
    "Neighborhood +",
    "GarageCars +",
    "Fireplaces +",
    "TotalBath"
  )
)


numeric_vars <- c(
  "TotalSF",
  "HouseAge",
  "YearsSinceRemod",
  "LotArea",
  "TotalBath"
)

k_suggestions <- sapply(numeric_vars, function(var) {
  n_unique <- length(unique(data_train_gam[[var]]))
  k_max <- min(n_unique / 4, 40) # No más de 20
  k_rec <- max(3, floor(k_max)) # Mínimo 3

  return(c(
    variable = var,
    n_unique = n_unique,
    k_max = k_max,
    k_recommended = k_rec
  ))
})


gam_formula_interacciones <- as.formula(
  paste(
    target_var,
    "~",
    "s(TotalSF,  bs='cr', k=40) +", # Tamaño total (curva)
    "s(HouseAge, bs='cr', by = OverallCond, k=29) +", # Antigüedad (curva)
    "s(YearsSinceRemod, bs='re', by = Neighborhood, k=40) +", # Tiempo desde reforma (curva)
    "s(LotArea, bs='cr', k=15) +", # Área parcela (curva)
    "OverallQual +", # Factores lineales
    "OverallCond +",
    #"Neighborhood +",
    "s(Neighborhood, bs = 're') +",
    "GarageCars +",
    "Fireplaces +",
    "s(TotalBath, bs = 'cr', k = 3) +",
    "BsmtQual +",
    "KitchenQual +",
    "GarageFinish"
  )
)


# 2. ENTRENAMIENTO
# -----------------------------------------------------------------------------
set.seed(123)
# Ajustamos el modelo GAM
gam_model <- gam(gam_formula, data = data_train_gam, method = "REML")

gam_model <- gam(
  gam_formula_interacciones,
  data = data_train_gam,
  method = "REML"
)

# Mostramos resumen (fíjate en la columna 'edf' de los términos suavizados)
# edf > 1 indica no-linealidad. edf = 1 indica que es una recta.
print(summary(gam_model))


# 3. DIAGNOSTICO + CURVAS
# -----------------------------------------------------------------------------
# Dibuja cómo afecta cada variable numérica al precio.
# Afecta mucho al rendimiento, descomentar solo cuando sea necesario!
#
#
# par(mfrow = c(2, 2))
# plot(gam_model, scheme = 1, shade = TRUE, pages = 1, all.terms = FALSE)
# gam.check(gam_model)
# par(mfrow = c(1, 1))

# Código de diagnóstico
problematic_cases <- data_train_gam %>%
  mutate(
    residuo = residuals(gam_model),
    pred = fitted(gam_model)
  ) %>%
  filter(residuo < -0.3) %>% # Casos con mayor sobrevalorización
  arrange(residuo) %>%
  dplyr::select(
    Id,
    Neighborhood,
    OverallQual,
    TotalSF,
    HouseAge,
    KitchenQual,
    residuo,
    pred,
    log_SalePrice
  )

# Ver patrones
problematic_cases %>%
  count(Neighborhood, sort = TRUE) # ¿Se concentran en barrios específicos?

problematic_cases %>%
  summarise(
    avg_qual = mean(as.numeric(OverallQual)),
    avg_age = mean(HouseAge)
  )

# 4. EVALUACIÓN Y TABLA FINAL
# -----------------------------------------------------------------------------
preds_gam <- predict(gam_model, newdata = data_val_gam)
rmse_gam <- sqrt(mean((preds_gam - data_val_gam[[target_var]])^2))

print(paste("RMSE GAM (Validation):", round(rmse_gam, 4)))

# Recopilamos todos los resultados
results_summary <- data.frame(
  Modelo = c("ElasticNet", "Lasso", "Ridge", "GAM (Splines)", "PCR", "KNN"),
  RMSE = c(
    best_enet_rmse,
    rmse_lasso,
    rmse_ridge,
    rmse_gam,
    best_pcr_rmse,
    rmse_knn
  )
) %>%
  arrange(RMSE)

# -----------------------------------------------------------------------------
# hacer csv
# -----------------------------------------------------------------------------
predecir_y_printear_test <- function(modelo, test_data, test_ids, n_preview = 10) {
  preds_log <- predict(modelo, newdata = test_data)
  res <- data.frame(
    Id = test_ids$Id,
    Prediccion = exp(as.numeric(preds_log))
  )
  print(utils::head(res, n_preview))
  return(res)
}

preds_test_df <- predecir_y_printear_test(
  modelo = gam_model,     
  test_data = data_pisos_test_imputed, 
  test_ids  = data_pisos_test_imputed,       
  n_preview = 10
)

submission <- data.frame(Id = preds_test_df$Id, SalePrice = preds_test_df$Prediccion)
write.csv(submission, "submission_GAM.csv", row.names = FALSE)
