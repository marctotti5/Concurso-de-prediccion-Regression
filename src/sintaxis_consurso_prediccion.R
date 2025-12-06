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
## --------------------------------- ANÁLISIS DE CORRELACIONES Y MULTICOLINEALIDAD --------------------------
## ----------------------------------------------------------------------------------------------------------

library(ggcorrplot)


# Correlation plot

numeric_vars <- data_pisos_train_imputed %>%
  select(where(is.numeric), -SalePrice)

cor_matrix <- cor(numeric_vars, use = "pairwise.complete.obs")

correlation_plot <- ggcorrplot(
  cor_matrix,
  method = "square",
  type = "lower",
  lab = TRUE,
  lab_size = 2,
  tl.cex = 6,
  title = "Correlation Matrix"
)
correlation_plot

# Condition Number (Kappa)
# Medida global de multicolinealidad.
# Regla general: < 10 (Bien), 10-30 (Moderada), > 30 (Severa/Grave)

# Seleccionar solo numéricas y ESCALAR (fundamental para kappa)
X_matrix <- data_pisos_train_imputed %>%
  select(where(is.numeric), -SalePrice) %>%
  scale()

# Calcular numero de condicion
cond_val <- kappa(X_matrix)

cond_val

# VIF (Variance Inflation Factor)
# Identifica que variable especifica causa la inflacion de varianza.
# VIF = 1 (Sin correlacion) | VIF > 5 (Alta) | VIF > 10 (Muy grave)

library(car) # Necesario para la funcion vif()

# 1. Ajustamos un modelo auxiliar con todas las numericas
model_vif <- lm(SalePrice ~ ., data = data_pisos_train_imputed %>% select(where(is.numeric)))

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

ggplotly(vif_graph)

# Diagnostico Matematico (Determinante y Autovalores)
# Confirma matematicamente la severidad detectada por el VIF.

# A) Determinante de la matriz de correlacion
# 1 = Independencia total | 0 = Multicolinealidad perfecta
det_val <- det(cor(numeric_vars))
cat("\n------------------------------------------------\n")
cat("DETERMINANTE DE LA MATRIZ:", format(det_val, scientific = FALSE), "\n")
if (det_val < 0.001) cat(" ALERTA: Determinante cercano a 0. Redundancia muy alta.\n")
cat("------------------------------------------------\n")

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

# 2. Ejecución del PCA con FactoMineR
# ncp = 10: Guardamos las primeras 10 dimensiones para analizar
res_pca <- PCA(X_pca, scale.unit = TRUE, ncp = 10, graph = FALSE)

# 3. Scree Plot (Gráfico de sedimentación)
# FUNDAMENTAL: ¿Cuánta información retenemos?
# Buscamos el "codo" o componentes con eigenvalue > 1 (Criterio de Kaiser)
scree_plot <- fviz_eig(res_pca, addlabels = TRUE, ylim = c(0, 50)) +
  labs(
    title = "Scree Plot: % de Varianza Explicada",
    subtitle = "Busca el 'codo' donde la ganancia de información se aplana"
  )
ggplotly(scree_plot)

# 4. Círculo de Correlaciones (Variables)
# Nos dice CÓMO se relacionan las variables originales con las nuevas dimensiones.
# Coloreamos por 'contrib': Las que más pesan en la definición de los ejes.
var_plot <- fviz_pca_var(res_pca,
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

# 5. Tabla de Contribuciones (Interpretación analítica)
# ¿Qué define a la Dimensión 1 y 2? (Esencial para dar sentido de negocio)
cat("\n--- Top 10 variables que definen la Dimensión 1 (Eje X) ---\n")
print(head(sort(res_pca$var$contrib[, 1], decreasing = TRUE), 10))

cat("\n--- Top 10 variables que definen la Dimensión 2 (Eje Y) ---\n")
print(head(sort(res_pca$var$contrib[, 2], decreasing = TRUE), 10))


## ----------------------------------------------------------------------------------------------------------
## -------------------------------------------------- MODELADO ----------------------------------------------
## ----------------------------------------------------------------------------------------------------------
