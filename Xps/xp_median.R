# Xps/xp_median.R

# ==============================
# 1. Cargar utilidades y modelo
# ==============================
source("../TT-2025-B156/R/utils.R")
source("../TT-2025-B156/Models/model_median.R")

# ==============================
# 2. Cargar y transponer Shen_1
# ==============================
shen <- read.csv("../TT-2025-B156/data/Shen_1.csv",
                 header = TRUE, stringsAsFactors = FALSE)

# Solo columnas numéricas
numeric_cols <- sapply(shen, is.numeric)
shen_num     <- shen[, numeric_cols]

# TRANSFORMACIÓN: transponer (filas = proteínas, columnas = muestras)
shen_t  <- t(shen_num)
shen_df <- as.data.frame(shen_t)

# Filas completas
shen_complete <- shen_df[complete.cases(shen_df), ]

cat("Dimensiones transpuestas (shen_complete):",
    nrow(shen_complete), "filas x",
    ncol(shen_complete), "columnas\n")

# ==============================
# 3. Ejecutar experimento
# ==============================
result_median_all <- evaluate_imputer(
  df         = shen_complete,
  impute_fun = impute_median,
  model_name = "Mediana",
  props      = c(0.1, 0.2, 0.3),
  n_rep      = 5
)

# ==============================
# 4. Resumen por proporción
# ==============================
resumen_mediana <- aggregate(
  cbind(mse, rmse, mae, r2) ~ prop,
  data = result_median_all,
  FUN  = mean
)

print(resumen_mediana)

