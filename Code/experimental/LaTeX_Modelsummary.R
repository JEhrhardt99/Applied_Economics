


library(modelsummary)

# Dummy-Modell erstellen
model <- lm(mpg ~ hp + wt, data = mtcars)

# Variablennamen im LaTeX-Math-Mode formatieren
var_labels <- c(
  "hp" = "$\\Pi_{it}$",
  "wt" = "$\\text{Weight}$"
)

# Modell zusammenfassen und `modelsummary` verwenden
modelsummary(
  model,
  coef_map = var_labels
)
