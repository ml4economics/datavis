# Beispiel-Daten
x <- seq(0, 10, length.out = 100)
y1 <- sin(x)
y2 <- cos(x)
y3 <- tan(x)

# Layout für 2 Diagramme in der ersten Spalte und 1 Diagramm in der zweiten Spalte
layout(matrix(c(1, 2, 3, 3), nrow = 2, ncol = 2))

# Erstes Diagramm (erste Spalte, erster Plot)
plot(x, y1, type = "l", col = "blue", main = "Plot 1: Sin(x)")

# Zweites Diagramm (erste Spalte, zweiter Plot)
plot(x, y2, type = "l", col = "red", main = "Plot 2: Cos(x)")

# Drittes Diagramm (zweite Spalte)
plot(x, y3, type = "l", col = "green", main = "Plot 3: Tan(x)")
