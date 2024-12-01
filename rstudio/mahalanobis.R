mahalanobis1 <- function(count) {
  data <- rnorm(count)
  mean_data <- mean(data)
  sd_data <- sd(data)
  distances = abs(data - mean_data) / sd_data
  distances_squared <- distances^2
  df <- mean(distances_squared)
  list(df = df, distances = distances[1:10])
}

mahalanobis2 <- function(count) {
  x <- 1:count+rnorm(count)
  y <- 1:count+rnorm(count,1,3)
  z <- cbind(x,y)
  # from docu : returns the SQUARED Mahalanobis distance of all rows in x
  distances_squared <- mahalanobis(z, colMeans(z), cov(z))
  # SQUARED Mahalanobis distance is Chi-square distributed -> mean = df
  df <- mean(distances_squared)
  list(df = df, distances = sqrt(distances_squared[1:10]))
}

mahalanobis3 <- function(count) {
  w <- rnorm(count)
  x <- rnorm(count,1,3)
  y <- rnorm(count,8,4)
  z <- cbind(w,x,y)
  distances_squared <- mahalanobis(z, colMeans(z), cov(z))
  df <- mean(distances_squared)
  list(df = df, distances = sqrt(distances_squared[1:10]))
}
