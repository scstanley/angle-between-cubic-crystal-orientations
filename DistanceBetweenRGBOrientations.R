
source("Quaternion.R")

euler <- function(p1, P, p2){
  return(c(p1, P, p2))
}

rgb <- function(r, g, b){
  return(c(r, g, b))
}

rgbToProperZXZEuler(rgb){
  r <- 1
  g <- 2
  b <- 3
  p1AngleRange <- 360
  PAngleRange <- 90
  p2AngleRange <- 90
  rgbRange <- 255
  return(euler(r * p1AngleRange, g * PAngleRange, b * p2AngleRange) / 255)
}

angleBetweenRGBs(rgb1, rgb2){
  p1 <- 1
  P <- 2
  p2 <- 3
  x <- 1
  y <- 2
  z <- 3
  w <- 4
  euler1 <- rgbToProperZXZEuler(rgb1)
  euler2 <- rgbToProperZXZEuler(rgb2)
  quat1 <- eulerZXZToQuat(euler1[p1], euler1[P], euler1[p2])
  quat2 <- eulerZXZToQuat(euler2[p1], euler2[P], euler2[p2])
  conjQuat1 <- quatConjugate(quat1)
  
  quatBetween <- quatMultiply(conQuat1, quat2)
  distBetween <- 2 * arccos(quatBetween[w])
  return(angleBetween)
}
