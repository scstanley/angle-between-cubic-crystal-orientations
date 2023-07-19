
source("Quaternion.R")

euler <- function(p1, P, p2){
  return(c(p1, P, p2))
}

rgb <- function(r, g, b){
  return(c(r, g, b))
}

rgbToProperZXZEuler <- function(rgb){
  r <- 1
  g <- 2
  b <- 3
  p1AngleRange <- 360
  PAngleRange <- 90
  p2AngleRange <- 90
  rgbRange <- 255
  return(euler(rgb[r] * p1AngleRange, rgb[g] * PAngleRange, rgb[b] * p2AngleRange) / 255)
}

angleBetweenQuaternions <- function(q1, q2){
  w <- 4
  conjQ1 <- quatConjugate(q1)
  quatBetween <- quatMultiply(conjQ1, q2)
  angleBetween <- acos(quatBetween[w]) * 360 / pi
  return(angleBetween)
}

angleBetweenRGBs <- function(rgb1, rgb2){
  p1 <- 1
  P <- 2
  p2 <- 3
  
  minAngle <- 360
  
  euler1 <- rgbToProperZXZEuler(rgb1)
  euler2 <- rgbToProperZXZEuler(rgb2)
  q1 <- eulerZXZToQuat(euler1[p1], euler1[P], euler1[p2])
  q2 <- eulerZXZToQuat(euler2[p1], euler2[P], euler2[p2])
  
  for(zOrXRot in seq(1:6)){
    unit <- quaternion(0,0,0,1)
    if(zOrXRot < 5){
      unit <- rotAboutAxisByAngle(unit, c(0,0,1), 90*(zOrXRot - 1))
    }
    else{
      unit <- rotAboutAxisByAngle(unit, c(1,0,0), 90*(((zOrXRot * 2) + 1) %% 4))
    }
    for(yRot in seq(1:4)){
      unit <- rotAboutAxisByAngle(unit, c(0,1,0), (yRot - 1) * 90)
      testQ2 <- quatMultiply(q2, unit)
      angleBetween <- angleBetweenQuaternions(q1, testQ2)
      minAngle <- min(minAngle, angleBetween)
    }
  }
  
  
  return(minAngle)
}
