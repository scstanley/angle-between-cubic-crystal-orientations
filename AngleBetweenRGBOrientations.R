
source("Quaternion.R")

# Create a vector containing the given Euler angles. This is mainly for appearance sake.
euler <- function(p1, P, p2){
  return(c(p1, P, p2))
}

# Creates a vector containing the given RGB values. Note, they should be on the range [0-255]. This is mainly for appearance sake.
rgb <- function(r, g, b){
  return(c(r, g, b))
}

# Converts the given RGB values to reduced, intrinsic, proper, Euler angles.
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

# Converts the given RGB values to quaternion orientations to calculate the angle between the two orientations. Returns and angle in degrees.
angleBetweenRGBs <- function(rgb1, rgb2){
  # angle indeces
  p1 <- 1
  P <- 2
  p2 <- 3
  
  minAngle <- 360 # arbitrarily large
  
  # convert to quaternion
  euler1 <- rgbToProperZXZEuler(rgb1)
  euler2 <- rgbToProperZXZEuler(rgb2)
  q1 <- eulerZXZToQuat(euler1[p1], euler1[P], euler1[p2])
  q2 <- eulerZXZToQuat(euler2[p1], euler2[P], euler2[p2])
  
  # Compare the 24 symmetrically equivalent orientations to minimize the angle between orientations.
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
      # print(testQ2)
      # print(angleBetween)
      if(abs(angleBetween) < minAngle) minAngle <- abs(angleBetween)
    }
  }
  
  
  return(minAngle)
}
