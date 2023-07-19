
quaternion <- function(x, y, z, w){
  return(c(x, y, z, w))  
}

eulerZXZToQuat <- function(p1,P,p2){
  p1 <- p1 * pi / 180
  P <- P * pi / 180
  p2 <- p2 * pi / 180
  thetaPlus <- (p1 + p2)/2
  thetaMinus <- (p1 - p2)/2
  cP <- cos(P)
  ttP <- tan(thetaPlus)
  ttM <- tan(thetaMinus)
  
  # Calculate quaternion values
  w <- sqrt((cP + 1) / (2 * (1 + ttP * ttP)))
  x <- sqrt((1 - cP) / (2 * (1 + ttM * ttM)))
  y <- x * ttM
  z <- w * ttP
  return(quatNormalized(quaternion(x,y,z,w)))
}

quatToEulerInDegreesZXZ <- function(quat){
  x <- 1
  y <- 2
  z <- 3
  w <- 4
  if(quat[x] == 0 && quat[y] == 0 && quat[z] == 0 && quat[w] == 0) {
    return(c(0,0,0))
  }
  
  i <- 3
  j <- 1
  k <- 2
  
  a <- quat[w]
  b <- quat[z]
  c <- quat[x]
  d <- quat[y]
  
  theta1 <- 0
  theta2 <- acos(2 * ((a*a+b*b)/(a*a+b*b+c*c+d*d)) - 1)
  theta3 <- 0
  thetaPlus <- atan2(b, a)
  thetaMinus <- atan2(d, c)
  
  if(theta2 == 0){
    theta1 <- 0
    theta3 <- 2 * thetaPlus
  }
  else if(theta2 == pi/2){
    theta1 <- 0
    theta3 <- 2 * thetaMinus
  }
  else{
    theta1 = thetaPlus - thetaMinus
    theta3 <- thetaPlus + thetaMinus
  }
  
  while(theta1 >= 2*pi) {
    theta1 <- theta1 - 2*pi
  }
  while(theta2 >= 2*pi) {
    theta2 <- theta2 - 2*pi
  }
  while(theta3 >= 2*pi) {
    theta3 <- theta3 - 2*pi
  }
  while(theta1 < 0) {
    theta1 <- theta1 + 2*pi
  }
  while(theta2 < 0) {
    theta2 <- theta2 + 2*pi
  }
  while(theta3 < 0) {
    theta3 <- theta3 + 2*pi
  }
  result <- c(theta3, theta2, theta1) * (180 / pi)
  return(result)
}

quatMultiply <- function(q1, q2){
  x <- 1
  y <- 2
  z <- 3
  w <- 4
  
  quat <- quaternion(0,0,0,0)
  quat[w] <- q1[w] * q2[w] - q1[x] * q2[x] - q1[y] * q2[y] - q1[z] * q2[z]
  quat[x] <- q1[w] * q2[x] + q1[x] * q2[w] + q1[y] * q2[z] - q1[z] * q2[y]
  quat[y] <- q1[w] * q2[y] - q1[x] * q2[z] + q1[y] * q2[w] + q1[z] * q2[x]
  quat[z] <- q1[w] * q2[z] + q1[x] * q2[y] - q1[y] * q2[x] + q1[z] * q2[w]
  
  return(quat)
}

quatNormalized <- function(quat){
  x <- quat[1]
  y <- quat[2]
  z <- quat[3]
  w <- quat[4]
  
  magnitude <- sqrt(x*x+y*y+z*z+w*w)
  quat[1] = quat[1] / magnitude
  quat[2] = quat[2] / magnitude
  quat[3] = quat[3] / magnitude
  quat[4] = quat[4] / magnitude
  return(quat)
}

quatConjugate <- function(quat){
  x <- 1
  y <- 2
  z <- 3
  w <- 4
  return(quaternion(-quat[x], -quat[y], -quat[z], quat[w]))
}

rotAboutAxisByAngle <- function(quat, axis, angleInDegrees){
  theta <- angleInDegrees * pi / 180
  axisVals <- sin(theta/2)
  rotQuat <- quatNormalized(c(axis[1] * axisVals, axis[2] * axisVals, axis[3] * axisVals, cos(theta/2)))
  return(quatMultiply(rotQuat, quat))
}
