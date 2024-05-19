rm(list=ls())
#=================================#
#=========Random numbers==========#
#=================================#

rRGTG = function(n,mu=1,sigma=1,nu=1,tau=0.5){ 
  if(any(mu<=0)) stop(paste("mu must be positive","\n",""))
  if(any(nu<=0)) stop(paste("nu must be positive","\n",""))
  if(any(tau<=0)|any(tau>=1)) stop(paste("tau must be between 0 and 1","\n",""))
  U   <- runif(n)
  h_l <- sigma+qgumbel(tau*(1-pgumbel(-sigma))+pgumbel(-sigma))
  X   <- mu*((sigma+qgumbel(U-U*pgumbel(-sigma)+pgumbel(-sigma)))/h_l)^(1/nu)
  return(X)
} 

#===============================================#
#=========Probability density function==========#
#===============================================#

dRGTG <- function(x,mu,sigma,nu,tau,log=FALSE){
  if(any(mu<=0))              stop(paste("mu must be positive"))
  if(any(nu<=0))              stop(paste("nu must be positive"))
  if(any(tau<=0)|any(tau>=1)) stop(paste("tau must be between 0 and 1"))
  h_l <- sigma+qgumbel(tau*(1-pgumbel(-sigma))+pgumbel(-sigma))
  lfy <- log(nu)+(nu-1)*log(x)-nu*log(mu)-log(1-pgumbel(-sigma))+log(h_l)+dgumbel(((x/mu)^nu)*h_l-sigma,log=TRUE)
  if(log== FALSE)
    fy <- exp(lfy)
  else fy <- lfy
  return(fy)
}

#===============================================#
#=========Log-likelihood function===============#
#===============================================#

llike.RGTG <- function(theta,data,tau)
{
  Z1     <- data$Z1 
  Z2     <- data$Z2
  Z3     <- data$Z3
  y      <- data$y
  r1     <- ncol(Z1)
  r2     <- ncol(Z2)
  r3     <- ncol(Z3)
  beta1  <- theta[1:r1]
  beta2  <- theta[(r1+1):(r1+r2)]
  beta3  <- theta[(r1+r2+1):(r1+r2+r3)]
  mu     <- c(exp(Z1%*%beta1))
  sigma  <- c(Z2%*%beta2)
  nu     <- c(exp(Z3%*%beta3))
  return(-sum(dRGTG(x = y,mu = mu,sigma = sigma , nu = nu,tau = tau,log=TRUE)))
}

#===============================================#
#========= -  Gradient==========================#
#===============================================#

g.RGTG <- function(theta,data,tau){
  Z1     <- data$Z1 
  Z2     <- data$Z2
  Z3     <- data$Z3
  y      <- data$y
  r1     <- ncol(Z1)
  r2     <- ncol(Z2)
  r3     <- ncol(Z3)
  beta1  <- theta[1:r1]
  beta2  <- theta[(r1+1):(r1+r2)]
  beta3  <- theta[(r1+r2+1):(r1+r2+r3)]
  mu     <- c(exp(Z1%*%beta1))
  sigma  <- c(Z2%*%beta2)
  nu     <- c(exp(Z3%*%beta3))
  #-----------------------------------------------
  h_l  <- sigma+qgumbel(tau*(1-pgumbel(-sigma))+pgumbel(-sigma))
  k_1  <- exp(-(y/mu)^(nu)*h_l+sigma)
  k_2  <- (dgumbel(-sigma)*(tau-1))/dgumbel(qgumbel(tau*(1-pgumbel(-sigma))+pgumbel(-sigma)))
  #-----------------------------------------------
  dldm <- -(nu/mu)+nu*(y^(nu)/mu^(nu+1))*h_l*(1-k_1) 
  dldd <- ((1+k_2)/h_l)-(dgumbel(-sigma)/(1-pgumbel(-sigma)))+(1-(y/mu)^(nu)*(1+k_2))*(1-k_1) 
  dldv <- (1/nu)+log(y)-log(mu)-(y/mu)^(nu)*log(y/mu)*h_l*(1-k_1)
  #----------------------------------------------
  g1 <- t(Z1)%*%diag(c(mu))%*%c(dldm) 
  g2 <- t(Z2)%*%c(dldd)
  g3 <- t(Z3)%*%diag(c(nu))%*%c(dldv) 
  gg <- c(c(-g1),c(-g2),c(-g3))
  return(gg)
}

#===============================================#
#=========Observed Information matrix===========#
#===============================================#

h.RGTG<-function(theta,data,tau){
  Z1     <- data$Z1 
  Z2     <- data$Z2
  Z3     <- data$Z3
  y      <- data$y
  r1     <- ncol(Z1)
  r2     <- ncol(Z2)
  r3     <- ncol(Z3)
  beta1  <- theta[1:r1]
  beta2  <- theta[(r1+1):(r1+r2)]
  beta3  <- theta[(r1+r2+1):(r1+r2+r3)]
  mu     <- c(exp(Z1%*%beta1))
  sigma  <- c(Z2%*%beta2)
  nu     <- c(exp(Z3%*%beta3))
  #---------------------------------------------------  
  h_l  <- sigma+qgumbel(tau*(1-pgumbel(-sigma))+pgumbel(-sigma))
  k_1  <- exp(-(y/mu)^(nu)*h_l+sigma)
  k_2  <- (dgumbel(-sigma)*(tau-1))/dgumbel(qgumbel(tau*(1-pgumbel(-sigma))+pgumbel(-sigma)))
  #-------------------------------------------------------------------------------------------
  dldm <- -(nu/mu)+nu*(y^(nu)/mu^(nu+1))*h_l*(1-k_1)                                            
  dldd <- ((1+k_2)/h_l)-(dgumbel(-sigma)/(1-pgumbel(-sigma)))+(1-(y/mu)^(nu)*(1+k_2))*(1-k_1)   
  dldv <- (1/nu)+log(y)-log(mu)-(y/mu)^(nu)*log(y/mu)*h_l*(1-k_1)
  #-------------------------------------------------------------------------------------------
  k_3     <- (y/mu)^(nu)*log(y/mu)*h_l
  u       <- qgumbel(tau*(1-pgumbel(-sigma))+pgumbel(-sigma))
  l       <- (dgumbel(-sigma)*(tau-1))/dgumbel(u)                   
  k_4     <- nu*(y^(nu)/mu^(nu+1))*(1+l)*(1-k_1)
  k_5     <- nu*(y^(nu)/mu^(nu+1))*h_l*k_1*(1-(y/mu)^(nu)*(1+l))                   
  t_1     <- -(1/mu)+k_3*(nu/mu)*(1-k_1)+(y/mu)^(nu)*(h_l/mu)*(1-k_1)
  t_2     <- (nu/mu)*(y/mu)^(2*nu)*log(y/mu)*h_l^2*k_1 
  kk_3    <- dgumbel(-sigma)*((exp(sigma)-1)/(1-pgumbel(-sigma)))+(dgumbel(-sigma)/(1-pgumbel(-sigma)))^2
  kk_4    <- ((tau-1)/h_l)*(dgumbel(-sigma)*(1-exp(sigma))/dgumbel(u)-(tau-1)*dgumbel(-sigma)^2*(exp(-u)-1)/dgumbel(u)^2)
  kk_5    <- -(1/h_l^2)*(1+(tau-1)*dgumbel(-sigma)/dgumbel(u))^2
  kk_6    <- -(1-(y/mu)^(nu)*(1+l))^(2)*k_1
  kk_7    <- -(y/mu)^(nu)*(tau-1)*(1-k_1)*((dgumbel(-sigma)*(1-exp(sigma))/dgumbel(u))-((tau-1)*dgumbel(-sigma)^2*(exp(-u)-1)/dgumbel(u)^2))
  k_33    <- -(y/mu)^(nu)*log(y/mu)*(1+l)*(1-k_1)
  k_44    <- k_1*k_3*(1-(y/mu)^(nu)*(1+l))
  #-------------------------------------------------------------------------------------------
  d2ldm2  <- (nu/mu^2)-nu*(nu+1)*(y^(nu)/mu^(nu+2))*h_l*(1-k_1)-nu^2*(y^(2*nu)/mu^(2*nu+2))*h_l^2*k_1
  d2ldmdd <- k_4-k_5
  d2ldmdv <- t_1+t_2
  d2ldd2  <- kk_3+kk_4+kk_5+kk_6+kk_7
  d2ldddv <- k_33+k_44
  d2ldv2 <- -(1/nu^2)-log(y/mu)*k_3*(1-k_1)-k_3^2*k_1
  #--------------------------------------------------------------------------------------------
  gg1 <- t(Z1)%*%diag(c(d2ldm2*mu*mu+dldm*mu))%*%Z1     
  gg2 <- t(Z1)%*%diag(c(d2ldmdd*mu))%*%Z2               
  gg3 <- t(Z1)%*%diag(c(d2ldmdv*mu*nu))%*%Z3            
  gg4 <- t(Z2)%*%diag(c(d2ldd2+dldd))%*%Z2           
  gg5 <- t(Z2)%*%diag(c(d2ldddv*nu))%*%Z3              
  gg6 <- t(Z3)%*%diag(c(d2ldv2*nu*nu+dldv*nu))%*%Z3     
  #--------------------------------------------------------------------------------------------------------------------------- 
  matrix <- rbind(cbind(-gg1,-gg2,-gg3),cbind(t(-gg2),-gg4,-gg5),cbind(t(-gg3),t(-gg5),-gg6))
  return(matrix)
}