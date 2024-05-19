rm(list=ls())
library(evd)

datos <- read.csv("C:/Users/Isaac/Documents/JOSUE/data.csv")

#==================================#
#===PROBABILITY DENSITY FUNCTION===#
#==================================#

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

#==================================#
#===LOG-LIKELIHOOD=================#
#==================================#

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

#==================================#
#===GRADIENT=======================#
#==================================#

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

#==================================#
#===OBSERVED INFORMATION MATRIX====#
#==================================#

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

#==================================#
#===Table 6 Results================#
#==================================#

y    <- datos$BMXBMI 
n    <- length(y)
Z1   <- model.matrix(~datos$BMXWAIST+datos$RIAGENDR)
Z2   <- model.matrix(~0+rep(1,n))
Z3   <- model.matrix(~0+rep(1,n))
data <- list(y = y,Z1 = Z1,Z2 = Z2, Z3=Z3)


tau  <- 0.5                                                                                   # p = 0.5
ini5  <- rep(1,ncol(Z1)+ncol(Z2)+ncol(Z3))                                                    #Initial Values
aux5  <- nlminb(ini5,llike.RGTG, gradient = g.RGTG, hessian = h.RGTG,data = data, tau = tau)  #Fit 
se5   <- sqrt(diag(solve(h.RGTG(aux5$par,data=data,tau=tau))))                                
aux5$par                                                                                      #ML Estimates
se5                                                                                           #Standard Error
2*pnorm(-abs(aux5$par/se5))                                                                   #Z-value


tau   <- 0.6                                                                                  # p = 0.6
ini6  <- c(2.07554215,0.01282577,0.03506912,3.50390486,1.30925501)                            #Initial Values
aux6  <- nlminb(ini6, llike.RGTG, gradient = g.RGTG, hessian = h.RGTG,data = data, tau = tau) #Fit 
se6   <- sqrt(diag(solve(h.RGTG(aux6$par,data=data,tau=tau))))                                
aux6$par                                                                                      #ML Estimates
se6                                                                                           #Standard Error
2*pnorm(-abs(aux6$par/se6))                                                                   #Z-value


tau   <- 0.7                                                                                  # p = 0.7
ini7  <- c(2.09603762,0.01282577,0.03506912,3.50390486,1.30925501)                            #Initial Values
aux7  <- nlminb(ini7, llike.RGTG, gradient = g.RGTG, hessian = h.RGTG,data = data, tau = tau) #Fit
se7   <- sqrt(diag(solve(h.RGTG(aux7$par,data=data,tau=tau))))
aux7$par                                                                                      #ML Estimates
se7                                                                                           #Standar Error
2*pnorm(-abs(aux7$par/se7))                                                                   #Z-value


tau   <- 0.8                                                                                  # p = 0.8
ini8  <- c(2.11832060,0.01282577,0.03506912,3.50390486,1.30925501)                            #Initial Values
aux8  <- nlminb(ini8, llike.RGTG, gradient = g.RGTG, hessian = h.RGTG,data = data, tau = tau) #Fit
se8   <- sqrt(diag(solve(h.RGTG(aux8$par,data=data,tau=tau))))                                 
aux8$par                                                                                      #ML Estimates
se8                                                                                           #Standard Error
2*pnorm(-abs(aux8$par/se8))                                                                   #Z-value


tau   <- 0.9                                                                                  # p = 0.9
ini9  <- c(2.14489549,0.01282577,0.03506912,3.50390486,1.30925501)                            #Initial Values
aux9  <- nlminb(ini9, llike.RGTG, gradient = g.RGTG, hessian = h.RGTG,data = data, tau = tau) #Fit
se9   <- sqrt(diag(solve(h.RGTG(aux9$par,data=data,tau=tau))))                                 
aux9$par                                                                                      #ML Estimates
se9                                                                                           #Standard Error             
2*pnorm(-abs(aux9$par/se9))                                                                   #Z-value


tau   <- 0.4                                                                                  # p = 0.4
ini4  <- c(2.07554215,0.01282577,0.03506912,3.50390486,1.30925501)                            #Initial Values
aux4  <- nlminb(ini4, llike.RGTG, gradient = g.RGTG, hessian = h.RGTG,data = data, tau = tau) #Fit
se4   <- sqrt(diag(solve(h.RGTG(aux4$par,data=data,tau=tau))))
aux4$par                                                                                      #ML Estimates
se4                                                                                           #Standard Error
2*pnorm(-abs(aux4$par/se4))                                                                   #Z-value


tau   <- 0.3                                                                                  # p = 0.3
ini3  <- c(2.05533354,0.01282577,0.03506912,3.50390486,1.30925501)                            #Initial Values
aux3  <- nlminb(ini3, llike.RGTG, gradient = g.RGTG, hessian = h.RGTG,data = data, tau = tau) #Fit
se3   <- sqrt(diag(solve(h.RGTG(aux3$par,data=data,tau=tau))))                            
aux3$par                                                                                      #ML Estimates
se3                                                                                           #Standard Error
2*pnorm(-abs(aux3$par/se3))                                                                   #Z-value


tau   <- 0.2                                                                                  # p = 0.2
ini2  <- c(2.03398145,0.01282577,0.03506912,3.50390486,1.30925501)                            #Initial Values
aux2  <- nlminb(ini2, llike.RGTG, gradient = g.RGTG, hessian = h.RGTG,data = data, tau = tau) #Fit
se2   <- sqrt(diag(solve(h.RGTG(aux2$par,data=data,tau=tau))))                                
aux2$par                                                                                      #ML Estimates
se2                                                                                           #Standard Error
2*pnorm(-abs(aux2$par/se2))                                                                   #Z-value


tau   <- 0.1                                                                                  # p = 0.1
ini1  <- c(2.00926449,0.01282577,0.03506912,3.50390486,1.30925501)                            #Initial Values
aux1  <- nlminb(ini1, llike.RGTG, gradient = g.RGTG, hessian = h.RGTG,data = data, tau = tau) #Fit
se1   <- sqrt(diag(solve(h.RGTG(aux1$par,data=data,tau=tau))))
aux1$par                                                                                      #ML Estimates
se1                                                                                           #Standard Error
2*pnorm(-abs(aux1$par/se1))                                                                   #Z-value
