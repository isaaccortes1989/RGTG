#===========================================#
#===Install and load the following packages
#===========================================#
library(evd)
library(dplyr)
library(Rlab) 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#This file requires the prior execution of "Functions" file.
amostras = seq(100,1000,50)

for (nss in 1:length(amostras)) {
  set.seed(100) 
  n  <- amostras[nss]
  z1 <- rlogis(n = n) 
  z2 <- rbern(n = n,0.6)
  #========================================
  Z1        <- model.matrix(~z1+z2)
  Z2        <- model.matrix(~0+rep(1,n))
  Z3        <- model.matrix(~0+rep(1,n))
  beta1     <- c(2.071,0.013,0.034)
  beta2     <- c(3.881) 
  beta3     <- c(1.220) 
  mu        <- c(exp(Z1%*%beta1))
  sigma     <- c(Z2%*%beta2)
  nu        <- c(exp(Z3%*%beta3))
  estimates <- c()
  see       <- c()
  #=========================================
  replicas  <- 3000 
  for(i in 1 :replicas){
    flag <- 0
    while(flag==0){
      tryCatch({
        tau   <- 0.5
        y     <- rRGTG(n, mu = mu, sigma = sigma, nu = nu, tau = tau)
        data <- list(y = y,Z1 = Z1,Z2 = Z2, Z3=Z3)
        ini  <- rep(1,ncol(Z1)+ncol(Z2)+ncol(Z3))
        aux4 <- nlminb(ini, llike.RGTG, gradient = g.RGTG, hessian = h.RGTG,data = data, tau = tau) 
        H    <- h.RGTG(aux4$par, data = data, tau = tau)
        se   <- sqrt(diag(solve(H)))
        
        if((aux4$convergence==0)&(sum(se=="NA")==0)&(sum(se=="NaN")==0)&(sum(se=="Error")==0))
        {
          estimates <- rbind(estimates,aux4$par)			
          see       <- rbind(see,se)
          flag<-1
          print(i)
        }else{flag<-0}  
        
      },error = function(e) flag = 0) #trycatch
    } #while
  }#for
  write.table(estimates,paste("5ESTIMATES",n,".txt",sep=""), sep="\t",row.names=FALSE)
  write.table(see,paste("5SE",n,".txt",sep=""),sep="\t",row.names=FALSE)
}

