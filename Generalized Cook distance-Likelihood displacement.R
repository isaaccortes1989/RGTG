#======================================================#
#===This Script requires the Script called Functions===#
#======================================================#

data <- read.csv("C:/Users/Isaac/Documents/JOSUE/data.csv")

y     <- data$BMXBMI 
n     <- length(y)
Z1    <- model.matrix(~data$BMXWAIST+data$RIAGENDR)
Z2    <- model.matrix(~0+rep(1,n))
Z3    <- model.matrix(~0+rep(1,n))
datos <- list(y = y,Z1 = Z1,Z2 = Z2, Z3=Z3)

tau   <- 0.9
ini9  <- c(2.18262716,0.01282577,0.03506912,3.50390486,1.30925501)
aux9  <- nlminb(ini9, llike.RGTG, gradient = g.RGTG, hessian = h.RGTG,data = datos, tau = tau) 
H9    <- solve(h.RGTG(aux9$par, data = datos, tau = tau))

L           <- -aux9$objective
GCD         <- c()
LD          <- c()
convergence <- c()
theta9      <- aux9$par

for (i in 1:n){
  Z1_            <- model.matrix(~data$BMXWAIST[-i]+data$RIAGENDR[-i])
  Z2_            <- model.matrix(~0+rep(1,n-1))
  Z3_            <- model.matrix(~0+rep(1,n-1))
  y_             <- data$BMXBMI[-i] 
  datos_         <- list(y = y_, Z1 = Z1_, Z2 = Z2_, Z3 = Z3_)
  aux            <- nlminb(ini9, llike.RGTG, gradient = g.RGTG, hessian = h.RGTG,data = datos_, tau = tau) 
  estimates      <- aux$par 
  GCD[i]         <- t((as.matrix(estimates)-as.matrix(theta9)))%*%H9%*%(as.matrix(estimates)-as.matrix(theta9)) 
  L_p            <- -aux$objective
  LD[i]          <- 2*(L-L_p)
  print(i)
  convergence[i] <- aux$convergence
}

sum(convergence)
write.csv(LD, "C:\\Users\\Isaac\\Documents\\JOSUE\\LD2.csv", row.names=FALSE)
write.csv(GCD, "C:\\Users\\Isaac\\Documents\\JOSUE\\GCD2.csv", row.names=FALSE)
