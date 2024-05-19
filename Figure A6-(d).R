library(evd)
set.seed(1000)

data <- read.csv("C:/Users/Isaac/Documents/JOSUE/data.csv")

y    <- data$BMXBMI
n    <- length(y)
Z1   <- model.matrix(~data$BMXWAIST+data$RIAGENDR) 
Z2   <- model.matrix(~0+rep(1,n)) 
Z3   <- model.matrix(~0+rep(1,n))
data <- list(y = y,Z1 = Z1,Z2 = Z2, Z3=Z3)
tau  <- 0.4

#=========================================#
#NQRs 
#=========================================#

theta   <- c(2.05533354,0.01282577,0.03506912,3.50390486,1.30925501)
r1      <- ncol(Z1)
r2      <- ncol(Z2)
r3      <- ncol(Z3)

beta1_r  <- matrix(theta[1:r1], nrow = r1, ncol = 1)
beta2_r  <- matrix(theta[(r1+1):(r1+r2)], nrow = r2, ncol = 1)
beta3_r  <- matrix(theta[(r1+r2+1):(r1+r2+r3)], nrow= r3, ncol = 1)
mu_r     <- c(exp(Z1%*%beta1_r))
sigma_r  <- c(Z2%*%beta2_r)
nu_r     <- c(exp(Z3%*%beta3_r))
h_l      <- sigma_r+qgumbel(tau*(1-pgumbel(-sigma_r))+pgumbel(-sigma_r))
G        <- (pgumbel((y/mu_r)^(nu_r)*h_l-sigma_r)-pgumbel(-sigma_r))/(1-pgumbel(-sigma_r)) 
R        <- qnorm(G)

#========================================================================================================#
#Get the estimates of the density parameters to simulate 100 samples of size 1,743 to build the envelopes
#========================================================================================================#

beta1_q  <- matrix(theta[1:r1], nrow = r1, ncol = 1)
beta2_q  <- matrix(theta[(r1+1):(r1+r2)], nrow = r2, ncol = 1)
beta3_q  <- matrix(theta[(r1+r2+1):(r1+r2+r3)], nrow= r3, ncol = 1)

mu_q    <- c(exp(Z1%*%beta1_q))
sigma_q <- c(Z2%*%beta2_q)
nu_q    <- c(exp(Z3%*%beta3_q))

U_q <- matrix(0,n,100)
e   <- matrix(0,n,100)
e1  <- numeric(n)
e2  <- numeric(n)

for(i in 1:100){
  U_q[,i] <- runif(n)                                                      
  h_l_q   <- sigma_q+qgumbel(tau*(1-pgumbel(-sigma_q))+pgumbel(-sigma_q)) 
  y_q     <- mu_q*((sigma_q+qgumbel(U_q[,i]-U_q[,i]*pgumbel(-sigma_q)+pgumbel(-sigma_q)))/h_l_q)^(1/nu_q)
  G_q     <- (pgumbel((y_q/mu_q)^(nu_q)*h_l_q-sigma_q)-pgumbel(-sigma_q))/(1-pgumbel(-sigma_q)) 
  e[,i]   <- qnorm(G_q) 
  e[,i]   <- sort(e[,i]) 
}

for(i in 1:n){
  eo    <- sort(e[i,])
  e1[i] <- (eo[2]+eo[3])/2 
  e2[i] <- (eo[97]+eo[98])/2 
} 

med     <- apply(e,1,mean)
Range_2 <- range(R,e1,e2)

#========================================================#
# Figure=================================================#
#========================================================#

par(mar = c(4, 5, 3, 3), las = 0, mgp = c(3.5, 0.5, 0), pty = "s")
results <- qqnorm(R, xlab = "", cex.axis = 1.2, cex.lab = 1.5, font.axis = 2, lwd = 0.05, ylab = "", ylim = Range_2, pch = 16, main = "")
axis(1, -5:5, cex.axis = 1.2, font = 2)
axis(2, -5:5, cex.axis = 1.2, font = 2)
mtext(text = "Theoretical quantiles", side = 1, line = 2, cex = 1.8)
mtext(text = "Empirical quantiles", side = 2, line = 2, cex = 1.8)  

par(new = TRUE)
qqnorm(e1, axes = F, xlab = "", ylab = "", type = "l", ylim = Range_2, lty = 1, main = "")
par(new = TRUE)
qqnorm(e2, axes = F, xlab = "", ylab = "", type = "l", ylim = Range_2, lty = 1, main = "")
par(new = TRUE)
qqnorm(med, axes = F, xlab = "", ylab = "", type = "l", ylim = Range_2, lty = 2, main = "")
text(3,5,labels = 1267, cex = 1.6, font = 2)

#=======================================#
#====Kolmogorov-Smirnov Test============#
#=======================================#

ks        <- ks.test(R,"pnorm",mean(R),sd(R)) 
results_2 <- c(ks$statistic, ks$p.value)
round(results_2,3)


