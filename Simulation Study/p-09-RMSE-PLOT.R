rm(list=ls())
library(latex2exp)

#=====================#
#Read Data============#
#=====================#

data1  <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/9ESTIMADOS100.txt",header=TRUE)
data2  <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/9ESTIMADOS150.txt",header=TRUE)
data3  <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/9ESTIMADOS200.txt",header=TRUE)
data4  <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/9ESTIMADOS250.txt",header=TRUE)
data5  <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/9ESTIMADOS300.txt",header=TRUE)
data6  <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/9ESTIMADOS350.txt",header=TRUE)
data7  <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/9ESTIMADOS400.txt",header=TRUE)
data8  <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/9ESTIMADOS450.txt",header=TRUE)
data9  <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/9ESTIMADOS500.txt",header=TRUE)
data10 <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/9ESTIMADOS550.txt",header=TRUE)
data11 <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/9ESTIMADOS600.txt",header=TRUE)
data12 <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/9ESTIMADOS650.txt",header=TRUE)
data13 <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/9ESTIMADOS700.txt",header=TRUE)
data14 <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/9ESTIMADOS750.txt",header=TRUE)
data15 <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/9ESTIMADOS800.txt",header=TRUE)
data16 <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/9ESTIMADOS850.txt",header=TRUE)
data17 <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/9ESTIMADOS900.txt",header=TRUE)
data18 <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/9ESTIMADOS950.txt",header=TRUE)
data19 <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/9ESTIMADOS1000.txt",header=TRUE)

data1  <- as.matrix(data1)
data2  <- as.matrix(data2)
data3  <- as.matrix(data3)
data4  <- as.matrix(data4)
data5  <- as.matrix(data5)
data6  <- as.matrix(data6)
data7  <- as.matrix(data7)
data8  <- as.matrix(data8)
data9  <- as.matrix(data9)
data10 <- as.matrix(data10)
data11 <- as.matrix(data11)
data12 <- as.matrix(data12)
data13 <- as.matrix(data13)
data14 <- as.matrix(data14)
data15 <- as.matrix(data15)
data16 <- as.matrix(data16)
data17 <- as.matrix(data17)
data18 <- as.matrix(data18)
data19 <- as.matrix(data19)

#==========================RMSE BIEN=================================#
theta <- c(2.071,0.013,0.034,3.881,1.220) 

RM1   <- sqrt(apply(t(apply(data1,1, function(x) (x-theta)^2)),2,mean))
RM2   <- sqrt(apply(t(apply(data2,1, function(x) (x-theta)^2)),2,mean))
RM3   <- sqrt(apply(t(apply(data3,1, function(x) (x-theta)^2)),2,mean))
RM4   <- sqrt(apply(t(apply(data4,1, function(x) (x-theta)^2)),2,mean))
RM5   <- sqrt(apply(t(apply(data5,1, function(x) (x-theta)^2)),2,mean))
RM6   <- sqrt(apply(t(apply(data6,1, function(x) (x-theta)^2)),2,mean))
RM7   <- sqrt(apply(t(apply(data7,1, function(x) (x-theta)^2)),2,mean))
RM8   <- sqrt(apply(t(apply(data8,1, function(x) (x-theta)^2)),2,mean))
RM9   <- sqrt(apply(t(apply(data9,1, function(x) (x-theta)^2)),2,mean))
RM10  <- sqrt(apply(t(apply(data10,1, function(x) (x-theta)^2)),2,mean))
RM11  <- sqrt(apply(t(apply(data11,1, function(x) (x-theta)^2)),2,mean))
RM12  <- sqrt(apply(t(apply(data12,1, function(x) (x-theta)^2)),2,mean))
RM13  <- sqrt(apply(t(apply(data13,1, function(x) (x-theta)^2)),2,mean))
RM14  <- sqrt(apply(t(apply(data14,1, function(x) (x-theta)^2)),2,mean))
RM15  <- sqrt(apply(t(apply(data15,1, function(x) (x-theta)^2)),2,mean))
RM16  <- sqrt(apply(t(apply(data16,1, function(x) (x-theta)^2)),2,mean))
RM17  <- sqrt(apply(t(apply(data17,1, function(x) (x-theta)^2)),2,mean))
RM18  <- sqrt(apply(t(apply(data18,1, function(x) (x-theta)^2)),2,mean))
RM19  <- sqrt(apply(t(apply(data19,1, function(x) (x-theta)^2)),2,mean))

rmbeta1  <- c(RM1[1],RM2[1],RM3[1],RM4[1],RM5[1],RM6[1],RM7[1],RM8[1],RM9[1],RM10[1],RM11[1],RM12[1],RM13[1],RM14[1],RM15[1],RM16[1],RM17[1],RM18[1],RM19[1])

rmbeta2  <- c(RM1[2],RM2[2],RM3[2],RM4[2],RM5[2],RM6[2],RM7[2],RM8[2],RM9[2],RM10[2],RM11[2],RM12[2],RM13[2],RM14[2],RM15[2],RM16[2],RM17[2],RM18[2],RM19[2])

rmbeta3  <- c(RM1[3],RM2[3],RM3[3],RM4[3],RM5[3],RM6[3],RM7[3],RM8[3],RM9[3],RM10[3],RM11[3],RM12[3],RM13[3],RM14[3],RM15[3],RM16[3],RM17[3],RM18[3],RM19[3])

rmlambda <- c(RM1[4],RM2[4],RM3[4],RM4[4],RM5[4],RM6[4],RM7[4],RM8[4],RM9[4],RM10[4],RM11[4],RM12[4],RM13[4],RM14[4],RM15[4],RM16[4],RM17[4],RM18[4],RM19[4])

rmnu     <- c(RM1[5],RM2[5],RM3[5],RM4[5],RM5[5],RM6[5],RM7[5],RM8[5],RM9[5],RM10[5],RM11[5],RM12[5],RM13[5],RM14[5],RM15[5],RM16[5],RM17[5],RM18[5],RM19[5])

n        <- seq(100,1000,50)

#===============#
#=====BETA'S====#
#===============#

par(mar = c(4,5,3,3), las = 0, mgp = c(3.5,0.5,0))
plot(n, rmbeta1, type = "l", pch = 20, xlab = "", ylab = "", lty = 1, lwd = 3, xlim = c(100,1000), ylim=c(0,0.02), font.axis = 1.8, cex.axis = 1.6, cex.lab = 2, font.lab = 1.8, main = "",col = "blue")
lines(n, rmbeta2, type = "l", lty = 1, lwd = 3, xlab = "", ylab = "",col = "red")
lines(n, rmbeta3, type = "l", lty = 1, lwd = 3, xlab = "", ylab = "",col = "green")
legend(x = c(700,900), y = c(0.010,0.014), box.lty = 0, horiz = FALSE, cex = 1.8, lty=c(1,1,1), lwd = 2, col=c("blue","red","green"),
       legend = c(text=TeX("$\\hat{\\beta}_{1p}$"),text=TeX("$\\hat{\\beta}_{2p}$"),text=TeX("$\\hat{\\beta}_{3p}$")))
mtext(text = "n",side = 1,line = 2, cex = 2)
mtext(text = "RMSE",side = 2,line = 2, cex = 2)

#======================#
#=====NU AND LAMBDA====#
#======================#

par(mar = c(4,5,3,3), las = 0, mgp = c(3.5,0.5,0))
plot(n, rmlambda, type = "l", pch = 20, xlab = "", ylab = "", lty = 1, lwd = 3, xlim = c(100,1000), ylim=c(0,5), font.axis = 1.8, cex.axis = 1.6, cex.lab = 2, font.lab = 1.8, main = "",col = "blue")
lines(n, rmnu, type = "l", lty = 1, lwd = 3, xlab = "", ylab = "",col = "red")
legend(x = c(700,900), y = c(3,3.4), box.lty = 0, horiz = FALSE, cex = 1.8, lty=c(1,1), lwd = 2, col=c("blue","red"),
       legend = c(text=TeX("$\\hat{\\lambda}_{p}$"),text=TeX("$\\hat{\\nu}_{p}$")))
mtext(text = "n",side = 1,line = 2, cex = 2)
mtext(text = "RMSE",side = 2,line = 2, cex = 2)
