rm(list=ls())
library(latex2exp)

#=====================#
#Read Data============#
#=====================#

data1  <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/1ESTIMATES100.txt",header=TRUE)
data2  <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/1ESTIMATES150.txt",header=TRUE)
data3  <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/1ESTIMATES200.txt",header=TRUE)
data4  <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/1ESTIMATES250.txt",header=TRUE)
data5  <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/1ESTIMATES300.txt",header=TRUE)
data6  <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/1ESTIMATES350.txt",header=TRUE)
data7  <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/1ESTIMATES400.txt",header=TRUE)
data8  <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/1ESTIMATES450.txt",header=TRUE)
data9  <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/1ESTIMATES500.txt",header=TRUE)
data10 <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/1ESTIMATES550.txt",header=TRUE)
data11 <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/1ESTIMATES600.txt",header=TRUE)
data12 <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/1ESTIMATES650.txt",header=TRUE)
data13 <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/1ESTIMATES700.txt",header=TRUE)
data14 <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/1ESTIMATES750.txt",header=TRUE)
data15 <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/1ESTIMATES800.txt",header=TRUE)
data16 <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/1ESTIMATES850.txt",header=TRUE)
data17 <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/1ESTIMATES900.txt",header=TRUE)
data18 <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/1ESTIMATES950.txt",header=TRUE)
data19 <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/1ESTIMATES1000.txt",header=TRUE)

#=========================================================================#
#==========================RELATIVE BIAS =================================#
#=========================================================================#

theta <- c(2.071,0.013,0.034,3.881,1.220) 
RB1   <- apply(t(apply(data1,1, function(x) ((x-theta)/theta))),2,mean)
RB2   <- apply(t(apply(data2,1, function(x) ((x-theta)/theta))),2,mean)
RB3   <- apply(t(apply(data3,1, function(x) ((x-theta)/theta))),2,mean)
RB4   <- apply(t(apply(data4,1, function(x) ((x-theta)/theta))),2,mean)
RB5   <- apply(t(apply(data5,1, function(x) ((x-theta)/theta))),2,mean)
RB6   <- apply(t(apply(data6,1, function(x) ((x-theta)/theta))),2,mean)
RB7   <- apply(t(apply(data7,1, function(x) ((x-theta)/theta))),2,mean)
RB8   <- apply(t(apply(data8,1, function(x) ((x-theta)/theta))),2,mean)
RB9   <- apply(t(apply(data9,1, function(x) ((x-theta)/theta))),2,mean)
RB10  <- apply(t(apply(data10,1, function(x) ((x-theta)/theta))),2,mean)
RB11  <- apply(t(apply(data11,1, function(x) ((x-theta)/theta))),2,mean)
RB12  <- apply(t(apply(data12,1, function(x) ((x-theta)/theta))),2,mean)
RB13  <- apply(t(apply(data13,1, function(x) ((x-theta)/theta))),2,mean)
RB14  <- apply(t(apply(data14,1, function(x) ((x-theta)/theta))),2,mean)
RB15  <- apply(t(apply(data15,1, function(x) ((x-theta)/theta))),2,mean)
RB16  <- apply(t(apply(data16,1, function(x) ((x-theta)/theta))),2,mean)
RB17  <- apply(t(apply(data17,1, function(x) ((x-theta)/theta))),2,mean)
RB18  <- apply(t(apply(data18,1, function(x) ((x-theta)/theta))),2,mean)
RB19  <- apply(t(apply(data19,1, function(x) ((x-theta)/theta))),2,mean)

rbbeta1  <- c(RB1[1],RB2[1],RB3[1],RB4[1],RB5[1],RB6[1],RB7[1],RB8[1],RB9[1],RB10[1],RB11[1],RB12[1],RB13[1],RB14[1],RB15[1],RB16[1],RB17[1],RB18[1],RB19[1])

rbbeta2  <- c(RB1[2],RB2[2],RB3[2],RB4[2],RB5[2],RB6[2],RB7[2],RB8[2],RB9[2],RB10[2],RB11[2],RB12[2],RB13[2],RB14[2],RB15[2],RB16[2],RB17[2],RB18[2],RB19[2])

rbbeta3  <- c(RB1[3],RB2[3],RB3[3],RB4[3],RB5[3],RB6[3],RB7[3],RB8[3],RB9[3],RB10[3],RB11[3],RB12[3],RB13[3],RB14[3],RB15[3],RB16[3],RB17[3],RB18[3],RB19[3])

rblambda <- c(RB1[4],RB2[4],RB3[4],RB4[4],RB5[4],RB6[4],RB7[4],RB8[4],RB9[4],RB10[4],RB11[4],RB12[4],RB13[4],RB14[4],RB15[4],RB16[4],RB17[4],RB18[4],RB19[4])

rbnu     <- c(RB1[5],RB2[5],RB3[5],RB4[5],RB5[5],RB6[5],RB7[5],RB8[5],RB9[5],RB10[5],RB11[5],RB12[5],RB13[5],RB14[5],RB15[5],RB16[5],RB17[5],RB18[5],RB19[5])


n       <- seq(100,1000,50)

#=============#
#Plot of RB===#
#=============#

par(mar = c(4,5,3,3), las = 0, mgp = c(3.5,0.5,0))
plot(n,rbbeta1, type = "l", pch = 20, xlab = "", ylab = "", lty = 1, lwd = 3, xlim = c(100,1000), ylim=c(-0.05,0.05), font.axis = 1.8, cex.axis = 1.6, cex.lab = 2, font.lab = 1.8, main = "",col = "blue")
lines(n,rbbeta2, type = "l", lty = 1, lwd = 3, xlab = "", ylab = "",col = "red")
lines(n,rbbeta3, type = "l", lty = 1, lwd = 3, xlab = "", ylab = "",col = "green")
legend(x = c(700,900), y = c(-0.01,-0.04), box.lty = 0, horiz = FALSE, cex = 1.8, lty=c(1,1,1), lwd = 2, col=c("blue","red","green"),
       legend = c(text=TeX("$\\hat{\\beta}_{1p}$"),text=TeX("$\\hat{\\beta}_{2p}$"),text=TeX("$\\hat{\\beta}_{3p}$")))
mtext(text = "n",side = 1,line = 2, cex = 2)
mtext(text = "RB",side = 2,line = 2, cex = 2)


par(mar = c(4,5,3,3), las = 0, mgp = c(3.5,0.5,0))
plot(n,rblambda, type = "l", pch = 20, xlab = "", ylab = "", lty = 1, lwd = 3, xlim = c(100,1000), ylim=c(-0.10,0.30), font.axis = 1.8, cex.axis = 1.6, cex.lab = 2, font.lab = 1.8, main = "",col = "blue")
lines(n,rbnu, type = "l", lty = 1, lwd = 3, xlab = "", ylab = "",col = "red")
legend(x = c(700,900), y = c(0.20,0.24), box.lty = 0, horiz = FALSE, cex = 1.8, lty=c(1,1), lwd = 2, col=c("blue","red"),
       legend = c(text=TeX("$\\hat{\\lambda}_{p}$"),text=TeX("$\\hat{\\nu}_{p}$")))
mtext(text = "n",side = 1,line = 2, cex = 2)
mtext(text = "RB",side = 2,line = 2, cex = 2)


