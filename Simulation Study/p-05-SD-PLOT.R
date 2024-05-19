rm(list=ls())
library(latex2exp)

#=====================#
#Read Data============#
#=====================#

data1  <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/5ESTIMATES100.txt",header=TRUE)
data2  <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/5ESTIMATES150.txt",header=TRUE)
data3  <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/5ESTIMATES200.txt",header=TRUE)
data4  <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/5ESTIMATES250.txt",header=TRUE)
data5  <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/5ESTIMATES300.txt",header=TRUE)
data6  <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/5ESTIMATES350.txt",header=TRUE)
data7  <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/5ESTIMATES400.txt",header=TRUE)
data8  <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/5ESTIMATES450.txt",header=TRUE)
data9  <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/5ESTIMATES500.txt",header=TRUE)
data10 <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/5ESTIMATES550.txt",header=TRUE)
data11 <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/5ESTIMATES600.txt",header=TRUE)
data12 <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/5ESTIMATES650.txt",header=TRUE)
data13 <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/5ESTIMATES700.txt",header=TRUE)
data14 <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/5ESTIMATES750.txt",header=TRUE)
data15 <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/5ESTIMATES800.txt",header=TRUE)
data16 <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/5ESTIMATES850.txt",header=TRUE)
data17 <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/5ESTIMATES900.txt",header=TRUE)
data18 <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/5ESTIMATES950.txt",header=TRUE)
data19 <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/5ESTIMATES1000.txt",header=TRUE)


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

SD1  <- apply(data1,2,function(x) sd(x))
SD2  <- apply(data2,2,function(x) sd(x))
SD3  <- apply(data3,2,function(x) sd(x))
SD4  <- apply(data4,2,function(x) sd(x))
SD5  <- apply(data5,2,function(x) sd(x))
SD6  <- apply(data6,2,function(x) sd(x))
SD7  <- apply(data7,2,function(x) sd(x))
SD8  <- apply(data8,2,function(x) sd(x))
SD9  <- apply(data9,2,function(x) sd(x))
SD10 <- apply(data10,2,function(x) sd(x))
SD11 <- apply(data11,2,function(x) sd(x))
SD12 <- apply(data12,2,function(x) sd(x))
SD13 <- apply(data13,2,function(x) sd(x))
SD14 <- apply(data14,2,function(x) sd(x))
SD15 <- apply(data15,2,function(x) sd(x))
SD16 <- apply(data16,2,function(x) sd(x))
SD17 <- apply(data17,2,function(x) sd(x))
SD18 <- apply(data18,2,function(x) sd(x))
SD19 <- apply(data19,2,function(x) sd(x))


sdbeta1  <- c(SD1[1],SD2[1],SD3[1],SD4[1],SD5[1],SD6[1],SD7[1],SD8[1],SD9[1],SD10[1],SD11[1],SD12[1],SD13[1],SD14[1],SD15[1],SD16[1],SD17[1],SD18[1],SD19[1])

sdbeta2  <- c(SD1[2],SD2[2],SD3[2],SD4[2],SD5[2],SD6[2],SD7[2],SD8[2],SD9[2],SD10[2],SD11[2],SD12[2],SD13[2],SD14[2],SD15[2],SD16[2],SD17[2],SD18[2],SD19[2])

sdbeta3  <- c(SD1[3],SD2[3],SD3[3],SD4[3],SD5[3],SD6[3],SD7[3],SD8[3],SD9[3],SD10[3],SD11[3],SD12[3],SD13[3],SD14[3],SD15[3],SD16[3],SD17[3],SD18[3],SD19[3])

sdlambda <- c(SD1[4],SD2[4],SD3[4],SD4[4],SD5[4],SD6[4],SD7[4],SD8[4],SD9[4],SD10[4],SD11[4],SD12[4],SD13[4],SD14[4],SD15[4],SD16[4],SD17[4],SD18[4],SD19[4])

sdnu     <- c(SD1[5],SD2[5],SD3[5],SD4[5],SD5[5],SD6[5],SD7[5],SD8[5],SD9[5],SD10[5],SD11[5],SD12[5],SD13[5],SD14[5],SD15[5],SD16[5],SD17[5],SD18[5],SD19[5])

n        <- seq(100,1000,50)

#===============#
#=====BETA'S====#
#===============#

par(mar = c(4,5,3,3), las = 0, mgp = c(3.5,0.5,0))
plot(n,  sdbeta1, type = "l", pch = 20, xlab = "", ylab = "", lty = 1, lwd = 3, xlim = c(100,1000), ylim=c(0,0.02), font.axis = 1.8, cex.axis = 1.6, cex.lab = 2, font.lab = 1.8, main = "",col = "blue")
lines(n, sdbeta2, type = "l", lty = 1, lwd = 3, xlab = "", ylab = "",col = "red")
lines(n, sdbeta3, type = "l", lty = 1, lwd = 3, xlab = "", ylab = "",col = "green")
legend(x = c(700,900), y = c(0.010,0.014), box.lty = 0, horiz = FALSE, cex = 1.8, lty=c(1,1,1), lwd = 2, col=c("blue","red","green"),
       legend = c(text=TeX("$\\hat{\\beta}_{1p}$"),text=TeX("$\\hat{\\beta}_{2p}$"),text=TeX("$\\hat{\\beta}_{3p}$")))
mtext(text = "n",side = 1,line = 2, cex = 2)
mtext(text = "SD",side = 2,line = 2, cex = 2)

#======================#
#=====NU AND LAMBDA====#
#======================#

par(mar = c(4,5,3,3), las = 0, mgp = c(3.5,0.5,0))
plot(n, sdlambda, type = "l", pch = 20, xlab = "", ylab = "", lty = 1, lwd = 3, xlim = c(100,1000), ylim=c(0,5), font.axis = 1.8, cex.axis = 1.6, cex.lab = 2, font.lab = 1.8, main = "",col = "blue")
lines(n, sdnu, type = "l", lty = 1, lwd = 3, xlab = "", ylab = "",col = "red")
legend(x = c(700,900), y = c(3,3.4), box.lty = 0, horiz = FALSE, cex = 1.8, lty=c(1,1), lwd = 2, col=c("blue","red"),
       legend = c(text=TeX("$\\hat{\\lambda}_{p}$"),text=TeX("$\\hat{\\nu}_{p}$")))
mtext(text = "n",side = 1,line = 2, cex = 2)
mtext(text = "SD",side = 2,line = 2, cex = 2)
