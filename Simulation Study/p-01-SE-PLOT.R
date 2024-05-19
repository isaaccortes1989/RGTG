rm(list=ls())
library(latex2exp)

#=====================#
#Read Data============#
#=====================#

data1  <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/1SE100.txt",header=TRUE)
data2  <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/1SE150.txt",header=TRUE)
data3  <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/1SE200.txt",header=TRUE)
data4  <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/1SE250.txt",header=TRUE)
data5  <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/1SE300.txt",header=TRUE)
data6  <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/1SE350.txt",header=TRUE)
data7  <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/1SE400.txt",header=TRUE)
data8  <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/1SE450.txt",header=TRUE)
data9  <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/1SE500.txt",header=TRUE)
data10 <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/1SE550.txt",header=TRUE)
data11 <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/1SE600.txt",header=TRUE)
data12 <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/1SE650.txt",header=TRUE)
data13 <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/1SE700.txt",header=TRUE)
data14 <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/1SE750.txt",header=TRUE)
data15 <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/1SE800.txt",header=TRUE)
data16 <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/1SE850.txt",header=TRUE)
data17 <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/1SE900.txt",header=TRUE)
data18 <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/1SE950.txt",header=TRUE)
data19 <- read.table("C:/Users/Isaac/Documents/RGTG/Simulation Study/1SE1000.txt",header=TRUE)

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

#==========================SE=================================#

SE1  <- apply(data1,2,mean)
SE2  <- apply(data2,2,mean)
SE3  <- apply(data3,2,mean)
SE4  <- apply(data4,2,mean)
SE5  <- apply(data5,2,mean)
SE6  <- apply(data6,2,mean)
SE7  <- apply(data7,2,mean)
SE8  <- apply(data8,2,mean)
SE9  <- apply(data9,2,mean)
SE10 <- apply(data10,2,mean)
SE11 <- apply(data11,2,mean)
SE12 <- apply(data12,2,mean)
SE13 <- apply(data13,2,mean)
SE14 <- apply(data14,2,mean)
SE15 <- apply(data15,2,mean)
SE16 <- apply(data16,2,mean)
SE17 <- apply(data17,2,mean)
SE18 <- apply(data18,2,mean)
SE19 <- apply(data19,2,mean)


sebeta1  <- c(SE1[1],SE2[1],SE3[1],SE4[1],SE5[1],SE6[1],SE7[1],SE8[1],SE9[1],SE10[1],SE11[1],SE12[1],SE13[1],SE14[1],SE15[1],SE16[1],SE17[1],SE18[1],SE19[1])

sebeta2  <- c(SE1[2],SE2[2],SE3[2],SE4[2],SE5[2],SE6[2],SE7[2],SE8[2],SE9[2],SE10[2],SE11[2],SE12[2],SE13[2],SE14[2],SE15[2],SE16[2],SE17[2],SE18[2],SE19[2])

sebeta3  <- c(SE1[3],SE2[3],SE3[3],SE4[3],SE5[3],SE6[3],SE7[3],SE8[3],SE9[3],SE10[3],SE11[3],SE12[3],SE13[3],SE14[3],SE15[3],SE16[3],SE17[3],SE18[3],SE19[3])

selambda <- c(SE1[4],SE2[4],SE3[4],SE4[4],SE5[4],SE6[4],SE7[4],SE8[4],SE9[4],SE10[4],SE11[4],SE12[4],SE13[4],SE14[4],SE15[4],SE16[4],SE17[4],SE18[4],SE19[4])

senu     <- c(SE1[5],SE2[5],SE3[5],SE4[5],SE5[5],SE6[5],SE7[5],SE8[5],SE9[5],SE10[5],SE11[5],SE12[5],SE13[5],SE14[5],SE15[5],SE16[5],SE17[5],SE18[5],SE19[5])

n        <- seq(100,1000,50)

#===============#
#=====BETA'S====#
#===============#

par(mar = c(4,5,3,3), las = 0, mgp = c(3.5,0.5,0))
plot(n,  sebeta1, type = "l", pch = 20, xlab = "", ylab = "", lty = 1, lwd = 3, xlim = c(100,1000), ylim=c(0,0.02), font.axis = 1.8, cex.axis = 1.6, cex.lab = 2, font.lab = 1.8, main = "",col = "blue")
lines(n, sebeta2, type = "l", lty = 1, lwd = 3, xlab = "", ylab = "",col = "red")
lines(n, sebeta3, type = "l", lty = 1, lwd = 3, xlab = "", ylab = "",col = "green")
legend(x = c(700,900), y = c(0.010,0.014), box.lty = 0, horiz = FALSE, cex = 1.8, lty=c(1,1,1), lwd = 2, col=c("blue","red","green"),
       legend = c(text=TeX("$\\hat{\\beta}_{1p}$"),text=TeX("$\\hat{\\beta}_{2p}$"),text=TeX("$\\hat{\\beta}_{3p}$")))
mtext(text = "n",side = 1,line = 2, cex = 2)
mtext(text = "SE",side = 2,line = 2, cex = 2)

#======================#
#=====NU AND LAMBDA====#
#======================#

par(mar = c(4,5,3,3), las = 0, mgp = c(3.5,0.5,0))
plot(n, selambda, type = "l", pch = 20, xlab = "", ylab = "", lty = 1, lwd = 3, xlim = c(100,1000), ylim=c(0,5), font.axis = 1.8, cex.axis = 1.6, cex.lab = 2, font.lab = 1.8, main = "",col = "blue")
lines(n, senu, type = "l", lty = 1, lwd = 3, xlab = "", ylab = "",col = "red")
legend(x = c(700,900), y = c(3,3.4), box.lty = 0, horiz = FALSE, cex = 1.8, lty=c(1,1), lwd = 2, col=c("blue","red"),
       legend = c(text=TeX("$\\hat{\\lambda}_{p}$"),text=TeX("$\\hat{\\nu}_{p}$")))
mtext(text = "n",side = 1,line = 2, cex = 2)
mtext(text = "SE",side = 2,line = 2, cex = 2)
