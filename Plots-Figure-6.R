#==============================#
#Plots of LD===================#
#==============================#

LD  <- read.csv("C:/Users/Isaac/Documents/RESULTADOSRGGUMBEL2/LD2.csv",header = TRUE)
LD2 <- LD[,1]
x   <- seq(1,1743,1)

par(mar = c(4,5,3,3), las = 0, mgp = c(3.5, 0.5, 0))
results <- plot(x,LD2, ylab = "", pch = 19, xlab = "", ylim = c(-25, 0), cex.axis = 1.2, cex.lab = 1.5, font.axis = 2, main = "")
axis(2, seq(-25, 0, 10), cex.axis = 1.2, font = 2)
mtext(text = "Index", side = 1, line = 2, cex = 1.8)
mtext(text = "Likelihood displacement", side = 2, line = 2, cex = 1.8)   
text(264,-16.5  ,labels = 264, cex = 1.6, font = 2)
text(486,-14.5,labels = 486, cex = 1.6, font = 2)
text(540,-13.5,labels = 516, cex = 1.6, font = 2)
text(1299,-12.8,labels = 1299, cex = 1.6, font = 2)

#==============================#
#Plots of GCD==================#
#==============================#

GCD  <- read.csv("C:/Users/Isaac/Documents/RESULTADOSRGGUMBEL2/GCD2.csv",header = TRUE)
GCD2 <- GCD[,1]
summary(GCD2)

x   <- seq(1,1743,1)
par(mar = c(4, 5, 3, 3), las = 0, mgp = c(3.5, 0.5, 0))
plot(x,GCD2, ylab = "", pch = 19, xlab = "", ylim = c(0, 0.006), cex.axis = 1.2, cex.lab = 1.5, font.axis = 2, main = "")
mtext(text = "Index", side = 1, line = 2, cex = 1.8)
mtext(text = "Generalized Cook's distance", side = 2, line = 2, cex = 1.8)   
text(264,0.0004,labels = 264, cex = 1.6, font = 2)
text(516,0.0004,labels = 516, cex = 1.6, font = 2)
text(1267,0.0057,labels = 1267, cex = 1.6, font = 2)
text(1299,0.0004,labels = 1299, cex = 1.6, font = 2)