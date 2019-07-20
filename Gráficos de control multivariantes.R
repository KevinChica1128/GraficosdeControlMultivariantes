#Gráficos de control multivariables
#Paquete 
install.packages("qcr")
require("qcr")

install.packages("MSQC")
require("MSQC")   

#Gráfico de control chi cuadrado
data("dowel1")
alpha<- 0.05 
p<- ncol(dowel1)
Xmv<- colMeans(dowel1)
S<- covariance(dowel1)
DDe<- eigen(S)$values
Ue<- eigen(S)$vectors
x11()
plot(Xmv[1], Xmv[2], xlim = c(0.46,0.54), ylim = c(0.95,1.06), xlab = "diameter",
     ylab = "length",pch = 3)
#Formar los ejes
inc <- atan ((Xmv[2] + Ue[2,1] - Xmv[2]) / (Xmv[1] + Ue[1,1] - Xmv[1]))
b <- (sqrt(DDe[1]) * sqrt(qchisq(1 - alpha,p))) * sin(inc)
a <- (sqrt(DDe[1]) * sqrt(qchisq(1 - alpha,p))) * cos(inc)
d <- (sqrt(DDe[2]) * sqrt(qchisq(1 - alpha,p))) * sin(inc)
c <- (sqrt(DDe[2]) * sqrt(qchisq(1 - alpha,p))) * cos(inc)
arrows(Xmv[1], Xmv[2], Xmv[1] + a, Xmv[2] + b)
arrows(Xmv[1], Xmv[2], Xmv[1] - a, Xmv[2] - b)
arrows(Xmv[1], Xmv[2], Xmv[1] - d, Xmv[2] + c)
arrows(Xmv[1], Xmv[2], Xmv[1] + d, Xmv[2] - c)

#Gráfico
angle <- seq(0, 2 * pi, length.out = 200)
ch <- cbind(sqrt(qchisq(1 - alpha,2)) * cos(angle), sqrt(qchisq(1 - alpha,2)) *
                sin(angle))
x11()
plot(Xmv[1], Xmv[2], xlim = c(0.46,0.54), ylim = c(0.95,1.06), xlab = "diameter",
     ylab = "length",pch = 3)
lines(t(Xmv - ((Ue %*% diag(sqrt(DDe))) %*% t(ch))),type = "l")
points(dowel1)
data("dowel2")
points(dowel2,pch =4)

#Usando la función mult.chart
x11()
mult.chart(dowel1,type="chi",alpha=0.05)

#Datos carbon
data("carbon1")

#Gráfico de control T^2
x11()
mult.chart(carbon1,type="t2",alpha = 0.05)

#Gráfico de varianza generalizada
x11()
gen.var(carbon1)

#Gráfico MEWMA
x11()
mult.chart(type="mewma", carbon1)

#Gráficos MCUSUM
data("carbon2")
Xmv <- mult.chart(carbon1, type = "t2") $Xmv
S <- mult.chart(carbon1, type = "t2") $covariance
x11()
par(mfrow=c(1,2))
mult.chart(type = "mcusum", carbon2, Xmv = Xmv, S = S)
mult.chart(type = "mcusum2",carbon2, Xmv=Xmv, S=S)

#Caso ilustrativo: Control de pitcheo
data("sabathia1")

#Vector de medias y matriz de varianzas y covarianzas y correlaciones
colMeans(sabathia1); covariance(sabathia1); cor(sabathia1)

#Gráfico por pares de variables
pairs(sabathia1,pch=19)

#Gráfica 3D
library(rgl)
plot3d(ellipse3d(cov(sabathia1), centre = colMeans(sabathia1), level = 0.99),
         xlab = "", ylab = "", zlab = "",type = "wire", col = "gray1", alpha = 0.2)
points3d(sabathia1, size = 4, cex = 2, add = TRUE)

#Gráfico T2 de Hotelling
mult.chart(type = "t2", sabathia1)

colm <- nrow(sabathia1)
vec <- (mult.chart(sabathia1,type = "t2")$Xmv)
mat <- (mult.chart(sabathia1,type = "t2")$covariance)

#Se cargan otros datos
data("sabathia2")
par(mfrow = c(1,2))
mult.chart(type = "t2", sabathia2, Xmv = vec, S = mat, colm = colm)


