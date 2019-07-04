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
inc <- atan ((Xmv[2] + Ue[2,1] - Xmv[2]) / (Xmv[1] + Ue[1,1] - Xmv[1]))
b <- (sqrt(DDe[1]) * sqrt(qchisq(1 - alpha,p))) * sin(inc)
a <- (sqrt(DDe[1]) * sqrt(qchisq(1 - alpha,p))) * cos(inc)
d <- (sqrt(DDe[2]) * sqrt(qchisq(1 - alpha,p))) * sin(inc)
c <- (sqrt(DDe[2]) * sqrt(qchisq(1 - alpha,p))) * cos(inc)
arrows(Xmv[1], Xmv[2], Xmv[1] + a, Xmv[2] + b)
arrows(Xmv[1], Xmv[2], Xmv[1] - a, Xmv[2] - b)
arrows(Xmv[1], Xmv[2], Xmv[1] - d, Xmv[2] + c)
arrows(Xmv[1], Xmv[2], Xmv[1] + d, Xmv[2] - c)
angle <- seq(0, 2 * pi, length.out = 200)
ch <- cbind(sqrt(qchisq(1 - alpha,2)) * cos(angle), sqrt(qchisq(1 - alpha,2)) *
                sin(angle))
lines(t(Xmv - ((Ue %*% diag(sqrt(DDe))) %*% t(ch))),type = "l")
points(dowel1)

#Usando la función mult.chart
mult.chart(dowel1,type="chi",alpha=0.05)
