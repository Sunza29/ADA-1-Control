# Creamos la base de datos 
data <- data.frame( control1 = c(200, 205, 195, 202, 186, 207, 
                                 194, 209, 200, 196, 190, 204, 
                                 196, 207, 200, 205, 209, 197,
                                 196, 198, 197, 195, 198, 199,
                                 191, 197, 190, 202), 
                    control2 = c(247, 250, 255, 243, 254, 263,
                                 251, 264, 253, 244, 261, 254,
                                 239, 236, 250, 259, 257, 256, 
                                 249, 257, 241, 255, 250, 259,
                                 247, 242, 256, 246)) 

# Optenemos la media, desviación estándar y CV para ambos controles
mean(data$control1)
mean(data$control2)

sd(data$control1)
sd(data$control2)

coef_var <- function(x, na.rm = FALSE) {
    sd(x, na.rm=na.rm) / mean(x, na.rm=na.rm)
}

coef_var(x=data$control1, na.rm=T) * 100
coef_var(x=data$control2, na.rm=T) * 100

# Creamos la gráfica para el control 1
plot(data$control1,
     type="o",
     xlab = "Días",
     ylab = "Valores", 
     pch = 21,
     bg = "black",
     main = "Control 1",
     axes = FALSE,
     bty = "7",
     ylim = c(180, 218),
     xlim = c(0, 28),
)

# Añadimos las etiquetas de los ticks 
axis(1, at = 0:28, labels = 0:28,)
axis(2, at = 180:218, labels = 180:218, las = 1)

# Agregamos las líneas de desviación al gráfico
lines <- {
    abline(mean(na.omit(data$control1)),0,col="blue")
    abline(mean(na.omit(data$control1))+sd(na.omit(data$control1)),0,col="green")
    abline(mean(na.omit(data$control1))-sd(na.omit(data$control1)),0,col="green")
    
    abline(mean(na.omit(data$control1))+2*sd(na.omit(data$control1)),0,col="yellow")
    abline(mean(na.omit(data$control1))-2*sd(na.omit(data$control1)),0,col="yellow")
    
    abline(mean(na.omit(data$control1))+3*sd(na.omit(data$control1)),0,col="red")
    abline(mean(na.omit(data$control1))-3*sd(na.omit(data$control1)),0,col="red")
}

# Creamos el gráfico para el control 2
plot(data$control2,
     type="o",
     xlab = "Días",
     ylab = "Valores", 
     pch = 21,
     bg = "black",
     main = "Control 2",
     axes = FALSE,
     bty = "7",
     ylim = c(227, 275),
     xlim = c(0, 28),
)
# Añadimos las etiquetas de los ticks
axis(1, at = 0:28, labels = 0:28,)
axis(2, at = 227:275, labels = 227:275, las = 1)

# Agregamos las líneas de desviación para el gráfico
lines <- {
    abline(mean(na.omit(data$control2)),0,col="blue")
    abline(mean(na.omit(data$control2))+sd(na.omit(data$control2)),0,col="green")
    abline(mean(na.omit(data$control2))-sd(na.omit(data$control2)),0,col="green")
    
    abline(mean(na.omit(data$control2))+2*sd(na.omit(data$control2)),0,col="yellow")
    abline(mean(na.omit(data$control2))-2*sd(na.omit(data$control2)),0,col="yellow")
    
    abline(mean(na.omit(data$control2))+3*sd(na.omit(data$control2)),0,col="red")
    abline(mean(na.omit(data$control2))-3*sd(na.omit(data$control2)),0,col="red")
}
