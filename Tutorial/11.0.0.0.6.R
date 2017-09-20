x <- 3:8
y <- 5*x + 3
par(mfcol = c(2, 2))
plot(y~x, xlim = c(3, 8), type = "l", lty = 1, col = 4) 
plot(y~x, xlim = c(3, 8), type = "l", lty = 2, col = 3) 
plot(y~x, xlim = c(3, 8), type = "l", lty = 3, col = 2) 
plot(y~x, xlim = c(3, 8), type = "l", lty = 4, col = 1) 

dev.print(png, file = "myplot.png", width = 1024, height = 768)
#saves as png file with name myplot.png in current wd