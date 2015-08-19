## Polygon class
library(methods)

# name & representation of the new polygon data type
setClass("polygon",
         representation(x = "numeric",
                        y = "numeric"))

# explain plot() how to plot the new data type 
# with @ you access the data in the new class
setMethod("plot", "polygon",
          function(x, y, ...) {
                  plot(x@x, x@y, type = "n", ...)
                  xp <- c(x@x, x@x[1])
                  yp <- c(x@y, x@y[1])
                  lines(xp, yp)
          })

# test new class and method
p <- new("polygon", x = c(1, 1.5, 3, 4), y = c(1, 2.5, 3, 1))

plot(p)