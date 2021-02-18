  #Linea recta 1:
  m <- 7 #pendiente
  b <- 7 #interseccion
  
  #funcion de la linea recta
  f <- function(m, b, x){
    return(m * x + b)
  }
  
  x <- seq(-22, 22, 0.01)
  y <- f(m, b, x) #Sirve para evaluar
  
  plot(x, y, type = "l", xlab = "Eje X", ylab = "Eje Y") #graficamos
  abline(h = 3, v = 3) 
  
  #Linea recta 2:
  m <- 5 #pendiente
  b <- 5 #interseccion
  
  #funcion de la linea recta
  f <- function(m, b, x){
    return(m * x + b)
  }
  
  x <- seq(-8, 8, 0.01)
  y <- f(m, b, x) 
  
  plot(x, y, type = "l", xlab = "Eje X", ylab = "Eje Y") 
  abline(h = 1, v = 1) 
  
  #Circunferencia 1:
  circunferencia <- function(h, k, r){
    if (r >= 0){ 
      if (r == 0){ 
        plot(x = h, y = k, xlab = "Eje X", ylab = "Eje Y") 
      } else{
        x <- seq(h - r, h + r, 0.01) 
        ypositiva <- k + sqrt(r^2 - ((x - h)^2)) # parte positiva de la circunferencia
        ynegativa <- k - sqrt(r^2 - ((x - h)^2)) # parte negativa de la circunferencia
        # graficamos primero la parte positiva
        plot(x, ypositiva, type = "l", xlim = c(h - (r + 1), h + (r + 1)), ylim = c(k - (r + 1), k + (r + 1)),
             xlab = "Eje X", ylab = "Eje Y")
        lines(x, ynegativa, type = "l") # agregamos la parte negativa
        abline(h = 0, v = 0) 
        points(x = h, y = k, col = "red") 
      }
    } else{
      return(print("El radio no es positivo."))
    }
  }
  
  # ejecutamos la funcion
  circunferencia(0, 0, 5)
  
  #Circunferencia 2
  circunferencia <- function(h, k, r){
    if (r >= 0){ 
      if (r == 0){ 
        plot(x = h, y = k, xlab = "Eje X", ylab = "Eje Y") 
      } else{
        x <- seq(h - r, h + r, 0.01) 
        ypositiva <- k + sqrt(r^2 - ((x - h)^2)) # parte positiva de la circunferencia
        ynegativa <- k - sqrt(r^2 - ((x - h)^2)) # parte negativa de la circunferencia
        # graficamos primero la parte positiva
        plot(x, ypositiva, type = "l", xlim = c(h - (r + 1), h + (r + 1)), ylim = c(k - (r + 1), k + (r + 1)),
             xlab = "Eje X", ylab = "Eje Y")
        lines(x, ynegativa, type = "l") # agregamos la parte negativa
        abline(h = 0, v = 0) 
        points(x = h, y = k, col = "red") 
      }
    } else{
      return(print("El radio no es positivo."))
    }
  }
  
  # ejecutamos la funcion
  circunferencia(1, 2, 3)
  
  #Parabola 1
  g <- function(x){
    return(3*x^2 + x - 6)
  }
  
  x <- seq(-6, 6, 0.01)#vector de -5 a 5
  y <- g(x)
  
  plot(x, y, type = "l", xlab = "Eje X", ylab = "Eje Y") #graficamos
  abline(h = 0, v = 0) #una linea horizontal que pasa por el 0 en las x y una linea vertical que pasa por el 0 en las y
  
  #Parabola 2
  g <- function(x){
    return(4*x^2 + x - 8)
  }
  
  x <- seq(-8, 8, 0.01)#vector de -5 a 5
  y <- g(x)
  
  plot(x, y, type = "l", xlab = "Eje X", ylab = "Eje Y") #graficamos
  abline(h = 1, v = 1) #una linea horizontal que pasa por el 0 en las x y una linea vertical que pasa por el 0 en las y
  
  #Elipse 1
  elipse <- function(h, k, a, b, horizontal){
    if (a > b){ # a tiene que ser mayor que b
      c <- sqrt(a^2 - b^2) # calculamos c
      if (horizontal){ # si es una elipse horizontal
        x <- seq(h - a, h + a, 0.01) #definimos el dominio
        ypositiva <- k + sqrt((b^2 - (b^2/a^2) * ((x - h)^2))) # parte positiva
        ynegativa <- k - sqrt((b^2 - (b^2/a^2) * ((x - h)^2))) # parte negativa
        # graficamos primero la parte positiva
        plot(x, ypositiva, type = "l", xlim = c(h - (a + 1), h + (a + 1)), ylim = c(k - (b + 1), k + (b + 1)),
             xlab = "Eje X", ylab = "Eje Y")
        lines(x, ynegativa, type = "l") # agregamos la parte negativa
        abline(h = 0, v = 0) # ejes coordenados
        points(x = c(h - c, h + c), y = c(k, k), col = "red") # focos
      } else{
        x <- seq(h - b, h + b, 0.01)
        ypositiva <- k + sqrt((a^2 - (a^2/b^2) * ((x - h)^2)))
        ynegativa <- k - sqrt((a^2 - (a^2/b^2) * ((x - h)^2)))
        plot(x, ypositiva, type = "l", xlim = c(h - (b + 1), h + (b + 1)), ylim = c(k - (a + 1), k + (a + 1)),
             xlab = "Eje X", ylab = "Eje Y")
        lines(x, ynegativa, type = "l")
        abline(h = 0, v = 0)
        points(x = c(h, h), y = c(k - c, k + c), col = "red")
      }
    } else {
      return(print("No cumple las condiciones para ser una elipse. (a no es mayor que b)"))
    }
  }
  
  elipse(0, 0, 9, 4, TRUE)
  
  #Elipse 2
  elipse <- function(h, k, a, b, horizontal){
    if (a > b){ # a tiene que ser mayor que b
      c <- sqrt(a^2 - b^2) # calculamos c
      if (horizontal){ # si es una elipse horizontal
        x <- seq(h - a, h + a, 0.01) #definimos el dominio
        ypositiva <- k + sqrt((b^2 - (b^2/a^2) * ((x - h)^2))) # parte positiva
        ynegativa <- k - sqrt((b^2 - (b^2/a^2) * ((x - h)^2))) # parte negativa
        # graficamos primero la parte positiva
        plot(x, ypositiva, type = "l", xlim = c(h - (a + 1), h + (a + 1)), ylim = c(k - (b + 1), k + (b + 1)),
             xlab = "Eje X", ylab = "Eje Y")
        lines(x, ynegativa, type = "l") # agregamos la parte negativa
        abline(h = 0, v = 0) # ejes coordenados
        points(x = c(h - c, h + c), y = c(k, k), col = "red") # focos
      } else{
        x <- seq(h - b, h + b, 0.01)
        ypositiva <- k + sqrt((a^2 - (a^2/b^2) * ((x - h)^2)))
        ynegativa <- k - sqrt((a^2 - (a^2/b^2) * ((x - h)^2)))
        plot(x, ypositiva, type = "l", xlim = c(h - (b + 1), h + (b + 1)), ylim = c(k - (a + 1), k + (a + 1)),
             xlab = "Eje X", ylab = "Eje Y")
        lines(x, ynegativa, type = "l")
        abline(h = 0, v = 0)
        points(x = c(h, h), y = c(k - c, k + c), col = "red")
      }
    } else {
      return(print("No cumple las condiciones para ser una elipse. (a no es mayor que b)"))
    }
  }
  
  elipse(1, 1, 3, 1, TRUE)
  
  #Hiperbola 1
  hiperbola <- function(h, k, a, b, horizontal){
    c <- sqrt(a^2 + b^2) # calculamos c
    if (horizontal){ # hiperbola sobre el eje x
      xizq <- seq(h - (a + 3), h - a, 0.01) # dominio izquierdo
      xder <- seq(h + a, h + (a + 3), 0.01) # dominio derecho
      yizqpositiva <- k + sqrt((b^2/a^2)*((xizq - h)^2) - b^2) # parte positiva del dominio izquierdo
      yizqnegativa <- k - sqrt((b^2/a^2)*((xizq - h)^2) - b^2) # parte negativa del dominio izquierdo
      yderpositiva <- k + sqrt((b^2/a^2)*((xder - h)^2) - b^2) # parte positiva del dominio derecho
      ydernegativa <- k - sqrt((b^2/a^2)*((xder - h)^2) - b^2) # parte negativa del dominio derecho
      # greficamos la parte positiva del dominio izquierdo
      plot(xizq, yizqpositiva, type = "l", xlim = c(h - (a + 4), h + (a + 4)), ylim = c(k - (b + 4), k + (b + 4)),
           xlab = "Eje X", ylab = "Eje Y")
      #Resto de la grafica
      lines(xizq, yizqnegativa, type = "l")
      lines(xder, ydernegativa, type = "l")
      lines(xder, yderpositiva, type = "l")
      
      abline(h = 0, v = 0) # ejes coordenados
      points(x = c(h - (a + c)), y = c(k), col = "red") # focos
    } else{ # hiperbola sobre el eje y
      yizq <- seq(k - (a + 3), k - a, 0.01) # rango inferior
      yder <- seq(k + a, k + (a + 3), 0.01) # rango superior
      xizqpositiva <- h + sqrt((b^2/a^2)*((yizq - k)^2) - b^2)
      xizqnegativa <- h - sqrt((b^2/a^2)*((yizq - k)^2) - b^2)
      xderpositiva <- h + sqrt((b^2/a^2)*((yder - k)^2) - b^2)
      xdernegativa <- h - sqrt((b^2/a^2)*((yder - k)^2) - b^2)
      # graficamos
      plot(xizqpositiva, yizq, type = "l", xlim = c(h - (b + 4), h + (b + 4)), ylim = c(k - (a + 4), k + (a + 4)),
           xlab = "Eje X", ylab = "Eje Y")
      lines(xizqnegativa, yizq, type = "l")
      lines(xdernegativa, yder, type = "l")
      lines(xderpositiva, yder, type = "l")
      abline(h = 1, v = 1)
      points(x = c(h), y = c(k - (a + c)), col = "red") # focos
    }
  }
  
  hiperbola(5, -3, 4, 8, FALSE)
  
  #Hiperbola 2
  hiperbola <- function(h, k, a, b, horizontal){
    c <- sqrt(a^2 + b^2) # calculamos c
    if (horizontal){ # hiperbola sobre el eje x
      xizq <- seq(h - (a + 3), h - a, 0.01) # dominio izquierdo
      xder <- seq(h + a, h + (a + 3), 0.01) # dominio derecho
      yizqpositiva <- k + sqrt((b^2/a^2)*((xizq - h)^2) - b^2) # parte positiva del dominio izquierdo
      yizqnegativa <- k - sqrt((b^2/a^2)*((xizq - h)^2) - b^2) # parte negativa del dominio izquierdo
      yderpositiva <- k + sqrt((b^2/a^2)*((xder - h)^2) - b^2) # parte positiva del dominio derecho
      ydernegativa <- k - sqrt((b^2/a^2)*((xder - h)^2) - b^2) # parte negativa del dominio derecho
      # greficamos la parte positiva del dominio izquierdo
      plot(xizq, yizqpositiva, type = "l", xlim = c(h - (a + 4), h + (a + 4)), ylim = c(k - (b + 4), k + (b + 4)),
           xlab = "Eje X", ylab = "Eje Y")
      #Resto de la grafica
      lines(xizq, yizqnegativa, type = "l")
      lines(xder, ydernegativa, type = "l")
      lines(xder, yderpositiva, type = "l")
      
      abline(h = 0, v = 0) # ejes coordenados
      points(x = c(h - (a + c)), y = c(k), col = "red") # focos
    } else{ # hiperbola sobre el eje y
      yizq <- seq(k - (a + 3), k - a, 0.01) # rango inferior
      yder <- seq(k + a, k + (a + 3), 0.01) # rango superior
      xizqpositiva <- h + sqrt((b^2/a^2)*((yizq - k)^2) - b^2)
      xizqnegativa <- h - sqrt((b^2/a^2)*((yizq - k)^2) - b^2)
      xderpositiva <- h + sqrt((b^2/a^2)*((yder - k)^2) - b^2)
      xdernegativa <- h - sqrt((b^2/a^2)*((yder - k)^2) - b^2)
      # graficamos
      plot(xizqpositiva, yizq, type = "l", xlim = c(h - (b + 4), h + (b + 4)), ylim = c(k - (a + 4), k + (a + 4)),
           xlab = "Eje X", ylab = "Eje Y")
      lines(xizqnegativa, yizq, type = "l")
      lines(xdernegativa, yder, type = "l")
      lines(xderpositiva, yder, type = "l")
      abline(h = 1, v = 1)
      points(x = c(h), y = c(k - (a + c)), col = "red") # focos
    }
  }
  
  hiperbola(5, 5, 5, 2.64, FALSE)