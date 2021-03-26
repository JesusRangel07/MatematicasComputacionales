biseccion = function(f, xa, xb, tol){
  if( sign(f(xa)) == sign(f(xb)) ){ stop("f(xa) y f(xb) tienen el mismo signo") }
  # a = min(xa,xb)
  # b = max(xa,xb)
  a = xa; b = xb
  k = 0
  #Par imprimir estado
  cat("----------------------------------------------------------\n")
  cat(formatC( c("a","b","m","Error est."), width = -15, format = "f", flag = " "), "\n")
  cat("----------------------------------------------------------\n")
  repeat{
    m = a + 0.5*(b-a)
    if( f(m)==0 ){ cat("Cero de f en [",xa,",",xb,"] es: ", m ) }
    if( sign(f(a)) != sign(f(m)) ){
      b = m
    } else { a = m }
    dx = (b-a)/2
    # imprimir estado
    cat(formatC( c(a,b,m,dx), digits=7, width = -15, format = "f", flag = " "), "\n")
    k = k+1
    #until
    if( dx < tol ){
      cat("----------------------------------------------------------\n\n")
      cat("Cero de f en [",xa,",",xb,"] es approx: ", m, "con error <=", dx)
      break;
    }
  } #repeat
}
## Pruebas
#Ejercicio 1
f = function(x) (x^3)
curve(f, -2,2); abline(h=0, v=0) #gráfico para decidir un intervalo
biseccion(f, -0.2, 0.1, 0.000001)

#Ejercicio 2
f = function(x) (x^5 - 100*x^4 + 3995*x^3 - 79700*x^2 + 794004*x - 3160075)
curve(f, -2,2); abline(h=0, v=0) #gráfico para decidir un intervalo
biseccion(f, 17, 22.2, 0.000001)

#Ejercicio 3
f = function(x) (x^3 - 2*x -5)
curve(f, -2,2); abline(h=0, v=0) #gráfico para decidir un intervalo
biseccion(f, 2, 9, 0.000001)
