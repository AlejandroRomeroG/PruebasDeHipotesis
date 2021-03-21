## PRUEBAS PARA MEDIAS ##

PruebaMediasCaso1 <- function(media,desvestpob,n,certeza,valorhipotetico){
  zgrande <- (media-valorhipotetico)/(desvestpob/sqrt(n))
  
  alfa <- 1-certeza
  alfachida <- alfa/2
  
  z <- -qnorm(alfachida)
  
  if(abs(zgrande)>=z) {
    cat("|Z| >= z\n", zgrande, " >= ", z, "\n", "Por lo tanto, podemos rechazar la hipotesis nula")
  } else{
    cat("z >= |Z|\n", z, " >= ", zgrande, "\n", "Por lo tanto, no podemos rechazar la hipotesis nula")
  }
}

PruebaMediasCaso2 <- function(media,desvestpob,n,certeza,valorhipotetico){
  zgrande <- (media-valorhipotetico)/(desvestpob/sqrt(n))
  
  alfa <- 1-certeza
  
  z <- -qnorm(alfa)
  
  if(zgrande>=z) {
    cat("Z >= z\n", zgrande, " >= ", z, "\n", "Por lo tanto, podemos rechazar la hipotesis nula")
  } else{
    cat("z >= Z\n", z, " >= ", zgrande, "\n", "Por lo tanto, no podemos rechazar la hipotesis nula")
  }
}

PruebaMediasCaso3 <- function(media,desvestpob,n,certeza,valorhipotetico){
  zgrande <- (media-valorhipotetico)/(desvestpob/sqrt(n))
  
  alfa <- 1-certeza
  
  z <- qnorm(alfa)
  
  if(zgrande<=z) {
    cat("Z <= -z\n", zgrande, " <= ", z, "\n", "Por lo tanto, podemos rechazar la hipotesis nula")
  } else{
    cat("-z <= Z\n", z, " <= ", zgrande, "\n", "Por lo tanto, no podemos rechazar la hipotesis nula")
  }
}

PruebaMediasVarianzaPoblacionalDesconocidaCaso1 <- function(media,desvest,n,certeza,valorhipotetico){
  tgrande <- (media-valorhipotetico)/(desvest/sqrt(n))
  
  alfa <- 1-certeza
  alfachida <- alfa/2
  
  t <- -qt(alfachida,n-1)
  
  if(abs(tgrande)>=t) {
    cat("|T| >= t\n", tgrande, " >= ", t, "\n", "Por lo tanto, podemos rechazar la hipotesis nula")
  } else{
    cat("t >= |T|\n", t, " >= ", tgrande, "\n", "Por lo tanto, no podemos rechazar la hipotesis nula")
  }
}

PruebaMediasVarianzaPoblacionalDesconocidaCaso2 <- function(media,desvest,n,certeza,valorhipotetico){
  tgrande <- (media-valorhipotetico)/(desvest/sqrt(n))
  
  alfa <- 1-certeza
  
  t <- -qt(alfa,n-1)
  
  if(tgrande>=t) {
    cat("T >= t\n", tgrande, " >= ", t, "\n", "Por lo tanto, podemos rechazar la hipotesis nula")
  } else{
    cat("t >= T\n", t, " >= ", tgrande, "\n", "Por lo tanto, no podemos rechazar la hipotesis nula")
  }
}

PruebaMediasVarianzaPoblacionalDesconocidaCaso3 <- function(media,desvest,n,certeza,valorhipotetico){
  tgrande <- (media-valorhipotetico)/(desvest/sqrt(n))
  
  alfa <- 1-certeza
  
  t <- qt(alfa,n-1)
  
  if(tgrande<=t) {
    cat("T <= -t\n", tgrande, " <= ", t, "\n", "Por lo tanto, podemos rechazar la hipotesis nula")
  } else{
    cat("-t <= T\n", t, " <= ", tgrande, "\n", "Por lo tanto, no podemos rechazar la hipotesis nula")
  }
}

## PRUEBAS PARA DIFERENCIAS DE MEDIAS ##

PruebaDiferenciaDeMediasCaso1 <- function(media1,media2,desvest1,desvest2,n1,n2,certeza,diferencia){
  zgrande <- (media1-media2-diferencia)/sqrt(((desvest1^2)/n1) + ((desvest2^2)/n2))
  
  alfa <- 1-certeza
  alfachida <- alfa/2
  
  z <- -qnorm(alfachida)
  
  if(abs(zgrande)>=z) {
    cat("|Z| >= z\n", zgrande, " >= ", z, "\n", "Por lo tanto, podemos rechazar la hipotesis nula")
  } else{
    cat("z >= |Z|\n", z, " >= ", zgrande, "\n", "Por lo tanto, no podemos rechazar la hipotesis nula")
  }
}

PruebaDiferenciaDeMediasCaso2 <- function(media1,media2,desvest1,desvest2,n1,n2,certeza,diferencia){
  zgrande <- (media1-media2-diferencia)/sqrt(((desvest1^2)/n1) + ((desvest2^2)/n2))
  
  alfa <- 1-certeza
  
  z <- -qnorm(alfa)
  
  if(zgrande>=z) {
    cat("Z >= z\n", zgrande, " >= ", z, "\n", "Por lo tanto, podemos rechazar la hipotesis nula")
  } else{
    cat("z >= Z\n", z, " >= ", zgrande, "\n", "Por lo tanto, no podemos rechazar la hipotesis nula")
  }
}

PruebaDiferenciaDeMediasCaso3 <- function(media1,media2,desvest1,desvest2,n1,n2,certeza,diferencia){
  zgrande <- (media1-media2-diferencia)/sqrt(((desvest1^2)/n1) + ((desvest2^2)/n2))
  
  alfa <- 1-certeza
  
  z <- qnorm(alfa)
  
  if(zgrande<=z) {
    cat("Z <= -z\n", zgrande, " <= ", z, "\n", "Por lo tanto, podemos rechazar la hipotesis nula")
  } else{
    cat("-z <= Z\n", z, " <= ", zgrande, "\n", "Por lo tanto, no podemos rechazar la hipotesis nula")
  }
}

PruebaDiferenciaDeMediasVarianzasIdenticasCaso1 <- function(media1,media2,desvest1,desvest2,n1,n2,certeza,diferencia){
  varp <- ((n1-1)*(desvest1^2)+(n2-1)*(desvest2^2))/(n1+n2-2)
  desvestp <- sqrt(varp)
  
  alfa <- 1-certeza
  alfachida <- alfa/2
  
  tgrande <- (media1-media2-diferencia)/(desvestp*sqrt(1/n1 + 1/n2))
  
  t <- -qt(alfachida,n1+n2-2)
  
  if(abs(tgrande)>=t) {
    cat("|T| >= t\n", tgrande, " >= ", t,"\n", "Por lo tanto, podemos rechazar la hipotesis nula")
  } else {
    cat("t >= |T|\n", t, " >= ", tgrande,"\n", "Por lo tanto, no podemos rechazar la hipotesis nula")
  }
}

PruebaDiferenciaDeMediasVarianzasIdenticasCaso2 <- function(media1,media2,desvest1,desvest2,n1,n2,certeza,diferencia){
  varp <- ((n1-1)*(desvest1^2)+(n2-1)*(desvest2^2))/(n1+n2-2)
  desvestp <- sqrt(varp)
  
  alfa <- 1-certeza
  
  tgrande <- (media1-media2-diferencia)/(desvestp*sqrt(1/n1 + 1/n2))
  
  t <- -qt(alfa,n1+n2-2)
  
  if(tgrande>=t) {
    cat("T >= t\n", tgrande, " >= ", t,"\n", "Por lo tanto, podemos rechazar la hipotesis nula")
  } else {
    cat("t >= T\n", t, " >= ", tgrande,"\n", "Por lo tanto, no podemos rechazar la hipotesis nula")
  }
}

PruebaDiferenciaDeMediasVarianzasIdenticasCaso3 <- function(media1,media2,desvest1,desvest2,n1,n2,certeza,diferencia){
  varp <- ((n1-1)*(desvest1^2)+(n2-1)*(desvest2^2))/(n1+n2-2)
  desvestp <- sqrt(varp)
  
  alfa <- 1-certeza
  
  tgrande <- (media1-media2-diferencia)/(desvestp*sqrt(1/n1 + 1/n2))
  
  t <- qt(alfa,n1+n2-2)
  
  if(tgrande<=t) {
    cat("T <= -t\n", tgrande, " <= ", t,"\n", "Por lo tanto, podemos rechazar la hipotesis nula")
  } else {
    cat("-t <= T\n", t, " <= ", tgrande,"\n", "Por lo tanto, no podemos rechazar la hipotesis nula")
  }
}

## PRUEBAS PARA VARIANZAS ##

PruebaVarianzasCaso1 <- function(varianzamuestral,n,certeza,varianzapoblacional){
  chigrande <- ((n-1)*varianzamuestral)/varianzapoblacional
  
  chi <- qchisq(1-((1-certeza)/2),n-1)
  
  if(chigrande>=chi){
    cat("X^2 >= X^2_[alfa/2,n-1]\n", chigrande, " >= ", chi,"\n", "Por lo tanto, podemos rechazar la hipotesis nula")
  } else{
    cat("X^2_[alfa/2,n-1] >= X^2\n", chi, " >= ", chigrande,"\n", "Por lo tanto, no podemos rechazar la hipotesis nula")
  }
}

PruebaVarianzasCaso2 <- function(varianzamuestral,n,certeza,varianzapoblacional){
  chigrande <- ((n-1)*varianzamuestral)/varianzapoblacional

  chi <- qchisq(certeza,n-1)
  
  if(chigrande>=chi){
    cat("X^2 >= X^2_[alfa,n-1]\n", chigrande, " >= ", chi,"\n", "Por lo tanto, podemos rechazar la hipotesis nula")
  } else{
    cat("X^2_[alfa,n-1] >= X^2\n", chi, " >= ", chigrande,"\n", "Por lo tanto, no podemos rechazar la hipotesis nula")
  }
}

PruebaVarianzasCaso3 <- function(varianzamuestral,n,certeza,varianzapoblacional){
  chigrande <- ((n-1)*varianzamuestral)/varianzapoblacional
  
  chi <- qchisq(1-certeza,n-1)
  
  if(chigrande<=chi){
    cat("X^2 <= X^2_[alfa,n-1]\n", chigrande, " <= ", chi,"\n", "Por lo tanto, podemos rechazar la hipotesis nula")
  } else{
    cat("X^2_[alfa,n-1] <= X^2\n", chi, " <= ", chigrande,"\n", "Por lo tanto, no podemos rechazar la hipotesis nula")
  }
}

PruebaVarianzasDosVarianzasMuestralesCaso1 <- function(varmuestral1,varmuestral2,n1,n2,certeza){
  if(varmuestral1>varmuestral2){
    fgrande <- varmuestral1/varmuestral2
    
    f <- qf(1-((1-certeza)/2),n1-1,n2-1)
    
    if(fgrande>=f) {
      cat("F >= f\n", fgrande, " >= ", f,"\n", "Por lo tanto, podemos rechazar la hipotesis nula")
    } else {
      cat("f >= F\n", f, " >= ", fgrande,"\n", "Por lo tanto, no podemos rechazar la hipotesis nula")
    }
    
  }else{
    fgrande <- varmuestral2/varmuestral1
    
    f <- qf(1-((1-certeza)/2),n2-1,n1-1)
    
    if(fgrande>=f) {
      cat("F >= f\n", fgrande, " >= ", f,"\n", "Por lo tanto, podemos rechazar la hipotesis nula")
    } else {
      cat("f >= F\n", f, " >= ", fgrande,"\n", "Por lo tanto, no podemos rechazar la hipotesis nula")
    }
    
  }
}

PruebaVarianzasDosVarianzasMuestralesCaso2 <- function(varmuestral1,varmuestral2,n1,n2,certeza){
  fgrande <- varmuestral1/varmuestral2
  
  f <- qf(certeza,n1-1,n2-1)
  
  if(fgrande>=f) {
    cat("F >= f\n", fgrande, " >= ", f,"\n", "Por lo tanto, podemos rechazar la hipotesis nula")
  } else {
    cat("f >= F\n", f, " >= ", fgrande,"\n", "Por lo tanto, no podemos rechazar la hipotesis nula")
  }
}

PruebaVarianzasDosVarianzasMuestralesCaso3 <- function(varmuestral1,varmuestral2,n1,n2,certeza){
  fgrande <- varmuestral2/varmuestral1
  
  f <- qf(certeza,n2-1,n1-1)
  
  if(fgrande>=f) {
    cat("F >= f\n", fgrande, " >= ", f,"\n", "Por lo tanto, podemos rechazar la hipotesis nula")
  } else {
    cat("f >= F\n", f, " >= ", fgrande,"\n", "Por lo tanto, no podemos rechazar la hipotesis nula")
  }
}

## PRUEBAS PARA PROPORCIONES ##

PruebaProporcionesCaso1 <- function(x,n,valorhipotetico,certeza){
  zgrande <- (x-(n*valorhipotetico))/sqrt(n*valorhipotetico*(1-valorhipotetico))
  
  alfa <- 1-certeza
  alfachida <- alfa/2
  
  z <- -qnorm(alfachida)
  
  if(abs(zgrande)>=z) {
    cat("|Z| >= z\n", zgrande, " >= ", z, "\n", "Por lo tanto, podemos rechazar la hipotesis nula")
  } else{
    cat("z >= |Z|\n", z, " >= ", zgrande, "\n", "Por lo tanto, no podemos rechazar la hipotesis nula")
  }
}

PruebaProporcionesCaso2 <- function(x,n,valorhipotetico,certeza){
  zgrande <- (x-(n*valorhipotetico))/sqrt(n*valorhipotetico*(1-valorhipotetico))
  
  alfa <- 1-certeza
  
  z <- -qnorm(alfa)
  
  if(zgrande>=z) {
    cat("Z >= z\n", zgrande, " >= ", z, "\n", "Por lo tanto, podemos rechazar la hipotesis nula")
  } else{
    cat("z >= Z\n", z, " >= ", zgrande, "\n", "Por lo tanto, no podemos rechazar la hipotesis nula")
  }
}

PruebaProporcionesCaso3 <- function(x,n,valorhipotetico,certeza){
  zgrande <- (x-(n*valorhipotetico))/sqrt(n*valorhipotetico*(1-valorhipotetico))
  
  alfa <- 1-certeza
  
  z <- qnorm(alfa)
  
  if(zgrande<=z) {
    cat("Z <= -z\n", zgrande, " <= ", z, "\n", "Por lo tanto, podemos rechazar la hipotesis nula")
  } else{
    cat("-z <= Z\n", z, " <= ", zgrande, "\n", "Por lo tanto, no podemos rechazar la hipotesis nula")
  }
}