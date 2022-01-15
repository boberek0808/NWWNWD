#' Szuka najmniejszej wspolnej wielokrotnosci oraz najwiekszego wspolnego dzielnika
#' @export
#' @param x integer variable
#' @param y integer variable
#' @param ... additional arguments to be passed in the future update of this package

NwdNww <- function(x, y, ...) UseMethod("NwdNww")

NwdNww.default <- function(x,y,...){
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(y))
  x2<-x
  y2<-y
  while(x!=y){
    if(x>y)
      x = x-y
    else
      y = y-x
  }
  nwd<-x
  nww<-x2*y2/nwd

  wyniki <- data.frame(nwd,nww)
  colnames(wyniki)<-c("Najwiekszy wspolny dzielnik", "Najmniejsza wspolna wielokrotnosc")
  wyniki <- list("Najwiekszy wspolny dzielnik" = nwd, "Najmniejsza wspolna wielokrotnosc" = nww)
  # Definiowanie klasy
  class(wyniki)<-append(class(wyniki),'NwdNww')
  return(wyniki)

}
# definiuję metodę print dla budowanej klasy
print.NwdNww <- function(x,y,...)
{
  cat("Najwiekszy wspolny dzielnik\n")
  print(x$`Najwiekszy wspolny dzielnik`)
  cat("\nNajmniejsza wspolna wielokrotnosc\n")
  print(x$`Najmniejsza wspolna wielokrotnosc`)
}

summary.NwdNww <- function(x,y,...)
{
  cat("Podsumowanie Nwd:\n")
  print(summary(x$`Najwiekszy wspolny dzielnik`))
  cat("Podsumowanie Nww:\n")
  print(summary(x$`Najmniejsza wspolna wielokrotnosc`))
  cat("Struktura:\n")
  str(x)
}




