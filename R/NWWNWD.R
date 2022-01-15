#' @title Projekt wyznaczajacy NWW oraz NWD
#' @author Jan Bobrowski Filip Chelmikowski
#' @description Szuka najmniejszej wspolnej wielokrotnosci oraz najwiekszego wspolnego dzielnika
#' @param x integer variable
#' @param y integer variable
#' @param ... additional arguments to be passed in the future update of this package
#' @return Funkcja zwaraca w wyniku NWW oraz NWD dwoch liczb
#' @rdname NwdNww
#' @export
NwdNww <- function(x, y, ...) UseMethod("NwdNww")


#' @title Wyliczanie NWD NWW
#' @author Jan Bobrowski Filip Chelmikowski
#' @description Szuka najmniejszej wspolnej wielokrotnosci oraz najwiekszego wspolnego dzielnika
#' @param x integer variable
#' @param y integer variable
#' @param ... additional arguments to be passed in the future update of this package
#' @return Funkcja zwaraca w wyniku NWW oraz NWD dwoch liczb
#' @rdname NwdNww.default
#' @export
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

#' @title Print NWD NWW
#' @author Jan Bobrowski Filip Chelmikowski
#' @description Szuka najmniejszej wspolnej wielokrotnosci oraz najwiekszego wspolnego dzielnika
#' @param x integer variable
#' @param y integer variable
#' @param ... additional arguments to be passed in the future update of this package
#' @return Funkcja zwaraca w wyniku NWW oraz NWD dwoch liczb
#' @rdname print.NwdNww
#' @export
print.NwdNww <- function(x,y,...)
{
  cat("Najwiekszy wspolny dzielnik\n")
  print(x$`Najwiekszy wspolny dzielnik`)
  cat("\nNajmniejsza wspolna wielokrotnosc\n")
  print(x$`Najmniejsza wspolna wielokrotnosc`)
}


#' @title Print NWD NWW
#' @author Jan Bobrowski Filip Chelmikowski
#' @description Szuka najmniejszej wspolnej wielokrotnosci oraz najwiekszego wspolnego dzielnika
#' @param object object of the NwdNww method
#' @param ... additional arguments to be passed in the future update of this package
#' @return Funkcja zwaraca w wyniku NWW oraz NWD dwoch liczb
#' @rdname summary.NwdNww
#' @export
summary.NwdNww<-function(object, ...)
{
  cat("Najwiekszy wspolny dzielnik:\n")
  cat(summary(object$'Najwiekszy wspolny dzielnik'))
  cat("\nNajmniejsza wspolna wielokrotnosc:\n")
  cat(summary(object$'Najmniejsza wspolna wielokrotnosc'))
}


