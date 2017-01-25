#' maakt een grafiek van een slechte curve of model met afwijkende metingen
#'
#' Deze functie genereert een grafiek met volgende info op basis van de gegeven dataset (= data van 1 domein-boomsoort-combinatie!):
#'
#' - puntenwolk metingen
#'
#' - curve Vlaams model
#'
#' - curve domeinmodel of verschoven Vlaams model (dus domeinspecifieke curve)
#'
#' - extremen en buigpunten waar aanwezig (met vertikale stippellijn?)
#'
#' - grenzen van bruikbaar interval
#'
#' - vermelding van boomsoort, domein, aantal waarnemingen, rmse, evt. parameters A, B en C
#'
#' - eventueel afwijkende metingen
#'
#' Idee is dat in de grafieken een afwijkende kleur het probleem aangeeft: afwijkende meting, buigpunt,...  Daarnaast wordt het probleem ook tekstueel weergegeven
#'
#' @param Data Dataset van 1 boomsoort-domeincombinatie met alle info die nodig is om een grafiek aan te maken: gemeten waarden, geschatte waarde voor domeinmodel en Vlaams model, aanduiding van afwijkende waarden, buigpunten, te tonen info als RMSE, parameterwaarden, reden waarom het model opgenomen is,...
#'
#' @return grafiek van de boomsoort-domeincombinatie waarin de problemen in rood aangeduid zijn
#'
#' @export
#'
#' @importFrom ggplot2 ggplot aes_ geom_jitter geom_line geom_vline scale_colour_manual alpha
#'

maakGrafiek <- function(Data){

  #testen dat BMS, DOMEIN_ID, ... maar 1 unieke waarde hebben!

  grafiek <-
    ggplot(Data, aes_(x = ~Omtrek, y = ~HOOGTE, colour = ~Afwijkend)) +
          scale_colour_manual(values = alpha(c("black","red"))) +
          geom_jitter(width = 0.03, height = 0) +
          geom_line(aes_(y = ~H_D_finaal), colour = "blue") +
          geom_line(aes_(y = ~H_VL_finaal), colour = "red") +
          geom_vline(xintercept = Data$Q5) + geom_vline(xintercept = Data$Q95) +
          geom_vline(xintercept = Data$Omtrek_Buigpunt, colour = "red") +
          geom_vline(xintercept = Data$Omtrek_Extr_Hoogte, colour = "red")

  return(grafiek)

}
