#' stelt de slechtste curves en afwijkende metingen grafisch voor in een rapport
#'
#' Deze functie rendert een Rmd-file.  In de Rmd-file wordt voor elk model uit de lijst een grafiek van dezelfde vorm gemaakt, op basis van aparte functie of child-rmd _maakGrafiek_ die voor elk item uit de lijst opnieuw opgeroepen wordt.
#'
#' Op de grafieken moet volgende info getoond worden:
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
#' @param Basismodel model per boomsoort
#' @param Rmse.basis rmse_domein en rmse_Vlaams
#' @param Afgeleidmodel ingeval van verschoven Vlaams model
#' @param Rmse.afgeleid ingeval van verschoven Vlaams model
#' @param SlechtsteModellen lijst met slechtste modellen, dit zijn modellen met hoge rmse, afwijkende vorm (op basis van extremen en buigpunten) en/of modellen met afwijkende metingen
#'
#' @return document (html/pdf) met te controleren curves (incl. aantal metingen per curve)

validatierapport <- function(Basismodel, Rmse.basis, Afgeleidmodel, Rmse.afgeleid, SlechtsteModellen){

}
