#' Genereert testdata voor 1 boomsoort-domeincombinatie
#'
#' Deze functie genereert een testdataset voor 1 domein die voldoet aan de opgegeven vereisten.
#'
#' @param nBomen  Aantal gemeten bomen voor het domein
#' @param minOmtrek  Minimale omtrek voor de data
#' @param maxOmtrek  Maximale omtrek voor de data
#' @param A  Parameter voor het intercept van de functie
#' @param B  Parameter voor de 1ste-graadsfunctie
#' @param C  Parameter voor de 2de-graadsfunctie
#' @param rmse  foutenmarge t.o.v. model voor de data
#'
#' @return Dataframe met testdata voor 1 boomsoort-domeincombinatie met velden C13 en HOOGTE.
#'
#' @export
#'
#'
#' @importFrom dplyr %>% mutate_ group_by_ ungroup
#' @importFrom stats runif rnorm
#'

testdata1domein <- function(nBomen = 100, minOmtrek = 20, maxOmtrek = 239,
                            A = 20, B = 15, C = 1, rmse = 3) {

  Metingen <-
    data.frame(C13 = round(runif(nBomen, minOmtrek, maxOmtrek))) %>%
    mutate_(
      Omtrek = ~floor(C13 / 10) / 10 + 0.05
    ) %>%
    group_by_(~Omtrek) %>%
    mutate_(
      HOOGTE = ~rnorm(n(),
                      A + B * log(Omtrek) + C * log(Omtrek)^2,
                      rmse * sqrt(n()))
    ) %>%
    ungroup() %>%
    select_(~-Omtrek)

  return(Metingen)
}

