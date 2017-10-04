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
#' @param sd  foutenmarge t.o.v. model voor de data
#'
#' @return Dataframe met testdata voor 1 boomsoort-domeincombinatie met velden C13 en HOOGTE.
#'
#' @export
#'
#' @importFrom dplyr %>% mutate_ group_by_ ungroup
#' @importFrom stats runif rnorm
#' @importFrom assertthat assert_that is.count
#'

testdata1domein <- function(nBomen = 100, minOmtrek = 20, maxOmtrek = 239,
                            A = 30, B = 15, C = 1, sd = 2) {

  assert_that(is.count(nBomen))
  assert_that(is.numeric(minOmtrek))
  assert_that(is.numeric(maxOmtrek))
  assert_that(minOmtrek < maxOmtrek)

  assert_that(is.numeric(A))
  assert_that(is.numeric(B))
  assert_that(is.numeric(C))
  assert_that(is.numeric(sd))
  assert_that(sd > 0)

  Metingen <-
    data.frame(C13 = round(runif(nBomen, minOmtrek, maxOmtrek))) %>%
    mutate_(
      Omtrek = ~floor(C13 / 10) / 10 + 0.05
    ) %>%
    group_by_(~Omtrek) %>%
    mutate_(
      HOOGTE = ~rnorm(n(),
                      A + B * log(Omtrek) + C * (log(Omtrek)) ^ 2,
                      sd),
      HOOGTE = ~ifelse(HOOGTE <= 0, 0.1, HOOGTE)
    ) %>%
    ungroup() %>%
    select_(~-Omtrek)

  return(Metingen)
}
