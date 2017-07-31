#' Genereert testdataset
#'
#' Deze functie genereert een testdataset die voldoet aan de opgegeven vereisten.
#'
#' @param nBomenDomein Vector met aantal gemeten bomen per domein
#' @param BMS Naam van boomsoort
#' @param IDbms Identificatienummer van boomsoort
#' @param minOmtrek Minimale omtrek voor de data
#' @param maxOmtrek Maximale omtrek voor de data
#'
#' @return Dataframe met testdata voor 1 boomsoort met velden ...
#'
#' @export
#'
#' @importFrom dplyr %>% mutate_ group_by_ ungroup do_
#' @importFrom stats rnorm
#'

testdataset <-
  function(nBomenDomein = c(200, 100, 500, 100, 100, 100, 50, 20, 8),
           BMS = "testboom", IDbms = 1,
           minOmtrek = 20, maxOmtrek = 239) {

  DOMEIN_ID <- LETTERS[1:length(nBomenDomein)]
  BOS_BHI <- paste("Domein", DOMEIN_ID, sep = "_")
  A <- 20
  B <- 15
  C <- 1
  rmse <- 3

  Metingen <-
    data.frame(BMS, IDbms, DOMEIN_ID, BOS_BHI, nBomen = nBomenDomein,
               minOmtrek, maxOmtrek, stringsAsFactors = FALSE) %>%
    group_by_(
      ~BMS, ~IDbms
    ) %>%
    mutate_(
      A = ~rnorm(length(nBomenDomein), A, 5),
      B = ~rnorm(length(nBomenDomein), B, 2),
      C = ~rnorm(length(nBomenDomein), C, 1),
      rmse = ~rnorm(length(nBomenDomein), rmse, 0.5)
    ) %>%
    ungroup() %>%
    group_by_(
      ~BMS, ~IDbms, ~DOMEIN_ID, ~BOS_BHI
    ) %>%
    do_(
      ~testdata1domein(.$nBomen, .$minOmtrek, .$maxOmtrek,
                       .$A, .$B, .$C, .$rmse)
    ) %>%
    ungroup() %>%
    mutate_(
      Status = ~"Niet gecontroleerd"
    )

  return(Metingen)
}
