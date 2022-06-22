#' @title Genereert testdataset
#'
#' @description
#' Deze functie genereert een testdataset die voldoet aan de opgegeven
#' vereisten.
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
#' @importFrom dplyr %>% mutate group_by ungroup do
#' @importFrom plyr .
#' @importFrom rlang .data
#' @importFrom stats rnorm
#' @importFrom assertthat assert_that is.count
#'

testdataset <-
  function(nBomenDomein = c(200, 100, 500, 100, 100, 100, 50, 20, 8),
           BMS = "testboom", IDbms = 1,
           minOmtrek = 20, maxOmtrek = 239) {

  DOMEIN_ID <- LETTERS[1:length(nBomenDomein)]  #nolint
  BOS_BHI <- paste("Domein", DOMEIN_ID, sep = "_")
  A <- 30
  B <- 15
  C <- 1
  sd <- 2

  for (nBomen in nBomenDomein) {
    assert_that(
      is.count(nBomen),
      msg = "Alle waarden in nBomenDomein moeten positieve gehele getallen zijn"
    )
  }
  assert_that(is.character(as.character(BMS)))
  assert_that(length(BMS) == 1)
  assert_that(is.character(as.character(IDbms)))
  assert_that(length(IDbms) == 1)
  assert_that(is.numeric(minOmtrek))
  assert_that(is.numeric(maxOmtrek))
  assert_that(minOmtrek < maxOmtrek)

  assert_that(is.character(as.character(DOMEIN_ID)))
  assert_that(is.character(as.character(BOS_BHI)))

  assert_that(is.numeric(A))
  assert_that(is.numeric(B))
  assert_that(is.numeric(C))
  assert_that(is.numeric(sd))
  assert_that(sd > 0)

  Metingen <-
    data.frame(BMS, IDbms, DOMEIN_ID, BOS_BHI, nBomen = nBomenDomein,
               minOmtrek, maxOmtrek, stringsAsFactors = FALSE) %>%
    group_by(
      .data$BMS, .data$IDbms
    ) %>%
    mutate(
      A = rnorm(length(nBomenDomein), A, 5),
      B = rnorm(length(nBomenDomein), B, 1),
      C = rnorm(length(nBomenDomein), C, 1),
      sd = rnorm(length(nBomenDomein), sd, 0.5)
    ) %>%
    ungroup() %>%
    group_by(
      .data$BMS, .data$IDbms, .data$DOMEIN_ID, .data$BOS_BHI
    ) %>%
    do(
      testdata1domein(.$nBomen, .$minOmtrek, .$maxOmtrek,
                       .$A, .$B, .$C, .$sd)
    ) %>%
    ungroup() %>%
    mutate(
      Status = "Niet gecontroleerd",
      ID = rownames(.)
    )

  return(Metingen)
}
