#' @title Berekent RMSE van afgeleid model
#'
#' @description
#' Deze functie berekent de RMSE op basis van verschil tussen de
#' hoogteschatting volgens het domeinmodel en de gemeten hoogte (voor
#' omtrekklassen > 0.5 m).  Dit is slechts een deel van de totale RMSE van het
#' afgeleid model, want behalve deze RMSE van de verschuiving moet hier ook de
#' RMSE van het Vlaams model (basismodel) in rekening gebracht worden.
#' Opgelet!  In tegenstelling tot de meeste functies van dit package werkt deze
#' functie op basis van de meetgegevens van 1 model.  Zie voorbeeld voor een
#' methode om deze functie te kunnen toepassen vertrekkend van het berekende
#' model.
#'
#'
#' @param Verschovenmodel Afgeleid model voor één boomsoort-domeincombinatie
#' (`lm`-object)
#' @param Boomsoort `BMS`
#' @param Domein `DOMEIN_ID`
#'
#' @return Dataframe met `BMS` (boomsoort), `DOMEIN_ID`, `RmseVerschuiving`
#'
#' @examples
#' library(dplyr)
#'
#' #Datasets inladen en het basismodel en afgeleid model berekenen
#' Data <- testdataset()
#' Datalijst <- initiatie(Data)
#' Data.basis <- Datalijst[["Basis"]]
#' Basismodel <- fit.basis(Data.basis)
#' Data.afgeleid <- Datalijst[["Afgeleid"]]
#' Afgeleidmodel <- fit.afgeleid(Data.afgeleid, Basismodel)
#'
#' # De RMSE berekenen voor de verschuiving van het Vlaams model naar een
#' # afgeleid model
#' Afgeleidmodel[[1]] %>%
#'   rowwise() %>%
#'   do(
#'     rmse.verschuiving(.$Model, .$BMS, .$DOMEIN_ID)
#'   ) %>%
#'   ungroup()
#' #Nota: voor een berekening van de volledige RMSE van een afgeleid model
#' #moet ook de RMSE van het basismodel in rekening gebracht worden
#'
#' @export
#'
#' @importFrom dplyr %>% mutate summarise select n
#' @importFrom rlang .data
#' @importFrom stats influence
#' @importFrom assertthat assert_that
#'

rmse.verschuiving <- function(Verschovenmodel, Boomsoort, Domein) {

  assert_that(inherits(Verschovenmodel, "lm"),
              msg = "Domeinsoortmodel moet een lineair model zijn (zie
              documentatie)")

  Rmse <- data.frame(RMSE = influence(Verschovenmodel)$sigma) %>%
    mutate(
      RMSE2 = .data$RMSE ^ 2
    ) %>%
    summarise(
      RMSE2 = sum(.data$RMSE2),
      nBomenModel = n()
    ) %>%
    mutate(
      RmseVerschuiving = sqrt(.data$RMSE2 / (.data$nBomenModel - 2)),
      BMS = Boomsoort,
      DOMEIN_ID = Domein
    ) %>%
    select(
      "BMS", "DOMEIN_ID", "nBomenModel", "RmseVerschuiving"
    )

  return(Rmse)

}
