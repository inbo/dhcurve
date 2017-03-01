#' Afgeleid model (= verschuiving t.o.v. Vlaams model) fitten op basis van het basismodel en de opgegeven dataset
#'
#' Functie die het Vlaams model verschuift op basis van domeinen met 10 - 50 bomen (Domeinmodel = Vlaams model - residu’s): op basis van omtrekklassen > 0.5 m
#'
#' @param Data.afgeleid dataframe 10-50
#' @param Basismodel model per boomsoort
#'
#' @return verschuiving per boomsoort en domein (dit is geen nieuw model, wel een verschuiving t.o.v. het Vlaams model)
#'
#' @export
#'
#' @importFrom dplyr %>% inner_join select_ distinct_ mutate_ group_by_ summarise_ ungroup
#'

fit.afgeleid <- function(Data.afgeleid, Basismodel) {

  Parameters <- modelparameters(Basismodel)

  ModelWeinigBomen <- Data.afgeleid %>%
    inner_join(Parameters %>%
                 select_(
                   ~BMS, ~Avl, ~Bvl, ~Cvl
                 ) %>%
                 distinct_(),
               by = c("BMS")
    ) %>%
    mutate_(
      H_VLmodel = ~Avl + Bvl * logOmtrek + Cvl * logOmtrek2,
      Resid = ~HOOGTE - H_VLmodel
    ) %>%
    mutate_(Resid2 = ~Resid^2) %>%
    group_by_(
      ~DOMEIN_ID,
      ~BOS_BHI,
      ~BMS,
      ~nBomen,
      ~Q5,
      ~Q5k,
      ~Q95,
      ~Q95k,
      ~nBomenInterval,
      ~nBomenOmtrek05,
      ~Avl,
      ~Bvl,
      ~Cvl
    ) %>%
    summarise_(
      sse = ~sum(c(Resid2)),
      gemRes = ~mean(Resid),
      maxResid = ~max(c(Resid2))
    ) %>%
    ungroup() %>%
    mutate_(
      Ad = ~Avl + gemRes,
      rmseD = ~sqrt(sse/(nBomenOmtrek05 - 2))
    )

  return(ModelWeinigBomen)
}
