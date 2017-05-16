#' Afgeleid model (= verschuiving t.o.v. Vlaams model) fitten op basis van het basismodel en de opgegeven dataset
#'
#' Functie die een model berekent voor domeinen met 10 - 50 bomen (binnen omtrekklassen > 0.5 m) door verschuiving van het Vlaams model voor die boomsoort.
#'
#' @param Data.afgeleid Data.afgeleid dataframe 10-50
#' @param Basismodel model per boomsoort
#'
#' @return list met model per boomsoort-domein-combinatie
#'
#' @export
#'
#' @importFrom stats lm
#' @importFrom dplyr %>% inner_join group_by_ do_ ungroup select_ filter_
#'

fit.afgeleid <- function(Data.afgeleid, Basismodel) {

  invoercontrole(Data.afgeleid, "fit")
  invoercontrole(Basismodel, "basismodel")

  #eerst doen we een hoogteschatting op basis van het Vlaams model voor alle
  #omtrekklassen binnen de ranges van de boomsoort-domein-combinaties waarvoor
  #we het afgeleid model maken.
  Hoogteschatting <- Basismodel %>%
    inner_join(
      Data.afgeleid,
      by = c("BMS")
    ) %>%
    group_by_(
      ~BMS,
      ~DOMEIN_ID
    ) %>%
    do_(
      ~hoogteschatting.basis(.$Model[[1]],
                             select_(., ~-Model),
                             "Basis")
    ) %>%
    ungroup() %>%
    select_(~-H_D_finaal)

  Afgeleidmodel <- Hoogteschatting %>%
    filter_(~!is.na(HOOGTE)) %>%
    group_by_(~BMS, ~DOMEIN_ID) %>%
    do_(
      Model = ~ lm(
        HOOGTE ~ 1 + offset(H_VL_finaal),
        data = .
      )
    ) %>%
    ungroup()

  return(list(Afgeleidmodel, Hoogteschatting))
}
