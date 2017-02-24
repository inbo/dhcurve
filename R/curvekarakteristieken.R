#' Berekent curvekarakteristieken van de domeinmodellen
#'
#' Berekent curvekarakteristieken die toelaten om mogelijke afwijkingen in curvevorm te detecteren: extremen (min en max), buigpunt,...
#'
#' @param Basismodel model per boomsoort
#' @param Data meetgegevens (enkel nodig voor model per boomsoort-domein-combinatie)
#'
#' @return dataframe met curvekarakteristieken per domein en boomsoort
#'
#' @export
#'
#' @importFrom dplyr %>% mutate_
#' @importFrom assertthat has_name
#'

curvekarakteristieken <- function(Basismodel, Data = NULL){

  Parameters <- modelparameters(Basismodel, Data) %>%
    mutate_(
      Omtrek_Extr_Hoogte.d = ~exp(-Bd/(2*Cd)),
      Extr_Hoogte.d = ~Ad + Bd * log(Omtrek_Extr_Hoogte.d) + Cd * (log(Omtrek_Extr_Hoogte.d))^2,
      Hoogteverschil.d = ~Extr_Hoogte.d - (Ad + Bd * log(Q95k) + Cd * (log(Q95k))^2),
      Omtrek_Buigpunt.d = ~exp(1 - Bd/(2*Cd)),
      Verschil_rico_BP_Q5.d = ~(2*Cd*log(Omtrek_Buigpunt.d) + Bd)/Omtrek_Buigpunt.d - (2*Cd*log(Q5k) + Bd)/Q5k,
      Verschil_rico_BP_Q5_per_omtrek.d = ~Verschil_rico_BP_Q5.d * (Omtrek_Buigpunt.d - Q5k)
    )

  if (!has_name(Basismodel, "DOMEIN_ID")) {
    Parameters <- Parameters %>%
      mutate_(
        Omtrek_Extr_Hoogte.vl = ~exp(-Bvl/(2*Cvl)),
        Extr_Hoogte.vl = ~Avl + Bvl * log(Omtrek_Extr_Hoogte.vl) + Cvl * (log(Omtrek_Extr_Hoogte.vl))^2,
        Hoogteverschil.vl = ~Extr_Hoogte.vl - (Avl + Bvl * log(Q95k) + Cvl * (log(Q95k))^2),
        Omtrek_Buigpunt.vl = ~exp(1 - Bvl/(2*Cvl)),
        Verschil_rico_BP_Q5.vl = ~(2*Cvl*log(Omtrek_Buigpunt.vl) + Bvl)/Omtrek_Buigpunt.vl - (2*Cvl*log(Q5k) + Bvl)/Q5k,
        Verschil_rico_BP_Q5_per_omtrek.vl = ~Verschil_rico_BP_Q5.vl * (Omtrek_Buigpunt.vl - Q5k)
      )
  }

  return(Parameters)

}
