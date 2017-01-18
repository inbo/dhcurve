#' Berekent curvekarakteristieken van de domeinmodellen
#'
#' Berekent curvekarakteristieken die toelaten om mogelijke afwijkingen in curvevorm te detecteren: extremen (min en max), buigpunt,...
#'
#' @param Basismodel model per boomsoort
#'
#' @return dataframe met curvekarakteristieken per domein en boomsoort
#'
#' @export
#'
#' @importFrom dplyr %>% mutate_
#'

curvekarakteristieken <- function(Basismodel){

  Parameters <- modelparameters(Basismodel) %>%
    mutate_(
      Omtrek_Extr_Hoogte.d = ~exp(-Bd/(2*Cd)),
      Extr_Hoogte.d = ~Ad + Bd * log(Omtrek_Extr_Hoogte.d) + Cd * (log(Omtrek_Extr_Hoogte.d))^2,
      Hoogteverschil.d = ~Extr_Hoogte.d - (Ad + Bd * log(Q95) + Cd * (log(Q95))^2),
      Omtrek_Buigpunt.d = ~exp(1 - Bd/(2*Cd)),
      Verschil_rico_BP_Q5.d = ~(2*Cd*log(Omtrek_Buigpunt.d) + Bd)/Omtrek_Buigpunt.d - (2*Cd*log(Q5) + Bd)/Q5,
      Verschil_rico_BP_Q5_per_omtrek.d = ~Verschil_rico_BP_Q5.d * (Omtrek_Buigpunt.d - Q5),

      Omtrek_Extr_Hoogte.vl = ~exp(-Bvl/(2*Cvl)),
      Extr_Hoogte.vl = ~Avl + Bvl * log(Omtrek_Extr_Hoogte.vl) + Cvl * (log(Omtrek_Extr_Hoogte.vl))^2,
      Hoogteverschil.vl = ~Extr_Hoogte.vl - (Avl + Bvl * log(Q95) + Cvl * (log(Q95))^2),
      Omtrek_Buigpunt.vl = ~exp(1 - Bvl/(2*Cvl)),
      Verschil_rico_BP_Q5.vl = ~(2*Cvl*log(Omtrek_Buigpunt.vl) + Bvl)/Omtrek_Buigpunt.vl - (2*Cvl*log(Q5) + Bvl)/Q5,
      Verschil_rico_BP_Q5_per_omtrek.vl = ~Verschil_rico_BP_Q5.vl * (Omtrek_Buigpunt.vl - Q5)
    )

  return(Parameters)

}
