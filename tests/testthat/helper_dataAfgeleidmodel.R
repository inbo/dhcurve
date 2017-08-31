
dataAfgeleidmodel <-
  function(nBomenBasis = 200, nBomenAfgeleid = 40, sd = 3,
           Extradata = NULL,
           Uitzonderingen  =
             data.frame(DOMEIN_ID = "", BMS = "", min_basis = NA,
                        min_afgeleid = NA, stringsAsFactors = FALSE)) {

  Metingen <- testdataset(rep(nBomenBasis, 6)) %>%
    bind_rows(
      testdata1domein(nBomen = nBomenAfgeleid, minOmtrek = 50,
                      A = 15, sd = sd) %>%
        mutate_(
          BMS = ~"testboom",
          IDbms = ~1,
          DOMEIN_ID = ~"Klein",
          BOS_BHI = ~"DOMEIN_Klein",
          Status = ~"Niet gecontroleerd",
          ID = ~as.character(as.integer(rownames(.)) + 1200)
        )
    ) %>%
    bind_rows(Extradata)

  Datalijst <- initiatie(Metingen, Uitzonderingen)

  Data.basis <- Datalijst[["Basis"]]
  Basismodel <- fit.basis(Data.basis)

  Data.afgeleid <- Datalijst[["Afgeleid"]]
  Afgeleidmodel <- fit.afgeleid(Data.afgeleid, Basismodel)

  return(list(Basisdata = Data.basis, Basismodel = Basismodel,
              Afgeleidedata = Data.afgeleid, Afgeleidmodel = Afgeleidmodel))
}
