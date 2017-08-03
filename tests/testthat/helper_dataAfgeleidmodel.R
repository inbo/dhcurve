
dataAfgeleidmodel <- function() {

  Metingen <- testdataset(rep(200, 6)) %>%
    bind_rows(
      testdata1domein(nBomen = 40, minOmtrek = 50, A = 15) %>%
        mutate_(
          BMS = ~"testboom",
          IDbms = ~1,
          DOMEIN_ID = ~"Klein",
          BOS_BHI = ~"DOMEIN_Klein",
          Status = ~"Niet gecontroleerd",
          ID = ~as.character(as.integer(rownames(.)) + 1200)
        )
    )

  Datalijst <- initiatie(Metingen)

  Data.basis <- Datalijst[["Basis"]]
  Basismodel <- fit.basis(Data.basis)

  Data.afgeleid <- Datalijst[["Afgeleid"]]
  Afgeleidmodel <- fit.afgeleid(Data.afgeleid, Basismodel)

  return(list(Basisdata = Data.basis, Basismodel = Basismodel,
              Afgeleidedata = Data.afgeleid, Afgeleidmodel = Afgeleidmodel))
}
