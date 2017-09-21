
dataAfwijkendeCurve <- function(nBomen = 200, sd = 3) {

  Metingen <-
    data.frame(DOMEIN_ID = c("HM", "LM"),
               BOS_BHI = c("HoogMinimum", "LaagMaximum"), nBomen,
               minOmtrek = 20, maxOmtrek = 239,
               A = 20, B = c(7, 5), C = c(4, -6), sd,
               stringsAsFactors = FALSE) %>%
    group_by_(
      ~DOMEIN_ID, ~BOS_BHI
    ) %>%
    do_(
      ~testdata1domein(.$nBomen, .$minOmtrek, .$maxOmtrek,
                       .$A, .$B, .$C, .$sd)
    ) %>%
    ungroup()

  Metingen <-
    merge(
      data.frame(BMS = c("testboom", "andereboom"), IDbms = 1:2,
                 stringsAsFactors = FALSE),
      Metingen
    ) %>%
    mutate_(
      Status = ~"Niet gecontroleerd",
      ID = ~as.character(as.integer(rownames(.)) + 6 * nBomen)
    ) %>%
    bind_rows(testdataset(rep(nBomen, 6)))

  Datalijst <- initiatie(Metingen)

  Data.basis <- Datalijst[["Basis"]]
  Basismodel <- fit.basis(Data.basis)

  Data.lokaal <- Datalijst[["Lokaal"]]
  Lokaalmodel <- fit.lokaal(Data.lokaal)

  return(list(Basisdata = Data.basis, Basismodel = Basismodel,
              Lokaledata = Data.lokaal, Lokaalmodel = Lokaalmodel))
}