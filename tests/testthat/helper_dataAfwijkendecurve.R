
dataAfwijkendeCurve <- function() {

  Metingen <-
    data.frame(DOMEIN_ID = c("HM", "LM"),
               BOS_BHI = c("HoogMinimum", "LaagMaximum"), nBomen = 200,
               minOmtrek = 20, maxOmtrek = 239,
               A = 20, B = c(7, 5), C = c(4, -6), sd = 3,
               stringsAsFactors = FALSE) %>%
    group_by_(
      ~DOMEIN_ID, ~BOS_BHI
    ) %>%
    do_(
      ~testdata1domein(.$nBomen, .$minOmtrek, .$maxOmtrek,
                       .$A, .$B, .$C, .$sd)
    ) %>%
    ungroup() %>%
    mutate_(
      Status = ~"Niet gecontroleerd",
      ID = ~rownames(.)
    )

  Metingen <-
    merge(
      data.frame(BMS = c("testboom", "andereboom"), IDbms = 1:2,
                 stringsAsFactors = FALSE),
      Metingen
    ) %>%
    bind_rows(testdataset(rep(200, 6)))

  Datalijst <- initiatie(Metingen)

  Data.basis <- Datalijst[["Basis"]]
  Basismodel <- fit.basis(Data.basis)

  Data.lokaal <- Datalijst[["Lokaal"]]
  Lokaalmodel <- fit.lokaal(Data.lokaal)

  return(list(Basisdata = Data.basis, Basismodel = Basismodel,
              Lokaledata = Data.lokaal, Lokaalmodel = Lokaalmodel))
}
