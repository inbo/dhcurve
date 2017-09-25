context("test validatie")

describe("validatie", {

  wd <- getwd()

  test_wd <- tempdir()

  setwd(test_wd)

  library(dplyr)
  library(tibble)

  #data genereren voor basismodel en afgeleid model
  Data <-
    dataAfgeleidmodel(Extradata =
                        data.frame(BMS = "testboom",
                                   IDbms = 1,
                                   DOMEIN_ID = rep(c(LETTERS[1:6], "Klein"), 2),
                                   BOS_BHI =
                                     rep(c(sprintf("Domein_%s", LETTERS[1:6]),
                                           "DOMEIN_Klein"),
                                         2),
                                   C13 = 200,
                                   HOOGTE = c(rep(1, 7), rep(50, 7)),
                                   Status = "Niet gecontroleerd",
                                   ID = as.character(2000:2013),
                                   stringsAsFactors = FALSE))

  Basismodel <- Data[["Basismodel"]]

  Afgeleidedata <- Data[["Afgeleidedata"]]
  Afgeleidmodel <- Data[["Afgeleidmodel"]]


  #data genereren voor lokaal model
  Metingen <- testdataset(200) %>%
    bind_rows(data.frame(BMS = "testboom",
                         IDbms = 1,
                         DOMEIN_ID = "A",
                         BOS_BHI = "Domein_A",
                         C13 = 200,
                         HOOGTE = c(1, 50),
                         Status = "Niet gecontroleerd",
                         ID = as.character(300:301),
                         stringsAsFactors = FALSE))

  Datalijst <- initiatie(Metingen)

  Data.lokaal <- Datalijst[["Lokaal"]]
  Lokaalmodel <- fit.lokaal(Data.lokaal)


  it("De uitvoer van de functies is correct", {
    expect_equal(validatie.basis(Basismodel) %>%
                   colnames(.),
                 c("BMS", "DOMEIN_ID", "BOS_BHI", "nBomenInterval",
                   "nBomenOmtrek05", "nBomen", "Q5k", "Q95k", "Omtrek",
                   "H_D_finaal", "H_VL_finaal", "IDbms", "C13", "HOOGTE",
                   "Status", "ID", "Rijnr", "logOmtrek", "logOmtrek2", "Q5",
                   "Q95", "rmseD", "maxResid", "HogeRmse", "Afwijkend"))
    expect_equal(validatie.afgeleid(Basismodel, Afgeleidmodel) %>%
                   colnames(.),
                 c("BMS", "DOMEIN_ID", "maxResid", "BOS_BHI", "nBomenInterval",
                   "nBomenOmtrek05", "nBomen", "Q5k", "Q95k", "Omtrek",
                   "H_VL_finaal", "IDbms", "C13", "HOOGTE", "Status", "ID",
                   "Rijnr", "logOmtrek", "logOmtrek2", "Q5", "Q95",
                   "H_D_finaal", "ResidD2", "nBomenModel", "RmseVerschuiving",
                   "rmseVL", "rmseD", "HogeRmse", "Afwijkend")
    )
    expect_equal(validatie.lokaal(Lokaalmodel, Data.lokaal) %>%
                   colnames(.),
                 c("BMS", "DOMEIN_ID", "BOS_BHI", "nBomenInterval",
                   "nBomenOmtrek05", "nBomen", "Q5k", "Q95k", "Omtrek",
                   "H_D_finaal", "IDbms", "C13", "HOOGTE", "Status", "ID",
                   "Rijnr", "logOmtrek", "logOmtrek2", "Q5", "Q95", "rmseD",
                   "maxResid", "HogeRmse", "Afwijkend")
    )
  })

  it("De afwijkende metingen worden correct geselecteerd", {
    expect_equal(validatie.basis(Basismodel, AantalDomHogeRMSE = 0) %>%
                   select(DOMEIN_ID, BMS, C13, HOOGTE, Afwijkend),
                 tibble(DOMEIN_ID =
                          LETTERS[rep(1:6, each = 2)],
                        BMS = "testboom",
                        C13 = 200,
                        HOOGTE = rep(c(1, 50), 6),
                        Afwijkend = TRUE
                 )
    )
    expect_equal(validatie.afgeleid(Basismodel, Afgeleidmodel,
                                    AantalDomHogeRMSE = 0) %>%
                   select(DOMEIN_ID, BMS, C13, HOOGTE),
                 tibble(DOMEIN_ID = "Klein",
                        BMS = "testboom",
                        C13 = 200,
                        HOOGTE = c(1, 50)
                 )
    )
    expect_equal(validatie.lokaal(Lokaalmodel, Data.lokaal,
                                  AantalDomHogeRMSE = 0) %>%
                   select(DOMEIN_ID, BMS, C13, HOOGTE),
                 tibble(DOMEIN_ID = "A",
                        BMS = "testboom",
                        C13 = 200,
                        HOOGTE = c(1, 50)
                 )
    )
  })

  Metingen <- testdataset(rep(200, 10))

  Datalijst <- initiatie(Metingen)

  Data.basis <- Datalijst[["Basis"]]
  Basismodel <- fit.basis(Data.basis)


  it("Selectie AantalDomHogeRMSE werkt correct", {
    expect_error(
      validatie.basis(Basismodel, AantalDomHogeRMSE = -1),
      "AantalDomHogeRMSE moet een positief geheel getal zijn."
    )
    expect_equal( (validatie.basis(Basismodel, AantalDomHogeRMSE = 2) %>%
                   filter(HogeRmse) %>%
                   select(DOMEIN_ID) %>%
                   distinct() %>%
                   summarise(n = n()))$n,
                 2
    )
    expect_equal( (validatie.basis(Basismodel, AantalDomHogeRMSE = 5) %>%
                    filter(HogeRmse) %>%
                    select(DOMEIN_ID) %>%
                    distinct() %>%
                    summarise(n = n()))$n,
                 5
    )
    expect_equal( (validatie.basis(Basismodel, AantalDomHogeRMSE = 8) %>%
                    filter(HogeRmse) %>%
                    select(DOMEIN_ID) %>%
                    distinct() %>%
                    summarise(n = n()))$n,
                 8
    )
  })

  setwd(wd)

})
