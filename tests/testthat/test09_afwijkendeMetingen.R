context("test afwijkendeMetingen")

describe("afwijkendemetingen", {

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
                                   HOOGTE = c(rep(1, 7), rep(60, 7)),
                                   Status = "Niet gecontroleerd",
                                   ID = as.character(2000:2013),
                                   stringsAsFactors = FALSE))

  #Rmse en hoogteschatting berekenen voor basismodel
  Basismodel <- Data[["Basismodel"]]
  Rmse <- Basismodel %>%
    rowwise() %>%
    do(
      rmse.basis(.$Model$data, "Basis", .$BMS)
    ) %>%
    ungroup()

  Hoogteschatting <- Basismodel %>%
    rowwise() %>%
    do(
      hoogteschatting.basis(.$Model, .$Model$data, "Basis", .$BMS)
    ) %>%
    ungroup()

  DatasetBasis <- Hoogteschatting %>%
    inner_join(Rmse %>% select(BMS, DOMEIN_ID, rmseD, maxResid),
               by = c("BMS", "DOMEIN_ID"))

  #Rmse en hoogteschatting berekenen voor afgeleid model
  Afgeleidedata <- Data[["Afgeleidedata"]]
  Afgeleidmodel <- Data[["Afgeleidmodel"]]

  AModel <- Afgeleidmodel[[1]]

  RmseVL <- Basismodel %>%
    filter(BMS %in% unique(AModel$BMS)) %>%
    rowwise() %>%
    do(
      rmse.basis(.$Model$data, "Basis", .$BMS)
    ) %>%
    ungroup() %>%
    mutate(
      sseVL = (rmseVL) ^ 2 * (nBomenOmtrek05 - 2)
    ) %>%
    group_by(BMS) %>%
    summarise(
      nBomen = sum(nBomen),
      nBomenInterval = sum(nBomenInterval),
      nBomenOmtrek05VL = sum(nBomenOmtrek05),
      rmseVL = sqrt(sum(sseVL) / (nBomenOmtrek05VL - 2))
    ) %>%
    ungroup()

  Rmse <- AModel %>%
    rowwise() %>%
    do(
      rmse.verschuiving(.$Model, .$BMS, .$DOMEIN_ID)
    ) %>%
    ungroup() %>%
    inner_join(
      RmseVL %>% select(BMS, rmseVL),
      by = c("BMS")
    ) %>%
    mutate(
      rmseD = sqrt(rmseVL ^ 2 + RmseVerschuiving ^ 2)
    )

  Hoogteschatting <- AModel %>%
    inner_join(
      Afgeleidmodel[[2]],
      by = c("BMS", "DOMEIN_ID")
    ) %>%
    group_by(
      BMS,
      DOMEIN_ID
    ) %>%
    do(
      hoogteschatting.afgeleid(.$Model[[1]],
                                select(., -.data$Model))
    ) %>%
    ungroup() %>%
    mutate(
      ResidD2 = (HOOGTE - H_D_finaal) ^ 2
    )

  DatasetAfgeleid <- Hoogteschatting %>%
    select(BMS, DOMEIN_ID, ResidD2) %>%
    filter(!is.na(ResidD2)) %>%
    group_by(BMS, DOMEIN_ID) %>%
    summarise(
      maxResid = max(c(ResidD2))
    ) %>%
    ungroup() %>%
    inner_join(
      Hoogteschatting,
      by = c("BMS", "DOMEIN_ID")
    ) %>%
    inner_join(
      Rmse,
      by = c("BMS", "DOMEIN_ID")
    )

  #data genereren voor lokaal model en berekeningen uitvoeren
  Metingen <- testdataset(200) %>%
    bind_rows(data.frame(BMS = "testboom",
                         IDbms = 1,
                         DOMEIN_ID = "A",
                         BOS_BHI = "Domein_A",
                         C13 = 200,
                         HOOGTE = c(1, 60),
                         Status = "Niet gecontroleerd",
                         ID = as.character(300:301),
                         stringsAsFactors = FALSE))

  Datalijst <- initiatie(Metingen)

  Data.lokaal <- Datalijst[["Lokaal"]]
  Lokaalmodel <- fit.lokaal(Data.lokaal)

  Rmse <- Data.lokaal %>%
    group_by_(
      ~BMS,
      ~DOMEIN_ID
    ) %>%
    do(
      rmse.basis(., "Lokaal", .data$BMS)
    ) %>%
    ungroup()

  Hoogteschatting <- Lokaalmodel %>%
    inner_join(
      Data.lokaal,
      by = c("BMS", "DOMEIN_ID")
    ) %>%
    group_by_(
      ~BMS,
      ~DOMEIN_ID
    ) %>%
    do(
      hoogteschatting.basis(.$Model[[1]],
                             select(., -.data$Model),
                             "Lokaal", .$BMS)
    ) %>%
    ungroup()

  DatasetLokaal <- Hoogteschatting %>%
    inner_join(Rmse %>% select_(~BMS, ~DOMEIN_ID, ~rmseD, ~maxResid),
               by = c("BMS", "DOMEIN_ID"))




  it("De uitvoer van de functie is correct", {
    expect_equal(afwijkendeMetingen(DatasetBasis) %>%
                   colnames(.),
                 c("DOMEIN_ID", "BOS_BHI", "nBomenInterval",
                   "nBomenOmtrek05", "nBomen", "Q5k", "Q95k", "Omtrek",
                   "H_D_finaal", "H_VL_finaal", "IDbms", "C13", "HOOGTE",
                   "Status", "ID", "Rijnr", "logOmtrek", "logOmtrek2", "Q5",
                   "Q95", "BMS", "rmseD", "maxResid", "HogeRmse", "Afwijkend"))
    expect_equal(afwijkendeMetingen(DatasetAfgeleid) %>%
                   colnames(.),
                 c("BMS", "DOMEIN_ID", "maxResid", "BOS_BHI", "nBomenInterval",
                   "nBomenOmtrek05", "nBomen", "Q5k", "Q95k", "Omtrek",
                   "H_VL_finaal", "IDbms", "C13", "HOOGTE", "Status", "ID",
                   "Rijnr", "logOmtrek", "logOmtrek2", "Q5", "Q95",
                   "H_D_finaal", "ResidD2", "nBomenModel", "RmseVerschuiving",
                   "rmseVL", "rmseD", "HogeRmse", "Afwijkend")
    )
    expect_equal(afwijkendeMetingen(DatasetLokaal) %>%
                   colnames(.),
                 c("DOMEIN_ID", "BOS_BHI", "nBomenInterval", "nBomenOmtrek05",
                   "nBomen", "Q5k", "Q95k", "Omtrek", "H_D_finaal", "IDbms",
                   "C13", "HOOGTE", "Status", "ID", "Rijnr", "logOmtrek",
                   "logOmtrek2", "Q5", "Q95", "BMS", "rmseD", "maxResid",
                   "HogeRmse", "Afwijkend")
    )
  })

  it("De afwijkende metingen worden correct geselecteerd", {
    expect_equal(afwijkendeMetingen(DatasetBasis, 0) %>%
                   select(DOMEIN_ID, BMS, C13, HOOGTE, Afwijkend),
                 tibble(DOMEIN_ID =
                          LETTERS[rep(1:6, each = 2)],
                        BMS = "testboom",
                        C13 = 200,
                        HOOGTE = rep(c(1, 60), 6),
                        Afwijkend = TRUE
                 )
    )
    expect_equal(afwijkendeMetingen(DatasetAfgeleid, 0) %>%
                   select(DOMEIN_ID, BMS, C13, HOOGTE),
                 tibble(DOMEIN_ID = "Klein",
                        BMS = "testboom",
                        C13 = 200,
                        HOOGTE = c(1, 60)
                 )
    )
    expect_equal(afwijkendeMetingen(DatasetLokaal, 0) %>%
                   select(DOMEIN_ID, BMS, C13, HOOGTE),
                 tibble(DOMEIN_ID = "A",
                        BMS = "testboom",
                        C13 = 200,
                        HOOGTE = c(1, 60)
                 )
    )
  })

  Metingen <- testdataset(rep(200, 10))

  Datalijst <- initiatie(Metingen)

  Data.basis <- Datalijst[["Basis"]]
  Basismodel <- fit.basis(Data.basis)

  Rmse <- Basismodel %>%
    rowwise() %>%
    do(
      rmse.basis(.$Model$data, "Basis", .$BMS)
    ) %>%
    ungroup()

  Hoogteschatting <- Basismodel %>%
    rowwise() %>%
    do(
      hoogteschatting.basis(.$Model, .$Model$data, "Basis", .$BMS)
    ) %>%
    ungroup()

  Dataset <- Hoogteschatting %>%
    inner_join(Rmse %>% select_(~BMS, ~DOMEIN_ID, ~rmseD, ~maxResid),
               by = c("BMS", "DOMEIN_ID"))

  it("Selectie AantalDomHogeRMSE werkt correct", {
    expect_error(
      afwijkendeMetingen(Dataset, -1),
      "AantalDomHogeRMSE moet een positief geheel getal zijn."
    )
    expect_equal((afwijkendeMetingen(Dataset, 2) %>%
                   filter(HogeRmse) %>%
                   select(DOMEIN_ID) %>%
                   distinct() %>%
                   summarise(n = n()))$n,
                 2
    )
    expect_equal((afwijkendeMetingen(Dataset, 5) %>%
                    filter(HogeRmse) %>%
                    select(DOMEIN_ID) %>%
                    distinct() %>%
                    summarise(n = n()))$n,
                 5
    )
    expect_equal((afwijkendeMetingen(Dataset, 8) %>%
                    filter(HogeRmse) %>%
                    select(DOMEIN_ID) %>%
                    distinct() %>%
                    summarise(n = n()))$n,
                 8
    )
  })

  setwd(wd)

})
