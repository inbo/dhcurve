context("test initiatie")

describe("initiatie", {

  wd <- getwd()
  test_wd <- tempdir()

  setwd(test_wd)

  library(dplyr)

  Dataset <- data.frame(DOMEIN_ID = c(rep("Bos1", 155),
                                      rep("Bos2", 110),
                                      rep("Bos3", 55),
                                      rep("Bos4", 55),
                                      rep("Bos5", 55),
                                      rep("Bos6", 100),
                                      rep("BosKlein", 15)),
                        BOS_BHI = c(rep("Bos1", 155),
                                    rep("Bos2", 110),
                                    rep("Bos3", 55),
                                    rep("Bos4", 55),
                                    rep("Bos5", 55),
                                    rep("Bos6", 100),
                                    rep("BosKlein", 15)),
                        BMS = c(rep("SoortModel", 55),
                                rep("SoortExtra", 55),
                                rep("SoortNiet", 45),
                                rep("SoortModel", 55),
                                rep("SoortExtra", 55),
                                rep("SoortModel", 55),
                                rep("SoortModel", 55),
                                rep("SoortModel", 55),
                                rep("SoortModel", 55),
                                rep("SoortExtra", 45),
                                rep("SoortModel", 15)),
                        IDbms = c(rep(1, 55),
                                rep(2, 55),
                                rep(3, 45),
                                rep(1, 55),
                                rep(2, 55),
                                rep(1, 55),
                                rep(1, 55),
                                rep(1, 55),
                                rep(1, 55),
                                rep(2, 45),
                                rep(1, 15)),
                        C13 = 100,
                        HOOGTE = 20,
                        Status = "Te controleren",
                        stringsAsFactors = FALSE)

  Resultaat1 <- data.frame(nBomenInterval = 55,
                           nBomenIntervalOmtrek05 = 55)
  Resultaat2 <- data.frame(C13 = 100,
                           HOOGTE = 20,
                           Status = "Te controleren",
                           Omtrek = 1.05,
                           stringsAsFactors = FALSE)
  Resultaat3 <- data.frame(logOmtrek = 0.048790164169,
                       logOmtrek2 = 0.00238048011968,
                       nBomen = 55,
                       nBomenOmtrek05 = 55,
                       Q5 = 0.95,
                       Q5k = 0.95,
                       Q95 = 1.15,
                       Q95k = 1.15)
  Resultaat3rest <- data.frame(logOmtrek = 0.048790164169,
                           logOmtrek2 = 0.00238048011968,
                           nBomen = 45,
                           nBomenOmtrek05 = 45,
                           Q5 = 0.95,
                           Q5k = 0.95,
                           Q95 = 1.15,
                           Q95k = 1.15)
  Resultaat3uitz <- data.frame(logOmtrek = 0.048790164169,
                               logOmtrek2 = 0.00238048011968,
                               nBomen = c(rep(45, 45),
                                          rep(55, 55),
                                          rep(45, 45),
                                          rep(15, 15)),
                               nBomenOmtrek05 = c(rep(45, 45),
                                                     rep(55, 55),
                                                     rep(45, 45),
                                                     rep(15, 15)),
                               Q5 = 0.95,
                               Q5k = 0.95,
                               Q95 = 1.15,
                               Q95k = 1.15)


  it("initiatie() geeft geen warnings", {
    expect_no_warning(initiatie(Dataset))
  })

  it("Dataset wordt correct opgedeeld", {
    expect_is(
      Output <- initiatie(Dataset),
      "list"
    )
    for (i in c("Basis", "Afgeleid", "Lokaal", "Rest")) {
      attr(Output[[i]]$Q5, "names") <- NULL
      attr(Output[[i]]$Q5k, "names") <- NULL
      attr(Output[[i]]$Q95, "names") <- NULL
      attr(Output[[i]]$Q95k, "names") <- NULL
    }
    expect_equal(as.data.frame(Output[["Basis"]]),
                 data.frame(BMS = "SoortModel",
                            DOMEIN_ID = c(rep("Bos1", 55),
                                          rep("Bos2", 55),
                                          rep("Bos3", 55),
                                          rep("Bos4", 55),
                                          rep("Bos5", 55),
                                          rep("Bos6", 55)),
                            Resultaat1,
                            BOS_BHI = c(rep("Bos1", 55),
                                        rep("Bos2", 55),
                                        rep("Bos3", 55),
                                        rep("Bos4", 55),
                                        rep("Bos5", 55),
                                        rep("Bos6", 55)),
                            IDbms = 1,
                            Resultaat2,
                            Rijnr = c(1:55, 156:210, 266:485),
                            Resultaat3,
                            VoorModelFit = TRUE,
                            nExtra = 0,
                            stringsAsFactors = FALSE))
    expect_equal(as.data.frame(Output[["Afgeleid"]]),
                 data.frame(BMS = "SoortModel",
                            DOMEIN_ID = c(rep("BosKlein", 15)),
                            nBomenInterval = 15,
                            nBomenIntervalOmtrek05 = 15,
                            BOS_BHI = c(rep("BosKlein", 15)),
                            IDbms = 1,
                            C13 = 100,
                            HOOGTE = 20,
                            Status = "Te controleren",
                            Omtrek = 1.05,
                            Rijnr = c(531:545),
                            logOmtrek = 0.048790164169,
                            logOmtrek2 = 0.00238048011968,
                            nBomen = 15,
                            nBomenOmtrek05 = 15,
                            Q5 = 0.95,
                            Q5k = 0.95,
                            Q95 = 1.15,
                            Q95k = 1.15,
                            stringsAsFactors = FALSE))
    expect_equal(as.data.frame(Output[["Lokaal"]]),
                 data.frame(BMS = "SoortExtra",
                            DOMEIN_ID = c(rep("Bos1", 55),
                                          rep("Bos2", 55)),
                            Resultaat1,
                            BOS_BHI = c(rep("Bos1", 55),
                                        rep("Bos2", 55)),
                            IDbms = 2,
                            Resultaat2,
                            Rijnr = c(56:110, 211:265),
                            Resultaat3,
                            VoorModelFit = TRUE,
                            nExtra = 0,
                            stringsAsFactors = FALSE))
    expect_equal(as.data.frame(Output[["Rest"]]) %>%
                   arrange(Rijnr),
                 data.frame(BMS = c(rep("SoortNiet", 45),
                                    rep("SoortExtra", 45)),
                            DOMEIN_ID = c(rep("Bos1", 45),
                                          rep("Bos6", 45)),
                            nBomenInterval = 45,
                            nBomenIntervalOmtrek05 = 45,
                            BOS_BHI = c(rep("Bos1", 45),
                                        rep("Bos6", 45)),
                            IDbms = c(rep(3, 45),
                                      rep(2, 45)),
                            Resultaat2,
                            Rijnr = c(111:155, 486:530),
                            Resultaat3rest,
                            stringsAsFactors = FALSE) %>%
                   arrange(Rijnr))
  })

  it("Uitzonderingen worden correct afgehandeld", {
    expect_is(
      Output <- initiatie(Dataset,
                          data.frame(DOMEIN_ID = c("Bos1", "BosKlein"),
                                     BMS = c("SoortExtra", "SoortModel"),
                                     min_basis = c(65, NA),
                                     min_afgeleid = c(NA, 25),
                                     stringsAsFactors = FALSE)),
      "list"
    )
    for (i in c("Basis", "Afgeleid", "Lokaal", "Rest")) {
      attr(Output[[i]]$Q5, "names") <- NULL
      attr(Output[[i]]$Q5k, "names") <- NULL
      attr(Output[[i]]$Q95, "names") <- NULL
      attr(Output[[i]]$Q95k, "names") <- NULL
    }
    expect_equal(as.data.frame(Output[["Basis"]]),
                 data.frame(BMS = "SoortModel",
                            DOMEIN_ID = c(rep("Bos1", 55),
                                          rep("Bos2", 55),
                                          rep("Bos3", 55),
                                          rep("Bos4", 55),
                                          rep("Bos5", 55),
                                          rep("Bos6", 55)),
                            Resultaat1,
                            BOS_BHI = c(rep("Bos1", 55),
                                        rep("Bos2", 55),
                                        rep("Bos3", 55),
                                        rep("Bos4", 55),
                                        rep("Bos5", 55),
                                        rep("Bos6", 55)),
                            IDbms = 1,
                            Resultaat2,
                            Rijnr = c(1:55, 156:210, 266:485),
                            Resultaat3,
                            VoorModelFit = TRUE,
                            nExtra = 0,
                            stringsAsFactors = FALSE))
    expect_equal(as.data.frame(Output[["Afgeleid"]]),
                 data.frame(BMS = character(),
                            DOMEIN_ID = character(),
                            nBomenInterval = integer(),
                            nBomenIntervalOmtrek05 = integer(),
                            BOS_BHI = character(),
                            IDbms = double(),
                            C13 = double(),
                            HOOGTE = double(),
                            Status = character(),
                            Omtrek = double(),
                            Rijnr = integer(),
                            logOmtrek = double(),
                            logOmtrek2 = double(),
                            nBomen = integer(),
                            nBomenOmtrek05 = integer(),
                            Q5 = double(),
                            Q5k = double(),
                            Q95 = double(),
                            Q95k = double(),
                            stringsAsFactors = FALSE))
    expect_equal(as.data.frame(Output[["Lokaal"]]),
                 data.frame(BMS = "SoortExtra",
                            DOMEIN_ID = c(rep("Bos2", 55)),
                            Resultaat1,
                            BOS_BHI = c(rep("Bos2", 55)),
                            IDbms = 2,
                            Resultaat2,
                            Rijnr = c(211:265),
                            Resultaat3,
                            VoorModelFit = TRUE,
                            nExtra = 0,
                            stringsAsFactors = FALSE))
    expect_equal(as.data.frame(Output[["Rest"]]) %>%
                   arrange(Rijnr),
                 data.frame(BMS = c(rep("SoortNiet", 45),
                                    rep("SoortExtra", 100),
                                    rep("SoortModel", 15)),
                            DOMEIN_ID = c(rep("Bos1", 100),
                                          rep("Bos6", 45),
                                          rep("BosKlein", 15)),
                            nBomenInterval = c(rep(45, 45),
                                               rep(55, 55),
                                               rep(45, 45),
                                               rep(15, 15)),
                            nBomenIntervalOmtrek05 = c(rep(45, 45),
                                               rep(55, 55),
                                               rep(45, 45),
                                               rep(15, 15)),
                            BOS_BHI = c(rep("Bos1", 100),
                                        rep("Bos6", 45),
                                        rep("BosKlein", 15)),
                            IDbms = c(rep(3, 45),
                                      rep(2, 100),
                                      rep(1, 15)),
                            Resultaat2,
                            Rijnr = c(111:155, 56:110, 486:530, 531:545),
                            Resultaat3uitz,
                            stringsAsFactors = FALSE) %>%
                   arrange(Rijnr))
  })

  Dataset$Status <-
    ifelse(Dataset$DOMEIN_ID == "BosKlein", "Goedgekeurd", "Niet gecontroleerd")

  Resultaat2status <- data.frame(C13 = 100,
                           HOOGTE = 20,
                           Status = "Niet gecontroleerd",
                           Omtrek = 1.05,
                           stringsAsFactors = FALSE)

  it("Dataset mag enkel goedgekeurde gegevens bevatten", {
    expect_is(
      Output <- initiatie(Dataset),
      "list"
    )
    for (i in c("Basis", "Afgeleid", "Lokaal", "Rest")) {
      attr(Output[[i]]$Q5, "names") <- NULL
      attr(Output[[i]]$Q5k, "names") <- NULL
      attr(Output[[i]]$Q95, "names") <- NULL
      attr(Output[[i]]$Q95k, "names") <- NULL
    }
    expect_equal(as.data.frame(Output[["Basis"]]),
                 data.frame(BMS = "SoortModel",
                            DOMEIN_ID = c(rep("Bos1", 55),
                                          rep("Bos2", 55),
                                          rep("Bos3", 55),
                                          rep("Bos4", 55),
                                          rep("Bos5", 55),
                                          rep("Bos6", 55)),
                            Resultaat1,
                            BOS_BHI = c(rep("Bos1", 55),
                                        rep("Bos2", 55),
                                        rep("Bos3", 55),
                                        rep("Bos4", 55),
                                        rep("Bos5", 55),
                                        rep("Bos6", 55)),
                            IDbms = 1,
                            Resultaat2status,
                            Rijnr = c(1:55, 156:210, 266:485),
                            Resultaat3,
                            VoorModelFit = TRUE,
                            nExtra = 0,
                            stringsAsFactors = FALSE))
    expect_equal(as.data.frame(Output[["Afgeleid"]]),
                 data.frame(BMS = "SoortModel",
                            DOMEIN_ID = c(rep("BosKlein", 15)),
                            nBomenInterval = 15,
                            nBomenIntervalOmtrek05 = 15,
                            BOS_BHI = c(rep("BosKlein", 15)),
                            IDbms = 1,
                            C13 = 100,
                            HOOGTE = 20,
                            Status = "Goedgekeurd",
                            Omtrek = 1.05,
                            Rijnr = c(531:545),
                            logOmtrek = 0.048790164169,
                            logOmtrek2 = 0.00238048011968,
                            nBomen = 15,
                            nBomenOmtrek05 = 15,
                            Q5 = 0.95,
                            Q5k = 0.95,
                            Q95 = 1.15,
                            Q95k = 1.15,
                            stringsAsFactors = FALSE))
    expect_equal(as.data.frame(Output[["Lokaal"]]),
                 data.frame(BMS = "SoortExtra",
                            DOMEIN_ID = c(rep("Bos1", 55),
                                          rep("Bos2", 55)),
                            Resultaat1,
                            BOS_BHI = c(rep("Bos1", 55),
                                        rep("Bos2", 55)),
                            IDbms = 2,
                            Resultaat2status,
                            Rijnr = c(56:110, 211:265),
                            Resultaat3,
                            VoorModelFit = TRUE,
                            nExtra = 0,
                            stringsAsFactors = FALSE))
    expect_equal(as.data.frame(Output[["Rest"]]) %>%
                   arrange(Rijnr),
                 data.frame(BMS = c(rep("SoortExtra", 45),
                                    rep("SoortNiet", 45)),
                            DOMEIN_ID = c(rep("Bos6", 45),
                                          rep("Bos1", 45)),
                            nBomenInterval = 45,
                            nBomenIntervalOmtrek05 = 45,
                            BOS_BHI = c(rep("Bos6", 45),
                                        rep("Bos1", 45)),
                            IDbms = c(rep(2, 45),
                                      rep(3, 45)),
                            Resultaat2status,
                            Rijnr = c(486:530, 111:155),
                            Resultaat3rest,
                            stringsAsFactors = FALSE) %>%
                   arrange(Rijnr))
  })

  it("De bestandsnaam moet eindigen op .html", {
    expect_error(initiatie(Dataset, Bestandsnaam = "Fout.htlm"),
                 "De bestandnaam moet eindigen op '.html'")
  })

  it("Dataset mag geen afgekeurde gegevens bevatten", {
    Dataset$Status <-
      ifelse(Dataset$DOMEIN_ID == "BosKlein", "Afgekeurd", "Niet gecontroleerd")
    expect_error(initiatie(Dataset),
                 "De kolom Status in de dataframe heeft niet voor alle records een geldige waarde.  Zorg dat enkel de waarden 'Niet gecontroleerd', 'Te controleren' en 'Goedgekeurd' voorkomen.") #nolint: line_length_linter
  })

  it("Hoge omtrekklassen worden correct afgehandeld (uitbreidingen)", {
    Dataset <- Dataset %>%
      bind_rows(
        data.frame(
          DOMEIN_ID = c(rep("Bos1", 15), rep("Bos2", 5), rep("BosKlein", 15),
                        rep("Bos1", 15), rep("Bos2", 5)),
          BOS_BHI = c(rep("Bos1", 15), rep("Bos2", 5), rep("BosKlein", 15),
                      rep("Bos1", 15), rep("Bos2", 5)),
          BMS = c(rep("SoortModel", 35), rep("SoortExtra", 20)),
          IDbms = c(rep(1, 35), rep(2, 20)),
          C13 = 210,
          HOOGTE = 30,
          Status = "Te controleren"
        )
      ) %>%
      bind_rows(
        data.frame(
          DOMEIN_ID = c(rep("Bos1", 15), rep("Bos2", 5), rep("BosKlein", 15),
                        rep("Bos1", 15), rep("Bos2", 5)),
          BOS_BHI = c(rep("Bos1", 15), rep("Bos2", 5), rep("BosKlein", 15),
                      rep("Bos1", 15), rep("Bos2", 5)),
          BMS = c(rep("SoortModel", 35), rep("SoortExtra", 20)),
          IDbms = c(rep(1, 35), rep(2, 20)),
          C13 = 270,
          HOOGTE = 40,
          Status = "Te controleren"
        )
      )
    Output <- initiatie(Dataset)
    expect_equal(
      Output[["Basis"]] %>%
        filter(!VoorModelFit) %>%
        count(DOMEIN_ID, nExtra),
      tibble(DOMEIN_ID = "Bos1", nExtra = 15, n = 15)
    )
    expect_equal(
      Output[["Afgeleid"]] %>%
        filter(C13 == 270) %>%
        count(DOMEIN_ID),
      tibble(DOMEIN_ID = "BosKlein", n = 15)
    )
    expect_equal(
      Output[["Lokaal"]] %>%
        filter((!VoorModelFit)) %>%
        count(DOMEIN_ID, nExtra),
      tibble(DOMEIN_ID = "Bos1", nExtra = 15, n = 15)
    )
    Uitzonderingen <-
      data.frame(DOMEIN_ID = c("Bos1", "Bos1"),
                 BMS = c("SoortModel", "SoortExtra"),
                 min_basis = NA,
                 min_afgeleid = NA,
                 min_uitbreiden_model = 14,
                 stringsAsFactors = FALSE)
    Output <- initiatie(Dataset, Uitzonderingen)
    expect_equal(
      Output[["Basis"]] %>%
        filter(!VoorModelFit) %>%
        count(DOMEIN_ID, nExtra),
      tibble(DOMEIN_ID = "Bos1", nExtra = 15, n = 15)
    )
    expect_equal(
      Output[["Afgeleid"]] %>%
        filter(C13 == 270) %>%
        count(DOMEIN_ID),
      tibble(DOMEIN_ID = "BosKlein", n = 15)
    )
    expect_equal(
      Output[["Lokaal"]] %>%
        filter((!VoorModelFit)) %>%
        count(DOMEIN_ID, nExtra),
      tibble(DOMEIN_ID = "Bos1", nExtra = 15, n = 15)
    )
    Uitzonderingen <- Uitzonderingen %>%
      bind_rows(
        data.frame(DOMEIN_ID = "BosKlein", BMS = "SoortModel",
                   stringsAsFactors = FALSE)
      ) %>%
      mutate(
        min_uitbreiden_model = 16
      )
    expect_warning(
      Output <- initiatie(Dataset, Uitzonderingen),
      "min_uitbreiden_model opgegeven voor afgeleid model, dit zal genegeerd worden \\(bij afgeleide modellen worden alle gegevens meegenomen\\)" #nolint: line_length_linter
    )
    expect_equal(
      nrow(Output[["Basis"]] %>%
             filter(!VoorModelFit)),
      0
    )
    expect_equal(
      Output[["Afgeleid"]] %>%
        filter(C13 == 270) %>%
        count(DOMEIN_ID),
      tibble(DOMEIN_ID = "BosKlein", n = 15)
    )
    expect_equal(
      nrow(Output[["Lokaal"]] %>%
             filter(!VoorModelFit)),
      0
    )
  })

  it("Hoge/lage omtrekklassen worden correct verwijderd", {
    Dataset2 <- Dataset %>%
      bind_rows(
        data.frame(
          DOMEIN_ID = c(rep("Bos1", 2), rep("BosKlein", 2), rep("Bos1", 2)),
          BOS_BHI = c(rep("Bos1", 2), rep("BosKlein", 2), rep("Bos1", 2)),
          BMS = c(rep("SoortModel", 4), rep("SoortExtra", 2)),
          IDbms = c(rep(1, 4), rep(2, 2)),
          C13 = c(48, 305, 48, 285, 48, 305),
          HOOGTE = c(10, 40),
          Status = "Te controleren"
        )
      )
    Output <- initiatie(Dataset)
    expect_message(
      Output2 <- initiatie(Dataset2),
      regexp = "Het rapport is opgeslagen in de working directory"
    )
    expect_equal(
      Output[["Basis"]] %>%
        select(-"nBomen"),
      Output2[["Basis"]] %>%
        select(-"nBomen")
    )
    expect_equal(
      Output[["Afgeleid"]] %>%
        select(-"nBomen", -"nBomenInterval", -"Q5", -"Q5k"),
      Output2[["Afgeleid"]] %>%
        select(-"nBomen", -"nBomenInterval", -"Q5", -"Q5k")
    )
    expect_equal(
      Output[["Lokaal"]] %>%
        select(-"nBomen"),
      Output2[["Lokaal"]] %>%
        select(-"nBomen")
    )
  })

  setwd(wd)

})
