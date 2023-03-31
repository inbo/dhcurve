context("test resultaat")

describe("resultaat", {

  set.seed(35611368)

  wd <- getwd()

  test_wd <- tempdir()

  setwd(test_wd)

  library(dplyr)
  library(tibble)

  Data <- dataAfwijkendeCurve()

  Basismodel1 <- Data[["Basismodel"]]
  Lokaledata <- Data[["Lokaledata"]]
  Lokaalmodel <- Data[["Lokaalmodel"]]

  it("De parameters worden correct berekend voor domeinen van het Basismodel", {
    resultaat <- resultaat(Basismodel1) %>%
      filter(DOMEIN_ID %in% c("HM", "LM")) %>%
      select(
        -nBomenOmtrek05, -nBomenInterval, -nBomenIntervalOmtrek05, -nExtra
      ) %>%
      as.data.frame(., stringsAsFactors = FALSE)
    attr(resultaat$Q5k, "names") <- NULL
    attr(resultaat$Q95k, "names") <- NULL
    attr(resultaat, "groups") <- NULL
    expect_equal(resultaat,
                 data.frame(BMS = "testboom",
                            DOMEIN_ID = c("HM", "LM"),
                            A = 20,
                            B = c(7, 5),
                            C = c(4, -6),
                            nBomen = 200,
                            Q5k = 0.25,
                            Q95k = 2.35,
                            RMSE = 3,
                            Modeltype = "basismodel",
                            stringsAsFactors = FALSE),
                 tolerance = 1)
  })

  it("De parameters worden correct berekend voor het Lokaal model", {
    expect_error(resultaat(Lokaalmodel))
    expect_error(resultaat(Lokaalmodel = Lokaalmodel))
    resultaat <-
      resultaat(Lokaalmodel = Lokaalmodel, Data.lokaal = Lokaledata) %>%
      select(
        -nBomenOmtrek05, -nBomenInterval, -nBomenIntervalOmtrek05, -nExtra
      ) %>%
      as.data.frame(., stringsAsFactors = FALSE)
    attr(resultaat$Q5k, "names") <- NULL
    attr(resultaat$Q95k, "names") <- NULL
    expect_equal(resultaat,
                 data.frame(DOMEIN_ID = c("HM", "LM"),
                            BMS = "andereboom",
                            A = 20,
                            B = c(7, 5),
                            C = c(4, -6),
                            nBomen = 200,
                            Q5k = 0.25,
                            Q95k = 2.35,
                            RMSE = 3,
                            Modeltype = "lokaal model",
                            stringsAsFactors = FALSE),
                 tolerance = 1)
  })

  Data <- dataAfgeleidmodel()

  Basismodel2 <- Data[["Basismodel"]]
  Afgeleidmodel <- Data[["Afgeleidmodel"]]


  it("functie resultaat() geeft geen warnings", {
    expect_no_warning(resultaat(Basismodel2, Afgeleidmodel,
                                Lokaalmodel, Lokaledata,
                                Data.onbruikbaar =
                                  data.frame(
                                    BMS = "restboom",
                                    DOMEIN_ID = "A",
                                    nBomenOmtrek05 = 0,
                                    nBomenInterval = 1,
                                    nBomenIntervalOmtrek05 = 0,
                                    IDbms = 20,
                                    BOS_BHI = "DOMEIN_A",
                                    C13 = 41,
                                    HOOGTE = 18,
                                    Status = "Niet gecontroleerd",
                                    ID = 3000,
                                    Omtrek = 0.45,
                                    logOmtrek = log(0.45),
                                    logOmtrek2 = log(0.45) ^ 2,
                                    nBomen = 5,
                                    Q5k = 0.25,
                                    Q95k = 0.45,
                                    nExtra = 1,
                                    stringsAsFactors = FALSE
                                  )))
  })

  it("De parameters worden correct berekend voor Afgeleid model", {
    expect_error(resultaat(Afgeleidmodel))
    expect_error(resultaat(Afgeleidmodel = Afgeleidmodel))
    resultaat <- resultaat(Basismodel2, Afgeleidmodel = Afgeleidmodel) %>%
      filter(Modeltype == "afgeleid model") %>%
      select(
        -nBomenOmtrek05, -nBomenInterval, -nBomenIntervalOmtrek05, -RMSE
      ) %>%
      as.data.frame(., stringsAsFactors = FALSE)
    attr(resultaat$Q5k, "names") <- NULL
    attr(resultaat$Q95k, "names") <- NULL
    attr(resultaat, "groups") <- NULL
    expect_equal(resultaat,
                 data.frame(BMS = "testboom",
                            DOMEIN_ID = "Klein",
                            A = 15,
                            B = 15,
                            C = 1,
                            nBomen = 40,
                            Q5k = 0.55,
                            Q95k = 2.35,
                            Modeltype = "afgeleid model",
                            nExtra = NA_integer_,
                            stringsAsFactors = FALSE),
                 tolerance = 1)
  })

  it("De dataset wordt correct samengesteld", {
    expect_message(resultaat())
    expect_equal(resultaat(Basismodel2, Afgeleidmodel,
                           Lokaalmodel, Lokaledata,
                           Data.onbruikbaar =
                             data.frame(
                               BMS = "restboom",
                               DOMEIN_ID = "A",
                               nBomenOmtrek05 = 0,
                               nBomenInterval = 1,
                               nBomenIntervalOmtrek05 = 0,
                               IDbms = 20,
                               BOS_BHI = "DOMEIN_A",
                               C13 = 41,
                               HOOGTE = 18,
                               Status = "Niet gecontroleerd",
                               ID = 3000,
                               Omtrek = 0.45,
                               logOmtrek = log(0.45),
                               logOmtrek2 = log(0.45) ^ 2,
                               nBomen = 5,
                               Q5k = 0.25,
                               Q95k = 0.45,
                               stringsAsFactors = FALSE
                             )) %>%
                   select(BMS, DOMEIN_ID, Modeltype) %>%
                   distinct() %>%
                   as_tibble(),
                 tibble(BMS = c(rep("testboom", 7), rep("andereboom", 2),
                                "restboom"),
                        DOMEIN_ID = c(LETTERS[1:6], "Klein", "HM", "LM", "A"),
                        Modeltype = c(rep("basismodel", 6), "afgeleid model",
                                      rep("lokaal model", 2), "Geen model")))
    expect_equal(resultaat(Basismodel2, Afgeleidmodel,
                           Lokaalmodel, Lokaledata,
                           Data.onbruikbaar =
                             data.frame(
                               BMS = "restboom",
                               DOMEIN_ID = "A",
                               nBomenOmtrek05 = 0,
                               nBomenInterval = 1,
                               nBomenIntervalOmtrek05 = 0,
                               IDbms = 20,
                               BOS_BHI = "DOMEIN_A",
                               C13 = 41,
                               HOOGTE = 18,
                               Status = "Niet gecontroleerd",
                               ID = 3000,
                               Omtrek = 0.45,
                               logOmtrek = log(0.45),
                               logOmtrek2 = log(0.45) ^ 2,
                               nBomen = 5,
                               Q5k = 0.25,
                               Q95k = 0.45,
                               stringsAsFactors = FALSE
                             )) %>%
                   filter(BMS == "restboom") %>%
                   select(BMS, DOMEIN_ID, A, B, C) %>%
                   as_tibble(),
                 tibble(BMS = "restboom",
                        DOMEIN_ID = "A",
                        A = as.double(NA), B = as.double(NA), C = as.double(NA))
                 )
    expect_equal(resultaat(Data.onbruikbaar =
                             data.frame(
                               BMS = "restboom",
                               DOMEIN_ID = "A",
                               nBomenOmtrek05 = 0,
                               nBomenInterval = 1,
                               nBomenIntervalOmtrek05 = 0,
                               IDbms = 20,
                               BOS_BHI = "DOMEIN_A",
                               C13 = 41,
                               HOOGTE = 18,
                               Status = "Niet gecontroleerd",
                               ID = 3000,
                               Omtrek = 0.45,
                               logOmtrek = log(0.45),
                               logOmtrek2 = log(0.45) ^ 2,
                               nBomen = 5,
                               Q5k = 0.25,
                               Q95k = 0.45,
                               stringsAsFactors = FALSE
                             )) %>%
                   select(BMS, DOMEIN_ID),
                 data.frame(BMS = "restboom", DOMEIN_ID = "A",
                            stringsAsFactors = FALSE))
  })

  setwd(wd)

})
