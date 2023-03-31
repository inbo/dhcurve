context("test afwijkendeCurves")

describe("afwijkendecurves", {

  wd <- getwd()

  test_wd <- tempdir()

  setwd(test_wd)

  library(dplyr)
  library(tibble)

  Data <- dataAfwijkendeCurve()

  Basismodel <- Data[["Basismodel"]]
  Lokaledata <- Data[["Lokaledata"]]
  Lokaalmodel <- Data[["Lokaalmodel"]]

  HMB <- curvekarakteristieken(Basismodel) %>%
    filter(DOMEIN_ID == "HM") %>%
    select(Ad, Bd, Cd)

  LMB <- curvekarakteristieken(Basismodel) %>%
    filter(DOMEIN_ID == "LM") %>%
    select(Ad, Bd, Cd)

  HML <- curvekarakteristieken(Lokaalmodel, Lokaledata) %>%
    filter(DOMEIN_ID == "HM") %>%
    select(Ad, Bd, Cd)

  LML <- curvekarakteristieken(Lokaalmodel, Lokaledata) %>%
    filter(DOMEIN_ID == "LM") %>%
    select(Ad, Bd, Cd)


  it("functie afwijkendecurves() geeft geen warnings", {
    expect_no_warning(afwijkendeCurves(Basismodel))
    expect_no_warning(afwijkendeCurves(Lokaalmodel, Lokaledata))
  })

  it("De uitvoer van de functie is correct", {
    expect_equal(afwijkendeCurves(Basismodel) %>%
                   as_tibble(),
                 tibble(DOMEIN_ID = c("HM", "LM"),
                        BMS = "testboom",
                        Omtrek_Buigpunt.d =
                          c(exp(1 - HMB$Bd / (2 * HMB$Cd)), NA),
                        Reden = c("curvevorm hol bij lage omtrekklassen",
                                  "curve daalt terug bij hoge omtrekklassen"),
                        Omtrek_Extr_Hoogte.d =
                          c(NA, exp(-LMB$Bd / (2 * LMB$Cd)))
        )
    )
    expect_equal(afwijkendeCurves(Lokaalmodel, Lokaledata),
                 tibble(DOMEIN_ID = c("HM", "LM"),
                        BMS = "andereboom",
                        Omtrek_Buigpunt.d =
                          c(exp(1 - HML$Bd / (2 * HML$Cd)), NA),
                        Reden = c("curvevorm hol bij lage omtrekklassen",
                                  "curve daalt terug bij hoge omtrekklassen"),
                        Omtrek_Extr_Hoogte.d =
                          c(NA, exp(-LML$Bd / (2 * LML$Cd)))
                 )

    )
    expect_error(
      afwijkendeCurves(Lokaalmodel),
      "Bij opgave van een lokaal model moet je ook de dataset meegeven"
    )
  })

  setwd(wd)

})
