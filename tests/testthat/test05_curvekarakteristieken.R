context("test curvekarakteristieken")

describe("curvekarakteristieken", {

  wd <- getwd()

  test_wd <- tempdir()

  setwd(test_wd)

  library(dplyr)
  library(tibble)

  Data <- dataAfwijkendeCurve()

  Basismodel <- Data[["Basismodel"]]
  Lokaledata <- Data[["Lokaledata"]]
  Lokaalmodel <- Data[["Lokaalmodel"]]


  it("Niet afwijkende curves vertonen geen extremen in relevant interval", {
    expect_equal(curvekarakteristieken(Basismodel) %>%
                   filter(!(Omtrek_Extr_Hoogte.d > 0.1 &
                            Omtrek_Extr_Hoogte.d < 2.4)) %>%
                   select(DOMEIN_ID) %>%
                   as_tibble(),
                 tibble(DOMEIN_ID = LETTERS[1:6]))
  })

  it("Afwijkende curves vertonen wel extreem bij omtrek tussen 0.1 en 2.4 m", {
    expect_equal(curvekarakteristieken(Basismodel) %>%
                   filter(Omtrek_Extr_Hoogte.d > 0.1 &
                            Omtrek_Extr_Hoogte.d < 2.4) %>%
                   select(DOMEIN_ID) %>%
                   as_tibble(),
                 tibble(DOMEIN_ID = c("HM", "LM")))
    expect_equal(curvekarakteristieken(Lokaalmodel, Lokaledata) %>%
                   filter(Omtrek_Extr_Hoogte.d > 0.2) %>%
                   select(DOMEIN_ID) %>%
                   as_tibble(),
                 tibble(DOMEIN_ID = c("HM", "LM")))
  })

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

  it("De relevante variabelen voor hoog maximum worden correct berekend", {
    resultaat <- curvekarakteristieken(Basismodel) %>%
        filter(DOMEIN_ID == "HM") %>%
        select(DOMEIN_ID,
               Omtrek_Extr_Hoogte.d,
               Extr_Hoogte.d,
               Hoogteverschil.d) %>%
        as_tibble()
    attr(resultaat$Hoogteverschil.d, "names") <- NULL
    expect_equal(
      resultaat,
      tibble(DOMEIN_ID = "HM",
               Omtrek_Extr_Hoogte.d = exp(-HMB$Bd / (2 * HMB$Cd)),
               Extr_Hoogte.d =
                 HMB$Ad + HMB$Bd * log(Omtrek_Extr_Hoogte.d) +
                 HMB$Cd * (log(Omtrek_Extr_Hoogte.d)) ^ 2,
               Hoogteverschil.d =
                 Extr_Hoogte.d -
                 (HMB$Ad + HMB$Bd * log(2.35) + HMB$Cd * (log(2.35)) ^ 2)))
    resultaat <- curvekarakteristieken(Lokaalmodel, Lokaledata) %>%
      filter(DOMEIN_ID == "HM") %>%
      select(DOMEIN_ID,
              Omtrek_Extr_Hoogte.d,
              Extr_Hoogte.d,
              Hoogteverschil.d)
    attr(resultaat$Hoogteverschil.d, "names") <- NULL
    expect_equal(resultaat,
                 tibble(DOMEIN_ID = "HM",
                        Omtrek_Extr_Hoogte.d = exp(-HML$Bd / (2 * HML$Cd)),
                        Extr_Hoogte.d =
                          HML$Ad + HML$Bd * log(Omtrek_Extr_Hoogte.d) +
                          HML$Cd * (log(Omtrek_Extr_Hoogte.d)) ^ 2,
                        Hoogteverschil.d = Extr_Hoogte.d -
                          (HML$Ad + HML$Bd * log(2.35) +
                             HML$Cd * (log(2.35)) ^ 2)))
  })

  it("De relevante variabelen voor laag minimum worden correct berekend", {
    resultaat <- curvekarakteristieken(Basismodel) %>%
       filter(DOMEIN_ID == "LM") %>%
       select(DOMEIN_ID,
              Omtrek_Extr_Hoogte.d,
              Omtrek_Buigpunt.d,
              Verschil_rico_BP_Q5.d) %>%
      as_tibble()
    attr(resultaat$Verschil_rico_BP_Q5.d, "names") <- NULL
    expect_equal(resultaat,
                 tibble(DOMEIN_ID = "LM",
                        Omtrek_Extr_Hoogte.d = exp(-LMB$Bd / (2 * LMB$Cd)),
                        Omtrek_Buigpunt.d = exp(1 - LMB$Bd / (2 * LMB$Cd)),
                        Verschil_rico_BP_Q5.d =
                          (2 * LMB$Cd * log(Omtrek_Buigpunt.d) + LMB$Bd) /
                          Omtrek_Buigpunt.d -
                          (2 * LMB$Cd * log(0.25) + LMB$Bd) / 0.25))
    resultaat <- curvekarakteristieken(Lokaalmodel, Lokaledata) %>%
       filter(DOMEIN_ID == "LM") %>%
       select(DOMEIN_ID,
              Omtrek_Extr_Hoogte.d,
              Omtrek_Buigpunt.d,
              Verschil_rico_BP_Q5.d)
    attr(resultaat$Verschil_rico_BP_Q5.d, "names") <- NULL
    expect_equal(resultaat,
                 tibble(DOMEIN_ID = "LM",
                        Omtrek_Extr_Hoogte.d = exp(-LML$Bd / (2 * LML$Cd)),
                        Omtrek_Buigpunt.d = exp(1 - LML$Bd / (2 * LML$Cd)),
                        Verschil_rico_BP_Q5.d =
                          (2 * LML$Cd * log(Omtrek_Buigpunt.d) + LML$Bd) /
                          Omtrek_Buigpunt.d -
                          (2 * LML$Cd * log(0.25) + LML$Bd) / 0.25))
  })

  it("Niet meegeven van de data bij het lokaal model geeft een foutmelding", {
    expect_error(
      curvekarakteristieken(
        Lokaalmodel),
      "Bij opgave van een lokaal model moet je ook de dataset meegeven"
    )
  })

  setwd(wd)

})
