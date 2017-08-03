context("test curvekarakteristieken")

test_wd <- tempdir()

setwd(test_wd)

library(dplyr)

Data <- dataAfwijkendeCurve()

Basismodel <- Data[["Basismodel"]]
Lokaledata <- Data[["Lokaledata"]]
Lokaalmodel <- Data[["Lokaalmodel"]]


test_that(
  "Niet afwijkende curves vertonen geen extremen in relevant interval", {
  expect_equal(curvekarakteristieken(Basismodel) %>%
                 filter(!(Omtrek_Extr_Hoogte.d > 0.1 &
                          Omtrek_Extr_Hoogte.d < 2.4)) %>%
                 select(DOMEIN_ID),
               tibble(DOMEIN_ID = LETTERS[1:6]))
})

test_that(
  "Afwijkende curves vertonen wel een extreem bij omtrek tussen 0.1 en 2.4 m", {
  expect_equal(curvekarakteristieken(Basismodel) %>%
                 filter(Omtrek_Extr_Hoogte.d > 0.1 &
                          Omtrek_Extr_Hoogte.d < 2.4) %>%
                 select(DOMEIN_ID),
               tibble(DOMEIN_ID = c("HM", "LM")))
  expect_equal(curvekarakteristieken(Lokaalmodel, Lokaledata) %>%
                 filter(Omtrek_Extr_Hoogte.d > 0.2) %>%
                 select(DOMEIN_ID),
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

test_that("De curven worden correct berekend (i.f.v. volgende testen)", {
  expect_equal(HMB$Ad, 20, tolerance = 1)
  expect_equal(HMB$Bd, 7, tolerance = 1)
  expect_equal(HMB$Cd, 4, tolerance = 1)
  expect_equal(LMB$Ad, 20, tolerance = 1)
  expect_equal(LMB$Bd, 5, tolerance = 1)
  expect_equal(LMB$Cd, -6, tolerance = 1)

  expect_equal(HML$Ad, 20, tolerance = 1)
  expect_equal(HML$Bd, 7, tolerance = 1)
  expect_equal(HML$Cd, 4, tolerance = 1)
  expect_equal(LML$Ad, 20, tolerance = 1)
  expect_equal(LML$Bd, 5, tolerance = 1)
  expect_equal(LML$Cd, -6, tolerance = 1)
})

test_that("De relevante variabelen voor hoog maximum worden correct berekend", {
  expect_equal(
    as.data.frame(
      curvekarakteristieken(Basismodel) %>%
        filter(DOMEIN_ID == "HM") %>%
        select(DOMEIN_ID,
               Omtrek_Extr_Hoogte.d,
               Extr_Hoogte.d,
               Hoogteverschil.d)),
    as.data.frame(
      tibble(DOMEIN_ID = "HM",
             Omtrek_Extr_Hoogte.d = exp(-HMB$Bd / (2 * HMB$Cd)),
             Extr_Hoogte.d =
               HMB$Ad + HMB$Bd * log(Omtrek_Extr_Hoogte.d) +
               HMB$Cd * (log(Omtrek_Extr_Hoogte.d)) ^ 2,
             Hoogteverschil.d =
               Extr_Hoogte.d -
               (HMB$Ad + HMB$Bd * log(2.35) + HMB$Cd * (log(2.35)) ^ 2))))
  expect_equal(as.data.frame(curvekarakteristieken(Lokaalmodel, Lokaledata) %>%
                               filter(DOMEIN_ID == "HM") %>%
                               select(DOMEIN_ID,
                                      Omtrek_Extr_Hoogte.d,
                                      Extr_Hoogte.d,
                                      Hoogteverschil.d)),
               as.data.frame(tibble(DOMEIN_ID = "HM",
                                    Omtrek_Extr_Hoogte.d =
                                      exp(-HML$Bd / (2 * HML$Cd)),
                                    Extr_Hoogte.d =
                                      HML$Ad +
                                      HML$Bd * log(Omtrek_Extr_Hoogte.d) +
                                      HML$Cd * (log(Omtrek_Extr_Hoogte.d)) ^ 2,
                                    Hoogteverschil.d =
                                      Extr_Hoogte.d -
                                      (HML$Ad + HML$Bd * log(2.35) +
                                         HML$Cd * (log(2.35)) ^ 2))))
})

test_that("De relevante variabelen voor laag minimum worden correct berekend", {
  expect_equal(curvekarakteristieken(Basismodel) %>%
                 filter(DOMEIN_ID == "LM") %>%
                 select(DOMEIN_ID,
                        Omtrek_Extr_Hoogte.d,
                        Omtrek_Buigpunt.d,
                        Verschil_rico_BP_Q5.d),
               tibble(DOMEIN_ID = "LM",
                      Omtrek_Extr_Hoogte.d = exp(-LMB$Bd / (2 * LMB$Cd)),
                      Omtrek_Buigpunt.d = exp(1 - LMB$Bd / (2 * LMB$Cd)),
                      Verschil_rico_BP_Q5.d =
                        (2 * LMB$Cd * log(Omtrek_Buigpunt.d) + LMB$Bd) /
                        Omtrek_Buigpunt.d -
                        (2 * LMB$Cd * log(0.25) + LMB$Bd) / 0.25))
  expect_equal(curvekarakteristieken(Lokaalmodel, Lokaledata) %>%
                 filter(DOMEIN_ID == "LM") %>%
                 select(DOMEIN_ID,
                        Omtrek_Extr_Hoogte.d,
                        Omtrek_Buigpunt.d,
                        Verschil_rico_BP_Q5.d),
               tibble(DOMEIN_ID = "LM",
                      Omtrek_Extr_Hoogte.d = exp(-LML$Bd / (2 * LML$Cd)),
                      Omtrek_Buigpunt.d = exp(1 - LML$Bd / (2 * LML$Cd)),
                      Verschil_rico_BP_Q5.d =
                        (2 * LML$Cd * log(Omtrek_Buigpunt.d) + LML$Bd) /
                        Omtrek_Buigpunt.d -
                        (2 * LML$Cd * log(0.25) + LML$Bd) / 0.25))
})

test_that(
  "Niet meegeven van de data bij het lokaal model geeft een foutmelding", {
    expect_error(curvekarakteristieken(Lokaalmodel),
                 "Bij opgave van een lokaal model moet je ook de dataset meegeven")
})
