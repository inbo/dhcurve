context("test validatie.uitbreiding")

describe("validatie.uitbreiding", {

  wd <- getwd()

  test_wd <- tempdir()

  setwd(test_wd)

  set.seed(987654321)
  library(dplyr)
  library(tibble)
  library(xml2)
  library(stringr)

  #data genereren voor basismodel
  MetingenBasis <- testdataset(maxOmtrek = 280) %>%
    filter(!(DOMEIN_ID == "D" & C13 > 239))

  Data.basis <- suppressMessages(initiatie(MetingenBasis))[["Basis"]]
  Basismodel <- fit.basis(Data.basis)

  #data genereren voor lokaal model
  MetingenLokaal <- testdataset(c(200, 100, 150), maxOmtrek = 290) %>%
    filter(!(DOMEIN_ID == "B" & C13 > 239))

  Data.lokaal <- suppressMessages(initiatie(MetingenLokaal))[["Lokaal"]]
  Lokaalmodel <- fit.lokaal(Data.lokaal)


  it("De uitvoer van de functies is correct", {
    expect_equal(validatie.uitbreiding(Basismodel, Data.basis) %>%
                   colnames(.),
                 c("BMS", "DOMEIN_ID", "MaxOmtrek")
    )
    expect_equal(validatie.uitbreiding(Lokaalmodel, Data.lokaal) %>%
                   colnames(.),
                 c("BMS", "DOMEIN_ID", "MaxOmtrek")
    )
  })

  it("Selectie AantalDomValidatie werkt correct", {
    expect_error(
      validatie.uitbreiding(Basismodel, Data.basis, AantalDomValidatie = -1),
      "AantalDomValidatie moet een positief geheel getal zijn."
    )
    test <-
      validatie.uitbreiding(Basismodel, Data.basis, AantalDomValidatie = 2)
    expect_equal(
      str_count(as.character(read_html("validatie_uitbreiding.html")),
                pattern = "Om uitbreiding goed te keuren"),
      2
    )
    expect_equal(nrow(test), 5)
    validatie.uitbreiding(Basismodel, Data.basis, AantalDomValidatie = 8)
    expect_equal(
      str_count(as.character(read_html("validatie_uitbreiding.html")),
                pattern = "Om uitbreiding goed te keuren"),
      5
    )
    test <-
      validatie.uitbreiding(Lokaalmodel, Data.lokaal, AantalDomValidatie = 1)
    expect_equal(
      str_count(as.character(read_html("validatie_uitbreiding.html")),
                pattern = "Om uitbreiding goed te keuren"),
      1
    )
    expect_equal(nrow(test), 2)
  })

  it("Toevoegen GoedgekeurdeUitbreidingen werkt correct", {
    expect_warning(
      validatie.uitbreiding(
        Basismodel, Data.basis,
        GoedgekeurdeUitbreidingen =
          data.frame(DOMEIN_ID = "D", BMS = "testboom", nBomenTerugTonen = 50)
      ),
      "Niet elk opgegeven record in GoedgekeurdeUitbreidingen heeft een afwijkende curve" #nolint: line_length_linter
    )
    expect_equal(
      suppressWarnings(
        validatie.uitbreiding(
          Basismodel, Data.basis,
          GoedgekeurdeUitbreidingen =
            data.frame(DOMEIN_ID = "D", BMS = "testboom", nBomenTerugTonen = 50)
        )
      ),
      validatie.uitbreiding(Basismodel, Data.basis)
    )
    Uitbreidingen <-
      validatie.uitbreiding(
        Basismodel, Data.basis,
        GoedgekeurdeUitbreidingen =
          data.frame(DOMEIN_ID = "B", BMS = "testboom", nBomenTerugTonen = 50)
      )
    expect_equal( #opletten, deze eerst, of validatierapport andere naam geven!
      str_count(as.character(read_html("validatie_uitbreiding.html")),
                pattern = "Om uitbreiding goed te keuren"),
      4
    )
    expect_equal(
      Uitbreidingen,
      validatie.uitbreiding(Basismodel, Data.basis)
    )
    expect_warning(
      validatie.uitbreiding(
        Lokaalmodel, Data.lokaal,
        GoedgekeurdeUitbreidingen =
          data.frame(DOMEIN_ID = "B", BMS = "boom", nBomenTerugTonen = 50)
      ),
      "Niet elk opgegeven record in GoedgekeurdeUitbreidingen heeft een afwijkende curve" #nolint: line_length_linter
    )
    expect_equal(
      suppressWarnings(
        validatie.uitbreiding(
          Lokaalmodel, Data.lokaal,
          GoedgekeurdeUitbreidingen =
            data.frame(DOMEIN_ID = "B", BMS = "boom", nBomenTerugTonen = 50)
        )
      ),
      validatie.uitbreiding(Lokaalmodel, Data.lokaal)
    )
    Uitbreidingen <-
      validatie.uitbreiding(
        Lokaalmodel, Data.lokaal,
        GoedgekeurdeUitbreidingen =
          data.frame(DOMEIN_ID = "C", BMS = "testboom", nBomenTerugTonen = 50)
      )
    expect_equal( #opletten, deze eerst, of validatierapport andere naam geven!
      str_count(as.character(read_html("validatie_uitbreiding.html")),
                pattern = "Om uitbreiding goed te keuren"),
      1
    )
    expect_equal(
      Uitbreidingen,
      validatie.uitbreiding(Lokaalmodel, Data.lokaal)
    )
  })

  AfwijkendeModellen <- dataAfwijkendeCurve(maxOmtrek = 270)
  Basisdata <- AfwijkendeModellen[["Basisdata"]] %>%
    filter(DOMEIN_ID == "LM")
  Basismodel <- AfwijkendeModellen[["Basismodel"]]
  Lokaledata <- AfwijkendeModellen[["Lokaledata"]]
  Lokaalmodel <- AfwijkendeModellen[["Lokaalmodel"]]

  validatie.uitbreiding(Model = Basismodel, Dataset = Basisdata)

  it("Behandeling LaagMaximum is correct bij uitbreiding", {
    Maximum <- (curvekarakteristieken(Basismodel) %>%
      filter(DOMEIN_ID == "LM"))$Extr_Hoogte.d
    Uitbreiding <- Basisdata %>%
      filter(!VoorModelFit) %>%
      summarise(
        Mediaan = median(HOOGTE),
        Min = min(HOOGTE),
        Max = max(HOOGTE)
      )
    validatie.uitbreiding(Model = Basismodel, Dataset = Basisdata)
    DiffMediaanMinMax <-
      str_split(
        gsub(
          ".*DiffMediaan: (.+;) DiffMin: (.+;) DiffMax: (-?\\d+\\.\\d{2})<\\/p>.*", #nolint: line_length_linter
          "\\1\\2\\3",
          as.character(read_html("validatie_uitbreiding.html"))
        ), ";", simplify = TRUE
      )
    expect_equal(
      round(c(Uitbreiding$Mediaan, Uitbreiding$Min, Uitbreiding$Max) - Maximum,
            2),
      as.numeric(DiffMediaanMinMax)
    )
  })
  
  setwd(wd)

})
