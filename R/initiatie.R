#' @title Initiatiestap met opsplitsen van dataset volgens modeltypen
#'
#' @description
#' De functie initiatie maakt de dataset klaar voor de verdere stappen van het
#' opstellen van diameter-hoogtecurves: ze berekent de noodzakelijke variabelen
#' en splitst de dataset op basis van het aantal metingen per
#' boomsoort-domeincombinatie in 4 delen:
#' 3 delen die als parameter meegegeven kunnen worden voor de
#' `fit.xxx()`-functies per modeltype (basismodel, afgeleid model en lokaal
#' model), en een vierde deel met de resterende gegevens.
#'
#'
#' @param Data Dataframe met metingen van bomen die als basis moeten dienen om
#' diameter-hoogtecurves op te stellen.  De dataframe moet de velden `DOMEIN_ID`
#' (identificatienummer voor domein), `BOS_BHI` (domeinnaam), `IDbms`
#' (identificatienummer voor boomsoort), `BMS` (boomsoort), `C13` (omtrek in
#' centimeter, gemeten op 1,3 m hoogte), `HOOGTE` (in meter) en `Status`
#' bevatten en mag eventueel velden `TYPE_METING` en `JAAR` bevatten (die worden
#' bij `rmse.basis()` als groeperende variabelen gebruikt).
#' `Status` mag enkel gegevens met status "Niet gecontroleerd", "Te controleren"
#' of "Goedgekeurd" bevatten,
#' dus gegevens met status "Afgekeurd" of "Negeren" moeten vooraf verwijderd
#' worden.
#' @param Uitzonderingen Lijst met boomsoort-domeincombinaties waarvoor
#' uitzonderingen gelden voor de limieten van minimum 50 en minimum 10 bomen.
#' De dataframe moet de velden `DOMEIN_ID`, `BMS`, `min_basis` (= vervangende
#' waarde voor 50) en `min_afgeleid` (= vervangende waarde voor 10) bevatten.
#' Daarnaast kan in een extra veld `min_uitbreiden_model` aangegeven worden
#' vanaf hoeveel metingen een uitbreiding naar hogere omtrekklassen opnieuw
#' bekeken moet worden (= vervangende waarde voor 10).
#' De waarde NA geeft telkens aan dat de defaultwaarde gebruikt mag worden.
#' De default voor dit argument is dat er geen uitzonderingen zijn
#' (wat sowieso het geval is als de curves nog niet gevalideerd zijn).
#' @param Bestandsnaam Een naam voor het validatierapport (`.html`-bestand) dat
#' gegenereerd wordt, bestaande uit een string die eindigt op `.html`
#' @param verbose Dit geeft de toestand van het systeem aan en zorgt ervoor dat
#' boodschappen niet onnodig gegeven worden.  (Defaultwaarde behouden.)
#' @param PathWD Het path van de working directory, dus het path waarin het
#' validatierapport opgeslagen moet worden (default wordt het in de op dat
#' moment actieve working directory opgeslagen).
#'
#' @return
#'
#' Als er gegevens verwijderd worden, genereert de functie een validatierapport
#' (`.html`-bestand) waarin een overzicht gegeven wordt van de verwijderde
#' gegevens, dit zijn gegevens met omtrek > 2.4 m en omtrek < 0.2 m.
#'
#' De functie geeft een list van dataframes terug, met in elke dataframe
#' behalve de variabelen uit `Data` de berekende variabelen `Omtrek`
#' (= omtrekklasse), `logOmtrek`, `logOmtrek2`, `Q5k` en `Q95k` (de grenzen van
#' het bruikbaar interval), `nBomen` (= aantal metingen behalve de verwijderde
#' gegevens),
#' `nBomenOmtrek05` (aantal metingen met omtrek > 0.5 m),
#' `nBomenInterval` (= aantal metingen binnen het bruikbaar interval),
#' `nBomenIntervalOmtrek05` (aantal metingen binnen het bruikbaar interval
#' met omtrek > 0.5 m))
#' en `nExtra` (aantal metingen boven het bruikbaar interval op basis waarvan
#' een uitbreiding van de curve naar hogere omtrekklassen gevalideerd zou
#' kunnen worden)
#'
#' De 4 dataframes die achtereenvolgens in de list zitten, zijn:
#' \itemize{
#'   \item `[["Basis"]]` gegevens van boomsoorten waarvoor meer dan 50 metingen
#'     (binnen het bruikbaar interval met omtrek > 0.5 m) beschikbaar zijn in
#'     minimum 6 domeinen, waarbij enkel gegevens worden opgenomen van de
#'     domeinen waarvoor minimum 50 metingen beschikbaar zijn.  Op basis van
#'     deze dataset kan een basismodel berekend worden, bestaande uit een
#'     Vlaams model per boomsoort en domeinspecifieke modellen.
#'   \item `[["Afgeleid"]]` gegevens van domeinen met minder metingen
#'     (< 50 metingen binnen het bruikbaar interval en > 10 metingen met
#'     omtrek > 0.5 m) van boomsoorten waarvoor een Vlaams model berekend
#'     kan worden (dus boomsoorten die in dataset "Basis" voorkomen), op basis
#'     waarvan een afgeleid model berekend kan worden.
#'   \item `[["Lokaal"]]` gegevens van domeinen met veel metingen voor een
#'     boomsoort (> 50 metingen binnen het bruikbaar interval met
#'     omtrek > 0.5 m) waarvan er te weinig domeinen (< 6) zijn
#'     met voldoende metingen om een Vlaams model op te stellen. Voor deze
#'     boomsoort-domeincombinaties kan een lokaal model berekend worden.
#'   \item `[["Rest"]]` metingen van de boomsoort-domeincombinaties die niet tot
#'     de 3 voorgaande categorieën behoren en waar dus geen model voor
#'     opgesteld kan worden.
#' }
#'
#' Voor de eerste 3 dataframes worden metingen buiten het bruikbaar interval
#' weggelaten; voor het afgeleid model (2de dataframe) worden ook de metingen
#' met omtrek <= 0,5 m weggelaten.
#'
#' In geval er gegevens verwijderd zijn, wordt aan de list een extra dataframe
#' `[["VerwijderdeGegevens"]]` toegevoegd met de gegevens uit het
#' validatierapport.
#'
#' @export
#'
#' @importFrom dplyr %>% filter mutate group_by ungroup inner_join select
#' distinct anti_join summarise n right_join
#' @importFrom rlang .data
#' @importFrom rmarkdown render
#' @importFrom assertthat assert_that has_name noNA is.flag
#' @importFrom stats quantile
#'

initiatie <-
  function(Data,
           Uitzonderingen = data.frame(DOMEIN_ID = "", BMS = "",
                                       min_basis = NA_integer_,
                                       min_afgeleid = NA_integer_,
                                       stringsAsFactors = FALSE),
           Bestandsnaam = "VerwijderdeGegevensInitiatie.html",
           verbose = TRUE,
           PathWD = getwd()) {

  min_basismodel <- 50
  min_domeinen_basismodel <- 6
  min_afgeleidmodel <- 10
  uitbreiden_model <- 10

  #variabelen 'gebruiken' om lintr-foutmelding weg te werken ----
  assert_that(is.count(min_domeinen_basismodel))


  #controle op invoer ----
  invoercontrole(Data, "initiatie")

  assert_that(inherits(Uitzonderingen, "data.frame"))
  assert_that(has_name(Uitzonderingen, "DOMEIN_ID"))
  assert_that(has_name(Uitzonderingen, "BMS"))
  assert_that(has_name(Uitzonderingen, "min_basis"))
  if (!all(is.na(Uitzonderingen$min_basis))) {
    assert_that(inherits(Uitzonderingen$min_basis, c("integer", "numeric")),
      msg = "Elke waarde van min_basis in de dataframe Uitzonderingen moet een getal of NA zijn") #nolint: line_length_linter
    if (inherits(Uitzonderingen$min_basis, "numeric")) {
      assert_that(
        max(
          abs(
            Uitzonderingen$min_basis - as.integer(Uitzonderingen$min_basis)
          ),
          na.rm = TRUE
        ) < 1e-6
      , msg = "Elke waarde van min_basis in de dataframe Uitzonderingen moet een geheel getal of NA zijn" #nolint: line_length_linter
      )
      Uitzonderingen$min_basis <- as.integer(Uitzonderingen$min_basis)
    }
    assert_that(all(Uitzonderingen$min_basis > min_basismodel, na.rm = TRUE),
      msg = "Elke waarde van min_basis in de dataframe Uitzonderingen moet > 50 zijn (of NA)") #nolint: line_length_linter
  }

  assert_that(has_name(Uitzonderingen, "min_afgeleid"))
  if (!all(is.na(Uitzonderingen$min_afgeleid))) {
    assert_that(inherits(Uitzonderingen$min_afgeleid, c("integer", "numeric")),
                msg = "Elke waarde van min_afgeleid in de dataframe Uitzonderingen moet een getal of NA zijn") #nolint: line_length_linter
    if (inherits(Uitzonderingen$min_afgeleid, "numeric")) {
      assert_that(
        max(
          abs(
            Uitzonderingen$min_afgeleid -
              as.integer(Uitzonderingen$min_afgeleid)
          ),
          na.rm = TRUE
        ) < 1e-6
        , msg = "Elke waarde van min_afgeleid in de dataframe Uitzonderingen moet een geheel getal of NA zijn" #nolint: line_length_linter
      )
      Uitzonderingen$min_afgeleid <- as.integer(Uitzonderingen$min_afgeleid)
    }
    assert_that(
      all(Uitzonderingen$min_afgeleid > min_afgeleidmodel, na.rm = TRUE),
      msg = "Elke waarde van min_afgeleid in de dataframe Uitzonderingen moet > 10 zijn (of NA)") #nolint: line_length_linter
  }

  if (has_name(Uitzonderingen, "min_uitbreiden_model")) {
    assert_that(
      inherits(Uitzonderingen$min_uitbreiden_model, c("integer", "numeric")),
      msg = "Elke waarde van min_uitbreiden_model in de dataframe Uitzonderingen moet een getal of NA zijn") #nolint: line_length_linter
    if (inherits(Uitzonderingen$min_uitbreiden_model, "numeric")) {
      assert_that(
        max(
          abs(
            Uitzonderingen$min_uitbreiden_model -
              as.integer(Uitzonderingen$min_uitbreiden_model)
          ),
          na.rm = TRUE
        ) < 1e-6
        , msg = "Elke waarde van min_uitbreiden_model in de dataframe Uitzonderingen moet een geheel getal of NA zijn" #nolint: line_length_linter
      )
      Uitzonderingen$min_uitbreiden_model <-
        as.integer(Uitzonderingen$min_uitbreiden_model)
    }
    assert_that(
      all(Uitzonderingen$min_uitbreiden_model > min_afgeleidmodel,
          na.rm = TRUE),
      msg = "Elke waarde van min_uitbreiden_model in de dataframe Uitzonderingen moet > 10 zijn (of NA)") #nolint: line_length_linter
  } else {
    Uitzonderingen <- Uitzonderingen %>%
      mutate(
        min_uitbreiden_model = NA_integer_
      )
  }

  #eerst een overzichtsrapp. maken met aantal bomen per domein-bms-comb: ----
  #nteDik, nTeDun (worden verwijderd in verdere analyse)
      #nInterval (worden behouden)
  assert_that(is.flag(verbose))
  assert_that(noNA(verbose))
  assert_that(is.character(Bestandsnaam))
  if (!grepl(".html$", Bestandsnaam)) {
    stop("De bestandnaam moet eindigen op '.html'")
  }

  DataRapport <- Data %>%
    group_by(
      .data$DOMEIN_ID,
      .data$BOS_BHI,
      .data$BMS
    ) %>%
    summarise(
      nTotaal = n(),
      nTeDun = sum(.data$C13 < 20),
      percTeDun = round(.data$nTeDun * 100 / .data$nTotaal, digits = 1),
      nTeDik = sum(.data$C13 > 240),
      percTeDik = round(.data$nTeDik * 100 / .data$nTotaal, digits = 1),
      nInterval = .data$nTotaal - .data$nTeDun - .data$nTeDik
    ) %>%
    ungroup() %>%
    filter(
      .data$nTotaal >= min_afgeleidmodel,
      .data$nTotaal != .data$nInterval
    )

  if (nrow(DataRapport > 0)) {
    render(system.file("OverzichtGegevens.Rmd", package = "dhcurve"),
           output_file = Bestandsnaam,
           output_dir = PathWD,
           encoding = "UTF-8")

    if (verbose) {
      message(sprintf("Het rapport is opgeslagen in de working directory: %s",
                      getwd()))
    }
  }

  #dan de aanmaak van de verder te gebruiken dataset: ----
        # extra variabelen berekenen (Q5 en Q95)
        # wegfilteren van te dikke/dunne bomen en bomen buiten Q5-Q95
  Data2 <- Data %>%
    filter(.data$HOOGTE != 0) %>%
    mutate(
      Omtrek = floor(.data$C13 / 10) / 10 + 0.05,
      Rijnr = seq_along(.data$C13),       #nummert de rijen oplopend
      logOmtrek = log(.data$Omtrek),
      logOmtrek2 = .data$logOmtrek ^ 2
    ) %>%
    group_by(
      .data$BMS,
      .data$DOMEIN_ID
    ) %>%
    mutate(
      nBomen = sum(.data$Omtrek < 2.40),
      nBomenOmtrek05 = sum(.data$Omtrek > 0.5 & .data$Omtrek < 2.40),
      Q5 = quantile(.data$Omtrek, probs = 0.05) - 0.1,
      #het klassenmidden van Q5:
      Q5k = pmax(floor(.data$Q5 * 10) / 10 + 0.05, 0.25),
      Q95 = quantile(.data$Omtrek[.data$Omtrek < 2.40], probs = 0.95) + 0.1,
      #het klassenmidden van Q95:
      Q95k = pmin(floor(.data$Q95 * 10) / 10 + 0.05, 2.35)
    ) %>%
    ungroup() %>%
    left_join(
      Uitzonderingen,
      by = c("BMS", "DOMEIN_ID")
    )

  Data.aantallen <- Data2 %>%
    filter(
      .data$Omtrek > .data$Q5k - 0.05,
      .data$Omtrek < .data$Q95k + 0.05
    ) %>%
    group_by(
      .data$BMS,
      .data$DOMEIN_ID
    ) %>%
    summarise(
      nBomenInterval = n()
    ) %>%
    ungroup() %>%
    inner_join(
      Data2,
      by = c("BMS", "DOMEIN_ID")
    ) %>%
    filter(
      .data$Omtrek > 0.5,
      .data$Omtrek > .data$Q5k - 0.05,
      .data$Omtrek < .data$Q95k + 0.05
    ) %>%
    group_by(
      .data$BMS,
      .data$DOMEIN_ID,
      .data$nBomenInterval
    ) %>%
    summarise(
      nBomenIntervalOmtrek05 = n()
    ) %>%
    ungroup() %>%
    inner_join(
      Data2,
      by = c("BMS", "DOMEIN_ID")
    )


  #en tenslotte de dataset opsplitsen ----

  # 1) alle bms-domeincombinaties met min. 50 metingen (omtrek > 0.5m) -----
  Data_Selectie_50 <- Data.aantallen %>%
    filter(
      .data$Omtrek > .data$Q5k - 0.05,
      ((.data$nBomenIntervalOmtrek05 > min_basismodel
        & is.na(.data$min_basis)) |
        (!is.na(.data$min_basis) &
           .data$nBomenIntervalOmtrek05 > .data$min_basis))
    ) %>%
    mutate(
      VoorModelFit = (.data$Omtrek < .data$Q95k + 0.05)
    ) %>%
    select(
      -"min_basis", -"min_afgeleid"
    )
  Data_Selectie_50 <- Data_Selectie_50 %>%
    filter(!.data$VoorModelFit) %>%
    count(.data$BMS, .data$DOMEIN_ID) %>%
    filter(.data$n >= uitbreiden_model) %>%
    right_join(Data_Selectie_50, by = c("BMS", "DOMEIN_ID")) %>%
    filter(!(is.na(.data$n) & !.data$VoorModelFit)) %>%
    mutate(
      nExtra = ifelse(is.na(.data$n), 0, .data$n)
    ) %>%
    select(-"n")
  if (has_name(Data_Selectie_50, "min_uitbreiden_model")) {
    Data_Selectie_50 <- Data_Selectie_50 %>%
      mutate(
        min_uitbreiden_model =
          ifelse(is.na(.data$min_uitbreiden_model), uitbreiden_model,
                 .data$min_uitbreiden_model)
      ) %>%
      filter(
        !(.data$nExtra < .data$min_uitbreiden_model & !.data$VoorModelFit)
      ) %>%
      select(-"min_uitbreiden_model")
  }

  # 1A) alle bms-domeincombinaties met min. 50 metingen in 6 domeinen ----
  Basisdata <- Data_Selectie_50 %>%
    select(
      "BMS",
      "DOMEIN_ID"
    ) %>%
    distinct() %>%
    group_by(
      .data$BMS
    ) %>%
    filter(
      n() >= min_domeinen_basismodel
    ) %>%
    ungroup() %>%
    inner_join(
      Data_Selectie_50,
      by = c("DOMEIN_ID", "BMS")
    )

  # 1B) alle bms-domeincomb's met min. 50 metingen, géén 6 domeinen ----
  Lokaledata <- Data_Selectie_50 %>%
    filter(
      !.data$BMS %in% unique(Basisdata$BMS)
    )

  # 2) alle bms-domeincomb's met géén 50 metingen, wel een basismodel ----
      # (basismodel: 6 andere domein met > 50 metingen van die bms)
  Data.afgeleid <- Data2 %>%
    filter(
      .data$BMS %in% unique(Basisdata$BMS)
    ) %>%
    anti_join(
      Basisdata %>%
        select("BMS", "DOMEIN_ID") %>%
        distinct(),
      by = c("BMS", "DOMEIN_ID")
    ) %>%
    filter(
      ((.data$nBomen > min_afgeleidmodel & is.na(.data$min_afgeleid)) |
        (!is.na(.data$min_afgeleid) & .data$nBomen > .data$min_afgeleid))
    ) %>%
    select(
      "BMS", "DOMEIN_ID",
      nBomenInterval = "nBomen",
      nBomenIntervalOmtrek05 = "nBomenOmtrek05"
    ) %>%
    distinct()
  # Kunstgreepje om de volgorde van de kolommen hetzelfde te krijgen als voor
  # de andere modellen
  Data.afgeleid <- Data.afgeleid %>%
    inner_join(Data2, by = c("BMS", "DOMEIN_ID")) %>%
    select(
      -"min_basis", -"min_afgeleid"
    )
  if (has_name(Data.afgeleid, "min_uitbreiden_model")) {
    Data.afgeleid <- Data.afgeleid %>%
      select(-"min_uitbreiden_model")
  }

  # 3) alle bms-domeincombinaties waar géén model voor lukt ----
  Data.rest <- Data.aantallen %>%
    anti_join(
      Data_Selectie_50 %>%
        select("BMS", "DOMEIN_ID") %>%
        distinct(),
      by = c("BMS", "DOMEIN_ID")
    ) %>%
    anti_join(
      Data.afgeleid %>%
        select("BMS", "DOMEIN_ID") %>%
        distinct(),
      by = c("BMS", "DOMEIN_ID")
    ) %>%
    select(
      -"min_basis", -"min_afgeleid"
    )
  if (has_name(Data.rest, "min_uitbreiden_model")) {
    Data.rest <- Data.rest %>%
      select(-"min_uitbreiden_model")
  }


  return(
    if (nrow(DataRapport > 0)) {
      list(Basis = Basisdata, Afgeleid = Data.afgeleid, Lokaal = Lokaledata,
              Rest = Data.rest, VerwijderdeGegevens = DataRapport)
    } else {
      list(Basis = Basisdata, Afgeleid = Data.afgeleid, Lokaal = Lokaledata,
           Rest = Data.rest)
    }
  )
}
