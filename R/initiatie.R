#' Initiatiestap met opsplitsen van dataset volgens modeltypen
#'
#' De functie initiatie maakt de dataset klaar voor de verdere stappen van het opstellen van DH-curves: ze berekent de noodzakelijke variabelen en splitst de dataset op basis van het aantal metingen per boomsoort-domeincombinatie in 4 delen: 3 delen die als parameter meegegeven kunnen worden voor de fit-functies per modeltype (basismodel, afgeleid model en lokaal model), en een 4de deel met de resterende gegevens.
#'
#'
#' @param Data Dataframe met metingen van bomen die als basis moeten dienen om DH-curves op te stellen.  De dataframe moet de velden DOMEIN_ID (identificatienummer voor domein), BOS_BHI (domeinnaam), IDbms (identificatienummer voor boomsoort), BMS (boomsoort), C13 (omtrek in centimeter, gemeten op 1,3 m hoogte), HOOGTE (in meter) en Status bevatten en mag eventueel velden TYPE_METING en JAAR bevatten (die worden bij rmse.basis als groeperende variabelen gebruikt). Status mag enkel gegevens met status 'Niet gecontroleerd', 'Te controleren' of 'Goedgekeurd' bevatten, dus gegevens met status 'Afgekeurd' of 'Negeren' moeten vooraf verwijderd worden.
#' @param Uitzonderingen Lijst met boomsoort-domeincombinaties waarvoor uitzonderingen gelden voor de limieten van minimum 50 en minimum 10 bomen.  De dataframe moet de velden DOMEIN_ID, BMS, min_basis (= vervangende waarde voor 50), min_afgeleid (= vervangende waarde voor 10) bevatten.
#' @param Bestandsnaam Een naam voor het validatierapport (html-bestand) dat gegenereerd wordt, bestaande uit een string die eindigt op '.html'
#' @param verbose Dit geeft de toestand van het systeem aan en zorgt ervoor dat boodschappen niet onnodig gegeven worden.  (Default-waarde behouden.)
#' @param PathWD Het path van de working directory, dus het path waarin het validatierapport opgeslagen moet worden (default wordt het in de op dat moment actieve working directory opgeslagen).
#'
#' @return
#'
#' Als er gegevens verwijderd worden, genereert de functie een validatierapport (html-bestand) waarin een overzicht gegeven wordt van de verwijderde gegevens, dit zijn gegevens met omtrek > 2.4 m en omtrek < 0.2 m.
#'
#' De functie geeft een list van dataframes terug, met in elke dataframe behalve de variabelen uit Data de berekende variabelen Omtrek (= omtrekklasse), logOmtrek, logOmtrek2, Q5k en Q95k (de grenzen van het bruikbaar interval), nBomen (= aantal metingen behalve de verwijderde gegevens), nBomenInterval (= aantal metingen binnen het bruikbaar interval) en nBomenOmtrek05 (aantal metingen binnen het bruikbaar interval met omtrek > 0.5 m)).
#'
#' De 4 dataframes die achtereenvolgens in de list zitten, zijn:
#'
#' - [["Basis"]] gegevens van boomsoorten waarvoor meer dan 50 metingen (binnen het bruikbaar interval met omtrek > 0.5 m) beschikbaar zijn in minimum 6 domeinen, waarbij enkel gegevens worden opgenomen van de domeinen waarvoor minimum 50 metingen beschikbaar zijn.  Op basis van deze dataset kan een basismodel berekend worden, bestaande uit een Vlaams model per boomsoort en domeinspecifieke modellen.
#'
#' - [["Afgeleid"]] gegevens van domeinen met minder metingen (10 - 50 metingen binnen het bruikbaar interval met omtrek > 0.5 m) van boomsoorten waarvoor een Vlaams model berekend kan worden (dus boomsoorten die in dataset "Basis" voorkomen), op basis waarvan een afgeleid model berekend kan worden.
#'
#' - [["Lokaal"]] gegevens van domeinen met veel metingen voor een boomsoort (> 50 metingen binnen het bruikbaar interval met omtrek > 0.5 m) waarvan er te weinig domeinen (< 6) zijn met voldoende metingen om een Vlaams model op te stellen.  Voor deze boomsoort-domeincombinaties kan een lokaal model berekend worden.
#'
#' - [["Rest"]] metingen van de domein-boomsoortcombinaties die niet tot de 3 voorgaande categorieën behoren en waar dus geen model voor opgesteld kan worden.
#'
#' Voor de eerste 3 dataframes worden metingen buiten het bruikbaar interval weggelaten; voor het afgeleid model (2de dataframe) worden ook de metingen met omtrek <= 0,5 m weggelaten.
#'
#' @export
#'
#' @importFrom dplyr %>% filter_ mutate_ group_by_ ungroup inner_join select_ distinct_ anti_join summarise_
#' @importFrom rmarkdown render
#' @importFrom assertthat assert_that has_name noNA is.flag
#'

initiatie <-
  function(Data,
           Uitzonderingen = data.frame(DOMEIN_ID = "", BMS = "",
                                       min_basis = NA, min_afgeleid = NA,
                                       stringsAsFactors = FALSE),
           Bestandsnaam = "VerwijderdeGegevensInitiatie.html",
           verbose = TRUE,
           PathWD = getwd()) {

  min_basismodel <- 50
  min_domeinen_basismodel <- 6
  min_afgeleidmodel <- 10

  #variabelen 'gebruiken' om lintr-foutmelding weg te werken
  assert_that(is.count(min_basismodel))
  assert_that(is.count(min_domeinen_basismodel))
  assert_that(is.count(min_afgeleidmodel))


  #controle op invoer
  invoercontrole(Data, "initiatie")

  assert_that(inherits(Uitzonderingen, "data.frame"))
  assert_that(has_name(Uitzonderingen, "DOMEIN_ID"))
  assert_that(has_name(Uitzonderingen, "BMS"))
  assert_that(has_name(Uitzonderingen, "min_basis"))
  if (!all(is.na(Uitzonderingen$min_basis))) {
    assert_that(inherits(Uitzonderingen$min_basis, c("integer", "numeric")),
      msg = "Elke waarde van min_basis in de dataframe Uitzonderingen moet een getal of NA zijn") #nolint
    if (inherits(Uitzonderingen$min_basis, "numeric")) {
      assert_that(
        max(
          abs(
            Uitzonderingen$min_basis - as.integer(Uitzonderingen$min_basis)
          ),
          na.rm = TRUE
        ) < 1e-6
      , msg = "Elke waarde van min_basis in de dataframe Uitzonderingen moet een geheel getal of NA zijn" #nolint
      )
      Uitzonderingen$min_basis <- as.integer(Uitzonderingen$min_basis)
    }
    assert_that(all(Uitzonderingen$min_basis > 0, na.rm = TRUE),
      msg = "Elke waarde van min_basis in de dataframe Uitzonderingen moet > 0 zijn (of NA)") #nolint
  }

  assert_that(has_name(Uitzonderingen, "min_afgeleid"))
  if (!all(is.na(Uitzonderingen$min_afgeleid))) {
    assert_that(inherits(Uitzonderingen$min_afgeleid, c("integer", "numeric")),
                msg = "Elke waarde van min_afgeleid in de dataframe Uitzonderingen moet een getal of NA zijn") #nolint
    if (inherits(Uitzonderingen$min_afgeleid, "numeric")) {
      assert_that(
        max(
          abs(
            Uitzonderingen$min_afgeleid - as.integer(Uitzonderingen$min_afgeleid)
          ),
          na.rm = TRUE
        ) < 1e-6
        , msg = "Elke waarde van min_afgeleid in de dataframe Uitzonderingen moet een geheel getal of NA zijn" #nolint
      )
      Uitzonderingen$min_afgeleid <- as.integer(Uitzonderingen$min_afgeleid)
    }
    assert_that(all(Uitzonderingen$min_afgeleid > 0, na.rm = TRUE),
                msg = "Elke waarde van min_afgeleid in de dataframe Uitzonderingen moet > 0 zijn (of NA)") #nolint
  }

  #eerst een rapport maken van de gegevens die verwijderd worden
  assert_that(is.flag(verbose))
  assert_that(noNA(verbose))
  assert_that(is.character(Bestandsnaam))
  if (!grepl(".html$", Bestandsnaam)) {
    stop("De bestandnaam moet eindigen op '.html'")
  }

  DataRapport <- Data %>%
    group_by_(
      ~DOMEIN_ID,
      ~BOS_BHI,
      ~BMS
    ) %>%
    summarise_(
      nTotaal = ~n(), #nolint
      nTeDun = ~sum(C13 < 20),
      percTeDun = ~round(nTeDun * 100 / nTotaal, digits = 1),
      nTeDik = ~sum(C13 > 240),
      percTeDik = ~round(nTeDik * 100 / nTotaal, digits = 1),
      nInterval = ~nTotaal - nTeDun - nTeDik
    ) %>%
    ungroup() %>%
    filter_(
      ~nTotaal >= 10
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

  #dan de extra variabelen berekenen
  Data2 <- Data %>%
    filter_(~HOOGTE != 0) %>%
    mutate_(
      Omtrek = ~ floor(C13 / 10) / 10 + 0.05,
      Rijnr = ~seq_along(C13),       #nummert de rijen oplopend
      logOmtrek = ~log(Omtrek),
      logOmtrek2 = ~logOmtrek ^ 2
    ) %>%
    filter_(
      ~Omtrek < 2.40
    ) %>%
    group_by_(
      ~BMS,
      ~DOMEIN_ID
    ) %>%
    mutate_(
      nBomen = ~n(),
      Q5 = ~quantile(Omtrek, probs = 0.05) - 0.1,
      #het klassemidden van Q5:
      Q5k = ~ pmax(floor(Q5 * 10) / 10 + 0.05, 0.25),
      Q95 = ~quantile(Omtrek, probs = 0.95) + 0.1,
      #het klassemidden van Q95:
      Q95k = ~ pmin(floor(Q95 * 10) / 10 + 0.05, 2.35)
    ) %>%
    ungroup() %>%
    filter_(
      ~Omtrek > Q5k - 0.05,
      ~Omtrek < Q95 + 0.05
    )

  Data.aantallen <- Data2 %>%
    group_by_(
      ~BMS,
      ~DOMEIN_ID
    ) %>%
    summarise_(
      nBomenInterval = ~n()
    ) %>%
    ungroup() %>%
    inner_join(
      Data2,
      by = c("BMS", "DOMEIN_ID")
    ) %>%
    filter_(
      ~Omtrek > 0.5,
      ~Omtrek > Q5k - 0.05,
      ~Omtrek < Q95k + 0.05
    ) %>%
    group_by_(
      ~BMS,
      ~DOMEIN_ID,
      ~nBomenInterval
    ) %>%
    summarise_(
      nBomenOmtrek05 = ~n()
    ) %>%
    ungroup() %>%
    inner_join(
      Data2,
      by = c("BMS", "DOMEIN_ID")
    ) %>%
    left_join(
      Uitzonderingen,
      by = c("BMS", "DOMEIN_ID")
    )


  #en tenslotte de dataset opsplitsen
  Data_Selectie_50 <- Data.aantallen %>%
    filter_(
      ~ ( (nBomenOmtrek05 > min_basismodel & is.na(min_basis)) |
        (!is.na(min_basis) & nBomenOmtrek05 > min_basis))
    ) %>%
    select_(
      ~-min_basis, ~-min_afgeleid
    )


  Basisdata <- Data_Selectie_50 %>%
    select_(
      ~BMS,
      ~DOMEIN_ID
    ) %>%
    distinct_() %>%
    group_by_(
      ~BMS
    ) %>%
    filter_(
      ~n() >= min_domeinen_basismodel
    ) %>%
    ungroup() %>%
    inner_join(
      Data_Selectie_50,
      by = c("DOMEIN_ID", "BMS")
    )


  Lokaledata <- Data_Selectie_50 %>%
    filter_(
      ~!BMS %in% unique(Basisdata$BMS)
    )


  Data.afgeleid <- Data.aantallen %>%
    filter_(
      ~BMS %in% unique(Basisdata$BMS)
    ) %>%
    anti_join(
      Basisdata %>%
        select_(~BMS, ~DOMEIN_ID) %>%
        distinct_(),
      by = c("BMS", "DOMEIN_ID")
    ) %>%
    filter_(
      ~ ( (nBomenOmtrek05 > min_afgeleidmodel & is.na(min_afgeleid)) |
        (!is.na(min_afgeleid) & nBomenOmtrek05 > min_afgeleid)),
      ~Omtrek > 0.5
    ) %>%
    mutate_(
      Q5k = ~ifelse(Q5k > 0.5, Q5k, 0.55)
    ) %>%
    select_(
      ~-min_basis, ~-min_afgeleid
    )

  Data.rest <- Data.aantallen %>%
    anti_join(
      Data_Selectie_50 %>%
        select_(~BMS, ~DOMEIN_ID) %>%
        distinct_(),
      by = c("BMS", "DOMEIN_ID")
    ) %>%
    anti_join(
      Data.afgeleid %>%
        select_(~BMS, ~DOMEIN_ID) %>%
        distinct_(),
      by = c("BMS", "DOMEIN_ID")
    ) %>%
    select_(
      ~-min_basis, ~-min_afgeleid
    )


  return(list(Basis = Basisdata, Afgeleid = Data.afgeleid, Lokaal = Lokaledata,
              Rest = Data.rest))
}
