#' @title Stelt de finale diameter-hoogtecurves grafisch voor in een rapport
#'
#' @description
#' De functie genereert een rapport (`.html`-bestand) in de working directory
#' (of opgegeven directory) met informatie en grafieken van de finale
#' modellen.
#'
#' Deze functie rendert het bestand `dhcurverapport.Rmd`, dat voor elke
#' boomsoort-domeincombinatie child `dhcurve.Rmd` toevoegt.
#'
#'
#' @param OutputIVANHO Lijst met geschatte hoogte per omtrekklasse (dataframe
#' zoals gegenereerd door de functie `outputIVANHO()`).
#' @param Datalijst Datasets met gemeten waarden die gebruikt zijn voor de
#' modellen, zoals gegenereerd door de functie `initiatie()`.
#' @param Bestandsnaam Een naam voor het rapport (`.html`-bestand) dat
#' gegenereerd wordt, bestaande uit een string die eindigt op `.html`
#' @param KleurUitbreiding Moeten de metingen van de uitbreiding in een andere
#' kleur weergegeven worden?
#' Default is `FALSE`, waarbij (eventuele) metingen van een uitbreiding niet in
#' een andere kleur weergegeven worden.
#' @inheritParams initiatie
#'
#' @return De functie genereert in de working directory (of opgegeven directory)
#' een rapport (html) met de te controleren modellen.  Hierin wordt per model
#' (boomsoort-domeincombinatie) de volgende algemene informatie vermeld:
#' boomsoort, domein (en ID), modeltype, aantal metingen, RMSE en bruikbaar
#' interval.
#'
#' Daaronder wordt telkens grafisch volgende info weergegeven:
#' - een puntenwolk die de metingen voorstelt (geen individuele metingen,
#'     maar een jitter)
#' - curve van het domeinmodel, aangeduid met zwarte ruiten
#' - grenzen van het bruikbaar interval (curves eindigen bij de
#'     klassenmiddens die overeenkomen met deze grenzen)
#'
#' @export
#'
#' @importFrom dplyr %>% mutate bind_rows group_by arrange ungroup right_join
#' @importFrom rlang .data
#' @importFrom rmarkdown render
#' @importFrom assertthat assert_that noNA is.flag has_name
#'

dhcurvesrapport <-
  function(OutputIVANHO, Datalijst,
           Bestandsnaam = "dhcurves.html",
           KleurUitbreiding = FALSE,
           verbose = TRUE, PathWD = getwd()) {

  # nocov start
  assert_that(inherits(OutputIVANHO, "data.frame"))
  assert_that(has_name(OutputIVANHO, "BMS"))
  assert_that(has_name(OutputIVANHO, "DOMEIN_ID"))
  assert_that(has_name(OutputIVANHO, "BOS_BHI"))
  assert_that(has_name(OutputIVANHO, "Omtrek"))
  assert_that(inherits(OutputIVANHO$Omtrek, "numeric"))
  assert_that(has_name(OutputIVANHO, "Hoogte"))
  assert_that(inherits(OutputIVANHO$Hoogte, "numeric"))
  assert_that(has_name(OutputIVANHO, "Modeltype"))

  assert_that(inherits(Datalijst, "list"))
  assert_that(has_name(Datalijst, "Basis"))
  assert_that(inherits(Datalijst[["Basis"]], "data.frame"))
  assert_that(has_name(Datalijst[["Basis"]], "BMS"))
  assert_that(has_name(Datalijst[["Basis"]], "DOMEIN_ID"))
  assert_that(has_name(Datalijst[["Basis"]], "BOS_BHI"))
  assert_that(has_name(Datalijst[["Basis"]], "nBomen"))
  assert_that(has_name(Datalijst[["Basis"]], "nBomenOmtrek05"))
  assert_that(has_name(Datalijst[["Basis"]], "nBomenInterval"))
  assert_that(has_name(Datalijst[["Basis"]], "nBomenIntervalOmtrek05"))
  assert_that(has_name(Datalijst[["Basis"]], "Omtrek"))
  assert_that(inherits(Datalijst[["Basis"]]$Omtrek, "numeric"))
  assert_that(has_name(Datalijst[["Basis"]], "HOOGTE"))
  assert_that(inherits(Datalijst[["Basis"]]$HOOGTE, "numeric"))

  assert_that(has_name(Datalijst, "Afgeleid"))
  assert_that(inherits(Datalijst[["Afgeleid"]], "data.frame"))
  assert_that(has_name(Datalijst[["Afgeleid"]], "BMS"))
  assert_that(has_name(Datalijst[["Afgeleid"]], "DOMEIN_ID"))
  assert_that(has_name(Datalijst[["Afgeleid"]], "BOS_BHI"))
  assert_that(has_name(Datalijst[["Afgeleid"]], "nBomen"))
  assert_that(has_name(Datalijst[["Afgeleid"]], "nBomenOmtrek05"))
  assert_that(has_name(Datalijst[["Afgeleid"]], "nBomenInterval"))
  assert_that(has_name(Datalijst[["Afgeleid"]], "nBomenIntervalOmtrek05"))
  assert_that(has_name(Datalijst[["Afgeleid"]], "Omtrek"))
  assert_that(inherits(Datalijst[["Afgeleid"]]$Omtrek, "numeric"))
  assert_that(has_name(Datalijst[["Afgeleid"]], "HOOGTE"))
  assert_that(inherits(Datalijst[["Afgeleid"]]$HOOGTE, "numeric"))

  assert_that(has_name(Datalijst, "Lokaal"))
  assert_that(inherits(Datalijst[["Lokaal"]], "data.frame"))
  assert_that(has_name(Datalijst[["Lokaal"]], "BMS"))
  assert_that(has_name(Datalijst[["Lokaal"]], "DOMEIN_ID"))
  assert_that(has_name(Datalijst[["Lokaal"]], "BOS_BHI"))
  assert_that(has_name(Datalijst[["Lokaal"]], "nBomen"))
  assert_that(has_name(Datalijst[["Lokaal"]], "nBomenOmtrek05"))
  assert_that(has_name(Datalijst[["Lokaal"]], "nBomenInterval"))
  assert_that(has_name(Datalijst[["Lokaal"]], "nBomenIntervalOmtrek05"))
  assert_that(has_name(Datalijst[["Lokaal"]], "Omtrek"))
  assert_that(inherits(Datalijst[["Lokaal"]]$Omtrek, "numeric"))
  assert_that(has_name(Datalijst[["Lokaal"]], "HOOGTE"))
  assert_that(inherits(Datalijst[["Lokaal"]]$HOOGTE, "numeric"))

  assert_that(is.logical(KleurUitbreiding))
  assert_that(is.flag(verbose))
  assert_that(noNA(verbose))
  assert_that(is.character(Bestandsnaam))
  if (!grepl(".html$", Bestandsnaam)) {
    stop("De bestandnaam moet eindigen op '.html'")
  }

  Dataset <- Datalijst[["Basis"]] %>% #nolint: object_usage_linter
    mutate(
      Modeltype = "basismodel"
    ) %>%
    bind_rows(
      Datalijst[["Afgeleid"]] %>%
        mutate(
          Modeltype = "afgeleid model"
        )
    ) %>%
    bind_rows(
      Datalijst[["Lokaal"]] %>%
        mutate(
          Modeltype = "lokaal model"
        )
    ) %>%
    right_join(
      OutputIVANHO,
      by = c("BMS", "DOMEIN_ID", "BOS_BHI", "Omtrek", "Modeltype"),
      suffix = c(".data", ".model")
    ) %>%
    group_by(.data$BMS, .data$DOMEIN_ID, .data$BOS_BHI) %>%
    mutate(
      # idee is om na's te vervangen door de waarde van de andere velden
      nBomen = max(.data$nBomen, na.rm = TRUE),
      nBomenOmtrek05 = max(.data$nBomenOmtrek05, na.rm = TRUE),
      nBomenInterval = max(.data$nBomenInterval, na.rm = TRUE),
      nBomenIntervalOmtrek05 = max(.data$nBomenIntervalOmtrek05, na.rm = TRUE),
      Q5k = max(.data$Q5k, na.rm = TRUE),
      Q95k = max(.data$Q95k, na.rm = TRUE),
      # moeten gegevens van uitbreiding weggelaten worden?
      MaxOmtrekModel = (max(.data$Omtrek * !is.na(.data$Hoogte)) - 0.2),
      UitbreidingWeglaten =
        max(.data$Omtrek * !is.na(.data$HOOGTE)) > .data$MaxOmtrekModel
    ) %>%
    ungroup() %>%
    filter(!(.data$UitbreidingWeglaten & !.data$VoorModelFit)) %>%
    arrange(.data$BMS, .data$DOMEIN_ID)


  render(system.file("dhcurvesrapport.Rmd", package = "dhcurve"),
         output_file = Bestandsnaam,
         output_dir = PathWD,
         quiet = TRUE,
         encoding = "UTF-8")

  if (verbose) {
    message(sprintf("Het rapport is opgeslagen in de working directory: %s",
                    getwd()))
  }
  # nocov end

}
