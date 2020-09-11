#' Stelt de slechtste curves en afwijkende metingen grafisch voor in een rapport
#'
#' De functie genereert een validatierapport (html-bestand) in de working
#' directory (of opgegeven directory) met informatie en grafieken van de te
#' controleren modellen.  De afwijkende metingen en curvedelen zijn in rood
#' aangeduid; boven de curve is het probleem ook woordelijk beschreven.
#'
#' Deze functie rendert het bestand Validatierapport.Rmd, dat afhankelijk van
#' de opgegeven variabele TypeRapport voor elke boomsoort-domeincombinatie als
#' child DomeincurveDynamisch.Rmd of Domeincurve.Rmd toevoegt.
#'
#'
#' @param SlechtsteModellen Lijst van de slechtste modellen, dit zijn modellen
#' met hoge rmse, afwijkende vorm (op basis van extremen en buigpunten) en/of
#' modellen met afwijkende metingen.  Deze dataframe moet volgende velden
#' bevatten: BMS (boomsoort), DOMEIN_ID en Reden (= tekstuele opsomming van
#' afwijkingen, om weer te geven boven grafiek).
#' @param AfwijkendeMetingen Lijst met afwijkende metingen (dataframe zoals
#' gegenereerd door de functie afwijkendeMetingen).
#' @param Dataset Dataset met gemeten waarden en geschatte waarde voor
#' domeinmodel en Vlaams model (inclusief RMSE)
#' @param Bestandsnaam Een naam voor het validatierapport (html-bestand) dat
#' gegenereerd wordt, bestaande uit een string die eindigt op '.html'
#' @param TypeRapport Default is 'Dynamisch', waarbij de figuren in het
#' html-bestand kunnen worden aangepast (meetgegevens weergeven door muis
#' erover te bewegen (inclusief ID als deze in de dataset meegegeven is), items
#' uit legende wegklikken, grafiek inzoomen,...).  Een andere optie is
#' 'Statisch', waarbij de figuren vast zijn.
#' @inheritParams initiatie
#'
#' @return De functie genereert in de working directory (of opgegeven directory)
#' een rapport (html) met de te controleren modellen.  Hierin wordt per model
#' (boomsoort-domeincombinatie) de volgende algemene informatie vermeld:
#' boomsoort, domein (+ ID), aantal metingen, rmse, bruikbaar interval en de
#' mogelijke problemen die bij het model optreden.
#'
#' Daaronder wordt telkens grafisch volgende info weergegeven:
#'
#' - een puntenwolk die de metingen voorstelt (geen individuele metingen, maar
#' een jitter)
#'
#' - curve van het Vlaams model (als beschikbaar, dus niet voor het lokaal
#' model)
#'
#' - curve van het domeinmodel
#'
#' - grenzen van het bruikbaar interval (curves eindigen bij de klassemiddens
#' die overeenkomen met deze grenzen)
#'
#' - afwijkende metingen: in rood (andere metingen in zwart)
#'
#' - afwijkende deel van een curve in rood (rest van curve in zwart)
#'
#' @export
#'
#' @importFrom dplyr %>% inner_join mutate_ left_join select_ distinct_ filter_
#' bind_rows group_by_ arrange_ ungroup summarise_ desc
#' @importFrom rmarkdown render
#' @importFrom assertthat assert_that noNA is.flag has_name
#'

validatierapport <-
  function(SlechtsteModellen, AfwijkendeMetingen, Dataset,
           Bestandsnaam = "Validatie.html",
           TypeRapport = c("Dynamisch", "Statisch"),
           verbose = TRUE, PathWD = getwd()) {

    TypeRapport <- match.arg(TypeRapport)
  assert_that(inherits(SlechtsteModellen, "data.frame"))
  assert_that(has_name(SlechtsteModellen, "BMS"))
  if (has_name(SlechtsteModellen, "Omtrek_Buigpunt")) {
    assert_that(inherits(SlechtsteModellen$Omtrek_Buigpunt, "numeric"))
  }
  assert_that(has_name(SlechtsteModellen, "Reden"))
  if (has_name(SlechtsteModellen, "Omtrek_Extr_Hoogte")) {
    assert_that(inherits(SlechtsteModellen$Omtrek_Extr_Hoogte, "numeric"))
  }

  assert_that(inherits(AfwijkendeMetingen, "data.frame"))
  assert_that(has_name(AfwijkendeMetingen, "BMS"))
  assert_that(has_name(AfwijkendeMetingen, "DOMEIN_ID"))
  assert_that(has_name(AfwijkendeMetingen, "C13"))
  assert_that(inherits(AfwijkendeMetingen$C13, "numeric"))
  assert_that(has_name(AfwijkendeMetingen, "HOOGTE"))
  assert_that(inherits(AfwijkendeMetingen$HOOGTE, "numeric"))
  assert_that(has_name(AfwijkendeMetingen, "Status"),
              msg = "De opgegeven dataframe heeft geen veld met naam Status")
  if (!all(AfwijkendeMetingen$Status %in%
           c("Niet gecontroleerd", "Te controleren", "Goedgekeurd", NA))) {
    stop("De kolom Status in de dataframe heeft niet voor alle records een
         geldige waarde.  Zorg dat enkel de waarden 'Niet gecontroleerd',
         'Te controleren' en 'Goedgekeurd' voorkomen, NA is ook toegelaten.")
  }

  invoercontrole(Dataset, "afgeleidedata")
  assert_that(has_name(Dataset, "H_D_finaal"))
  assert_that(inherits(Dataset$H_D_finaal, "numeric"))
  assert_that(has_name(Dataset, "rmseD"))
  assert_that(inherits(Dataset$rmseD, "numeric"))
  assert_that(has_name(Dataset, "maxResid"))          #nolint
  assert_that(inherits(Dataset$maxResid, "numeric"))

  assert_that(is.flag(verbose))
  assert_that(noNA(verbose))
  assert_that(is.character(Bestandsnaam))
  if (!grepl(".html$", Bestandsnaam)) {
    stop("De bestandnaam moet eindigen op '.html'")
  }
  assert_that(is.character(TypeRapport))
  TypeRapport <- tolower(TypeRapport)
  assert_that(TypeRapport %in% c("dynamisch", "statisch"))

  Selectie <- Dataset %>%
    inner_join(
      SlechtsteModellen,
      by = c("BMS", "DOMEIN_ID")
    ) %>%
    left_join(
      AfwijkendeMetingen %>%
        filter_(~Status != "Goedgekeurd") %>%
        select_(~BMS, ~DOMEIN_ID, ~C13, ~HOOGTE) %>%
        distinct_() %>%
        mutate_(TeControlerenAfwijking = ~TRUE),
      by = c("BMS", "DOMEIN_ID", "C13", "HOOGTE")
    ) %>%
    mutate_(
      TeControlerenAfwijking =
        ~factor(ifelse(is.na(TeControlerenAfwijking),
                       FALSE, TeControlerenAfwijking),
                levels = c(TRUE, FALSE),
                labels = c("Te controleren", "OK"))
    )

  #om curves bij afwijkingen een andere kleur te geven (enkel nodig waar
  #buigpunten berekend zijn)
  if (has_name(Selectie, "Omtrek_Buigpunt")) {
    Selectie2 <- Selectie %>%
      mutate_(
        Omtrek_BP = ~ (((Omtrek_Buigpunt * 100) %/% 10) * 10 + 5) / 100,
        Omtrek_Max = ~ (((Omtrek_Extr_Hoogte * 100) %/% 10) * 10 + 5) / 100,
        CurveSlecht =
          ~ifelse(!is.na(Omtrek_BP) & (Omtrek <= Omtrek_BP), TRUE,
                  FALSE),
        CurveSlecht =
          ~ifelse(!is.na(Omtrek_Max) & (Omtrek >= Omtrek_Max),
                  TRUE, CurveSlecht)
      )

    Selectie <- Selectie2 %>%
      filter_(~Omtrek == Omtrek_BP | Omtrek == Omtrek_Max) %>%
      mutate_(
        CurveSlecht = ~FALSE
      ) %>%
      bind_rows(
        Selectie2
      )

  } else {
    Selectie$CurveSlecht <- FALSE
  }

  Selectie$CurveSlecht <-
    factor(Selectie$CurveSlecht, levels = c(TRUE, FALSE),
           labels = c("Te controleren", "OK"))


  Selectie <- Selectie %>%
    arrange_(~ desc(maxResid))


  render(system.file("Validatierapport.Rmd", package = "dhcurve"),
         output_file = Bestandsnaam,
         output_dir = PathWD,
         encoding = "UTF-8")

  if (verbose) {
    message(sprintf("Het rapport is opgeslagen in de working directory: %s",
                    getwd()))
  }

}
