library(readxl)
library(writexl)

bmiSitesURL <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_BMI_Sites/FeatureServer"
agol_username = "mojn_data"
agol_password = keyring::key_get(service = "AGOL", username = agol_username)

bmiSites <- fetchagol::fetchRawData(bmiSitesURL, agol_username, agol_password)

path <- paste0(here::here(),
               "/labreport",
               "/NPS - Mojave Desert Network_11251_WW2025-05-20.xlsx")

bmidata <- path |> 
  readxl::excel_sheets() |> 
  rlang::set_names() |> 
  purrr::map(readxl::read_excel, path = path)

sites <- bmiSites$data$BMI_Sites |>
  dplyr::select(Park, Project, SiteCode, SiteName, SiteGroup)

taxa <- bmidata$`NPS Format` |>
  dplyr::select(Phylum, Class, Order) |>
  unique()

bmidata$BMI_SiteVisit <- bmidata$`Site & sample info` |>
  dplyr::rename(SampleID = `Sample ID`,
                LabSiteID = `Site ID`,
                VisitID = `Visit ID`,
                WaterbodyName = `Waterbody Name`,
                NAMC_Latitude = `NAMC Latitude`,
                NAMC_Longitude = `NAMC Longitude`,
                Customer_Latitude = `Customer Latitude`,
                Customer_Longitude = `Customer Longitude`,
                CollectionDateText = `Collection Date`,
                SampleType = `Sample Type`,
                SamplerType = `Sampler Type`,
                FieldNotes = `Field Notes`,
                LabNotes = `Lab Notes`,
                FieldSplit = `Field Split`,
                LabSplit = `Lab Split`,
                SplitCount = `Split Count`,
                ReceivedDate = `Received Date`,
                CompletionDate = `Completion Date`) |>
  dplyr::mutate(VisitID = dplyr::case_when(VisitID == "NA" ~ "ND",
                                           TRUE ~ VisitID),
                FieldNotes = dplyr::case_when(FieldNotes == "NA" ~ "-none-",
                                              TRUE ~ FieldNotes),
                LabNotes = dplyr::case_when(LabNotes == "NA" ~ "-none-",
                                            TRUE ~ LabNotes),
                Laboratory = "NAMC",
                SampleID = as.integer(SampleID)) |>
  dplyr::left_join(sites, by = c("LabSiteID" = "SiteCode")) |>
  dplyr::mutate(AnalysisType = dplyr::case_when(grepl(LabSiteID, "QA") ~ "Lab Duplicate",
                                                TRUE ~ "Routine"),
                VisitType = "Primary",
                FieldSeason = ifelse(lubridate::month(as.Date(CollectionDateText)) < 10,
                                     lubridate::year(as.Date(CollectionDateText)),
                                     lubridate::year(as.Date(CollectionDateText)) + 1),
                CollectionDate = as.Date(CollectionDateText),
                SiteCode = LabSiteID) |>
  dplyr::select(SampleID, Laboratory, Park, Project, SiteCode, SiteName, SiteGroup, FieldSeason, AnalysisType, VisitID, COMID, WaterbodyName,
                NAMC_Latitude, NAMC_Longitude, Customer_Latitude, Customer_Latitude, CollectionDateText, Ecosystem, Habitat, SampleType, SamplerType,
                FieldNotes, LabNotes, Area, FieldSplit, LabSplit, SplitCount, CollectionDate, LabSiteID, VisitType)

join <- bmidata$BMI_SiteVisit |>
  dplyr::select(SampleID, Laboratory, Park, Project, SiteCode, SiteName, SiteGroup, FieldSeason, AnalysisType, VisitID, CollectionDateText, CollectionDate)

bmidata$BMI_Species <- bmidata$`Raw taxa list` |>
  dplyr::mutate(SampleID = `Sample ID`) |>
  dplyr::mutate(SampleID = as.integer(SampleID)) |>
  dplyr::rename(SiteID = `Site ID`,
                VisitID = `Visit ID`,
                OTUName = `OTU Name`,
                LabCount = SplitCount) |>
  dplyr::select(-c("VisitID", "SiteID")) |>
  dplyr::left_join(taxa, by = "Order") |>
  dplyr::mutate(Phylum = dplyr::case_when(ScientificName == "Nematoda" ~ "Nematoda",
                                          ScientificName == "Oligochaeta" ~ "Oligochaeta",
                                          ScientificName == "Platyhelminthes" ~ "Platyhelminthes",
                                          TRUE ~ Phylum),
                Class = dplyr::case_when(Order == "Collembola" ~ "Collembola",
                                         TRUE ~ Class),
                StationName = NA_character_,
                SampleType = NA_character_,
                Notes = dplyr::case_when(is.na(Notes) ~ "-none-",
                                         TRUE ~ Notes)) |>
  dplyr::left_join(join, by = "SampleID") |>
  dplyr::select(SampleID, Laboratory, Park, Project, SiteCode, SiteName, SiteGroup, FieldSeason, AnalysisType, VisitID,
                Phylum, Class, Order, Family, SubFamily, Genus, Species, ScientificName, LifeStage, BigRareCount, Notes, OTUName, CollectionDateText, CollectionDate, StationName, SampleType, `Sample ID`, LabCount)

bmidata$BMI_Metrics <- bmidata$Metrics |>
  dplyr::rename(`Unique Richness - NonInsects` = `Unique Richness - Non-insects`,
                `Density - NonInsects` = `Density - Non-insects`,
                `Unique Richness - Feed_CollectorFilterer` = `Unique Richness - Feed_Collector-Filterer`,
                `Density - Feed_CollectorFilterer` = `Density - Feed_Collector-Filterer`,
                `Unique Richness - Feed_CollectorGatherer` = `Unique Richness - Feed_Collector-Gatherer`,
                `Density - Feed_CollectorGatherer` = `Density - Feed_Collector-Gatherer`,
                `Unique Richness - Feed_PiercerHerbivore` = `Unique Richness - Feed_Piercer-Herbivore`,
                `Density - Feed_PiercerHerbivore` = `Density - Feed_Piercer-Herbivore`,
                `Unique Richness - LongLivedTaxa` = `Unique Richness - Long-Lived Taxa`) |>
  dplyr::rename_at(dplyr::vars(dplyr::everything()), ~stringr::str_replace_all(., "\\s+", "")) |>
  dplyr::rename_at(dplyr::vars(dplyr::everything()), ~stringr::str_replace_all(., "_", "")) |>
  dplyr::rename_at(dplyr::vars(dplyr::everything()), ~stringr::str_replace_all(., "-", "_")) |>
  dplyr::select(-c("VisitID", "SiteID")) |>
  dplyr::mutate(SampleID = as.integer(SampleID)) |>
  dplyr::left_join(join, by = "SampleID") |>
  tidyr::pivot_longer(cols = matches('^(Unique|Density)'),
                      names_to = "Attribute",
                      values_to = "Value") |>
  dplyr::select(SampleID, Park, Project, SiteCode, SiteName, SiteGroup, FieldSeason, AnalysisType, CollectionDateText, CollectionDate, InvasiveSpeciesList, Attribute, Value)

bmilist <- bmidata[c("BMI_SiteVisit", "BMI_Metrics", "BMI_Species")]

writexl::write_xlsx(bmilist, "BMIReportTidy_20250520.xlsx", format_headers = FALSE)