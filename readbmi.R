library(tidyverse)
library(fetchagol)
library(NPSdataverse)
pak::pak("nationalparkservice/mojn-stlk-rpackage@irene-dev2")
pak::pak("nationalparkservice/mojn-stlk-rpackage@master")
library(streamsandlakes)

agol_username = "mojn_data"
agol_password = keyring::key_get(service = "AGOL", username = agol_username)
bmi_url <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_HYDRO_BMI_Database/FeatureServer"

bmidata <- fetchagol::fetchRawData(bmi_url, agol_username, agol_password)
bmidata <- fetchagol::cleanData(bmidata)

bmidata$data[['BMI_Metadata']] <- NULL
names(bmidata$data) <- c("BMIspecies", "BMImetrics", "BMIvisits")

bmidata$data <- lapply(bmidata$data, function(df) {
  df |>
    dplyr::mutate(CollectionDate = as.Date(CollectionDateText, tz = "America/Los_Angeles"))
})

bmidata <- bmidata$data

bmidata$BMIvisits <- bmidata$BMIvisits |>
  dplyr::filter(Project %in% c("STLK", "SLS")) |>
  dplyr::rename(SubsiteCode = SiteCode) |>
  dplyr::rename(SiteCode = SiteGroup,
                decimalLatitude = Customer_Latitude,
                decimalLongitude = Customer_Longitude) |>
  dplyr::mutate(SiteCode = dplyr::case_when(Project == "STLK" ~ SubsiteCode,
                                            TRUE ~ SiteCode)) |>
  dplyr::mutate(SiteName = dplyr::case_when(grepl("Baker Creek", WaterbodyName) ~ "Baker Creek",
                                            grepl("Pakoon", WaterbodyName) ~ "Pakoon Spring",
                                            grepl("Tassi", WaterbodyName) ~ "Tassi Spring",
                                            grepl("Blue Point", WaterbodyName) ~ "Blue Point Spring",
                                            grepl("Fortynine", WaterbodyName) ~ "Fortynine Palms Oasis",
                                            grepl("Mound", WaterbodyName) ~ "Mound Spring",
                                            grepl("Nevares", WaterbodyName) ~ "Nevares Spring",
                                            grepl("Texas", WaterbodyName) ~ "Texas Spring",
                                            grepl("Travertine", WaterbodyName) ~ "Travertine Spring",
                                            grepl("Saratoga", WaterbodyName) ~ "Saratoga Spring",
                                            grepl("Smithwater", WaterbodyName) ~ "Smithwater Canyon Spring",
                                            grepl("Snake", WaterbodyName) ~ "Snake Creek",
                                            grepl("Boiler", WaterbodyName) ~ "Boiler Spring",
                                            grepl("STRW0", SiteCode) ~ "Strawberry Spring",
                                            TRUE ~ WaterbodyName)) |>
  dplyr::select(SampleID, Laboratory, Project, Park, SiteCode, SubsiteCode, SiteName, decimalLatitude, decimalLongitude, CollectionDate, FieldSeason, VisitType, AnalysisType, SampleType, SamplerType, Habitat, Ecosystem, Area, FieldSplit, LabSplit, SplitCount, FieldNotes, LabNotes) |>
  dplyr::arrange(SiteCode, CollectionDate)

bmidata$BMImetrics <- bmidata$BMImetrics |>
  dplyr::filter(Project %in% c("STLK", "SLS")) |>
  dplyr::rename(SubsiteCode = SiteCode) |>
  dplyr::rename(SiteCode = SiteGroup) |>
  dplyr::mutate(SiteCode = dplyr::case_when(Project == "STLK" ~ SubsiteCode,
                                            TRUE ~ SiteCode)) |>
  dplyr::select(SampleID, Project, Park, SiteCode, SubsiteCode, CollectionDate, FieldSeason, AnalysisType, InvasiveSpeciesList, Attribute, Value) |>
  dplyr::arrange(SiteCode, CollectionDate)

bmidata$BMIspecies <- bmidata$BMIspecies |>
  dplyr::filter(Project %in% c("STLK", "SLS")) |>
  dplyr::rename(SubsiteCode = SiteCode) |>
  dplyr::rename(SiteCode = SiteGroup,
                Order = Order_) |>
  dplyr::mutate(SiteCode = dplyr::case_when(Project == "STLK" ~ SubsiteCode,
                                            TRUE ~ SiteCode)) |>
  dplyr::select(SampleID, Project, Park, SiteCode, SubsiteCode, CollectionDate, FieldSeason, Phylum, Class, Order, Family, SubFamily, Genus, Species, ScientificName, OTUName, LifeStage, Notes, LabCount, BigRareCount) |>
  dplyr::arrange(SiteCode, CollectionDate)


# Set output folder for CSV exports
dest.folder <- paste0(here::here(),"/",format(Sys.time(), format="%Y-%m-%dT%H%M%S"))

# Folder export/sls must already exist. Code below will only make the date-time subfolder
if (file.exists(dest.folder)) {
  cat("The folder already exists")
} else {
  dir.create(dest.folder)
}

# Export results to CSV files
readr::write_csv(bmidata$BMIvisits, file.path(dest.folder, paste0("BMIvisits", ".csv")), na = "", append = FALSE, col_names = TRUE)
readr::write_csv(bmidata$BMImetrics, file.path(dest.folder, paste0("BMImetrics", ".csv")), na = "", append = FALSE, col_names = TRUE)
readr::write_csv(bmidata$BMIspecies, file.path(dest.folder, paste0("BMIspecies", ".csv")), na = "", append = FALSE, col_names = TRUE)