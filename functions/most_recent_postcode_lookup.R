#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# most_recent_postcode_lookup.R
# June 2023
# Bella Tortora Brayda 
# Adapted from Jumping rivers code
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

postcode_folder <- "/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/"

most_recent_postcode_lookup <- function(folder) {
  files <- list.files(folder)
  postcode_lookups <- stringr::str_detect(files, "Scottish_Postcode_Directory_[0-9]{4}_[0-9].rds")
  files <- files[postcode_lookups]
  year_version <- stringr::str_remove(stringr::str_extract(files, "[0-9]{4}_[0-9].rds"), ".rds")
  numeric_ver <- as.numeric(stringr::str_remove(year_version, "_"))
  most_recent <- year_version[numeric_ver == max(numeric_ver)]
  file <- glue::glue("Scottish_Postcode_Directory_{most_recent}.rds")
  file_path <- paste0(folder, file)
  if (file.exists(file_path)) {
    return(file_path)
  }
  error_message <- glue::glue(
    "Cannot find file {file_path}. Ensure file exists in folder and follows a <YYYY>_<X> format where <YYYY> is a year and <X> a single-digit version number." # nolint
  )
  stop(error_message)
}


