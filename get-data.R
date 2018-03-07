library(here)
library(glue)

# Downloads ACA data for 2014-2018
# This is about 500 MB total

if (!dir.exists("data")) {
  dir.create("data")
}

aca2018_file <- here("data", "QHP_PY2018_Medi-_Indi-_Land.csv")
aca2017_file <- here("data", "2017_QHP_Landscape_Individual_Market_Medical.csv")
aca2016_file <- here("data", "2016_QHP_Landscape_Individual_Market_Medical.csv")
aca2015_file <- here("data", "2015_QHP_Landscape_Individual_Market_Medical.csv")
aca2014_file <- here("data", "2014_QHP_Landscape_Individual_Market_Medical.csv")

aca2018_url <- "https://data.healthcare.gov/api/views/hd64-a3rh/rows.csv?accessType=DOWNLOAD"
aca2017_url <- "https://data.healthcare.gov/api/views/enpz-m4q6/rows.csv?accessType=DOWNLOAD"
aca2016_url <- "https://data.healthcare.gov/api/views/v7sn-c66v/rows.csv?accessType=DOWNLOAD"
aca2015_url <- "https://data.healthcare.gov/api/views/mp8z-jtg7/rows.csv?accessType=DOWNLOAD"
aca2014_url <- "https://data.healthcare.gov/api/views/b8in-sz6k/rows.csv?accessType=DOWNLOAD"

for (year in 2014:2018) {
  filename <- get(glue("aca{year}_file"))
  url <- get(glue("aca{year}_url"))
  if (!file.exists(filename)) {
    print(glue("Downloading {url} to {filename}"))
    download.file(url, destfile = filename, quiet = 0)
  }
}
