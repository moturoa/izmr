



library(shiny)
library(jsonlite)
library(glue)
library(curl)
library(dplyr)
library(DT)

library(futile.logger)
library(DBI)
library(pool)

source("R/izmSearchModule.R")
source("R/voorbeeldModule.R")
source("R/depseudoModule.R")
source("R/pm_decrypt.R")
source("R/parse_depseudo_result.R")
source("R/utils.R")



source("R/pseudoData.R")

.pdb <- pseudoData$new(
  filename = "c:/repos/ede/izm_frontend/data/ede_izm_postgres_copy.sqlite"
)


h <- .pdb$query("select * from bzsprsq00 limit 100")


# zelfde persoon
.pdb$get_person_brp("QlJRetmqc", what = "anr")
.pdb$get_person_brp("KRqBpXnO3", what = "bsn")


# niemand
.pdb$get_person_brp("filvbdfivudfbvdf", what = "bsn")


# familie
.pdb$get_family("QlJRetmqc", what = "anr")


p <- .pdb$get_person_brp("y6EgMcHfG")

.pdb$get_person_brp(what = "adres", adres = list(
  postcode = p$vblpostcode,
  huisnummer = p$vblhuisnummer,
  huisletter = p$vblhuisletter,
  huisnummertoevoeging = p$vblhuisnummertoevoeging
))





