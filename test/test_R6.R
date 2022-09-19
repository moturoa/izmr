

#library(izmr)
devtools::load_all()

# .pdb <- pseudoData$new(
#   filename = "c:/repos/ede/izm_frontend/data/ede_izm_postgres_copy.sqlite"
# )

.pdb <- pseudoData$new(
  config_file = "c:/repos/ede_izm_frontend/conf/config.yml",
  schema = "pseudodata"
)


# zelfde persoon
.pdb$get_person_brp("QlJRetmqc", what = "anr")
.pdb$get_person_brp("KRqBpXnO3", what = "bsn")


# niemand
.pdb$get_person_brp("filvbdfivudfbvdf", what = "bsn")


# familie
.pdb$get_family("QlJRetmqc", what = "anr")


# mensen op adres (inc. verhuisd/overleden!)
p <- .pdb$get_person_brp("y6EgMcHfG")

.pdb$get_person_brp(what = "adres", adres = p)




