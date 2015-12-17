source("AlgorithmUtil.R")
source("BCRAT.R")

gail_snp_rr = function(avatars) {
  snp = function(snp_name, rr) {
    if( snp_name %in% colnames(avatars)) {
      return(rr^(avatars[,snp_name]))
    } else {
      return(1)
    }
  }
  
  # TODO CHECK NUMBERS / NAMES
  return(
    snp("RS2981582",  1.26)
  * snp("RS3803662",  1.20)
  * snp("RS889312" ,  1.13)
  * snp("RS3817198",  1.07)
  * snp("RS13281615", 1.08)
  * snp("RS13387042", 1.20)
  * snp("RS104585",   0.88) # protective snp
  )
}

Gail08 = function(population, years) {
  gail_algorithm(population, years, aux_rr=gail_snp_rr)
}

register_algorithm("Gail08", Gail08, T, T, append(gail_fields, "RACE"))
