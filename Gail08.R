source("GailAlgorithm.R")

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

# TODO change to BCRAT
Gail08 = function(avatars) gail_algorithm(avatars, Gail89, gail_snp_rr)
