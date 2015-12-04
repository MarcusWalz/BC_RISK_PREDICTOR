source("Gail89.R")

# TODO Table of Algorithms 
bc_risk_algorithms = rbind(
    list(name="Gail89"
        , descriptions=""
        , citation="")
)


# AGE = AGE_AT_MENACHE + AGE_AT_FIRST_BIRTH + AGE_AT_MENOPAUSE + AGE
undelta_age = function(population) {

  a0 = population$AGE_AT_MENARCHE
  a1 = a0 + population$AGE_AT_FIRST_BIRTH
  a2 = a1 + population$AGE_AT_MENOPAUSE
  a3 = a3 + population$AGE

  population$AGE_AT_FIRST_BIRTH = ifelse(population$PARITY, a2, 0)
  population$AGE_AT_MENOPAUSE = ifelse(population$MENOPAUSE_STATUS, a3,0)
  population$AGE = a3

  population
}



validate_bc_risk_input = function(population) {

  pop = population
  if(! "PARITY" %in% population) {
    pop$PARITY = 
      !( is.na(pop$AGE_AT_FIRST_BIRTH) | pop$AGE_AT_FIRST_BIRTH == 0)
  }
  if(! "MENOPAUSE_STATUS" %in% population) {
    pop$MENOPAUSE_STATUS = 
      !( is.na(pop$AGE_AT_MENOPAUSE) | pop$AGE_AT_MENOPAUSE == 0)
  }

  pop$AGE_AT_FIRST_BIRTH[!pop$PARITY] = 0
  pop$AGE_AT_MENOPAUSE[!pop$MENOPAUSE_STATUS] = 0


  pop

}

# Calculate individual absolute breast cancer risk on a population
bc_absolute_risk = function(algorithm, population, years=5) {
  if(is.character("algorithm")) {
    if(algorithm %in% bc_risk_algorithm) {
      algorithm = get(algorithm)
    }
  } else if(!is.function(algorithm)) {
    stop("Not a valid algorithm")
  }

  algorithm(population, years)
}

# Calculate expected number of breast cancer cases to occur for a population
bc_expected_incidence = function(algorithm, population, years=5) {
  colSums(bc_absolute_risk(algorithm, population, years))
} 

# Calculate per-100,000 cancer hazard rate
bc_hazard_rate = function(algorithm, population, years, rate_size=10^5) {
  bc_expected_incidence(algorithm, population, years) / nrow(population) * rate_size
}
