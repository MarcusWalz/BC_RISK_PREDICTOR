source("Data.R")           # For validation functions
source("AlgorithmUtils.R") # For algorithm lookup/registation functions

# Source all algorithms here!
source("Gail89.R")
source("CAREGail.R") 
source("Rosner96.R")

# print(bc_risk_algorithms)

# list installed algorithms
list_algorithms = function() {
  names(bc_risk_algorithms)
}

# Calculate individual absolute breast cancer risk on a population
bc_absolute_risk = function(algorithm, population, years=5, aux_params = list()) {
  if(is.character(algorithm) && algorithm %in% list_algorithms()) {
      # get the algorithm
      algorithm_f = get_algorithm(algorithm)$func # bc_risk_algorithms[[algorithm]]$func
      # ensure algorithm has required fields
      check_required_fields(algorithm, population)
  } else {
    stop(paste("bc risk algorithm:", algorithm, "not found"))
  }

  do.call( algorithm_f
         , append(list(population=population, years=years), aux_params)
         )
}

# Calculate expected number of breast cancer cases to occur for a population
bc_expected_incidence = function( algorithm, population
                                , years=5, aux_params=list()) {
  colSums(bc_absolute_risk(algorithm, population, years, aux_params()))
} 

# Calculate per-100,000 cancer hazard rate
bc_hazard_rate = function( algorithm, population, years
                         , rate_size=10^5, aux_params=list()) {
  bc_expected_incidence(algorithm, population, years, aux_params()) / nrow(population) * rate_size
}
