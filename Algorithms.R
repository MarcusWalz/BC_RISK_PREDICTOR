source("Data.R") # For validation functions

# Global variable with list of algorithms
bc_risk_algorithms = list()

add_algorithm = function(name, f, description="") {
  bc_risk_algorithms[[name]] = list(name=name, description=description, func=f)
  bc_risk_algorithms <<- bc_risk_algorithms
}

source("Gail89.R")
add_algorithm("Gail89", Gail89, "")
source("CAREGail.R") 
add_algorithm("CAREGail", CAREGail, "")
source("Rosner96.R")
add_algorithm("Rosner96", Rosner96, "")

# print(bc_risk_algorithms)

# list installed algorithms
list_algorithms = function() {
  names(bc_risk_algorithms)
}

# Calculate individual absolute breast cancer risk on a population
bc_absolute_risk = function(algorithm, population, years=5) {
  if(is.character(algorithm) && algorithm %in% list_algorithms()) {
      algorithm = bc_risk_algorithms[[algorithm]]$func
  }

  if(!is.function(algorithm)) {
    stop(paste("bc_risk algorithm:", algorithm, "not found"))
  }

  do.call(algorithm, list(population=population, years=years))
}

# Calculate expected number of breast cancer cases to occur for a population
bc_expected_incidence = function(algorithm, population, years=5) {
  colSums(bc_absolute_risk(algorithm, population, years))
} 

# Calculate per-100,000 cancer hazard rate
bc_hazard_rate = function(algorithm, population, years, rate_size=10^5) {
  bc_expected_incidence(algorithm, population, years) / nrow(population) * rate_size
}
