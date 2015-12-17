source("Data.R")           # For validation functions
source("AlgorithmUtil.R") # For algorithm lookup/registation functions

# Source all algorithms here!
source("Gail89.R")
source("Gail08.R")
source("BCRAT.R")

source("CAREGail.R") 
source("Rosner96.R")

# print(bc_risk_algorithms)

# list installed algorithms
list_algorithms = function() {
  names(bc_risk_algorithms)
}

bc_risk_algorithm = function(algorithm, population, years=5, filter_output="*", aux_params = list()) {
  if(is.character(algorithm) && algorithm %in% list_algorithms()) {
      # get the algorithm
      algorithm_f = get_algorithm(algorithm)$func # bc_risk_algorithms[[algorithm]]$func
      # ensure algorithm has required fields
      check_required_fields(algorithm, population)
  } else {
    stop(paste("bc risk algorithm:", algorithm, "not found"))
  }

  out = do.call( algorithm_f
               , append(list(population=population, years=years), aux_params)
               )

  # return only absolute risk and/or relative risk
  if(filter_output == "AR") {
    filter_output = "^AR [1-9][0-9]?$"
  } else if(filter_output == "RR") {
    filter_output = "^RR$"
  } else if(filter_output == "AR+RR" || filter_output == "RR+AR") {
    filter_output = "^(AR [1-9][0-9]?)|(RR)$"
  }

  # filter columns by regex 
  out=out[,grep(filter_output, colnames(out)),drop=FALSE]

  if(ncol(out) == 0) {
    stop(paste("algorithm:", algorithm,  "does not return anything matching", filter_output))
  }

  out
}

# Calculate individual absolute breast cancer risk on a population
bc_absolute_risk = function(algorithm, population, years=5, aux_params=list()) {
  if(!get_algorithm(algorithm)$ar) {
    stop("Algorithm ", algorithm, "does not output absolute risk")
  }
  bc_risk_algorithm(algorithm, population, years, filter_output="AR", aux_params)
}

# Calculate expected number of breast cancer cases to occur for a population
bc_expected_incidence = function( algorithm, population
                                , years=5, aux_params=list()) {
  colSums(bc_absolute_risk(algorithm, population, years, aux_params))
} 

# Calculate per-100,000 cancer hazard rate
bc_hazard_rate = function( algorithm, population, years
                         , rate_size=10^5, aux_params=list()) {
  bc_expected_incidence(algorithm, population, years, aux_params) / nrow(population) * rate_size
}

list_algorithms()
