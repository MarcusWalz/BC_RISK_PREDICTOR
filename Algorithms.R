
source("Gail89.R")
source("Tice08.R")

# TODO Table of Algorithms 
breast_cancer_risk_algorithms = rbind(
    list(name="Gail89", descriptions="", citation="")
# , list(name=" ",
)

# TODO Function that runs multiple algorithms at once
run_algorithms = function(table, avatars, years=5) {

 # prefix colnames with algorithm name, bind everything togother.
}


run_all_algorithms = function(avatars, years) {
  run_algorithms(breast_cancer_risk_algorithms, avatars, years)
}


