# enusures that bc_risk_algorithm exists
if(!exists("bc_risk_algorithms")) {
  bc_risk_algorithms <<- list()
}
# Register an algorithm:
# name: algorithm name
# ar: TRUE if algorithm reports absolute risk
# rr: TRUE if algorithm reports relative risk
# req_fields: vector of required fields
register_algorithm = function(name, f, ar, rr, req_fields) {
  bc_risk_algorithms[[name]] <<- list(
    name=name, func=f, ar=ar, rr=rr, req_fields=req_fields
  )
}

get_algorithm = function(name) {
  if(! name %in% bc_risk_algorithms) {
    stop("bc risk algorithm", name, "not registered")
  }

  bc_risk_algorithms[[name]]
}

check_required_fields = function(name, input) {
    required_algorithm=get_algorithm(name)$req_fields

    has_fields = required_fields %in% colnames(input)
    if(!all(has_fields) ) {
      stop(paste("Can't execute"
                , name
                , "input missing field(s):"
                , paste(required_fields[!has_fields], collapse=", ")))
    }
}
