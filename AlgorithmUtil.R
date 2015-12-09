
has_required_fields = function(alg_name, input, required_fields) {
    has_fields = required_fields %in% colnames(input)
    if(!all(has_fields) ) {
      stop(paste("Can't execute", alg_name, "input missing field(s):", paste(required_fields[!has_fields], collapse=", ")))
    }
}
