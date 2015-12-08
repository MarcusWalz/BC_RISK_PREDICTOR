
# AGE = AGE_AT_MENACHE + AGE_AT_FIRST_BIRTH + AGE_AT_MENOPAUSE + AGE
undelta_age = function(population) {

  a0 = round(population$AGE_AT_MENARCHE)
  a1 = round(a0 + population$AGE_AT_FIRST_BIRTH)
  a2 = round(a1 + population$AGE_AT_MENOPAUSE)
  a3 = round(a2 + population$AGE)

  population$AGE_AT_MENARCHE    = a0
  population$AGE_AT_FIRST_BIRTH = ifelse(population$PARITY, a2, 0)
  population$AGE_AT_MENOPAUSE   = ifelse(population$MENOPAUSE_STATUS, a3,0)
  population$AGE                = a3

  population
}


risk_factor = factor("low", "medium", "high")

AGE_FIELD = 
  list( field = "AGE"
      , description = "Patient Age. Numeric"
      , valid = function(AGE) {
        is.numeric(AGE) && all(AGE > 0)
      }
      , risk = function(AGE) {
        ifelse(AGE < 5, "low", "medium")
      }
  )

AGE_AT_MENARCHE_FIELD = 
  list( field = "AGE_AT_MENARCHE"
      , description = "Age at menarche. Numeric."
      , valid = function(AGE_AT_MENARCHE) {
        is.numeric(AGE_AT_MENARCHE)
      }
      , risk = function(AGE) {
        ifelse(AGE < 13, "high", "medium")
      }
      )

AGE_AT_MENOPAUSE_FIELD = 
  list( field = "AGE_AT_MENOPAUSE"
      , description = "Age at menopause. Numeric"
      , valid = function(AGE_AT_MENOPAUSE) {
        is.numeric(AGE_AT_MENOPAUSE)
      }
      , risk = function(AGE_AT_MENOPAUSE) {
        ifelse(AGE_AT_MENOPAUSE, "low", "low")
      }
      )

# Multifield validator, checks that AGE > AGE_AT_MENARCHE
AGE_AND_AGE_AT_MENARCHE_FIELD =
  list( field = c("AGE", "AGE_AT_MENARCHE")
      , description = "AGE should be greater than AGE_AT_MENARCHE."
      , valid = function(AGE, AGE_AT_MENARCHE) {
        all(AGE > AGE_AT_MENARCHE) 
      }
      )

# filter null values in a field
filter_null = function(l) {
  Filter( function(x) !is.null(x), l)
}

binary_to_field = function(name, predicate, risk=NULL) {
  filter_null(
    list( field = name
        , description = paste("1 or T if", predicate, ", 0 or F otherwise.") 
        , valid = function(field) {
          is.logical(field) || all( field == 0 | field == 1) 
        }
        , risk = { 
            if( !is.null(risk) ) {
              function(field) {
                risk[as.numeric(field)]
              }
            } else {
              NULL
            }
          }
        )
  )
}

PARITY_FIELD           = binary_to_field("PARITY", "women has had a child")
MENOPAUSE_STATUS_FIELD = binary_to_field("MENOPAUSE_STATUS", "women has menopause")

factor_to_field = function(name, factor, risk = NULL, allow_na=F) {
  filter_null(
    list( field = name
        , description = paste(
          name, "as a factor, either:", paste(levels(factor), collapse=", ")
          , ifelse(allow_na, "NA.", ".")
        )
        , valid = function(fact) {
          all(fact %in% levels(factor) | (allow_na & (fact == NA)))
        }
        , risk = { 
            if( !is.null(risk) ) {
              function(field) {
                risk[as.numeric(field)]
              }
            } else {
              NULL
            }
          }
        )
  )
}


RACE_FACTOR = factor(c("Asian", "Black", "Hispanic", "White"))

RACE_FIELD = factor_to_field("RACE", RACE_FACTOR)

BC_RISK_FIELDS =
  list( AGE_FIELD
      , AGE_AT_MENARCHE_FIELD
      , AGE_AT_MENOPAUSE_FIELD
      , AGE_AND_AGE_AT_MENARCHE_FIELD
      , RACE_FIELD
      , PARITY_FIELD
      , MENOPAUSE_STATUS_FIELD
      )

BC_RISK_FIELD_DESC = 
  do.call(rbind, Map(function(field) {
      data.frame( row.names      = paste(field$field, collapse=", ")
                , has_validator  = "valid" %in% names(field)
                , is_risk_factor = "risk" %in% names(field)
                , description    = field$description
                )
    }, BC_RISK_FIELDS)
  )

print(BC_RISK_FIELD_DESC)

validate_bc_risk_input = function(population, warn=F) {
  errors = FALSE

  for(field in BC_RISK_FIELDS) {
    if( all(field$field %in% colnames(population)) && "valid" %in% names(field)) {
      tryCatch( 
        {
          is_valid_column = do.call(field$valid, 
            lapply(field$field, function(f) population[, f])
          )
          if( !is_valid_column ) { stop("Column did not pass validator") }
        }
        , error = function(err) {
          write(paste("Input error in"
                     , paste(field$field, collapse=", ")
                     , "--"
                     , field$description), stderr())
          errors <<- TRUE
        })
    }
  }

  if( errors ) {
    ifelse(warn, warning, stop)("bc_risk input format invalid")
  }
  return(!errors)
}

# Construct a random test population that's valid
random_population = function(n = 1000) {
  AGE_AT_MENARCHE = rnorm(n, 13, 1)
  AGE_AT_FIRST_BIRTH = abs(rnorm(n, 10, 4))
  AGE_AT_MENOPAUSE = abs(rnorm(n, 15, 4))
  AGE = abs(rnorm(n, 5, 4))

  PARITY = sample(c(0,1), n, replace=T, c(0.3,0.7))
  MENOPAUSE_STATUS = sample(c(0,1), n, replace=T, c(0.3,0.7))
  
  RACE   = as.factor(sample(levels(RACE_FACTOR), n, replace=T))
  BIOPSY = sample(c(1, 2, 3), n, replace=T)

  undelta_age(
    data.frame( AGE
              , AGE_AT_MENARCHE
              , AGE_AT_FIRST_BIRTH
              , AGE_AT_MENOPAUSE
              , PARITY
              , MENOPAUSE_STATUS
              , RACE
              , BIOPSY )
    )
}

#random_population(300)
validate_bc_risk_input(random_population(300))
