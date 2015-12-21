# preprocess_population handles the redundent variables PARITY and
# MENOPAUSE status so they're consistent with AGE_AT_FIRST_BIRTH and 
#   AGE_AT_MENOPAUSE.
preprocess_population = function(population) {
 
  for(column in c("PARITY", "MENOPAUSE_STATUS")) {
    age_column = ifelse(column == "PARITY", "AGE_AT_FIRST_BIRTH", "AGE_AT_MENOPAUSE")

    # Check to make sure column is actually present.
    if(!(age_column %in% colnames(population))) {
      warning(paste(column, "not in input population."))
      next
    }

    # 1st Make Sure that AGE_AT_FIRST_BIRTH and AGE_AT_MENOPAUSE conform 
    #   to PARITY and MENOPAUSE_STATUS if they're present:
    if(column %in% colnames(population)) {
      # make parity trues and falses 
      if(!is.logical(population[,column])) {
        population[,column] = population[,column] != 0
      }

      population[!population[,column], column] = 0
    } 

    # 2nd set NA's to 0  
    population[is.na(population[,age_column]),age_column] = 0


    # 3rd set parity to false if AGE_AT_FIRST_BIRTH or AGE_AT_MENOPAUSE are zero
    population[,column] = population[,age_column] != 0
  }

  # Ensure HYPERPLASIA is NA when BIOPSY is 0!
  if(all(c("BIOPSY", "HYPERPLASIA") %in% colnames(population))) {
    old_hyperplasia == population$HYPERPLASIA
    population$HYPERPLASIA[population$BIOPSY == 0] = NA

    if(!all(old_hypeplasia == population$HYPERPLASIA)) {
      warning("HYPERPLASIA was non-na where BIOPSY=0")
    }
  }

  # Convert old numeric BI-RAD density values into a,b,c,d format
  if("DENSITY" %in% colnames(population) & is.numeric(population)) {
    warning('HYPERPLASIA is numeric, should be of type
            factor("a","b","c","d")')
    population$DENSITY = as.factor(population$DENSITY)
    levels(population$DENSITY) <- c("a","b","c","d")
  }

  population
}

# Assume AGES are offset s.t.:
# AGE = AGE_AT_MENACHE + AGE_AT_FIRST_BIRTH + AGE_AT_MENOPAUSE + AGE
# Helpful for generating random avatars.
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

# Below are column characteristcs, it's a named list with
# the following values:
#   column - name of column(s)
#   valid - validation function where input params are same as column
#     returns descriptive error using stop(...) or False if there is an issue
#   risk  - risk_factor calculator where input pararms are same as column
#     returns either "low", "medium", or "high". This is used as a heuristic
#     to help validate algoritm performance.
#
# Sometimes the realtionship between two columns needs to be
# checked, (e.g. ensursing age is greater than age at menarche)
# in which case column is a vector of strings (i.e. characters in R)
# and valid and risk functions recieve. See AGE_AND_AGE_AT_MENARCHE for an
# example.

# valid and risk functions are only executed if all columns are present.
#

AGE_COLUMN = 
  list( column = "AGE"
      , description = "Patient Age. Numeric"
      , valid = function(AGE) {
        is.numeric(AGE) && all(AGE > 0)
      }
      , risk = function(AGE) {
        ifelse(AGE < 5, "low", "medium")
      }
  )

AGE_AT_MENARCHE_COLUMN = 
  list( column = "AGE_AT_MENARCHE"
      , description = "Age at menarche. Numeric."
      , valid = function(AGE_AT_MENARCHE) {
        is.numeric(AGE_AT_MENARCHE)
      }
      , risk = function(AGE) {
        ifelse(AGE < 13, "high", "medium")
      }
      )

BIOPSY_COLUMN =
  list( column = "BIOPSY"
        , description = "BIOPSY. Numeric or 0, 1, >=2"
        , valid = function(BIOPSY) {
            all(is.numeric(BIOPSY ) | BIOPSY == "0" | BIOPSY == "1" | BIOPSY == ">=2")
        }
        , risk = function(BIOPSY) {
          ifelse(BIOPSY == ">=2", "high", "low")
        }
  )

HYPERPLASIA_COLUMN = 
  list( column = "HYPERPLASIA"
        , description = "HYPERPLASIA. true, false, NA"
        , valid = function(HYPERPLASIA) {
          all(is.na(HYPERPLASIA) | isTRUE(HYPERPLASIA) | identical(HYPERPLASIA, FALSE))
        }, risk = function(HYPERPLASIA) {
          ifelse(isTRUE(HYPERPLASIA), "high", "low")
        }
  )

DENSITY_COLUMN =
  list( column = "DENSITY"
        , description = "DENSITY. a, b, c, d"
        , valid = function(DENSITY) {
            all(DENSITY == "a" | DENSITY == "b" | DENSITY == "c"  | DENSITY == "d")
          }
          , risk = function(DENSITY) {
            ifelse(DENSITY == "d", "high", "low")
          }
  )

FIRST_DEGREE_RELATIVES_COLUMN =
  list( column = "FIRST_DEGREE_RELATIVES"
        , description = "FIRST_DEGREE_RELATIVES. numeric"
        , valid = function(FIRST_DEGREE_RELATIVES) {
          all(is.numeric(FIRST_DEGREE_RELATIVES) && FIRST_DEGREE_RELATIVES >= 0)
        }
        , risk = function(FIRST_DEGREE_RELATIVES) {
          ifelse(FIRST_DEGREE_RELATIVES > 1 , "high", "low")
        }
  )

AGE_AT_FIRST_BIRTH_COLUMN = 
  list( column = "AGE_AT_FIRST_BIRTH"
        , description = "Age at first birth. Numeric."
        , valid = function(AGE_AT_FIRST_BIRTH) {
          is.numeric(AGE_AT_FIRST_BIRTH) && all(AGE_AT_FIRST_BIRTH >= 0)
        }
        , risk = function(AGE_AT_FIRST_BIRTH) {
          ifelse(AGE_AT_FIRST_BIRTH > 35, "high", "low")
        }
  )

AGE_AT_MENOPAUSE_COLUMN = 
  list( column = "AGE_AT_MENOPAUSE"
      , description = "Age at menopause. Numeric"
      , valid = function(AGE_AT_MENOPAUSE) {
        is.numeric(AGE_AT_MENOPAUSE)
      }
      , risk = function(AGE_AT_MENOPAUSE) {
        ifelse(AGE_AT_MENOPAUSE, "low", "low")
      }
      )

# Multicolumn validator, checks that AGE > AGE_AT_MENARCHE
AGE_AND_AGE_AT_MENARCHE_COLUMN =
  list( column = c("AGE", "AGE_AT_MENARCHE")
      , description = "AGE should be greater than AGE_AT_MENARCHE."
      , valid = function(AGE, AGE_AT_MENARCHE) {
        all(AGE > AGE_AT_MENARCHE) 
      }
      )

AGE_AND_AGE_AT_FIRST_BIRTH =
  list( column = c("AGE", "AGE_AT_FIRST_BIRTH")
        , description = "AGE should be greater than or equal to AGE_AT_FIRST_BIRTH."
        , valid = function(AGE, AGE_AT_FIRST_BIRTH) {
          all(AGE >= AGE_AT_FIRST_BIRTH) 
        }
  )

AGE_AT_FIRST_BIRTH_AND_AGE_AT_MENARCHE_COLUMN =
  list( column = c("AGE_AT_FIRST_BIRTH", "AGE_AT_MENARCHE")
        , description = "AGE_AT_FIRST_BIRTH should be greater than AGE_AT_MENARCHE."
        , valid = function(AGE_AT_FIRST_BIRTH, AGE_AT_MENARCHE) {
          all(AGE_AT_FIRST_BIRTH > AGE_AT_MENARCHE | AGE_AT_FIRST_BIRTH == 0) 
        }
  )

AGE_AND_AGE_AT_MENOPAUSE =
  list( column = c("AGE", "AGE_AT_MENOPAUSE")
        , description = "AGE should be greater than or equal to AGE_AT_MENOPAUSE"
        , valid = function(AGE, AGE_AT_MENOPAUSE) {
          all(AGE >= AGE_AT_MENOPAUSE) 
        }
  )

AGE_AT_MENOPAUSE_AND_AGE_AT_MENARCHE_COLUMN =
  list( column = c("AGE_AT_MENOPAUSE", "AGE_AT_MENARCHE")
        , description = "AGE_AT_MENOPAUSE should be greater than AGE_AT_MENARCHE."
        , valid = function(AGE_AT_MENOPAUSE, AGE_AT_MENARCHE) {
          all(AGE_AT_MENOPAUSE > AGE_AT_MENARCHE | AGE_AT_MENOPAUSE == 0) 
        }
  )

# filter null values in a column
filter_null = function(l) {
  Filter( function(x) !is.null(x), l)
}

binary_to_column = function(name, predicate, risk=NULL) {
  filter_null(
    list( column = name
        , description = paste("1 or T if", predicate, ", 0 or F otherwise.") 
        , valid = function(column) {
          is.logical(column) || all( column == 0 | column == 1) 
        }
        , risk = { 
            if( !is.null(risk) ) {
              function(column) {
                risk[as.numeric(column)]
              }
            } else {
              NULL
            }
          }
        )
  )
}

PARITY_COLUMN           = binary_to_column("PARITY", "women has had a child")
MENOPAUSE_STATUS_COLUMN = binary_to_column("MENOPAUSE_STATUS", "women has menopause")

factor_to_column = function(name, factor, risk = NULL, allow_na=F) {
  filter_null(
    list( column = name
        , description = paste(
          name, "as a factor, either:", paste(levels(factor), collapse=", ")
          , ifelse(allow_na, "NA.", ".")
        )
        , valid = function(fact) {
          all(fact %in% levels(factor) | (allow_na & (fact == NA)))
        }
        , risk = { 
            if( !is.null(risk) ) {
              function(column) {
                risk[as.numeric(column)]
              }
            } else {
              NULL
            }
          }
        )
  )
}


RACE_FACTOR = factor(c("Asian", "Black", "Hispanic", "White"))

RACE_COLUMN = factor_to_column("RACE", RACE_FACTOR)

Validate_to_rs_column = function(name, discriptions) {
  filter_null(
    list( column = name
          , description = discriptions
          , valid = function(column) {
            all( column == "0" | column == "1" | column == "2") 
          }
    )
  )
}


rs2981582_COLUMN = Validate_to_rs_column("rs2981582", "rs2981582. 0, 1, 2")
rs3803662_COLUMN = Validate_to_rs_column("rs3803662", "rs3803662. 0, 1, 2")
rs889312_COLUMN  = Validate_to_rs_column("rs889312", "rs889312. 0, 1, 2")
rs3817198_COLUMN = Validate_to_rs_column("rs3817198", "rs3817198. 0, 1, 2")
rs13281615_COLUMN = Validate_to_rs_column("rs13281615", "rs13281615. 0, 1, 2")
rs13387042_COLUMN = Validate_to_rs_column("rs13387042", "rs13387042. 0, 1, 2")
rs1045485_COLUMN = Validate_to_rs_column("rs1045485", "rs1045485. 0, 1, 2")


BC_RISK_COLUMNS =
  list( AGE_COLUMN
      , AGE_AT_MENARCHE_COLUMN
      , AGE_AT_FIRST_BIRTH
      , AGE_AT_MENOPAUSE_COLUMN
      , AGE_AND_AGE_AT_MENARCHE_COLUMN
      , RACE_COLUMN
      , PARITY_COLUMN
      , MENOPAUSE_STATUS_COLUMN
      , Biopsy_COLUMN
      , HYPERPLASIA_COLUMN
      )

BC_RISK_COLUMN_DESC = 
  do.call(rbind, Map(function(column) {
      data.frame( row.names      = paste(column$column, collapse=", ")
                , has_validator  = "valid" %in% names(column)
                , is_risk_factor = "risk" %in% names(column)
                , description    = column$description
                )
    }, BC_RISK_COLUMNS)
  )

print(BC_RISK_COLUMN_DESC)


# Ensures that a population complies with input format, 
#   if not helpful messages are printed to stderr.
# Stops execution, if an issue is discovered, unless warn=TRUE, in which case
# a warning is issued and the function returns False when an error is detected.

validate_population = function(population, warn=F) {
  errors = FALSE

  for(column in BC_RISK_COLUMNS) {
    if( all(column$column %in% colnames(population)) && "valid" %in% names(column)) {
      tryCatch( 
        {
          is_valid_column = do.call(column$valid, 
            lapply(column$column, function(f) population[, f])
          )
          if( !is_valid_column ) { stop("Column did not pass validator") }
        }
        , error = function(err) {
          write(paste("Input error in"
                     , paste(column$column, collapse=", ")
                     , "--"
                     , column$description), stderr())
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
              , BIOPSY
              , HYPERPLASIA)
    )
}

# preprocess_population(random_population(300))
# validate_population(random_population(300))
