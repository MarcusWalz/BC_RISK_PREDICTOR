# Breast Cancer Risk Prediction Alogrithms

Let `my_population` be a dataframe in the format described [below](#input-format).

Predict indivdual 5, 10, 20 year absolute risks of developing breast cancer for a population:

    per_patient_risk = bc_absolute_risk("Gail89", my_population, years=c(5,10,20))

Predict the expected number of breast cancer cases in `my_population` over a 10 year period using Rosner96:

    incidents = bc_expected_incidents("Rosner96", my_population, years=10)

Predict the 5 year per-100,000 breast cancer incidence rate of `my_population` using the Gail89 algorithm:

    hazard_rate = bc_hazard_rate("Gail89", my_population, years=5)


Supported Algorithms 
-------------------

* Gail89 --- TODO
* Tice08 --- TODO

Input Format
-------------------

All algorithms are designed to use the same input format. However, not all
variables are required for every algorithm. Each Breast Cancer Risk algorithm 
checks that the required variables are supplied prior to execution. 

The function `preprocess_population(population)` does the following:

1. Constructs `PARITY` and `MENOPAUSE_STATUS` if absent, or checks that `AGE_AT_FIRST_BIRTH` 
  and `AGE_AT_MENOPAUSE` are consistent with `PARITY` and `MENOPAUSE_STATUS`.
2. Converts `NA`s in `AGE_AT_FIRST_BIRTH` and `AGE_AT_MENOPAUSE` to `0`s. 

The function `validate_population(population)` ensures that:

1. Input is in the correct format.
2. Members of a population are logical; e.g. `AGE` is
  greater than or equal `AGE_AT_FIRST_BIRTH`.


<dl>
  <dt>AGE</dt>
  <dd>Current Age of Patient. Numeric</dd>

  <dt>AGE_AT_MENARCHE</dt>
  <dd>Age patient had menache. Numeric</dd>

  <dt>AGE_AT_FIRST_BIRTH</dt>
  <dd>Age patient had first child. 
  <code>0</code> or <code>NA</code> if patient has no children. Numeric
  </dd>

  <dt>PARITY</dt>
  <dd><code>1</code> or <code>TRUE</code> if patient has had a child, <code>0</code> or <code>FALSE</code> otherwise. 
  If <code>PARITY</code> is absent, it will be constructed from <code>AGE_AT_FIRST_BIRTH</code>, s.t. 
  if <code>AGE_AT_FIRST_BIRTH</code> is <code>0</code> or <code>NA</code>
  then <code>PARITY</code> is set to <code>FALSE</code>, otherwise <code>PARITY</code> is set to <code>TRUE</code>.

  <dt>AGE_AT_MENOPAUSE</dt>
  <dd>Age patient underwent meonpause. <code>0</code> or <code>NA</code>
  if patient has not undergone menopause yet. Numeric.  </dd>


  <dt>MENOPAUSE_STATUS<dt>
  <dd><code>1</code> or <code>TRUE</code> if patient has had menopause,
    <code>0</code> or <code>FALSE</code> otherwise. Can be derived from <code>AGE_AT_MENOPAUSE</code>.</dd>

  <dt>RACE</dt>
  <dd>Either <code>White</code>, <code>Black</code>, <code>Hispanic</code>, or <code>Asian</code>.</dd>

  <dt>Biopsy<dt>
  <dd>Number of breast biopsy's a patient has had. Either numeric or <code>"0"</code>, <code>"1"</code>, 
    <code>">=2"</code>.
  </dd>

</dl>

Creating Algorithms
-------------------------------

At minimum, a breast cancer risk algorithm is an R function which takes two
arguments, `population` a dataframe in the format described above and `years`
a vector of how many years in the future you with to project absolute risk.
E.g. `years = 5` will calculate the 5 year absolute risk, and `years = c(5,10)`
will calculate the 5 and 10 year absolute risk. Additional arguments can
be provided as long as default values are set.

An algorithm needs to return a matrix or dataframe with named columns. Where `RR` 
is the patients current relative risk, `AR 5` is the five absolute risk, and `AR 10`
is the ten year asolute risk. To prevent R from reducing a matrix to vector
remember to subset using: `[,,reduce=F]`. Additional columns can be provided.

If, for whatever reason it's impossible to calculate absolute risk for a patient
fire off a `warning()` explaining why and return `NA`. 

In addition, we use the function `register_algorithm` in order to bind metadata to
the algorithm which allows it to be used by helper functions in `Algorithms.R`. 
These helper functions check that the input is sufficient and that algorithms
produce the desired output before executing an algorithm.

Here is a simple algorithm:

```R
source("AlgorithmUtils.R")

my_algorithm = function(population, years) {
  ... # let RR be a vector of relative risks and
      # let ARs be a matrix of absolute risks
  # set colnames
  colnames(ARs) = paste("AR", years)
  cbind(RR, ARs)
}

register_algorithm("my_algorirthm" the name you wish to call the alg from
                  , my_algorithm,  the risk alg function itself
                  , AR = TRUE  # TRUE iff risk alg returns absolute risks
                  , RR = TRUE  # TRUE iff risk alg returns relative risks
                  , req_fields = c("AGE", "BIOPSY") # Fields requierd to use alg
                  )

and running:

```R
> my_algorithm(my_popualtion, c(5,10))
```

will produce a table like this:

|  | RR | AR 5 | AR 10  |
|--|----|------|--------|
|1 |  3 |  .30 |  .4    |
|2 |  4 |  .28 |  .8    |


```
