# Breast Cancer Risk Prediction Alogrithms

Let `my_population` be a dataframe in the format described [below](#iformat).

Calculate indivdual 5, 10, 20 year absolute risk of developing breast cancer for a population (TODO):

    per_patient_risk = bc_risk("Gail89", my_population, years=c(5,10,20))

Calculate the 5 year per 100,000 breast cancer incidence rate of a population according to the Gail89 model (TODO):

    expected_cases = hazard_rate("Gail89", my_population, years=5)

Supported Algorithms 
-------------------

* Gail89 --- TODO
* Tice08 --- TODO

Input Format <a name="iformat"></a>
-------------------

All algorithms are designed to use the same input format. However, not all
variables are required for every algorithm. Each Breast Cancer Risk algorithm 
checks that the required variables are supplied prior to execution. 

The function `preprocess_input(input)` (todo) should be used to validate input and infer
missing variables:

1. Ensures `AGE_AT_MENACHE` &lt; `AGE_AT_FIRST_BRITH` &lt; `AGE_AT_MENOPAUSE`
  &lt; `AGE`.
2. Constructs `PARITY` and `MENOPAUSE_STATUS` if absent, or checks that `AGE_AT_FIRST_BIRTH` 
  and `AGE_AT_MENOPAUSE` are consistent with `PARITY` and `MENOPAUSE_STATUS`.
3. Converts `NA`s in `AGE_AT_FIRST_BIRTH` and `AGE_AT_MENOPAUSE` to `0`s. 
4. Checks, all variables are of the correct type.

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
