# Breast Cancer Risk Prediction Alogrithms


Input Format
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
  <dd>Age patient had first child. `0` or `NA` if patient has no children. Numeric</dd>

  <dt>PARITY</dt>
  <dd>`1` or `TRUE` if patient has had a child, `0` or `FALSE` otherwise. 
  If `PARITY` is absent, it will be constructed from `AGE_AT_FIRST_BIRTH`, s.t. 
  if `AGE_AT_FIRST_BIRTH` is `0` or `NA` then `PARITY` is set to `FALSE`,
  otherwise `PARITY` is set to `TRUE`.

  <dt>AGE_AT_MENOPAUSE</dt>
  <dd>Age patient underwent meonpause. `0` if patient has not undergone menopause. Numeric</dd>


  <dt>MENOPAUSE_STATUS<dt>
  <dd>`1` or `TRUE` if patient has had menopause, `0` or `FALSE` otherwise. Can be
  derived from `AGE_AT_MENOPAUSE`.</dd>

  <dt>RACE</dt>
  <dd>Either `White`, `Black`, `Hispanic`, or `Asian`.</dd>

  <dt>Biopsy<dt>
  <dd>Number of breast biopsy's a patient has had. Either numeric or `"0"`, `"1"` `">=2"`.</dd>

</dl>
