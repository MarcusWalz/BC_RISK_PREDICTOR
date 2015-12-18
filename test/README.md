Test Overview
----------------------------


## Gail89

* Relative risk to absolute risk projections validated by replicating table 4.
  Projections are within 1/10 of a percent, i.e. rounding error.
* Gail89's relative risk calculation examples are approximations designed
  for easy calculation, our code calculates relative risk more accurately 
  using the regression formula. Expect a 2-3% difference in relative risks.

## CAREGail

* Same as Gail89, but using table 3 instead of table 4.

## BCRAT

* Tested by comparing 5 year absolute risks reported on the BCRAT website
  to 5 year absolute risks reported by our code. As of 12/18/15 results are 
  not within the 1/10 percent threshold.
