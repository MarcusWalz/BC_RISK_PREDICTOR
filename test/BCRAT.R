# TODO Use Gail model with constants found in BCRAT source code.

source("../BCRAT.R", chdir = TRUE)
source("Utils.R")

print("hello")
test = data.frame( AGE = c(40, 45, 35, 43, 35, 55)
, AGE_AT_MENARCHE = c(14, 9, 12, 15, 14, 12)
, AGE_AT_FIRST_BIRTH = c(25, 35, 30, 22, 27, 25)
, FIRST_DEGREE_RELATIVES = c(2, 2, 1, 0, 1, 2)
, BIOPSY = c(0, 2, 1, 0, 1, 1)
, PARITY = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)
, RACE = c("White", "White", "White", "White", "White", "White")
, HYPERPLASIA = c(NA,NA,NA,NA, TRUE, FALSE)
, TEST_AR_5 = c(.018, 0.132, 0.018, 0.006, 0.016, 0.048)
)

compare_output(BCRAT, test, 5)
