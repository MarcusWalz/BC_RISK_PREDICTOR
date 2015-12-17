# TODO Use Gail model with constants found in BCRAT source code.


source("../BCRAT.R", chdir = TRUE)
source("Utils.R")

# import test data via google docs
test = read.csv("https://docs.google.com/spreadsheets/d/1d8TWYED1xy5kIxTgrix6u2dtdGbKzmXEk9qmAI0Et9U/pub?gid=959123307&single=true&output=csv")

compare_output(BCRAT, test, 5)
