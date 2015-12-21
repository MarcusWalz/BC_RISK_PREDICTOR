source("../Algorithms.R", chdir=T)

# Compares output to values in dataframe. Numeric only!
# TEST_X column input dataframe will be compared to the
# column X in the output dataframe.
# 
# digits = number of decimal places to round output

compare_output = function(algorithm, df, years = 5, digits=3, aux_params = list()) {
  if(is.function(algorithm)) {
    output =
      do.call(algorithm
        , append(list(population = df, years = years), aux_params)
      )
  } else {
    output = bc_risk_algorithm(algorithm, df, years, aux_params)
  }

  # vector of columns with test data
  test_columns = 
    gsub("TEST_", "", colnames(df)[grep("^TEST_", colnames(df))])

  for(column in test_columns) {
    test_c = df[,paste("TEST", column, sep="_")]
    out_c = output[,column]

    print(column)
    compare = data.frame(validation = test_c, output = round(out_c,digits))
    compare$diff = compare$validation - compare$output
    print(compare)

    
    print(paste("Max  abs diff:", max( abs(compare$diff))))
    print(paste("Mean abs diff:", mean(abs(compare$diff))))
  }
}
