# generics, rf

    Code
      print(ad.rf)
    Output
      
      Call:
      fairadapt(formula = y ~ ., prot.attr = "a", adj.mat = adj.mat, 
          train.data = train, test.data = test)
      
      
      Adapting variables:
        y, x
      
      Based on protected attribute a 
      
        AND
      
      Based on causal graph:
        y a x z
      y 0 0 0 0
      a 0 0 1 0
      x 1 0 0 0
      z 1 1 1 0
      

---

    Code
      summary(ad.rf)
    Warning <simpleWarning>
      argument is not numeric or logical: returning NA
      argument is not numeric or logical: returning NA
      argument is not numeric or logical: returning NA
      argument is not numeric or logical: returning NA
    Output
      
      Call:
      NULL
      
      Protected attribute:                 a
      Protected attribute levels:          0, 1
      Adapted variables:                   y, x
      
      Number of training samples:          100
      Number of test samples:              100
      Quantile method:                     rangerQuants
      
      Total variation (before adaptation): NA
      Total variation (after adaptation):  NA

# generics, linear

    Code
      print(ad.lin)
    Output
      
      Call:
      fairadapt(formula = y ~ ., prot.attr = "a", adj.mat = adj.mat, 
          train.data = train, test.data = test, quant.method = linearQuants)
      
      
      Adapting variables:
        y, x
      
      Based on protected attribute a 
      
        AND
      
      Based on causal graph:
        y a x z
      y 0 0 0 0
      a 0 0 1 0
      x 1 0 0 0
      z 1 1 1 0
      

---

    Code
      summary(ad.lin)
    Warning <simpleWarning>
      argument is not numeric or logical: returning NA
      argument is not numeric or logical: returning NA
      argument is not numeric or logical: returning NA
      argument is not numeric or logical: returning NA
    Output
      
      Call:
      NULL
      
      Protected attribute:                 a
      Protected attribute levels:          0, 1
      Adapted variables:                   y, x
      
      Number of training samples:          100
      Number of test samples:              100
      Quantile method:                     linearQuants
      
      Total variation (before adaptation): NA
      Total variation (after adaptation):  NA

# generics, cts

    Code
      print(ad.cts)
    Output
      
      Call:
      fairadapt(formula = y ~ ., prot.attr = "a", adj.mat = adj.mat, 
          train.data = cts, test.data = cts)
      
      
      Adapting variables:
        y, x
      
      Based on protected attribute a 
      
        AND
      
      Based on causal graph:
        y a x z
      y 0 0 0 0
      a 0 0 1 0
      x 1 0 0 0
      z 1 1 1 0
      

