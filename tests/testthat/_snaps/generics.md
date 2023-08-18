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
    Output
      
      Call:
      fairadapt(formula = y ~ ., prot.attr = "a", adj.mat = adj.mat, 
          train.data = train, test.data = test)
      
      Protected attribute:                 a
      Protected attribute levels:          0, 1
      Adapted variables:                   x, y
      
      Number of training samples:          100
      Number of test samples:              100
      Quantile method:                     rangerQuants
      
      Total variation (before adaptation): -0.0153
      Total variation (after adaptation):  -0.07085

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
    Output
      
      Call:
      fairadapt(formula = y ~ ., prot.attr = "a", adj.mat = adj.mat, 
          train.data = train, test.data = test, quant.method = linearQuants)
      
      Protected attribute:                 a
      Protected attribute levels:          0, 1
      Adapted variables:                   x, y
      
      Number of training samples:          100
      Number of test samples:              100
      Quantile method:                     linearQuants
      
      Total variation (before adaptation): -0.0153
      Total variation (after adaptation):  -0.05233

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
      

---

    Code
      summary(ad.cts)
    Output
      
      Call:
      fairadapt(formula = y ~ ., prot.attr = "a", adj.mat = adj.mat, 
          train.data = cts, test.data = cts)
      
      Protected attribute:                 a
      Protected attribute levels:          0, 1
      Adapted variables:                   x, y
      
      Number of training samples:          100
      Number of test samples:              100
      Quantile method:                     rangerQuants
      
      Total variation (before adaptation): 0.07294
      Total variation (after adaptation):  -0.02209

---

    Code
      print(ad.disc)
    Output
      
      Call:
      fairadapt(formula = y ~ ., prot.attr = "a", adj.mat = adj.mat, 
          train.data = cts, test.data = NULL)
      
      
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
      summary(ad.disc)
    Output
      
      Call:
      fairadapt(formula = y ~ ., prot.attr = "a", adj.mat = adj.mat, 
          train.data = cts, test.data = NULL)
      
      Protected attribute:                 a
      Protected attribute levels:          0, 1
      Adapted variables:                   x, y
      
      Number of training samples:          100
      Number of test samples:              0
      Quantile method:                     rangerQuants
      
      Total variation (before adaptation): 0.07294
      Total variation (after adaptation):  -0.03197

