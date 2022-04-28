# fairadaptBoot

    {
      "type": "double",
      "attributes": {},
      "value": [-0.0405844]
    }

---

    {
      "type": "double",
      "attributes": {},
      "value": [0.00487013]
    }

---

    Code
      print(ran)
    Output
      
      Call:
      fairadaptBoot(formula = y ~ ., prot.attr = "a", adj.mat = adj.mat, 
          train.data = train, test.data = test, keep.object = TRUE, 
          n.boot = 3L, seed = 202)
      
      Bootstrap repetitions: 3 
      
      Adapting variables:
        y, x
      
      Based on protected attribute a 
      
        AND
      
      Based on causal graph:
        a y x
      a 0 0 1
      y 0 0 0
      x 0 1 0
      

---

    Code
      summary(ran)
    Output
      
      Call:
      fairadaptBoot(formula = y ~ ., prot.attr = "a", adj.mat = adj.mat, 
          train.data = train, test.data = test, keep.object = TRUE, 
          n.boot = 3L, seed = 202)
      
      Bootstrap repetitions:      3
      Protected attribute:        a
      Protected attribute levels: 0, 1
      Adapted variables:          y, x
      
      Number of training samples: 100
      Number of test samples:     0
      Quantile method:            quant.method
      
      Randomness considered:      finsamp
      fairadapt objects saved:    TRUE

---

    Code
      print(rto)
    Output
      
      Call:
      fairadaptBoot(formula = y ~ ., prot.attr = "a", train.data = train, 
          test.data = test, top.ord = c("a", "x", "y"), n.boot = 3L, 
          seed = 202)
      
      Bootstrap repetitions: 3 
      
      Adapting variables:
        x, y
      
      Based on protected attribute a 
      
        AND
      
      Based on topological order:
        axy

---

    Code
      summary(rto)
    Output
      
      Call:
      fairadaptBoot(formula = y ~ ., prot.attr = "a", train.data = train, 
          test.data = test, top.ord = c("a", "x", "y"), n.boot = 3L, 
          seed = 202)
      
      Bootstrap repetitions:      3
      Protected attribute:        a
      Protected attribute levels: 0, 1
      
      Number of training samples: 100
      Number of test samples:     0
      Quantile method:            quant.method
      
      Randomness considered:      finsamp
      fairadapt objects saved:    FALSE

---

    Code
      print(charmod)
    Output
      
      Call:
      fairadaptBoot(formula = score ~ ., prot.attr = "gender", adj.mat = adj.mat, 
          train.data = uni, keep.object = TRUE, n.boot = 3L, seed = 203)
      
      Bootstrap repetitions: 3 
      
      Adapting variables:
        edu, test, score
      
      Based on protected attribute gender 
      
        AND
      
      Based on causal graph:
             gender edu test score
      gender      0   1    1     1
      edu         0   0    0     1
      test        0   0    0     1
      score       0   0    0     0
      

---

    Code
      summary(charmod)
    Output
      
      Call:
      fairadaptBoot(formula = score ~ ., prot.attr = "gender", adj.mat = adj.mat, 
          train.data = uni, keep.object = TRUE, n.boot = 3L, seed = 203)
      
      Bootstrap repetitions:      3
      Protected attribute:        gender
      Protected attribute levels: 0, 1
      Adapted variables:          edu, test, score
      
      Number of training samples: 1000
      Number of test samples:     0
      Quantile method:            quant.method
      
      Randomness considered:      finsamp
      fairadapt objects saved:    TRUE

---

    Code
      print(mod)
    Output
      
      Call:
      fairadaptBoot(formula = two_year_recid ~ ., prot.attr = "race", 
          adj.mat = adj.mat, train.data = train, test.data = test, 
          n.boot = 3, seed = 203)
      
      Bootstrap repetitions: 3 
      
      Adapting variables:
        juv_fel_count, juv_misd_count, juv_other_count, priors_count, c_charge_degree, two_year_recid
      
      Based on protected attribute race 
      
        AND
      
      Based on causal graph:
                      age sex juv_fel_count juv_misd_count juv_other_count priors_count c_charge_degree race two_year_recid
      age               0   0             1              1               1            1               1    0              1
      sex               0   0             1              1               1            1               1    0              1
      juv_fel_count     0   0             0              0               0            1               1    0              1
      juv_misd_count    0   0             0              0               0            1               1    0              1
      juv_other_count   0   0             0              0               0            1               1    0              1
      priors_count      0   0             0              0               0            0               1    0              1
      c_charge_degree   0   0             0              0               0            0               0    0              1
      race              0   0             1              1               1            1               1    0              1
      two_year_recid    0   0             0              0               0            0               0    0              0
      

---

    Code
      summary(mod)
    Output
      
      Call:
      fairadaptBoot(formula = two_year_recid ~ ., prot.attr = "race", 
          adj.mat = adj.mat, train.data = train, test.data = test, 
          n.boot = 3, seed = 203)
      
      Bootstrap repetitions:      3
      Protected attribute:        race
      Protected attribute levels: Non-White, White
      Adapted variables:          juv_fel_count, juv_misd_count, juv_other_count, priors_count, c_charge_degree, two_year_recid
      
      Number of training samples: 1000
      Number of test samples:     0
      Quantile method:            quant.method
      
      Randomness considered:      finsamp
      fairadapt objects saved:    FALSE

