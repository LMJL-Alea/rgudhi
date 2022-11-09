# The BettiCurve class works

    Code
      bc$apply(dgm)
    Output
      # A tibble: 100 x 2
          Grid     Value
         <dbl> <dbl[1d]>
       1     1         0
       2     1         0
       3     1         0
       4     1         0
       5     1         0
       6     1         0
       7     1         0
       8     1         0
       9     1         0
      10     1         0
      # ... with 90 more rows

---

    Code
      bc$fit(list(dgm))$transform(list(dgm))
    Output
      # A tibble: 100 x 2
          Grid Value    
         <dbl> <list>   
       1     1 <dbl [1]>
       2     1 <dbl [1]>
       3     1 <dbl [1]>
       4     1 <dbl [1]>
       5     1 <dbl [1]>
       6     1 <dbl [1]>
       7     1 <dbl [1]>
       8     1 <dbl [1]>
       9     1 <dbl [1]>
      10     1 <dbl [1]>
      # ... with 90 more rows

---

    Code
      bc$fit_transform(list(dgm))
    Output
      # A tibble: 100 x 2
          Grid Value    
         <dbl> <list>   
       1     1 <dbl [1]>
       2     1 <dbl [1]>
       3     1 <dbl [1]>
       4     1 <dbl [1]>
       5     1 <dbl [1]>
       6     1 <dbl [1]>
       7     1 <dbl [1]>
       8     1 <dbl [1]>
       9     1 <dbl [1]>
      10     1 <dbl [1]>
      # ... with 90 more rows

# The ComplexPolynomial class works

    Code
      cp$apply(dgm)
    Output
      # A tibble: 10 x 2
          Grid Value                      
         <int> <cpl[1d]>                  
       1     1  0.000000e+00-8.594235e-01i
       2     2 -3.282706e-01+0.000000e+00i
       3     3  0.000000e+00+7.314312e-02i
       4     4  1.047682e-02+0.000000e+00i
       5     5  0.000000e+00-1.000447e-03i
       6     6 -6.368947e-05+0.000000e+00i
       7     7  0.000000e+00+2.606487e-06i
       8     8  6.222435e-08+0.000000e+00i
       9     9  0.000000e+00-6.602107e-10i
      10    10  0.000000e+00+0.000000e+00i

---

    Code
      cp$fit(list(dgm))$transform(list(dgm))
    Output
      # A tibble: 10 x 2
          Grid Value    
         <int> <list>   
       1     1 <cpl [1]>
       2     2 <cpl [1]>
       3     3 <cpl [1]>
       4     4 <cpl [1]>
       5     5 <cpl [1]>
       6     6 <cpl [1]>
       7     7 <cpl [1]>
       8     8 <cpl [1]>
       9     9 <cpl [1]>
      10    10 <cpl [1]>

---

    Code
      cp$fit_transform(list(dgm))
    Output
      # A tibble: 10 x 2
          Grid Value    
         <int> <list>   
       1     1 <cpl [1]>
       2     2 <cpl [1]>
       3     3 <cpl [1]>
       4     4 <cpl [1]>
       5     5 <cpl [1]>
       6     6 <cpl [1]>
       7     7 <cpl [1]>
       8     8 <cpl [1]>
       9     9 <cpl [1]>
      10    10 <cpl [1]>

# The Entropy class works

    Code
      ent$apply(dgm)
    Output
      # A tibble: 100 x 2
             Grid     Value
            <dbl> <dbl[1d]>
       1 0             2.20
       2 0.000965      2.20
       3 0.00193       2.20
       4 0.00289       2.20
       5 0.00386       2.20
       6 0.00482       2.20
       7 0.00579       2.20
       8 0.00675       2.20
       9 0.00772       2.20
      10 0.00868       2.20
      # ... with 90 more rows

---

    Code
      ent$fit(list(dgm))$transform(list(dgm))
    Output
      # A tibble: 100 x 2
             Grid Value    
            <dbl> <list>   
       1 0        <dbl [1]>
       2 0.000965 <dbl [1]>
       3 0.00193  <dbl [1]>
       4 0.00289  <dbl [1]>
       5 0.00386  <dbl [1]>
       6 0.00482  <dbl [1]>
       7 0.00579  <dbl [1]>
       8 0.00675  <dbl [1]>
       9 0.00772  <dbl [1]>
      10 0.00868  <dbl [1]>
      # ... with 90 more rows

---

    Code
      ent$fit_transform(list(dgm))
    Output
      # A tibble: 100 x 2
             Grid Value    
            <dbl> <list>   
       1 0        <dbl [1]>
       2 0.000965 <dbl [1]>
       3 0.00193  <dbl [1]>
       4 0.00289  <dbl [1]>
       5 0.00386  <dbl [1]>
       6 0.00482  <dbl [1]>
       7 0.00579  <dbl [1]>
       8 0.00675  <dbl [1]>
       9 0.00772  <dbl [1]>
      10 0.00868  <dbl [1]>
      # ... with 90 more rows

# The Landscape class works

    Code
      land$apply(dgm)
    Output
      # A tibble: 500 x 3
             Grid LandscapeId     Value
            <dbl>       <int> <dbl[1d]>
       1 0                  1   0.00134
       2 0.000965           1   0.00267
       3 0.00193            1   0.00401
       4 0.00289            1   0.00535
       5 0.00386            1   0.00669
       6 0.00482            1   0.00802
       7 0.00579            1   0.00936
       8 0.00675            1   0.0107 
       9 0.00772            1   0.0120 
      10 0.00868            1   0.0134 
      # ... with 490 more rows

---

    Code
      land$fit(list(dgm))$transform(list(dgm))
    Output
      # A tibble: 500 x 3
             Grid LandscapeId Value    
            <dbl>       <int> <list>   
       1 0                  1 <dbl [1]>
       2 0.000965           1 <dbl [1]>
       3 0.00193            1 <dbl [1]>
       4 0.00289            1 <dbl [1]>
       5 0.00386            1 <dbl [1]>
       6 0.00482            1 <dbl [1]>
       7 0.00579            1 <dbl [1]>
       8 0.00675            1 <dbl [1]>
       9 0.00772            1 <dbl [1]>
      10 0.00868            1 <dbl [1]>
      # ... with 490 more rows

---

    Code
      land$fit_transform(list(dgm))
    Output
      # A tibble: 500 x 3
             Grid LandscapeId Value    
            <dbl>       <int> <list>   
       1 0                  1 <dbl [1]>
       2 0.000965           1 <dbl [1]>
       3 0.00193            1 <dbl [1]>
       4 0.00289            1 <dbl [1]>
       5 0.00386            1 <dbl [1]>
       6 0.00482            1 <dbl [1]>
       7 0.00579            1 <dbl [1]>
       8 0.00675            1 <dbl [1]>
       9 0.00772            1 <dbl [1]>
      10 0.00868            1 <dbl [1]>
      # ... with 490 more rows

# The PersistenceImage class works

    Code
      pei$apply(dgm)
    Output
      # A tibble: 400 x 3
               X     Y     Value
           <dbl> <dbl> <dbl[1d]>
       1 0           0      1.43
       2 0.00503     0      1.43
       3 0.0101      0      1.43
       4 0.0151      0      1.43
       5 0.0201      0      1.43
       6 0.0251      0      1.43
       7 0.0302      0      1.43
       8 0.0352      0      1.43
       9 0.0402      0      1.43
      10 0.0452      0      1.43
      # ... with 390 more rows

---

    Code
      pei$fit(list(dgm))$transform(list(dgm))
    Output
      # A tibble: 400 x 3
               X     Y Value    
           <dbl> <dbl> <list>   
       1 0           0 <dbl [1]>
       2 0.00503     0 <dbl [1]>
       3 0.0101      0 <dbl [1]>
       4 0.0151      0 <dbl [1]>
       5 0.0201      0 <dbl [1]>
       6 0.0251      0 <dbl [1]>
       7 0.0302      0 <dbl [1]>
       8 0.0352      0 <dbl [1]>
       9 0.0402      0 <dbl [1]>
      10 0.0452      0 <dbl [1]>
      # ... with 390 more rows

---

    Code
      pei$fit_transform(list(dgm))
    Output
      # A tibble: 400 x 3
               X     Y Value    
           <dbl> <dbl> <list>   
       1 0           0 <dbl [1]>
       2 0.00503     0 <dbl [1]>
       3 0.0101      0 <dbl [1]>
       4 0.0151      0 <dbl [1]>
       5 0.0201      0 <dbl [1]>
       6 0.0251      0 <dbl [1]>
       7 0.0302      0 <dbl [1]>
       8 0.0352      0 <dbl [1]>
       9 0.0402      0 <dbl [1]>
      10 0.0452      0 <dbl [1]>
      # ... with 390 more rows

# The Silhouette class works

    Code
      sil$apply(dgm)
    Output
      # A tibble: 100 x 2
             Grid     Value
            <dbl> <dbl[1d]>
       1 0          0      
       2 0.000965   0.00136
       3 0.00193    0.00273
       4 0.00289    0.00409
       5 0.00386    0.00546
       6 0.00482    0.00682
       7 0.00579    0.00818
       8 0.00675    0.00955
       9 0.00772    0.0109 
      10 0.00868    0.0123 
      # ... with 90 more rows

---

    Code
      sil$fit(list(dgm))$transform(list(dgm))
    Output
      # A tibble: 100 x 2
             Grid Value    
            <dbl> <list>   
       1 0        <dbl [1]>
       2 0.000965 <dbl [1]>
       3 0.00193  <dbl [1]>
       4 0.00289  <dbl [1]>
       5 0.00386  <dbl [1]>
       6 0.00482  <dbl [1]>
       7 0.00579  <dbl [1]>
       8 0.00675  <dbl [1]>
       9 0.00772  <dbl [1]>
      10 0.00868  <dbl [1]>
      # ... with 90 more rows

---

    Code
      sil$fit_transform(list(dgm))
    Output
      # A tibble: 100 x 2
             Grid Value    
            <dbl> <list>   
       1 0        <dbl [1]>
       2 0.000965 <dbl [1]>
       3 0.00193  <dbl [1]>
       4 0.00289  <dbl [1]>
       5 0.00386  <dbl [1]>
       6 0.00482  <dbl [1]>
       7 0.00579  <dbl [1]>
       8 0.00675  <dbl [1]>
       9 0.00772  <dbl [1]>
      10 0.00868  <dbl [1]>
      # ... with 90 more rows

# The TopologicalVector class works

    Code
      tov$apply(dgm)
    Output
      # A tibble: 10 x 2
          Grid     Value
         <int> <dbl[1d]>
       1     1  2.78e-17
       2     2  2.78e-17
       3     3  2.78e-17
       4     4  2.78e-17
       5     5  2.78e-17
       6     6  2.78e-17
       7     7  2.78e-17
       8     8  2.78e-17
       9     9  2.78e-17
      10    10  2.78e-17

---

    Code
      tov$fit(list(dgm))$transform(list(dgm))
    Output
      # A tibble: 10 x 2
          Grid Value    
         <int> <list>   
       1     1 <dbl [1]>
       2     2 <dbl [1]>
       3     3 <dbl [1]>
       4     4 <dbl [1]>
       5     5 <dbl [1]>
       6     6 <dbl [1]>
       7     7 <dbl [1]>
       8     8 <dbl [1]>
       9     9 <dbl [1]>
      10    10 <dbl [1]>

---

    Code
      tov$fit_transform(list(dgm))
    Output
      # A tibble: 10 x 2
          Grid Value    
         <int> <list>   
       1     1 <dbl [1]>
       2     2 <dbl [1]>
       3     3 <dbl [1]>
       4     4 <dbl [1]>
       5     5 <dbl [1]>
       6     6 <dbl [1]>
       7     7 <dbl [1]>
       8     8 <dbl [1]>
       9     9 <dbl [1]>
      10    10 <dbl [1]>

