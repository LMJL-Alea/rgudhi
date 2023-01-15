# The Atol class works

    Code
      vr$fit(list(dgm))$transform(list(dgm))
    Output
      # A tibble: 2 x 2
         Grid Value    
        <int> <list>   
      1     1 <dbl [1]>
      2     2 <dbl [1]>

---

    Code
      vr$fit_transform(list(dgm))
    Output
      # A tibble: 2 x 2
         Grid Value    
        <int> <list>   
      1     1 <dbl [1]>
      2     2 <dbl [1]>

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
      # A tibble: 1 x 2
         Grid     Value
        <int> <dbl[1d]>
      1     1      2.20

---

    Code
      ent$fit(list(dgm))$transform(list(dgm))
    Output
      # A tibble: 1 x 2
         Grid Value    
        <int> <list>   
      1     1 <dbl [1]>

---

    Code
      ent$fit_transform(list(dgm))
    Output
      # A tibble: 1 x 2
         Grid Value    
        <int> <list>   
      1     1 <dbl [1]>

# The Landscape class works

    Code
      land$apply(dgm)
    Output
      # A tibble: 500 x 3
             Grid LandscapeId     Value
            <dbl>       <int> <dbl[1d]>
       1 0.000945           1   0.00134
       2 0.00189            1   0.00267
       3 0.00284            1   0.00401
       4 0.00378            1   0.00535
       5 0.00473            1   0.00669
       6 0.00567            1   0.00802
       7 0.00662            1   0.00936
       8 0.00756            1   0.0107 
       9 0.00851            1   0.0120 
      10 0.00945            1   0.0134 
      # ... with 490 more rows

---

    Code
      land$fit(list(dgm))$transform(list(dgm))
    Output
      # A tibble: 500 x 3
             Grid LandscapeId Value    
            <dbl>       <int> <list>   
       1 0.000945           1 <dbl [1]>
       2 0.00189            1 <dbl [1]>
       3 0.00284            1 <dbl [1]>
       4 0.00378            1 <dbl [1]>
       5 0.00473            1 <dbl [1]>
       6 0.00567            1 <dbl [1]>
       7 0.00662            1 <dbl [1]>
       8 0.00756            1 <dbl [1]>
       9 0.00851            1 <dbl [1]>
      10 0.00945            1 <dbl [1]>
      # ... with 490 more rows

---

    Code
      land$fit_transform(list(dgm))
    Output
      # A tibble: 500 x 3
             Grid LandscapeId Value    
            <dbl>       <int> <list>   
       1 0.000945           1 <dbl [1]>
       2 0.00189            1 <dbl [1]>
       3 0.00284            1 <dbl [1]>
       4 0.00378            1 <dbl [1]>
       5 0.00473            1 <dbl [1]>
       6 0.00567            1 <dbl [1]>
       7 0.00662            1 <dbl [1]>
       8 0.00756            1 <dbl [1]>
       9 0.00851            1 <dbl [1]>
      10 0.00945            1 <dbl [1]>
      # ... with 490 more rows

# The PersistenceImage class works

    Code
      pei$apply(dgm)
    Warning <lifecycle_warning_deprecated>
      `cross_df()` was deprecated in purrr 1.0.0.
      i Please use `tidyr::expand_grid()` instead.
      i See <https://github.com/tidyverse/purrr/issues/768>.
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
    Warning <lifecycle_warning_deprecated>
      `cross_df()` was deprecated in purrr 1.0.0.
      i Please use `tidyr::expand_grid()` instead.
      i See <https://github.com/tidyverse/purrr/issues/768>.
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
    Warning <lifecycle_warning_deprecated>
      `cross_df()` was deprecated in purrr 1.0.0.
      i Please use `tidyr::expand_grid()` instead.
      i See <https://github.com/tidyverse/purrr/issues/768>.
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
       1 0.000945   0.00134
       2 0.00189    0.00267
       3 0.00284    0.00401
       4 0.00378    0.00535
       5 0.00473    0.00669
       6 0.00567    0.00802
       7 0.00662    0.00936
       8 0.00756    0.0107 
       9 0.00851    0.0120 
      10 0.00945    0.0134 
      # ... with 90 more rows

---

    Code
      sil$fit(list(dgm))$transform(list(dgm))
    Output
      # A tibble: 100 x 2
             Grid Value    
            <dbl> <list>   
       1 0.000945 <dbl [1]>
       2 0.00189  <dbl [1]>
       3 0.00284  <dbl [1]>
       4 0.00378  <dbl [1]>
       5 0.00473  <dbl [1]>
       6 0.00567  <dbl [1]>
       7 0.00662  <dbl [1]>
       8 0.00756  <dbl [1]>
       9 0.00851  <dbl [1]>
      10 0.00945  <dbl [1]>
      # ... with 90 more rows

---

    Code
      sil$fit_transform(list(dgm))
    Output
      # A tibble: 100 x 2
             Grid Value    
            <dbl> <list>   
       1 0.000945 <dbl [1]>
       2 0.00189  <dbl [1]>
       3 0.00284  <dbl [1]>
       4 0.00378  <dbl [1]>
       5 0.00473  <dbl [1]>
       6 0.00567  <dbl [1]>
       7 0.00662  <dbl [1]>
       8 0.00756  <dbl [1]>
       9 0.00851  <dbl [1]>
      10 0.00945  <dbl [1]>
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

