# Class `CubicalComplex` works

    Code
      cc$compute_persistence()$cofaces_of_persistence_pairs()
    Output
      [[1]]
      list()
      
      [[2]]
      [[2]][[1]]
      [1] 10
      
      

---

    Code
      cc$persistence()
    Output
      # A tibble: 1 x 3
        dimension birth death
            <int> <dbl> <dbl>
      1         0     0   Inf

---

    Code
      cc$compute_persistence()$persistence_intervals_in_dimension(0)
    Output
      # A tibble: 1 x 2
        birth death
        <dbl> <dbl>
      1     0   Inf

# Class `PeriodicCubicalComplex` works

    Code
      pcc$compute_persistence()$cofaces_of_persistence_pairs()
    Output
      [[1]]
      list()
      
      [[2]]
      [[2]][[1]]
      [1] 0
      
      [[2]][[2]]
      [1] 9
      
      

---

    Code
      pcc$persistence()
    Output
      # A tibble: 2 x 3
        dimension birth death
            <int> <dbl> <dbl>
      1         1     1   Inf
      2         0     0   Inf

---

    Code
      pcc$compute_persistence()$persistence_intervals_in_dimension(0)
    Output
      # A tibble: 1 x 2
        birth death
        <dbl> <dbl>
      1     0   Inf

