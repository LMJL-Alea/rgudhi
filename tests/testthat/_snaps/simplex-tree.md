# Class `SimplexTree` works

    Code
      st$filtration(1:2)
    Output
      [1] 0.6840403

---

    Code
      st$extended_persistence()
    Output
      $Ordinary
      list()
      
      $Relative
      list()
      
      $`Extended+`
      $`Extended+`[[1]]
      $`Extended+`[[1]][[1]]
      [1] 0
      
      $`Extended+`[[1]][[2]]
      $`Extended+`[[1]][[2]][[1]]
      [1] 0
      
      $`Extended+`[[1]][[2]][[2]]
      [1] 0.6840403
      
      
      
      
      $`Extended-`
      $`Extended-`[[1]]
      $`Extended-`[[1]][[1]]
      [1] 1
      
      $`Extended-`[[1]][[2]]
      $`Extended-`[[1]][[2]][[1]]
      [1] 0.6840403
      
      $`Extended-`[[1]][[2]][[2]]
      [1] 0
      
      
      
      

---

    Code
      st$flag_persistence_generators()
    Output
      [[1]]
            [,1] [,2] [,3]
       [1,]    3    3    2
       [2,]    4    4    3
       [3,]    5    5    4
       [4,]    6    6    5
       [5,]    7    7    6
       [6,]    8    8    7
       [7,]    9    9    0
       [8,]    0    9    8
       [9,]    1    1    0
      [10,]    2   10    1
      
      [[2]]
      [[2]][[1]]
            [,1] [,2] [,3] [,4]
       [1,]   10    0   10    0
       [2,]   10    2   10    2
       [3,]   10    3   10    3
       [4,]   10    4   10    4
       [5,]   10    5   10    5
       [6,]   10    6   10    6
       [7,]   10    7   10    7
       [8,]   10    8   10    8
       [9,]   10    9   10    9
      [10,]    2    1   10    9
      
      
      [[3]]
      [1] 10
      
      [[4]]
      list()
      

---

    Code
      st$get_boundaries(1:2)
    Output
      # A tibble: 2 x 2
        simplex   filtration
        <list>         <dbl>
      1 <int [1]>         -1
      2 <int [1]>         -2

---

    Code
      st$get_filtration()
    Output
      # A tibble: 41 x 2
         simplex   filtration
         <list>         <dbl>
       1 <int [1]>         -3
       2 <int [1]>         -2
       3 <int [1]>         -2
       4 <int [1]>         -2
       5 <int [2]>         -2
       6 <int [1]>         -2
       7 <int [2]>         -2
       8 <int [1]>         -2
       9 <int [2]>         -2
      10 <int [1]>         -2
      # ... with 31 more rows

---

    Code
      st$get_simplices()
    Output
      # A tibble: 41 x 2
         simplex   filtration
         <list>         <dbl>
       1 <int [3]>          2
       2 <int [2]>         -1
       3 <int [3]>          2
       4 <int [2]>         -2
       5 <int [2]>          2
       6 <int [1]>         -2
       7 <int [3]>          2
       8 <int [2]>         -1
       9 <int [2]>          1
      10 <int [1]>         -1
      # ... with 31 more rows

---

    Code
      st$get_skeleton(0)
    Output
      # A tibble: 11 x 2
         simplex   filtration
         <list>         <dbl>
       1 <int [1]>         -2
       2 <int [1]>         -1
       3 <int [1]>         -2
       4 <int [1]>         -2
       5 <int [1]>         -2
       6 <int [1]>         -2
       7 <int [1]>         -2
       8 <int [1]>         -2
       9 <int [1]>         -2
      10 <int [1]>         -2
      11 <int [1]>         -3

---

    Code
      st$lower_star_persistence_generators()
    Output
      [[1]]
      [[1]][[1]]
            [,1] [,2]
       [1,]    3    3
       [2,]    4    4
       [3,]    5    5
       [4,]    6    6
       [5,]    7    7
       [6,]    8    8
       [7,]    9    9
       [8,]    0    9
       [9,]    1    1
      [10,]    2   -1
      
      [[1]][[2]]
            [,1] [,2]
       [1,]   -1   -1
       [2,]   -1   -1
       [3,]   -1   -1
       [4,]   -1   -1
       [5,]   -1   -1
       [6,]   -1   -1
       [7,]   -1   -1
       [8,]   -1   -1
       [9,]   -1   -1
      
      [[1]][[3]]
           [,1] [,2]
      [1,]   -1   -1
      
      
      [[2]]
      [[2]][[1]]
      [1] 10
      
      

