fun evenJ (0 : int) : bool = false
  	    | evenJ 1 = true
  	    | evenJ n = evenJ (n - 2)
