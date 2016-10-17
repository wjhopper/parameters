setClass(Class = "parameter",
         slots = list(value = "numeric",
                      name = "character",
                      upper_bound = "numeric",
                      lower_bound = "numeric"),
         prototype = list(value = NA_real_,
                          name = NA_character_,
                          upper_bound = Inf,
                          lower_bound = -Inf))
