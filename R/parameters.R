setClass(Class = "parameter",
         slots = list(value = "numeric",
                      name = "character",
                      upper_bound = "numeric",
                      lower_bound = "numeric"),
         prototype = list(value = NA_real_,
                          name = NA_character_,
                          upper_bound = Inf,
                          lower_bound = -Inf),
         validity = function(object) {
           if (object@value <= object@upper_bound && object@value >= object@lower_bound) {
             return(TRUE)
           } else {
             return(paste("Supplied parameter value", object@value, "is not between",
                          object@lower_bound, "and", object@upper_bound))
           }
         })
