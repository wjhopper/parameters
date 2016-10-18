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

setGeneric(name = "value",
           def = function(object) standardGeneric("value")
           )

setMethod(f = "value",
          signature = "parameter",
          definition = function(object) {
            x <- object@value
            names(x) <- object@name
            return(x)
          }
          )

setGeneric(name = "value<-",
           def = function(object, value) standardGeneric("value<-")
           )

setMethod(f = "value<-",
          signature = "parameter",
          definition = function(object, value) {
            object@value <- value
            valid <- validObject(object)
            if (valid) {
              return(object)
            }
          })
