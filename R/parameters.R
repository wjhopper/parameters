#' @title Bounded parameter objects
#'
#' @description An S4 class to represent a bounded parameter for a model
#'
#' @details No constructor function exists for \code{parameter} objects. Parameter
#' objects should be instantiated with the \code{new()} function directly.
#'
#' @slot value A numeric vector of length one, which acts as the parameters value
#' @slot name A character vector of length one, which acts as a semantic label
#'   for what the parameter value represents
#' @slot upper_bound A numeric vector of length one. The \code{value} slot may
#'   not be assinged a value greater than the value of the \code{upper_bound} slot.
#' @slot lower_bound A numeric vector of length one. The \code{value} slot may
#'   not be assinged a value less than the value of the \code{lower_bound} slot.
#'
#' @importFrom methods new
#' @examples
#' new("parameter", value = .5, name = "binomial_p",
#'     upper_bound = 1, lower_bound = 0)
parameter <- setClass(Class = "parameter",
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

#' @name value
#' @title value
#' @description Extract or replact the value of parameter object
NULL

#' @rdname value
#' @param object A \code{parameter} object
#'
#' @export
#' @examples
#' p <- new("parameter", value = .5, name = "binomial_p",
#'          upper_bound = 1, lower_bound = 0)
#' value(p)
#' value(p) <- .9
#' value(p)


setGeneric(name = "value",
           def = function(object) standardGeneric("value")
           )

#' @describeIn value Returns the value held the in the \code{value} slot as a
#' named numeric vector.
#' @export
setMethod(f = "value",
          signature = "parameter",
          definition = function(object) {
            x <- object@value
            names(x) <- object@name
            return(x)
          }
          )

#' @rdname value
#'
#' @param value A numeric vector of length one. Must be fall between the parameter
#' objects upper and lower bounds.
#'
#' @export
setGeneric(name = "value<-",
           def = function(object, value) standardGeneric("value<-")
           )

#' @describeIn value Replaces the value held the in the \code{value} slot
#' @importFrom methods validObject
#' @export
setMethod(f = "value<-",
          signature = "parameter",
          definition = function(object, value) {
            object@value <- value
            valid <- validObject(object)
            if (valid) {
              return(object)
            }
          })

#' @export
print.parameter <- function(x, ...) {
  cat(paste("Parameter:", x@name, "=", x@value),
      paste("Upper Bound =", x@upper_bound, "Lower Bound =", x@lower_bound),
      sep = "\n")
}

#' @describeIn parameter Print details about the object to the console
#' @param object herp
#' @importFrom methods show
#' @export
setMethod(f = "show",
          signature = "parameter",
          definition = function(object) {
            print.parameter(object)
          })
