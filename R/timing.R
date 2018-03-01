#' Timing class for recording computation times
#'
# Code is copied from my Master Thesis, circa 2008.
# Create an object of class 'timing', a list containing:
#    now:     the last known value of sum(proc.time()[1:2])
#    times:   named vector of processes to keep track of
#    calls:   number of calls to proc.time within update.timing
#' @export
new.timing <- function(tnames="usertime") {
  structure(list(
    times=structure(numeric(length(tnames)),names=tnames),
    calls=0L,
    now=sum(proc.time()[1:2])
  ), class="timing")
}

#' Print method for class timing.
#' @method print timing
#' @export
print.timing <- function(timing) {
  cat("A timing object")
  if (timing$calls>0) cat(" with calls=", timing$calls, sep="")
  cat(".  Total=",sum(timing$times)," seconds.\n",sep="")
  print(timing$times)
}

#' An update method for a timing object
#'
#' Assumes times[process] already exists (checking would take time).
#' Make sure length(process)==1.
#' Note that process can be a character(1) from names(timing$times).
#' @method update timing
#' @export
update.timing <- function(timing, process=1) {
  now <- sum(proc.time()[1:2])
  timing$times[process] <- timing$times[process] + (now - timing$now)
  timing$now <- now
  timing$calls <- timing$calls + 1L
  timing
}

