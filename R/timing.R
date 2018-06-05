#' Timing class for recording computation times
#'
#' The 'timing' class records the computation time of R calculations.
#' There are three methods: new, update and print.
#'
#' @details
#' Create an object of class 'timing', a list containing:
#'    now:     the last known value of sum(proc.time()[1:2])
#'    times:   named vector of processes to keep track of
#'    calls:   number of calls to proc.time within update.timing
#' @method new timing
#' @param processes character vector for names of processes to follow.
#' @export
new.timing <- function(processes="P1") {
  now = proc.time()
  structure(list(
    times = setNames(numeric(length(processes)), processes),
    calls = 0L,
    now = now[1] + now[2]
  ), class = "timing")
}

#' An update method for a timing object
#'
#' Assumes times[process] already exists (checking would take time).
#' Make sure length(process)==1.
#' Note that process can be a character(1) from names(timing$times).
#' @method update timing
#' @param timing a 'timing' object.
#' @param process length 1; which process within timing to be updated (numeric index or name)
#' @param ... not used.
#' @export
update.timing <- function(timing, process=1, ...) {
  now = proc.time()
  now = now[1] + now[2]
  timing$times[process] <- timing$times[process] + (now - timing$now)
  timing$now <- now
  timing$calls <- timing$calls + 1L
  timing
}

#' Print method for class timing.
#' @method print timing
#' @param timing a 'timing' object
#' @param ... additional arguments for print.default().
#' @export
print.timing <- function(timing, ...) {
  cat("A timing object")
  if (timing$calls>0) cat(" with calls=", timing$calls, sep="")
  cat(".  Total=",sum(timing$times)," seconds.\n",sep="")
  print(timing$times, ...)
}

