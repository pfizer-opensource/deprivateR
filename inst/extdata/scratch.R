# manage states and territories
if (is.null(state) == TRUE){
  if (puerto_rico == FALSE){
    states <- state.abb
  } else if (puerto_rico == TRUE){
    states <- c(state.abb, "PR")
  }
} else if (is.null(state) == FALSE){
  if (puerto_rico == FALSE){
    states <- state
  } else if (puerto_rico == TRUE){
    states <- c(state, "PR")
  }
}








