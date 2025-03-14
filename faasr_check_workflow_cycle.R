source('faasr_predecessors_list.R')

# workflow implementation - check loop iteratively, predecessors.
faasr_check_workflow_cycle <- function(faasr){
  
  # implement dfs cycle detection - recursive function (returns boolean)
  is_cyclic <- function(adj_graph, node){
    # if the current action is already in the recursive call stack
    # then there must be a cycle
    if (isTRUE(stack[[node]])) {
      return(TRUE)
	  }

	  # mark action as visited and add it to recursive call stack
	  stack[[node]] <<- TRUE
    visited[[node]] <<- TRUE

	  # recursively check all of the
    # action's successors
    if(!is.null(adj_graph[[node]])){
      for (action in adj_graph[[node]]) {
        # if the successor action creates a cycle
        # then the graph is cyclical
        if (!(isTRUE(visited[[action]])) && is_cyclic(adj_graph, action)) {
          return(TRUE)
        # if the successor is in the recursion call stack, then
        # there must be a  cycle
        } else if (isTRUE(stack[[action]])) {
          return(TRUE)
        }
      }
    }
    # Finished exploring branch. Remove node from recursion stack
    stack[[node]] <<- FALSE
    return(FALSE)
  }
	
  # build empty lists for the graph and predecessors.
  adj_graph <- list()

  # build the graph indicating adjacent nodes, e.g., "F1":["F2","F3"], so on.
  for (func in names(faasr$FunctionList)) {
    if (length(faasr$FunctionList[[func]]$InvokeNext) != 0){
      for (invoke_next in faasr$FunctionList[[func]]$InvokeNext){
        parts <- unlist(strsplit(invoke_next, "[()]"))
        adj_graph[[func]] <- unique(c(adj_graph[[func]], parts[1]))
      }
    }
  }

  # check next functions of FunctionInvoke are in the function list
  for (func in names(adj_graph)){
    for (path in adj_graph[[func]]){
      if (!(path %in% names(faasr$FunctionList))){
        err_msg <- paste0('{\"faasr_check_workflow_cycle\":\"invalid next function ',path,' is found in ',func,'\"}', "\n")
        # message(err_msg)
        stop()
      }
    }
  }
  # build an empty recursion call stack
  stack <- list()
  # build an empty visited list
  visited <- list()
  # do dfs starting with faasr$FunctionInvoke.
  cycle <- is_cyclic(adj_graph, faasr$FunctionInvoke)

  if(cycle == TRUE){
	    err_msg <- paste0('{\"faasr_check_workflow_cycle\":\"cycle detected in graph\"}', "\n")
	    # message(err_msg)
	    stop()
  }
	
  # call faasr_predecessors_list and get a list of function:predecessor sets.
  pre <- faasr_predecessors_list(faasr, adj_graph)

  # check for unreachable functions
  check <- TRUE
  message(names(visited))
  for (func in names(faasr$FunctionList)){
    if(!(func %in% names(visited))){
      err_msg <- paste0('{\"faasr_check_workflow_cycle\":\"unreachable action: ',func,'\"}', "\n")
      message(err_msg)
      stop()
    }
  }
	
  return(pre[[faasr$FunctionInvoke]])
}
