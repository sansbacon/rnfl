library(parallel)
library(doParallel)
library(GA)

#Define fitness function to ensure that solutions meet criteria

evalFunc <- function(x) {
  current_solution_salary <- x %*% dataset$salary
  current_solution_fpts <- x %*% dataset$fpts
  
  if(sum(dataset$position[x==1] == "QB")  ==  0    )
    return(0)
  
  if(sum(dataset$position[x==1] == "RB")  ==  0    )
    return(0)
  
  if(sum(dataset$position[x==1] == "WR")  == 0    )
    return(0)
  
  if(sum(dataset$position[x==1] == "TE")  ==  0    )
    return(0)
  
  if(sum(dataset$position[x==1] == "TE")  ==  0    )
    return(0)
  
  if(sum(dataset$position[x==1] == "DST")  ==  0   )
    return(0)
  
  if (sum(x)>9                             ||
      current_solution_salary > salarylimit ||
      sum(dataset$position[x==1] =="QB")>1  ||
      sum(dataset$position[x==1] =="RB")>3  ||
      sum(dataset$position[x==1] =="WR")>4  ||
      sum(dataset$position[x==1] =="TE")>2  || 
      sum(dataset$position[x==1] =="DST")>1  )
    return(0) else return(current_solution_fpts)
  
}