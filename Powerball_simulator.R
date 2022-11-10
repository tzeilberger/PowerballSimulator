############################################
############################################
###
### Function to simulate Powerball
###
############################################
############################################

powerball <- function(x){
  if(!is.vector(x))
    stop("Input must be a vector")
  
  if(sum(x %% 1) > 0)
    stop("Input must contain only natural numbers")
  
  if(any(x[1:5] < 1 | x[1:5] > 69))
    stop("First five numbers must be selected from 1 to 69")
  
  if(any(x[6] < 1 | x[6] > 26))
    stop("Last number must be a selected from 1 to 26")
  
  if(any(duplicated(x[1:5])))
    stop("First five numbers cannot contain repetition")
  
  if(identical(x, c(sample(1:69, size = 5, replace = FALSE, prob = NULL), sample(1:26, size = 1, replace = FALSE, prob = NULL))))
    message("JACKPOT!")  
  
  if(!identical(x, c(sample(1:69, size = 5, replace = FALSE, prob = NULL), sample(1:26, size = 1, replace = FALSE, prob = NULL))))
    message("LOSER!")  
}


############################################
############################################
###
### Play Powerball! (without the threat of becoming a policy failure i.e. a billionaire)
###
############################################
############################################
###
### Instructions: In a vector of length 6,
### (1) input five numbers selected from 1 to 69 (no repetition), 
### (2) for the sixth number, input one number selected from 1 to 26.
### (3) See example below with the vector 1, 69, 56, 23, 12, 26 (keep or replace the example numbers with your lucky numbers and find out if you hit the jackpot!)
###
############################################
############################################

powerball(x = c(1, 69, 56, 23, 12, 26))
