
####################################
# Function to check nhs number is valid according 
# to modulus 11 algorithm

nhs.check = function(vec){ 
  # function to check nhs number is valid according to modulus 11 algorithm
  #
  # Args: vec: vector of nhs numbers
  #
  # output: data.frame of nhs numbers and if valid or invalid
  
  x = NULL
  for(i in 1:length(vec)){
    t = vec[i]
    
    num = nchar(t)
    
    if(num != 10){
      check.digit = NA
    } else{
      remainder = sum(as.numeric(substring(t, 1:(num - 1), 1:(num - 1))) * 10:2) %% 11
      check.digit = 11 - remainder
    }
    
    if (is.na(check.digit)){
      NULL
    } else if (check.digit == 11){
      check.digit = 0
    } else {
      NULL
    }
    
    
    if (is.na(check.digit)){
      y = "invalid"
    } else if(num != 10){
      y = "invalid"
    } else if(check.digit == as.numeric(substring(t, num, num))){
      y = "valid"
    } else {
      y = "invalid"
    }     
    x = append(x, y)
  }
  d = data.frame(nhs.number = vec, check = x)
}




