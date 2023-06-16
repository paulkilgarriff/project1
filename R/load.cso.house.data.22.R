
install_if_not_present <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name)
  }
}

#load packages required
list_packages <- c("csodata")
print("Loading year built data (FY102) from CSO website")

for (i in list_packages) {
  install_if_not_present(i)
  library(i, character.only = TRUE)
}

retry_attempts <- 5
attempt <- 1

#load household data from cso website
while(attempt <= retry_attempts){
  tryCatch({
    hou_age <- cso_get_data("FY102", pivot_format = "long")
    
    # If the code above runs successfully, the dataframe is presumably loaded
    # Break the loop as there is no need to try again
    attempt <- retry_attempts + 1
  },
  error = function(e){
    # An error occurred, print a message and the error
    print(paste("Attempt", attempt, "failed with error:"))
    print(e)
    
    # If we've reached the maximum number of attempts, stop trying
    if(attempt == retry_attempts){
      stop("Failed to load household data after several attempts")
    }
    # Otherwise, increment the attempt counter
    attempt <- attempt + 1
  })
}


#another data

print("Loading tenure data (FY034B) from CSO website")
attempt <- 1

#load household data from cso website
while(attempt <= retry_attempts){
  tryCatch({
    tenure <- cso_get_data("FY034B", pivot_format = "long")
    
    # If the code above runs successfully, the dataframe is presumably loaded
    # Break the loop as there is no need to try again
    attempt <- retry_attempts + 1
  },
  error = function(e){
    # An error occurred, print a message and the error
    print(paste("Attempt", attempt, "failed with error:"))
    print(e)
    
    # If we've reached the maximum number of attempts, stop trying
    if(attempt == retry_attempts){
      stop("Failed to load household data after several attempts")
    }
    # Otherwise, increment the attempt counter
    attempt <- attempt + 1
  })
}



print("Loading sewerage data (FY039) from CSO website")
attempt <- 1

#load household data from cso website
while(attempt <= retry_attempts){
  tryCatch({
    sewer <- cso_get_data("FY039", pivot_format = "long")
    
    # If the code above runs successfully, the dataframe is presumably loaded
    # Break the loop as there is no need to try again
    attempt <- retry_attempts + 1
  },
  error = function(e){
    # An error occurred, print a message and the error
    print(paste("Attempt", attempt, "failed with error:"))
    print(e)
    
    # If we've reached the maximum number of attempts, stop trying
    if(attempt == retry_attempts){
      stop("Failed to load household data after several attempts")
    }
    # Otherwise, increment the attempt counter
    attempt <- attempt + 1
  })
}


print("Loading house type data (FY033B) from CSO website")
attempt <- 1

#load household data from cso website
while(attempt <= retry_attempts){
  tryCatch({
    hs_type <- cso_get_data("FY033B", pivot_format = "long")
    
    # If the code above runs successfully, the dataframe is presumably loaded
    # Break the loop as there is no need to try again
    attempt <- retry_attempts + 1
  },
  error = function(e){
    # An error occurred, print a message and the error
    print(paste("Attempt", attempt, "failed with error:"))
    print(e)
    
    # If we've reached the maximum number of attempts, stop trying
    if(attempt == retry_attempts){
      stop("Failed to load household data after several attempts")
    }
    # Otherwise, increment the attempt counter
    attempt <- attempt + 1
  })
}



