# Returns a data from of the hospital with the selected rank based on the outcome selected
rankall <- function(outcome, num = "best") {
        # Read in data
        dta <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        # Check for valid arguments
        valid_outcome <- c("heart attack", "heart failure", "pneumonia")
        if (!(tolower(outcome) %in% valid_outcome)) {
                stop("invalid outcome")
        }
        # Trim data frame down to name, state, and selected outcome
        outcome_column <- 0
        if (tolower(outcome) == "heart attack") {
                outcome_column <- 11
        } else if (tolower(outcome) == "heart failure") {
                outcome_column <- 17
        } else {outcome_column <- 23}
        dta_trimmed <- dta[, c(2, 7, outcome_column)]
        
        # Remove cases where the outcome column is NA
        na_logical <- dta_trimmed[, 3] == "Not Available"
        dta_trimmed <- dta_trimmed[!na_logical,]
        
        # For each state select the specified rank based on the outcome variable
        state_factors <- gl(54, 1, labels = unique(dta_trimmed[, 2]))
        complete_dta_ranked <- data.frame(Hospital.Name = character(0), 
                                          State = character(0),
                                          Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack = character(0))
        counter <- 1
        
        for (state in state_factors) {
                dta_single_state <- dta_trimmed[dta_trimmed$State == state ,]
                
                # Test if number of rows is less than num variable
                if (is.numeric(num) & (nrow(dta_single_state) > num)) {
                        valid_num <- TRUE
                } else valid_num <- {
                        FALSE
                }
                # Order the data for the state by outcome
                ordered_dta <- dta_single_state[order(as.numeric(dta_single_state[, 3]), 
                                                       dta_single_state[, 1]),]
                
                # Appends the appropriate observation to the return data frame
                if (valid_num == TRUE) {
                        complete_dta_ranked[counter, ] <- ordered_dta[num, ] 
                } else if (num == "best") {
                        complete_dta_ranked[counter, ] <- ordered_dta[1, ]
                } else if (num == "worst") {
                        complete_dta_ranked[counter, ] <- ordered_dta[nrow(ordered_dta), ]
                } else {
                        complete_dta_ranked[counter, ] <- c("NA", state, "NA")
                }
                counter <- counter + 1
        }
        
        # Return data frame of the specified rank in the outcome for each state
        complete_dta_ranked <- complete_dta_ranked[order(complete_dta_ranked$State), ]
        return(complete_dta_ranked[, 1:2])
}
