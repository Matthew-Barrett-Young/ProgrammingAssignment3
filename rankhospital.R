# Finds hospital with the selected rank in the selected outcome from a specified state.
rankhospital <- function(state, outcome, num = "best") {
        # Read the data
        df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        # Check for valid arguments
        valid_outcome <- c("heart attack", "heart failure", "pneumonia")
        if (!(toupper(state) %in% df$State)) {
                stop("invalid state")
        } else if (!(tolower(outcome) %in% valid_outcome)) {
                stop("invalid outcome")
        }
        
        # Limit data frame to selected state
        new_outcome <- df[df$State == toupper(state),]
        
        # Order the data set by the chosen outcome and break ties by hospital name
        outcome_column <- 0
        if (tolower(outcome) == "heart attack") {
                outcome_column <- 11
        } else if (tolower(outcome) == "heart failure") {
                outcome_column <- 17
        } else {outcome_column <- 23}
        ordered_df <- new_outcome[order(as.numeric(new_outcome[, outcome_column]), new_outcome[, 2]),]
        
        # Remove cases where the outcome column is NA
        na_logical <- ordered_df[, outcome_column] == "Not Available"
        ordered_df_complete <- ordered_df[!na_logical,]
        
        # Return the name of the hospital from the selected rank or the best one
        number_in_state <- nrow(ordered_df_complete)
        if (tolower(num) == "best") {
                return(ordered_df_complete[1, 2])
        } else if (tolower(num) == "worst") {
                return(ordered_df_complete[number_in_state, 2])
        } else if (num > number_in_state) {
                return(NA)
        } else {
                return(ordered_df_complete[num, 2])
        }
}

