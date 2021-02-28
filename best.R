# Finds the best hospital in the state. 
best <- function(state, outcome) {
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

        # Order the data set by the chosen outcome
        outcome_column <- 0
        if (tolower(outcome) == "heart attack") {
                outcome_column <- 11
        } else if (tolower(outcome) == "heart failure") {
                outcome_column <- 17
        } else {outcome_column <- 23}
        ordered_df <- new_outcome[order(as.numeric(new_outcome[, outcome_column])),]

        # Test for ties and select first alphabetical hospital to return
        best_hospitals <- ordered_df[ordered_df[,outcome_column] == ordered_df[1, outcome_column], 2]
        if (length(best_hospitals) > 1) {
                ordered_best <- order(best_hospitals)
                final_df <- best_hospitals[ordered_best[1]]
                return(final_df)
        } else {return(best_hospitals)}
}

