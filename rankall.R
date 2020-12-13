

################################################################################
# This function returns the nth ranking hospital for each state (and D.C., if
# available. NA's may be returned in case no hospital has a ranking in one of
# the three outcome categories, or if the argument requires a rank greater than
# the total number of hospitals in the state. The result is a dataframe with
# columns state and hospital (name). In the absence of indications in the
# assignment text, ties are settled with alphabetical order for consistency
# with the other 2 functions.
################################################################################
rankall <- function(outcome = c("heart attack", "heart failure", "pneumonia"),
                    ranking) {

    ## Prepare list of state abbreviations
    states <- c("AK","AL","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID",
                "IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO",
                "MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA",
                "RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")

    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    valid_diag <- c("heart attack", "heart failure", "pneumonia")

    # Diagnoses are defined per assignment requirements
    # I'm assuming the same limitations on this arguments apply as in the
    # best() function of same assignment
    if (!(outcome %in% valid_diag))
        stop("ERROR: invalid outcome")

    # State must be a valid abbreviation
    if (!(state %in% states))
        stop("ERROR: invalid state abbreviation")

    # It is not possible to match text (outcome argument) to column names
    # in the database, because names of conditions (e.g., 'heart attack') show
    # up in many, many columns in the DB. Thus, we record the corresponding
    # column name in this variable.
    col <- switch(
        outcome,
        "heart attack" =
            "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
        "heart failure" =
            "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
            "pneumonia" =
            "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    )

    # Coerce outcome values into numeric vector, accepting NA's. This is needed
    # for sorting. We can use variable `col` in the double-bracket syntax for
    # the data frame. Suppressing NA messages for readability of other errors.
    data[[col]] <- suppressWarnings(as.numeric(data[[col]]))

    # We use function order to sort them by state, then from lowest to highest.
    # By default, NAs (which are hospitals who do not reach a sufficient number
    # of cases to have a meaningful rate) are placed last. In case of ties,
    # hospital name gives precedence, in analogy with other functions.
    ord_data <- data[order(
        data$State,
        data[[col]],
        data$Hospital.Name),]

    # Prepare data frame which will carry the results
    results <- data.frame(hospital = character(), state = character())
    # Row counter for row binding
    row_cnt <- 1

    # Loop over the 50
    for (st in states) {
        # Counter is equal to index value of vector (state abbreviation)
        # We need to use moronic subsetting in the absence of dplyr/data.table

        # First subset index selects the rows belonging to a state
        state_hosp <- ord_data[ord_data$State == st,]
        # Then we select the right element
        if (is.numeric(ranking))
            selected <- state_hosp[ranking, "Hospital.Name"]
        else if (ranking == "best")
            selected <- state_hosp[1, "Hospital.Name"]
        else if (ranking == "worst")
            selected <- state_hosp[nrow(state_hosp), "Hospital.Name"]
        else
            # if it doesn't fit here, argument is not supported
            stop("Invalid \`rank\` argument")

        results[row_cnt,] <- c(
            # Hospital name, as selected by `ranking`
            selected,
            # Then we add the state
            st
        )

        # Count with the counter
        row_cnt <- row_cnt + 1
    }

    results


}

