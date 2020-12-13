

rankhospital <- function(state,
                         outcome = c("heart attack", "heart failure", "pneumonia"),
                         ranking
                         ) {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    valid_diag <- c("heart attack", "heart failure", "pneumonia")
    ## Check that state and outcome are valid
    ## In this use case, we can use the data itself to verify that argument
    ## is one of the states
    if (!(state %in% data$State))
        stop("ERROR: invalid state")

    # Diagnoses are defined per assignment requirements
    # I'm assuming the same limitations on this arguments apply as in the
    # best() function of same assignment
    if (!(outcome %in% valid_diag))
        stop("ERROR: invalid outcome")

    ## Subset data selecting rows for this particular state
    subdata <- data[data$State == state,]
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
    subdata[[col]] <- suppressWarnings(as.numeric(subdata[[col]]))
    # Filter out hospitals which don't have specific data on this particular
    # outcome
    subdata <- subdata[!is.na(subdata[[col]]),]
    # We use function order to sort them from lowest to highest. By
    # default, NAs (which are hospitals who do not reach a sufficient number of
    # cases to have a meaningful rate) are placed last. In case of ties,
    # hospital name gives precedence.
    subdata <- subdata[order(
        subdata[[col]],
        subdata$Hospital.Name),]

    # Data is now sorted. We're supposed to return either the best, worst or
    # nth ranking hospital
    if (is.numeric(ranking))
        # If numeric, return nth rank
        subdata[ranking, "Hospital.Name"]
    else if (ranking == "best")
        # return first ranking
        subdata[1, "Hospital.Name"]
    else if (ranking == "worst")
        # return last hospital in the rankings
        subdata[nrow(subdata), "Hospital.Name"]
    else
        # if it doesn't fit here, argument is not supported
        stop("Invalid \`rank\` argument")
}

