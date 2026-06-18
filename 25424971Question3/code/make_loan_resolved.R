make_loan_resolved <- function(data) {

    data %>%
        filter(!is.na(concluded_loan))

}