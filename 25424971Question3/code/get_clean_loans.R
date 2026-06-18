get_clean_loans <- function(data) {

data %>%
    select(loan_amnt, term, int_rate, grade, sub_grade,
           emp_title:loan_status, purpose, addr_state, dti, earliest_cr_line,
           tax_liens, pub_rec_bankruptcies, num_rev_accts,
           num_tl_120dpd_2m:pct_tl_nvr_dlq, num_accts_ever_120_pd, mort_acc,
           acc_now_delinq, delinq_amnt, chargeoff_within_12_mths,
           all_util, bc_util, il_util,
           avg_cur_bal, total_rev_hi_lim, total_bal_ex_mort,
           tot_coll_amt
           ) %>%
    mutate(
        loan_outcome = case_when(
            loan_status == "Fully Paid" ~ "paid",
            loan_status == "Charged Off" ~ "loss",
            loan_status == "Current" ~ "current",
            loan_status == "Default" ~ "default",
            loan_status %in% c("Late (31-120 days)", "In Grace Period", "Late (16-30 days) ") ~ "partial default"
                                ),

        concluded_loan = case_when(
            loan_outcome %in% c("loss", "default") ~ 1,
            loan_outcome == "paid" ~ 0,
            TRUE  ~ NA_integer_
            ),


        default_status = case_when(
            loan_outcome  %in% c("default", "loss") ~ "broad default",
            loan_outcome  %in% c("paid", "current") ~ "broad paid",
            loan_outcome == "partial default" ~ "partial default"
            ),

        term = as.numeric(str_remove(term, " months")),


        emp_length = factor(emp_length,
                            levels = c("< 1 year", "1 year", "2 years", "3 years",
                                       "4 years", "5 years", "6 years", "7 years",
                                       "8 years", "9 years", "10+ years"),
                            ordered = TRUE),

        verification_status = factor(verification_status,
                                     levels = c("Not Verified",
                                                "Source Verified",
                                                "Verified")),

        credit_age_yrs = 2026 - as.integer(str_sub(earliest_cr_line, -4)),

    ) %>%
        filter(dti <= quantile(dti, 0.99, na.rm = TRUE),
               dti >= 0,
               bc_util <= quantile(bc_util, 0.99, na.rm = TRUE)

               )

}

