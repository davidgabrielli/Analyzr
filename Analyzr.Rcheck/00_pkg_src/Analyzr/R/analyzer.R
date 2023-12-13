#' Data Analyzer
#'
#' A package for examining and formatting data.
#'
#' @param data A data frame containing binary, continuous, nonimal factor, or ordinal factor.
#' @param sample_size An integer determining the sample size of each variable used for normality testing. Default is 50 or max in dataframe if less than 50.
#' @param norm_test True or False used to determine if Shapiro-Wilk Normality testing is conducted on variables in dataframe.
#'
#' @return A list of all variables and types, a list of missing variables, and a Shapiro-Wilk normality metric.
#' Places variables into new dataframes based on variable class and evaluates readiness for correlation testing
#' by corr_function.
#' @return full_shapiro_df A dataframe with the Shapiro-Wilk results for all variables in your data file
#' @return sig_shapiro_df A dataframe with the Shapiro-Wilk results for all variables that were statistically significant (p <= 0.05)
#' @return failed_var A list of the variables that failed the Shapiro-Wilk Normality Test
#' @return cont_vars A dataframe of all the continuous variables in your data file - will be empty if none
#' @return bi_vars A dataframe of all the binary variables in your data file - will be empty if none
#' @return nom_vars A dataframe of all the nominal factor variables in your data file - will be empty if none
#' @return ord_vars A dataframe of all the ordered factor variables in your data file - will be empty if none
#' @return missing_info A dataframe with the missingness summary
#' @return variables A summary table of the variable types by variable name
#' @return var_type_count A summary table of the variable counts by type
#'
#' @export
#' 
#' @examples
#' data(iris)
#' data(mtcars)
#' data(train)
#' 
#' @import stats
#' tidyverse
#' knitr
#' kableExtra
#' dplyr
#' dunn.test
#' DescTools
#' corrplot
#' vcd
#' VIM
#'
#'


analyzer <- function(data, sample_size = 50, norm_test = T) {


    # Variable and Missingness

    # Check for data.frame data class
    if (!is.data.frame(data)) {
        cat("\n\nYour data file is not a dataframe class. Please convert your file to class dataframe and rerun the function.\n\n")
    } else {


        # Check for character variables
        has_char <- any(sapply(data, is.character))

        if (has_char) {
            char_names <- names(which(sapply(data, is.character)))
            char_vars <- data.frame(`Character Variable` = char_names)

            cat("\n\nThere are charactor variables in your data set. These will be treated as Nominal Factor variables. If this is incorrect, please reclassify these variables and rerun the function. \n",
                fill = T)

            cat("\nThe character variables are:\n")
            print(char_vars)

        }

        # Get variable types
        cont_names <- names(which(sapply(data, is.numeric)))
        cont_vars <- data[, cont_names, drop = F]
        bi_names <- names(which(sapply(data, function(x) (is.factor(x) && length(levels(x)) == 2) || (is.character(x) && length(unique(x)) == 2))))
        bi_vars <- data[, bi_names, drop = F]
        nom_names <- names(which(sapply(data, function(x) (is.factor(x) && length(levels(x)) >= 3 && !is.ordered(x)) || (is.character(x) && length(unique(x)) >=
            3 && !is.ordered(x)))))
        nom_vars <- data[, nom_names, drop = F]
        ord_names <- names(which(sapply(data, function(x) is.ordered(x) && length(levels(x)) >= 3)))
        ord_vars <- data[, ord_names, drop = F]

        # Determine max length of name vectors
        max_len <- max(length(cont_names), length(bi_names), length(nom_names), length(ord_names))

        # Adjust vectors to all have the same length
        cont_names_full <- c(cont_names, rep("-", length.out = c(max_len - length(cont_names))))
        bi_names_full <- c(bi_names, rep("-", length.out = c(max_len - length(bi_names))))
        nom_names_full <- c(nom_names, rep("-", length.out = c(max_len - length(nom_names))))
        ord_names_full <- c(ord_names, rep("-", length.out = c(max_len - length(ord_names))))

        # Create a dataframe with counts of variable type
        var_count_df <- data.frame(Continuous = length(cont_names), Binary = length(bi_names), `Non-Binary Nominal` = length(nom_names), `Non-Binary Ordered` = length(ord_names))

        # Create a dataframe with each variable
        var_df <- data.frame(Continuous = cont_names_full, Binary = bi_names_full, `Non-Binary Nominal` = nom_names_full, `Non-Binary Ordered` = ord_names_full)

        # Create formatted tables of variable types
        var_count_table <- kable(var_count_df, format = "pipe", align = "c")
        var_table <- kable(var_df, format = "pipe", align = "c")

        # Get sample size
        sample_sz <- nrow(data)

        # Print the variable statistics
        cat("\nData Set Summary")
        cat("\n\nThe sample size for this data set is ", sample_sz, "\n\n")
        cat("Counts of variable by class")
        print(var_count_table)

        if (max_len > 10) {

            cat("\n\nVariable List for each class")
            print(var_table)

        } else {

            cat("\n\nVariable List for each class")
            print(var_table)
        }


        # Analyze missingness

        missing_values <- data %>%
            summarise_all(function(x) sum(is.na(x)))

        if (sum(missing_values) > 0) {
            missing_counts <- sapply(data, function(x) sum(is.na(x)))
            missing_columns <- names(missing_counts[missing_counts > 0])

            # Combine column names with their corresponding number of missing values
            missing_info <- data.frame(MissingCount = missing_counts[missing_columns])

            cat(paste("\n\nThere are", length(missing_columns), "variables with missing values and the Shapiro-Wilk test was not conducted on them. Please address this missingness and rerun this function. The MICE package is recommended to assist with imputation."),
                fill = T)

            cat("\n\nThe variables with missingness are:")
            print(missing_info)
            
            invisible(list(cont_vars = cont_vars, bi_vars = bi_vars, nom_vars = nom_vars, ord_vars = ord_vars, missing_df = missing_info, variables = var_table, var_type_count = var_count_table))
            
        } else {

            # No missingness

            # Clear missing_info object
            missing_info <- data.frame()

            cat("\n\nWoot woot! There are no missing values in this data set.\n")

        
        # Shapiro-Wilk Normality

        if (norm_test == T) {
            # Initialize a lists to store results
            shapiro_results <- list()
            sig_results <- list()

            # Initialize failed and missing variable vectors
            failed_var <- character()
            missing_var <- character()

            # Initialize dataframes
            shapiro_results_df <- data.frame()
            sig_results_df <- data.frame()

            # Loop through each continuous variable
            for (col_name in cont_names) {
                col <- cont_vars[[col_name]]

                # Check if the column has more than one non-NA value
                if (sum(!is.na(col)) > 1) {
                  if (sample_size > 17) {
                    if (sample_size > 50) {
                      sample_data <- sample(col, size = sample_size)

                      # Run Shapiro-Wilk test
                      shapiro_test <- shapiro.test(sample_data)
                    } else {
                      # Run Shapiro-Wilk test
                      shapiro_test <- shapiro.test(col)
                    }


                    # Check if the test was successful and populate data.frame
                    if (!is.null(shapiro_test$p.value)) {
                      shapiro_res <- data.frame(Variable = col_name, W = shapiro_test$statistic, P_Value = shapiro_test$p.value)
                      shapiro_results[[col_name]] <- shapiro_res
                    }
                    if (shapiro_test$p.value < 0.05) {
                      sig_res <- data.frame(Variable = col_name, W = shapiro_test$statistic, P_Value = shapiro_test$p.value)
                      sig_results[[col_name]] <- sig_res
                      failed_var <- c(failed_var, col_name)

                    }

                  } else {
                    cat("\n\nThe data set sample size is less than 17, so the Shapiro Wilk test was not conducted. Please use alternate normality testing, such as a Q-Q plot or histogram.\n",
                      fill = T)
                  }

                }


                # Combine All Shapiro-Wilk results into a data frame
                shapiro_results_df <- do.call(rbind, shapiro_results)

                # Replace column index with a sequence starting from 1
                rownames(shapiro_results_df) <- seq_len(nrow(shapiro_results_df))

                # Combine Sig Shapiro-Wilk results into a data frame
                sig_results_df <- do.call(rbind, sig_results)

                # Replace column index with a sequence starting from 1
                rownames(sig_results_df) <- seq_len(nrow(sig_results_df))
            }
            # Print normality test result

            cat("\n\nA number of variables failed the Shapiro-Wilk Normality test and may not be normally distributed. It is recommended to plot a histogram of each variable to verify these findings and perform the appropriate transformations if necessary prior to continuing to do correlation testing. The results for all variables with p-values and w scores can be found in the output table. \n\n",
                fill = T)

            

            cat("The variables that failed the normality test are:\n")
            cat(paste(failed_var, collapse = "\n"), "\n")
            
            invisible(list(full_shapiro_df = shapiro_results_df, sig_shapiro_df = sig_results_df, failed_var = failed_var, cont_vars = cont_vars, bi_vars = bi_vars, nom_vars = nom_vars,
                           ord_vars = ord_vars, variables = var_table, var_type_count = var_count_table))
            
            
        } else {
            cat("Normality testing was not conducted on the variables in your data set. Use caustion when testing for correlation with the Point-Biserial Correlation Test or other metrics that assume normality.",
                fill = T)

          invisible(list(cont_vars = cont_vars, bi_vars = bi_vars, nom_vars = nom_vars, ord_vars = ord_vars, missing_df = missing_info, variables = var_table, var_type_count = var_count_table))
        }
      }
    }
}

