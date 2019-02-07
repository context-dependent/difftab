#' Tabulate wilcoxon signed rank significance test for multiple sets of differences
#'
#' @param dat 
#' @param var 
#' @param f 
#' @param alternative 
#' @param paired 
#' @param browse 
#'
#' @return
#' @export
#'
#' @examples
wilcoxon_signed_rank_test <- function(dat, 
                                      f, 
                                      alternative = "two.sided", 
                                      paired = TRUE, 
                                      browse = FALSE) {
  
  if(browse) browser()
  
  lhs <- rlang::f_lhs(f)
  rhs <- rlang::f_rhs(f)
  
  dat <- dat %>% dplyr::filter(length(unique(!!rhs)) == 2)
  
  dat <- dat %>% 
    tidyr::nest() %>% 
    dplyr::mutate(
      wilcox_test = purrr::map(
        data, 
        ~wilcox.test(
            f,
            data = .x,
            alternative = alternative,
            paired = paired, 
            correct = FALSE
          )
        ) %>% 
        purrr::map(clean_wilcox_test),
      
      observed_difference = purrr::map(
        data, 
        ~ dplyr::group_by(.x, !!rhs) %>%
            dplyr::summarize(
              observed_mean = mean(!!lhs)
            ) %>% 
            dplyr::arrange(!!rhs) %>% 
            dplyr::summarize(
              difference_formula = str_c(last(!!rhs), " - ", first(!!rhs)),
              observed_difference = last(observed_mean) - first(observed_mean)
            ) 
        )
    ) 
  
  dat$N <- dat$data %>% 
    map(
      ~ dplyr::group_by(.x, !!rhs) %>% 
          dplyr::summarize(n = n()) %>% 
          dplyr::mutate(var = str_c("N_", !!rhs)) %>% 
          dplyr::select(n, var) %>% 
          tidyr::spread(var, n)
    )
  
  dat <- dat %>% 
    dplyr::select(
      -data
    ) %>% 
    tidyr::unnest()
  
  dat
  
}

clean_wilcox_test <- function(test_object) {
  
  tibble::tibble(
    p_value = test_object$p.value
  )
  
}
