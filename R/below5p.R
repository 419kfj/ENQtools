#' @title below 5%
#'
#' @param df active variables df for MCA
#'

below5p <- function(df){
  df %>%
    dplyr::mutate(across(everything(), ~ replace_na(as.character(.), "NA"))) -> active
  names(active) -> col_labels # 変数ラベル名をcol_labelsに格納

  below5p_vec　<-　NULL
  for (i in col_labels){
    active %>% count(!!as.name(i)) %>% dplyr::mutate(rate=round(100*n/sum(n),2)) -> tmp
    print(tmp)
    tmp %>% rename(V1 = 1) -> tmp2
    tmp %>% dplyr::filter(rate <= 5) %>% select(1) %>% unlist(1) -> below5p
    below5p_vec_add <- str_c(i,".",below5p)
    below5p_vec <-　append(below5p_vec,below5p_vec_add)
    ggplot2::ggplot(tmp2,aes(x=V1,y=rate)) + ggplot2::geom_col(fill="lightblue") + ggtitle(i) -> p
    plot(p)
  }

  return(below5p_vec)
}
