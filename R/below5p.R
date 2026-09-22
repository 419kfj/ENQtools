#' @title get under 5% categories
#' @param graph TRUE/FALSE 入力したdfの度数分布を棒グラフで表示する。default:FALSE
#' @param table TRUE/FALSE 入力したdfの度数分布を表で表示する。default:FALSE
#' @param df active変数として投入する df。変数は、factorであること。NAは、"NA"に変換しておくこと。MCA用
#' @return below5p_vec これは、文字列で出力されるので、exclに指定する時は、他のベクトルをフォーマットを合わせること
#' [ ] todo labelの表示順を文字順にしないこと。2026/09/16
#'
below5p <- function(df,graph=FALSE,table=FALSE){
#  df  |>
#    mutate(across(everything(), ~ replace_na(as.character(.), "NA"))) -> active
    names(df) -> col_labels # 変数ラベル名をcol_labelsに格納

  below5p_vec　<-　NULL
  for (i in col_labels){
    # make freq table(tmp)
    df  |> dplyr::count(!!as.name(i)) |> dplyr::mutate(rate=round(100*n/sum(n),2),check=ifelse(rate <= 5, "*","")) -> tmp
    if(table){print(tmp)}
    tmp  |> dplyr::rename(V1 = 1) -> tmp2
    tmp  |> dplyr::filter(rate <= 5)  |> dplyr::select(1)  |> unlist(1) -> below5p
    below5p_vec_add <- stringr::str_c(i,".",below5p)
    below5p_vec <-　append(below5p_vec,below5p_vec_add)
    # draw bar charts
    if(graph){
    ggplot2::ggplot(tmp2, ggplot2::aes(x = V1, y = rate)) +
      ggplot2::geom_col(fill = "lightblue") +
      ggplot2::geom_hline(yintercept = 5, color = "red", linetype = "dashed", size = 0.5) +
      ggplot2::ggtitle(i) -> p
    print(p)
    }
  }
  return(below5p_vec)
}
