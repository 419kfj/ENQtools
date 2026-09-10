# 2重化recode
#   Greenacre 2017=2020のchap26、CARME2014 chap15 を参照。
#
# vari = "A"
# bind_cols(
#   doubling(dd,"A", M = 5),
#   doubling(dd,"B", M = 5),
#   doubling(dd,"C", M = 5),
#   doubling(dd,"D", M = 5)
# )
#　

doubling <- function(df,vari,sel = NULL,M){
  M <- M

  if (!is.null(sel)) {
    df <- df |>
      mutate(!!vari := na_if(.data[[vari]], sel))
  }

  vari_minus <- str_c(vari,"-")
  vari_plus <- str_c(vari,"+")

  df[[vari_minus]] <- df[[vari]] -1
  df[[vari_plus]] <- M - df[[vari]]

  out_df <- tibble(
    !!vari_minus := df[[vari]] - 1,
    !!vari_plus  := M - df[[vari]]
  )

  return(out_df)
}

# bind_cols(
#   doubling(dd,"A", M = 5),
#   doubling(dd,"B", M = 5),
#   doubling(dd,"C", M = 5),
#   doubling(dd,"D", M = 5)
# )
