#' @title mosaic with sort
#'
#' @param tbl mosaic表示する表をdfで与える
#' @param sort_num sortしたい列番号（sortなしのNULLがdefault）
#' @param title グラフのタイトル
#' @param Rcol RColorbrewerの色セット名（"Set2"がdefault）
#' @param lmar 左マージン
#' @param tmar topマージン
#' @importFrom showtext showtext_auto
#' @examples
#' \dontrun{
#' mosaic_sort(Q1_tbl_df, sort_num = 1, title = "Q1 サービスの利用頻度")
#' }
#' @export


mosaic_sort <- function(tbl,Rcol="Set2",sort_num=NULL,title="mosaic_sort",lmar=5,tmar=5,rot=c(left=0,top=0,right=0)){
  # tbl=dataframe, tbl=Q1_tbl_df
  #　Rcol=色セット名
  #　sort_cat　sortするカテゴリ番号　NULLだと、sortなし sort_cat=1

  showtext::showtext_auto(TRUE)

  nc <- (dim(tbl))[2]
  nr <- (dim(tbl))[1]
  # 1. 4色を取得
  colset <- brewer.pal(4, Rcol)
  # 2. 各色を「行数（32回）」ずつ繰り返した、長さ124（31×4）のベクトルを作る
  # これにより、1列目＝色1、2列目＝色2 ... と綺麗に並びます
  col_matrix <- rep(colset, each = nr)


  # 列変数のカテゴリ「日常的」で降順sortする
  #tbl <- Q1_tbl_mx # 分析対象のデータをtblに格納
  #sort_cat = NULL

  c_names <- colnames(tbl)
  sort_idx <- c_names[sort_num] # 1 "日常的"

  if(!is.null(sort_num)){
    sort_idx <- order(tbl[, sort_idx], decreasing = TRUE) # indexの取得
    tbl_sorted <- as.table(as.matrix(tbl[sort_idx,])) # 行をsort_idxで並べ替えて、tbl_sortedに格納
  } else {tbl_sorted <- as.table(as.matrix(tbl))}

  dim_list <- dimnames(tbl_sorted)
  dimnames(tbl_sorted) <- list("rows" = dim_list[[1]],
                               "cols" = dim_list[[2]])
  # セルに表示する割合値を計算

  N <- 1200
  prop_tbl <- 100*(tbl_sorted/N)  #その行も合計1200になっていたので、それを割合を計算し、100倍して「％」にする。

  #prop_tbl_1 %>% str()

  text_matrix <- matrix(round(as.matrix(prop_tbl), 1), nrow = nrow(prop_tbl)) # 割合表示用のmatrixを生成
  text_table <- as.table(text_matrix) # このmatrixをtableに変換し、text_tableとする。ただ、行名列名、行カテゴリ、列カテゴリがとんでる
  dimnames(text_table) <- dimnames(tbl_sorted) #もとの_sortedのdimnamesにコピー。

  vcd::mosaic(#as.matrix(tbl_sorted),
    tbl_sorted,
    gp=grid::gpar(fill=col_matrix,col=0),
    #              rot_labels = c(left = 0, top = 45,right=0),
    rot_labels =rot,
    margins=c(left=lmar,top=tmar),
    just_labels=c(left="right",top="left"),
    keep_aspect_ratio=FALSE,
    main = title,
    pop = FALSE
  )
  labeling = vcd::labeling_cells(text = text_table,clip = FALSE)(tbl_sorted)

  # mosaic_sort(Q1_tbl_df,sort_num = 1,title = "Q1 サービスの利用頻度")
}
