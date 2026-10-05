#' @title mosaic with sort
#'
#' @param tbl mosaic表示する表をdfで与える:このdfは、make_Grid_table.R で作成すること
#' @param sort_num sortしたい列番号（sortなしのNULLがdefault）
#' @param title グラフのタイトル
#' @param Rcol RColorbrewerの色セット名（"Set2"がdefault）
#' @param lmar 左マージン
#' @param tmar topマージン
#' @param rot 行、列の表示ラベルの回転 rot=c(left=0,top=0,right=0)
#' @param rate TRUE：mosaicのセルに行比率を表示する。 FALSE:度数をそのまま表示
#' @param ... mosaic() にパラメータを渡す
#' @importFrom showtext showtext_auto
#' @examples
#' \dontrun{
#' df |> make_Grid_table() -> df.grd
#' mosaci_sort(df.grd)
#'
#' 度数表をdfとして入力すれば、度数でsortし、それを度数（rate=FALSE）か比率（rate=TRYE：default）で表示する
#'
#比率表をprop.tableでつくって、入力する場合は、以下のようにする。
# prop.table(q4.grd[,1:4] |> as.matrix(),1) |>
# as.data.frame() |> mosaic_sort(sort_num = 4)
#'
#' }
#' @export

mosaic_sort <- function (tbl, Rcol = "Set2", sort_num = NULL, title = "mosaic_sort",
                          lmar = 5, tmar = 5, rot = c(left = 0, top = 0, right = 0),
                          N = NULL, rate = TRUE, ...)
{
  showtext::showtext_auto(TRUE)
  nc <- (dim(tbl))[2]
  nr <- (dim(tbl))[1]
  colset <- RColorBrewer::brewer.pal(nc, Rcol)
  col_matrix <- rep(colset, each = nr)
  c_names <- colnames(tbl)
  sort_idx <- c_names[sort_num]
  if (!is.null(sort_num)) {
    sort_idx <- order(tbl[, sort_idx], decreasing = TRUE)
    tbl_sorted <- as.table(as.matrix(tbl[sort_idx, ]))
  }
  else {
    tbl_sorted <- as.table(as.matrix(tbl))
  }
  dim_list <- dimnames(tbl_sorted)
  dimnames(tbl_sorted) <- list(rows = dim_list[[1]], cols = dim_list[[2]])
  if (rate) {
    ptbl <- prop.table(as.matrix(tbl_sorted), margin = 1)
    prop_tbl <- 100 * ptbl
  }
  else {
    prop_tbl <- tbl_sorted
  }
  text_matrix <- matrix(round(as.matrix(prop_tbl), 1), nrow = nrow(prop_tbl))
  text_table <- as.table(text_matrix)
  dimnames(text_table) <- dimnames(tbl_sorted)
  vcd::mosaic(tbl_sorted, gp = grid::gpar(fill = col_matrix,
                                          col = 0), rot_labels = rot, margins = c(left = lmar,
                                                                                  top = tmar), just_labels = c(left = "right", top = "left"),
              keep_aspect_ratio = FALSE, main = title, pop = FALSE,...)
  labeling = (vcd::labeling_cells(text = text_table, clip = FALSE))(tbl_sorted)
}
