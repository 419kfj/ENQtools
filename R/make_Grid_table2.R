#'
#'
#' @title  make Grid table
#' これで生成したdfをつかって、mosaic_sortを実行できる。
#' 行変数（複数） これの回答カテゴリは同一であること！
#'

# Grid Mosaic Genenral

make_Grid_table <- function(df) {

  # 1. 各変数の水準（レベル）を取得
  # factorのレベル情報を取得（※ factorではない列がある場合は水準として重複を除いた値を利用）
  var_levels <- lapply(df, function(x) {
    if (is.factor(x)) {
      levels(x)
    } else {
      unique(as.character(x))
    }
  })

  # 2. 変数間での選択肢の一致チェック
  # 基準となる第1変数の選択肢
  ref_levels <- var_levels[[1]]
  mismatches <- list()

  # 2番目以降の変数と第1変数の選択肢（順番を含む）を比較
  if (length(df) > 1) {
    for (i in 2:length(df)) {
      if (!identical(ref_levels, var_levels[[i]])) {
        mismatches[[length(mismatches) + 1]] <- sprintf(
          "「%s」と「%s」", names(df)[1], names(df)[i]
        )
      }
    }
  }

  # 選択肢が異なる変数が存在する場合は警告を出して終了
  if (length(mismatches) > 0) {
    warning(
      "選択肢（水準）が一致しない変数があります:\n",
      paste(mismatches, collapse = "\n"),
      call. = FALSE
    )
    return(NULL)
  }

  # 3. 積み重ね集計表の作成
  # 各変数の値の度数をカウントして結合
  grid_matrix <- do.call(rbind, lapply(df, function(x) {
    table(factor(x, levels = ref_levels))
  }))

  # 行名と列名の設定
  rownames(grid_matrix) <- names(df)
  colnames(grid_matrix) <- ref_levels

  # 4. データフレーム化してリストで返却
  Gridtbl <- as.data.frame(grid_matrix)

  return(Gridtbl)
}
