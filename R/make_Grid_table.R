#'
#'
#' @title  make Grid table
#' これで生成したdfをつかって、mosaic_sortを実行できる。
#' 行変数（複数） これの回答カテゴリは同一であること！
#'

# Grid Mosaic Genenral

make_Grid_table <- function(df,cols_num){

  library(tidyverse)
  library(showtext)
  showtext_auto(TRUE)

  #load("R/df_all.rda")
  #cols_num <- 28:38

  N <- nrow(df)

  selected_vars <- df[,cols_num] %>% names#input$variables

  # 【★超重要・一撃必殺の防弾処理】
  # 選択された列のデータを、Factor型から「ただのプレーンな文字列型(character)」に強制変換する
  # これにより、3838環境下での "1", "2" への化けを根絶します
  cleaned_data <- df_all[,cols_num]
  #df_all %>%
  #dplyr::mutate(across(dplyr::all_of(selected_vars), as.character))

  # 1. 以降の集計は、すべてこの文字列化した「cleaned_data」をベースに行う！
  vectors <- purrr::map(selected_vars, ~ {
    cleaned_data %>%
      dplyr::pull(!!sym(.x)) %>%
      unique()
  })

  union_all <- reduce(vectors, union)
  union_all <- ifelse(is.na(union_all), "NA", union_all)

  # カテゴリを集計するための関数（ここも cleaned_data を見るように徹底）
  count_categories <- function(x) {
    table(factor(x, levels = union_all, exclude = NULL))
  }

  # 2. selected_vars の各列ごとにカテゴリを集計
  category_count_tbl <- cleaned_data %>%
    dplyr::reframe(across(dplyr::all_of(selected_vars), ~ count_categories(.)))

  # （※これ以降の3、4、5、6の行列変換・描画コードは、今のままで全く触らなくて大丈夫です！）

  # 3. マトリックス（行列）形式に変換し、行名（回答カテゴリ）を付与
  cat_tbl <- as.matrix(category_count_tbl)
  rownames(cat_tbl) <- union_all

  # 4. 行列を転置して、Rの標準機能（order）で頑丈に並び替え
  mat_t <- t(cat_tbl)
  #order_vec <- order(mat_t[, 1], decreasing = TRUE)
  #sorted_matrix <- mat_t[order_vec, , drop = FALSE]
  sorted_matrix <- mat_t

  # 5. 2次元のクロス表オブジェクト（table）に完全固定
  final_table <- as.table(sorted_matrix)
  names(dimnames(final_table)) <- c("Question", "Response")

  # 【★この1行だけを仕込む！】
  #print("--- 3838環境での final_table のナマの構造 ---")
  #str(final_table)

  # 6. 完全に構造が保証された table を mosaic に投入 2026/07/10　shade=TRUEを削除。col_matrixをつくり、使う

  final_table
  # %>%
  #   vcd::mosaic(#shade = TRUE,
  #     rot_labels = c(0, 0),
  #     margins = c(left = 12, top = 5),
  #     just_labels = c(left = "right", top = "left"))

  return (final_table,N)
}
