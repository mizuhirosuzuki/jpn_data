# Function to create the main part in each page
main_part <- function(data_show) {

  if (nrow(data_show) > 0) {
    for (i in seq(nrow(data_show))) {
      
      data_i <- data_show %>% slice(i)
      
      data_name <- data_i$データ名
      data_url <- data_i$URL
      data_org <- data_i$`データを管理・提供している団体・個人`
      
      data_publicity <- data_i$データ利用の申請
      data_explanation <- data_i$`データ提供元によるデータの説明`
      data_explanation <- ifelse(is.na(data_explanation), "", data_explanation)
      data_comments <- data_i$`その他コメント`
      data_comments <- ifelse(is.na(data_comments), "", data_comments)
      
      data_area <- data_i$`関連する分野 （複数選択可）`
      data_area_split <- str_split(data_area, ";", simplify = TRUE) %>% as.vector()
      
      for (i in seq_along(data_area_split)) {
        area <- str_trim(data_area_split[i])
        if (area == "労働") {
          data_area_split[i] <- paste0("[", area, "](labor.html)")
        } else if (area == "保健・健康") {
          data_area_split[i] <- paste0("[", area, "](health.html)")
        } else if (area == "農業") {
          data_area_split[i] <- paste0("[", area, "](agriculture.html)")
        } else if (area == "教育・保育") {
          data_area_split[i] <- paste0("[", area, "](education.html)")
        } else if (area == "交通・移住") {
          data_area_split[i] <- paste0("[", area, "](transportation.html)")
        } else if (area == "政治") {
          data_area_split[i] <- paste0("[", area, "](politics.html)")
        } else if (area == "産業") {
          data_area_split[i] <- paste0("[", area, "](industry.html)")
        } else if (area == "天候") {
          data_area_split[i] <- paste0("[", area, "](weather.html)")
        } else if (area == "地理情報") {
          data_area_split[i] <- paste0("[", area, "](geo.html)")
        } else if (area == "自然言語") {
          data_area_split[i] <- paste0("[", area, "](nlp.html)")
        } else if (area == "画像データ") {
          data_area_split[i] <- paste0("[", area, "](image.html)")
        } else if (area == "その他") {
          data_area_split[i] <- paste0("[", area, "](other.html)")
        }
      }
      
      paste(data_area_split, collapse = ", ")
      
      data_example1 <- data_i$`当該データを用いた研究・分析例 （1）`
      data_example2 <- data_i$`当該データを用いた研究・分析例 （2）`
      data_example3 <- data_i$`当該データを用いた研究・分析例 （3）`
      
      cat("\n")
      cat("\n")
      cat("## ", data_name, "\n")
      cat("関連分野: ", paste(data_area_split, collapse = ", "), "\n")
      cat("\n")
      cat("URL: ", data_url, "\n")
      cat("\n")
      cat("データを管理・提供している団体・個人: ", data_org, "\n\n ")
      cat("\n")
      cat("データ利用の申請: ", data_publicity, "\n\n ")
      cat("\n")
      
      if (str_trim(data_explanation) != "") {
        cat("#### データ提供元によるデータの説明 \n")
        cat("\n")
        cat(data_explanation, "\n")
        cat("\n")
      }

      j <- 0
      for (example in c(data_example1, data_example2, data_example3)) {
        example <- ifelse(is.na(example), "", example)
        if (str_trim(example) != "") {
          j <- j + 1
            cat("#### 研究・分析例  その", j, "\n")
            cat("\n")
            cat(example, "\n")
            cat("\n")
        }
      }

      if (str_trim(data_comments) != "") {
        cat("#### その他コメント \n")
        cat("\n")
        cat(data_comments, "\n")
        cat("\n")
      }

    } 
  }

}
