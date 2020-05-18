generate_prefec <- function(sep = NULL) {
  prefec <- c("北海道", "青森", "岩手", "宮城", "福島", "茨城", "千葉",
              "神奈川", "静岡", "愛知", "三重", "和歌山", "大阪",
              "徳島", "高知", "愛媛", "大分", "宮崎", "鹿児島")
  if (is.null(sep)) return(prefec)
  paste(prefec, collapse = sep)
}
