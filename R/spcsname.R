#' Change species name in a specific prefecture
#'
#' @param dat Tidy tibble data from load_catch
#' @param prefec Prefecture that contains species name to be changed
#' @param original Species name to be changed
#' @param newname New species name
#' @return tidy data with new species name
recode_spcsname <- function(dat, prefec, original, newname) {
  dat %>%
    dplyr::mutate(
      Species = dplyr::if_else(Prefec %in% c(prefec),
                               dplyr::if_else(Species == original,
                                              newname, Species), Species)
    )
}
#' Standardize species name in a database
#'
#' @param dat Tidy data from load_catch
#' @return tidy data with new species name
standardize_spcsname <- function(dat) {

  all_prefec <- generate_prefec()

  dat %>%
    recode_spcsname("千葉", "2種混じり", "さば類") %>% # 小型銘柄で種判別ができなかったもの
    recode_spcsname("千葉", "その他", "さば類") %>% # 小型銘柄で種判別ができなかったもの
    recode_spcsname("和歌山", "さば", "さば類") %>%
    recode_spcsname("宮崎", "その他", "その他シラス") %>% # 確認の後修正
    recode_spcsname("鹿児島", "その他", "その他魚種") %>% # 確認の後修正
    recode_spcsname("鹿児島", "メナガ", "マルアジ") %>%
    recode_spcsname(all_prefec, "maiwashi", "マイワシ") %>% # レガシー使用県(HK, TK, EH, KG) # nolint
    recode_spcsname(all_prefec, "サバ類", "さば類") %>%
    recode_spcsname(all_prefec, "カタクチ", "カタクチイワシ") %>%
    recode_spcsname(all_prefec, "ウルメ", "ウルメイワシ") %>%
    recode_spcsname(all_prefec, "サバ", "さば類") %>%
    recode_spcsname(all_prefec, "　ｺﾞﾏｻﾊﾞ", "ゴマサバ") %>%
    recode_spcsname(all_prefec, "ｻﾊﾞ類", "さば類") %>%
    recode_spcsname(all_prefec, "ｺﾞﾏｻﾊﾞ", "ゴマサバ") %>%
    recode_spcsname("大分", "カタクチイワシ稚魚", "カタクチシラス") %>% # 担当者(上村さん)確認済
    recode_spcsname("宮崎", "イワシシラス", "マシラス") %>% # 要確認
    recode_spcsname("鹿児島", "ソウダ", "ソウダガツオ") %>% #ソウダガツオ2種混じり？要確認
    recode_spcsname("鹿児島", "メチカ", "マルソウダ") %>%
    recode_spcsname("高知", "計", "さば類") %>% # マサバゴマサバ活サバ混入 修正必要アリ
    dplyr::mutate(Species = dplyr::if_else(Prefec == "高知",
                                            dplyr::if_else(is.na(Species),
                                                           "さば類", Species),
                                            Species))
}
