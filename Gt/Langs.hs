module Gt.Langs
    (
      langs
    , langs_descrs
    , Lang
    )
where

type Lang = String

langs :: [Lang]
langs =
    words
        "auto af sq ar be bg ca zh-CN hr cs da nl en et tl fi fr gl de el iw\
         \ hi hu is id ga it ja ko lv lt mk ms mt no fa pl pt ro ru sr sk sl es\
         \ sw sv th tr uk vi cy yi"

langs_descrs :: [String]
langs_descrs =
    words
        "AutoDetect Afrikaans Albanian Arabic Belarusian Bulgarian \
        \ Catalan Chinese Croatian Czech Danish Dutch English Estonian \
        \ Filipino Finnish French Galician German Greek Hebrew Hindi \
        \ Hungarian Icelandic Indonesian Irish Italian Japanese Korean \
        \ Latvian Lithuanian Macedonian Malay Maltese Norwegian Persian \
        \ Polish Portuguese Romanian Russian Serbian Slovak Slovenian \
        \ Spanish Swahili Swedish Thai Turkish Ukrainian Vietnamese \
        \ Welsh Yiddish"