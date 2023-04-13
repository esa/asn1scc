"------------------------------------------------------------------------------
"  Description: Vim ASN.1/ACN detection file
"     Language: ACN (2023)
"    Copyright: Copyright (C) 2023 Maxime Perrotin
"       Author: Maxime Perroti <Maxime.Perrotin@esa.int>
"      Version: 1.0
"     $HeadURL: https://taste.tools
"------------------------------------------------------------------------------

if exists("s:loaded_ftdetect_acn")
    finish
endif

let s:loaded_ftdetect_acn=45

autocmd BufNewFile,BufRead *.acn setfiletype acn

finish " 1}}}

"------------------------------------------------------------------------------
" vim: textwidth=78 nowrap tabstop=3 shiftwidth=3 softtabstop=3 noexpandtab
" vim: foldmethod=marker
