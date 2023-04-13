" Vim syntax file
" Language:	ACN (ASN.1 Coding Notation)
" Maintainer:	Maxime Perrotin <Maxime.Perrotin@esa.int>
" URL:		https://taste.tools
" Last Change:	2023 Apr 13

" quit when a syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

let s:cpo_save = &cpo
set cpo&vim

" keyword definitions
syn keyword acnExternal		DEFINITIONS BEGIN END CONSTANT IMPORTS EXPORTS FROM
" syn keyword acnFieldOption	
syn keyword acnTypeInfo		ABSENT PRESENT
syn keyword acnBoolValue	TRUE FALSE
syn keyword acnNumber		MIN MAX byte word dword ASCII BCD big little
syn match   acnNumber		"\<pos-int\>"
syn match   acnNumber		"\<IEEE754-1985-32\>"
syn match   acnNumber		"\<IEEE754-1985-64\>"
syn match   acnNumber		"\<null-terminated\>"
syn match   acnNumber		"\<twos-complement\>"
syn match   acnType		"\<present-when\>"
syn match   acnType		"\<encode-values\>"
syn match   acnType		"\<mapping-function\>"
syn match   acnType		"\<post-encoding-function\>"
syn match   acnType		"\<post-encoding-validator\>"
syn match   acnType		"\<true-value\>"
syn match   acnType		"\<align-to-next\>"
syn match   acnType		"\<false-value\>"
syn match   acnType		"\<save-position\>"
syn match   acnType		"\<termination-pattern\>"
syn keyword acnType		INTEGER BOOLEAN NULL
syn keyword acnType		encoding endianness pattern size determinant
syn match   acnType		"\.\.\."
syn keyword acnStructure	or and not

" Strings and constants
syn match   acnSpecial		contained "\\\d\d\d\|\\."
syn region  acnString		start=+"+  skip=+\\\\\|\\"+  end=+"+  contains=acnSpecial
syn match   acnCharacter	"'[^\\]'"
syn match   acnSpecialCharacter "'\\.'"
syn match   acnNumber		"-\=\<\d\+L\=\>\|0[xX][0-9a-fA-F]\+\>"
syn match   acnLineComment	"--.*"
syn match   acnLineComment	"--.*--"

" syn match acnDefinition "^\s*[a-zA-Z][-a-zA-Z0-9_.\[\] \t{}]* *::="me=e-3 contains=acnType
syn match acnBraces     "[{}]"

syn sync ccomment acnComment

" Define the default highlighting.
" Only when an item doesn't have highlighting yet
" hi def link acnDefinition	Function
hi def link acnBraces		Function
hi def link acnStructure	Statement
hi def link acnBoolValue	Boolean
hi def link acnSpecial		Special
hi def link acnString		String
hi def link acnCharacter	Character
hi def link acnSpecialCharacter	acnSpecial
hi def link acnNumber		acnValue
hi def link acnComment		Comment
hi def link acnLineComment	acnComment
hi def link acnType		Type
hi def link acnTypeInfo		PreProc
hi def link acnValue		Number
hi def link acnExternal		Include
hi def link acnFieldOption	Type

let &cpo = s:cpo_save
unlet s:cpo_save
let b:current_syntax = "acn"

" vim: ts=4
