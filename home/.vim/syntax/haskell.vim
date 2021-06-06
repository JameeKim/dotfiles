if version < 600
  syn clear
elseif exists("b:current_syntax")
  finish
endif

syn spell notoplevel

syn keyword haskellWhere            where
syn keyword haskellForall           forall
syn keyword haskellKeyword          let in
syn keyword haskellKeyword          infix infixl infixr
syn keyword haskellKeyword          do case of
syn keyword haskellConditional      if then else
syn keyword haskellModuleKeyword    module where
      \ contained
syn keyword haskellImportKeyword    import safe qualified as hiding
      \ contained
syn keyword haskellDataKeyword      data
      \ contained
syn keyword haskellNewtypeKeyword   newtype
      \ contained
syn keyword haskellTypeKeyword      type
      \ contained
syn keyword haskellClassKeyword     class
      \ contained
syn keyword haskellInstanceKeyword  instance
      \ contained
syn keyword haskellTodo             TODO FIXME NOTE
      \ contained

syn match haskellModule
      \ "^\s*\<module\>\s\+\S\+"
      \ nextgroup=haskellImportParens,haskellWhere
      \ skipwhite skipnl skipempty
      \ contains=
      \   haskellModuleKeyword,
      \   haskellModuleName

syn match haskellImport
      \ "^\s*\<import\>\s\+\(\<safe\>\s\+\)\?\<[a-zA-Z0-9_'.]\+\>\(\_s\+\<hiding\>\)\?"
      \ nextgroup=haskellImportParens
      \ skipwhite skipnl skipempty
      \ contains=
      \   haskellImportKeyword,
      \   haskellModuleName
syn match haskellImport
      \ "^\s*\<import\>\s\+\(\<safe\>\s\+\)\?\<qualified\>\s\+\<[a-zA-Z0-9_'.]\+\>\(\_s\+\<as\>\s\+\<[A-Z][a-zA-Z0-9_']*\>\)\?\(\_s\+\<hiding\>\)\?"
      \ nextgroup=haskellImportParens
      \ skipwhite skipnl skipempty
      \ contains=
      \   haskellImportKeyword,
      \   haskellModuleName
syn region haskellImportParens
      \ matchgroup=haskellDelimiter
      \ start="(" end=")"
      \ contained
      \ contains=
      \   haskellType,
      \   haskellIdentifier,
      \   haskellImportParens,
      \   haskellOperator,
      \   haskellSeparator,
      \   haskellLineComment,
      \   haskellBlockComment

syn match haskellDerive "\<deriving\>\s\+\<\(anyclass\|instance\|newtype\|stock\)\>"
syn match haskellDerive
      \ "\<deriving\>\_s*(.\+)"
      \ contains=haskellImportParens

syn match haskellTypeDecl
      \ "\<[a-z][a-zA-Z0-9_']*\>\(\_s*,\_s*\<[a-z][a-zA-Z0-9_']*\>\)*\_s*::[^}]*"
      \ contains=
      \   haskellIdentifier,
      \   haskellSeparator,
      \   haskellOperator,
      \   haskellForall,
      \   haskellType,
      \   haskellTypeParen,
      \   haskellTypeList

syn match haskellData
      \ "^\s*\<data\>\_.\{-}="
      \ contains=
      \   haskellDataKeyword,
      \   haskellType,
      \   haskellTypeVar,
      \   haskellTypeParen,
      \   haskellOperator
syn match haskellNewtype
      \ "^\s*\<newtype\>\_.\{-}="
      \ contains=
      \   haskellNewtypeKeyword,
      \   haskellType,
      \   haskellOperator
syn match haskellTypeDefine
      \ "^\s*\<type\>\_.\{-}=.*"
      \ contains=
      \   haskellTypeKeyword,
      \   haskellType,
      \   haskellTypeVar,
      \   haskellOperator,
      \   haskellForall,
      \   haskellTypeParen,
      \   haskellTypeList
syn match haskellClass
      \ "^\s*\<class\>\_.\{-}\<where\>"
      \ contains=
      \   haskellClassKeyword,
      \   haskellWhere,
      \   haskellForall,
      \   haskellType,
      \   haskellTypeVar,
      \   haskellTypeParen,
      \   haskellOperator
syn match haskellInstance
      \ "^\s*\<instance\>\_.\{-}\<where\>"
      \ contains=
      \   haskellInstanceKeyword,
      \   haskellWhere,
      \   haskellForall,
      \   haskellType,
      \   haskellTypeVar,
      \   haskellTypePare,
      \   haskellOperatorn

syn region haskellFieldDecl
      \ matchgroup=haskellDelimiter
      \ start="{" end="}"
      \ contained
      \ contains=haskellTypeDecl

syn match haskellFieldAssign
      \ "\<[a-z][a-zA-Z0-9_']*\>\_s*="
      \ contained
      \ contains=
      \   haskellIdentifier,
      \   haskellOperator

syn match haskellNumber
      \ "\<[0-9]\+\>\|\<0[xX][0-9a-fA-F]\+\>\|\<0[oO][0-7]\+\>\|\<0[bB][10]\+\>"
syn match haskellFloat "\<[0-9]\+\.[0-9]\+\([eE][-+]\=[0-9]\+\)\=\>"

syn region haskellString
      \ start=+"+ skip=+\\"+ end=+"+
      \ contains=@Spell
syn match haskellChar "\<'[^'\\]'\|'\\.'\|'\\u[0-9a-fA-F]\{4}'\>"

syn match haskellSeparator "[,;]"
syn match haskellOperator "[-!#$%&\*\+/<=>\?@\\^|~:.]\+\|\<_\>"
syn match haskellBacktick "`[a-zA-Z_][a-zA-Z0-9_\.']*`"

syn region haskellParens
      \ matchgroup=haskellDelimiter
      \ start="(" end=")"
      \ contains=TOP
syn region haskellBrackets
      \ matchgroup=haskellDelimiter
      \ start="\[" end="]"
      \ contains=TOP
syn region haskellBlock
      \ matchgroup=haskellDelimiter
      \ start="{" end="}"
      \ contains=TOP,haskellFieldAssign

syn match haskellIdentifier "\<[a-z][a-zA-Z0-9_']*\>" contained
syn match haskellType "\<[A-Z][a-zA-Z0-9_']*\>" contained
syn match haskellTypeVar "\<[a-z][a-zA-Z0-9_']*\>" contained
syn match haskellConstructor "\<[A-Z][a-zA-Z0-9_']*\>\(\_s*{.\{-}}\)\?" contains=haskellFieldDecl
syn match haskellModuleName
      \ "\<[A-Z][a-zA-Z0-9_']*\>\(\.\<[A-Z][a-zA-Z0-9_']*\>\)*"
      \ contained

syn region haskellTypeParen
      \ matchgroup=haskellDelimiter
      \ start="(" end=")"
      \ contained
      \ contains=
      \   haskellType,
      \   haskellTypeParen,
      \   haskellTypeList,
      \   haskellOperator,
      \   haskellSeparator
syn region haskellTypeList
      \ matchgroup=haskellDelimiter
      \ start="\[" end="]"
      \ contained
      \ contains=
      \   haskellType,
      \   haskellTypeParen,
      \   haskellTypeList,
      \   haskellOperator,
      \   haskellSeparator

syn match haskellLineComment
      \ "---*\([^-!#$%&\*\+./<=>\?@\\^|~].*\)\?$"
      \ contains=
      \   haskellTodo,
      \   @Spell
syn region haskellBlockComment
      \ start="{-" end="-}"
      \ contains=
      \   haskellBlockComment,
      \   haskellTodo,
      \   @Spell
syn region haskellPragma
      \ matchgroup=haskellDelimiter
      \ start="{-#" end="#-}"
      \ contains=haskellPragmaKeyword
syn match haskellPragmaKeyword "\<[A-Z][A-Z_]*\>" contained

hi def link haskellWhere            haskellKeyword
hi def link haskellForall           haskellKeyword
hi def link haskellModuleKeyword    haskellKeyword
hi def link haskellKeyword          Keyword
hi def link haskellConditional      Conditional
hi def link haskellImportKeyword    Include
hi def link haskellDataKeyword      haskellKeyword
hi def link haskellNewtypeKeyword   haskellKeyword
hi def link haskellTypeKeyword      haskellKeyword
hi def link haskellClassKeyword     haskellKeyword
hi def link haskellInstanceKeyword  haskellKeyword
hi def link haskellTodo             Todo

hi def link haskellDerive           haskellKeyword

hi def link haskellNumber           Number
hi def link haskellFloat            Float

hi def link haskellString           String
hi def link haskellChar             String

hi def link haskellSeparator        Delimiter
hi def link haskellOperator         Operator
hi def link haskellBacktick         Operator

hi def link haskellDelimiter        Delimiter

hi def link haskellIdentifier       Identifier
hi def link haskellType             Type
hi def link haskellConstructor      Boolean
hi def link haskellModuleName       Title

hi def link haskellLineComment      Comment
hi def link haskellBlockComment     Comment
hi def link haskellPragma           SpecialComment
hi def link haskellPragmaKeyword    PreProc

let b:current_syntax = "haskell"

" vim:wrap:
