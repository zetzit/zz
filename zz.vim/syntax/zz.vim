" Vim syntax file

" quit when a syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

" Based on C
runtime! syntax/c.vim
unlet b:current_syntax


syn keyword zzType              u8 i8 u16 i16 u32 i32 u64 i64 isize usize
syn keyword zzStructure	        enum struct fn const trait test
syn keyword zzImport            using
syn keyword zzVisibility        export pub
syn keyword zzKeywords          mut as
syn keyword zzDanger            unsafe
syn keyword zzOperator          len tail

syn region zzCommentLineDoc     start="//\%(//\@!\|!\)"         end="$"  contains=@Spell


" Default highlighting
hi def link zzImport            PreProc
hi def link zzStructure         Structure
hi def link zzVisibility        PreProc
hi def link zzKeywords          Type 
hi def link zzDanger            Macro
hi def link zzCommentLineDoc    SpecialComment
hi def link zzType              cType
hi def link zzOperator          cOperator

let b:current_syntax = "zz"

" vim: ts=8
