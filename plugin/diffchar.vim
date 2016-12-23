" diffchar.vim : Highlight the exact differences, based on characters and words
"
"  ____   _  ____  ____  _____  _   _  _____  ____   
" |    | | ||    ||    ||     || | | ||  _  ||  _ |  
" |  _  || ||  __||  __||     || | | || | | || | ||  
" | | | || || |__ | |__ |   __|| |_| || |_| || |_||_ 
" | |_| || ||  __||  __||  |   |     ||     ||  __  |
" |     || || |   | |   |  |__ |  _  ||  _  || |  | |
" |____| |_||_|   |_|   |_____||_| |_||_| |_||_|  |_|
"
" Last Change: 2016/12/23
" Version:     6.5
" Author:      Rick Howe <rdcxy754@ybb.ne.jp>

if exists('g:loaded_diffchar')
	finish
endif
let g:loaded_diffchar = 6.5

let s:save_cpo = &cpo
set cpo&vim

" Commands
command! -range SDChar call diffchar#ShowDiffChar(range(<line1>, <line2>))
command! -range RDChar call diffchar#ResetDiffChar(range(<line1>, <line2>))
command! -range TDChar call diffchar#ToggleDiffChar(range(<line1>, <line2>))

" Configurable Keymaps
noremap <silent> <Plug>ToggleDiffCharAllLines :%TDChar<CR>
noremap <silent> <Plug>ToggleDiffCharCurrentLine :TDChar<CR>
nnoremap <silent> <Plug>JumpDiffCharPrevStart
				\ :call diffchar#JumpDiffChar(0, 0)<CR>
nnoremap <silent> <Plug>JumpDiffCharNextStart
				\ :call diffchar#JumpDiffChar(1, 0)<CR>
nnoremap <silent> <Plug>JumpDiffCharPrevEnd
				\ :call diffchar#JumpDiffChar(0, 1)<CR>
nnoremap <silent> <Plug>JumpDiffCharNextEnd
				\ :call diffchar#JumpDiffChar(1, 1)<CR>
if !hasmapto('<Plug>ToggleDiffCharAllLines', 'nv')
if empty(maparg('<F7>', 'nv'))
	map <silent> <F7> <Plug>ToggleDiffCharAllLines
endif
endif
if !hasmapto('<Plug>ToggleDiffCharCurrentLine', 'nv')
if empty(maparg('<F8>', 'nv'))
	map <silent> <F8> <Plug>ToggleDiffCharCurrentLine
endif
endif
if !hasmapto('<Plug>JumpDiffCharPrevStart', 'n')
	nmap <silent> [b <Plug>JumpDiffCharPrevStart
endif
if !hasmapto('<Plug>JumpDiffCharNextStart', 'n')
	nmap <silent> ]b <Plug>JumpDiffCharNextStart
endif
if !hasmapto('<Plug>JumpDiffCharPrevEnd', 'n')
	nmap <silent> [e <Plug>JumpDiffCharPrevEnd
endif
if !hasmapto('<Plug>JumpDiffCharNextEnd', 'n')
	nmap <silent> ]e <Plug>JumpDiffCharNextEnd
endif

" Set a difference unit type
if !exists('g:DiffUnit')
let g:DiffUnit = 'Word1'	" \w\+ word and any \W single character
" let g:DiffUnit = 'Word2'	" non-space and space words
" let g:DiffUnit = 'Word3'	" \< or \> character class boundaries
" let g:DiffUnit = 'Char'	" any single character
" let g:DiffUnit = 'CSV(,)'	" split characters
endif

" Set a difference unit matching colors
if !exists('g:DiffColors')
let g:DiffColors = 0		" always 1 color
" let g:DiffColors = 1		" 4 colors in fixed order
" let g:DiffColors = 2		" 8 colors in fixed order
" let g:DiffColors = 3		" 16 colors in fixed order
" let g:DiffColors = 100	" all available colors in dynamic random order
endif

" Make a corresponding unit visible when cursor is moved on a diff unit
if !exists('g:DiffPairVisible')
let g:DiffPairVisible = 2	" cursor-like highlight + echo
" let g:DiffPairVisible = 1	" cursor-like highlight
" let g:DiffPairVisible = 0	" nothing visible
endif

" Set a difference unit updating while editing
if !exists('g:DiffUpdate')
let g:DiffUpdate = 1		" enable
" let g:DiffUpdate = 0		" disable
endif

" Set a time length (ms) to apply this plugin's internal algorithm first
if !exists('g:DiffSplitTime')
let g:DiffSplitTime = 100	" when timeout, split to diff command
" let g:DiffSplitTime = 0	" always apply diff command only
endif

" Set a diff mode synchronization to show/reset exact differences
if !exists('g:DiffModeSync')
let g:DiffModeSync = 1		" enable
" let g:DiffModeSync = 0	" disable
endif

" Set this plugin's DiffCharExpr() to the diffexpr option if empty
if !exists('g:DiffExpr')
let g:DiffExpr = 1		" enable
" let g:DiffExpr = 0		" disable
endif
if g:DiffExpr && empty(&diffexpr)
let &diffexpr = 'diffchar#DiffCharExpr()'
endif

" Set an event group of this plugin
augroup diffchar
au!
au! FilterWritePost * call diffchar#SetDiffModeSync()
augroup END

let &cpo = s:save_cpo
unlet s:save_cpo
