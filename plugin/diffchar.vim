" diffchar.vim: Highlight the exact differences, based on characters and words
"
"  ____   _  ____  ____  _____  _   _  _____  ____   
" |    | | ||    ||    ||     || | | ||  _  ||  _ |  
" |  _  || ||  __||  __||     || | | || | | || | ||  
" | | | || || |__ | |__ |   __|| |_| || |_| || |_||_ 
" | |_| || ||  __||  __||  |   |     ||     ||  __  |
" |     || || |   | |   |  |__ |  _  ||  _  || |  | |
" |____| |_||_|   |_|   |_____||_| |_||_| |_||_|  |_|
"
" Last Change:	2022/06/01
" Version:		9.0 (on or after patch-8.1.1418 and nvim-0.5.0)
" Author:		Rick Howe (Takumi Ohtani) <rdcxy754@ybb.ne.jp>
" Copyright:	(c) 2014-2022 by Rick Howe

" This 9.0 version requires:
" * the OptionSet autocommand event triggered with the diff option
	" patch-8.0.0736 (nvim-0.3.0), patch-8.1.0414 (nvim-0.3.2)
" * window ID argument in matchaddpos()/matchdelete()/getmatches()
	" patch-8.1.0218 (nvim-0.3.5), patch-8.1.1084 (nvim-0.4.4)
" * the DiffUpdated autocommand event
	" patch-8.1.0397 (nvim-0.3.2)
" * the win_execute() function
	" patch-8.1.1418 (nvim-0.5.0)
if exists('g:loaded_diffchar') || !has('diff') || v:version < 800 ||
													\!exists('*win_execute')
	finish
endif
let g:loaded_diffchar = 9.0

let s:save_cpo = &cpoptions
set cpo&vim

" Options
if !exists('g:DiffUnit')		" a type of diff unit
	" let g:DiffUnit = 'Char'	" any single character
	let g:DiffUnit = 'Word1'	" \w\+ word and any \W single character
	" let g:DiffUnit = 'Word2'	" non-space and space words
	" let g:DiffUnit = 'Word3'	" \< or \> character class boundaries
	" let g:DiffUnit = 'CSV(,)'	" split characters (eg: ',', ';', '\t')
	" let g:DiffUnit = '/{pattern}/'	" pattern to split
endif

if !exists('g:DiffColors')		" matching colors for changed units
	let g:DiffColors = 0		" always 1 color
	" let g:DiffColors = 1		" up to 4 colors in fixed order
	" let g:DiffColors = 2		" up to 8 colors in fixed order
	" let g:DiffColors = 3		" up to 16 colors in fixed order
	" let g:DiffColors = 4		" all available colors in fixed order
	" let g:DiffColors = 100	" all colors in dynamic random order
endif

if !exists('g:DiffPairVisible')	" a visibility of corresponding diff units
	" let g:DiffPairVisible = 0	" disable
	let g:DiffPairVisible = 1	" highlight
	" let g:DiffPairVisible = 2	" highlight + echo
	" let g:DiffPairVisible = 3	" highlight + popup/floating at cursor pos
	" let g:DiffPairVisible = 4	" highlight + popup/floating at mouse pos
endif

" Keymaps
for [key, plg, cmd] in [
	\['[b', '<Plug>JumpDiffCharPrevStart',
									\':call diffchar#JumpDiffChar(0, 0)'],
	\[']b', '<Plug>JumpDiffCharNextStart',
									\':call diffchar#JumpDiffChar(1, 0)'],
	\['[e', '<Plug>JumpDiffCharPrevEnd',
									\':call diffchar#JumpDiffChar(0, 1)'],
	\[']e', '<Plug>JumpDiffCharNextEnd',
									\':call diffchar#JumpDiffChar(1, 1)'],
	\['<Leader>g', '<Plug>GetDiffCharPair',
									\':call diffchar#CopyDiffCharPair(0)'],
	\['<Leader>p', '<Plug>PutDiffCharPair',
									\':call diffchar#CopyDiffCharPair(1)']]
	if !hasmapto(plg, 'n') && empty(maparg(key, 'n'))
		if get(g:, 'DiffCharDoMapping', 1)
			call execute('nmap <silent> ' . key . ' ' . plg)
		endif
	endif
	call execute('nnoremap <silent> ' . plg . ' ' . cmd . '<CR>')
endfor

" Event groups
let g:DiffCharInitEvent = ['augroup diffchar', 'autocmd!',
				\'autocmd OptionSet diff call diffchar#ToggleDiffModeSync(0)',
															\'augroup END']
call execute(g:DiffCharInitEvent)
call execute('autocmd diffchar VimEnter * ++once
					\ if &diff | call diffchar#ToggleDiffModeSync(1) | endif')

let &cpoptions = s:save_cpo
unlet s:save_cpo

" vim: ts=4 sw=4
