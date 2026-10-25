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
" Last Change: 2026/10/25
" Version:     10.1 (on or after vim 9.0 and nvim 0.7.0)
" Author:      Rick Howe (Takumi Ohtani) <rdcxy754@ybb.ne.jp>
" Copyright:   (c) 2014-2026 Rick Howe
" License:     MIT

if exists('g:loaded_diffchar') || !has('diff') ||
                                      \(v:version < 900 && !has('nvim-0.7.0'))
  finish
endif
let g:loaded_diffchar = 10.1

let s:save_cpo = &cpoptions
set cpo&vim

" Keymaps
for [s:key, s:plg, s:cmd] in [
  \['[b', '<Plug>JumpDiffCharPrevStart',
                                      \':<C-U>call diffchar#JumpDiffChar(0)'],
  \[']b', '<Plug>JumpDiffCharNextStart',
                                      \':<C-U>call diffchar#JumpDiffChar(1)'],
  \['[e', '<Plug>JumpDiffCharPrevEnd',
                                      \':<C-U>call diffchar#JumpDiffChar(2)'],
  \[']e', '<Plug>JumpDiffCharNextEnd',
                                      \':<C-U>call diffchar#JumpDiffChar(3)'],
  \['<Leader>g', '<Plug>GetDiffCharPair',
                                  \':<C-U>call diffchar#CopyDiffCharPair(0)'],
  \['<Leader>p', '<Plug>PutDiffCharPair',
                                  \':<C-U>call diffchar#CopyDiffCharPair(1)']]
  if !hasmapto(s:plg, 'n') && maparg(s:key, 'n') =~ '^$\|_defaults.lua'
    if get(g:, 'DiffCharDoMapping', 1)
      call execute('nmap <silent> ' . s:key . ' ' . s:plg)
    endif
  endif
  call execute('nnoremap <silent> ' . s:plg . ' ' . s:cmd . '<CR>')
endfor

" Event groups
let g:DiffCharInitEvent = ['augroup diffchar', 'autocmd!',
                \'autocmd OptionSet diff call diffchar#ToggleDiffModeSync()',
                                                              \'augroup END']
call execute(g:DiffCharInitEvent)
call execute('autocmd diffchar VimEnter * ++once
                    \ if &diff | call diffchar#ToggleDiffModeSync(1) | endif')

" remove 'inline' from &diffopt if set only by default not to disable plugin
let s:inl = 'inline'
if &diffopt =~ s:inl
  let s:dip = split(&diffopt, ',')
  set diffopt&
  let s:def = split(&diffopt, ',')
  if match(filter(copy(s:dip), 'index(s:def, v:val) == -1'), s:inl) == -1
    call filter(s:dip, 'v:val !~ s:inl')
  endif
  let &diffopt = join(s:dip, ',')
endif

let &cpoptions = s:save_cpo
unlet s:save_cpo

" vim: ts=2 sw=0 sts=-1 et
