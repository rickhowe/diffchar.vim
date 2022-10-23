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
" Last Change: 2022/10/23
" Version:     9.1 (on or after patch-8.1.1418 and nvim-0.5.0)
" Author:      Rick Howe (Takumi Ohtani) <rdcxy754@ybb.ne.jp>
" Copyright:   (c) 2014-2022 by Rick Howe

let s:save_cpo = &cpoptions
set cpo&vim

" Vim feature, function, event and patch number which this plugin depends on
" patch-8.1.1832: win_execute() fixed to work in other tabpage
" patch-8.2.4204: screenpos() fixed to return zero row for invisible line
let s:VF = {
  \'WinScrolled': exists('##WinScrolled'),
  \'WinClosed': exists('##WinClosed'),
  \'PopupWindow': has('popupwin'),
  \'FloatingWindow': exists('*nvim_create_buf'),
  \'GetMousePos': exists('*getmousepos'),
  \'WinExecFixed': has('patch-8.1.1832') || has('nvim-0.5.0'),
  \'StrikeAttr': has('gui_running') || !empty(&t_Ts) && !empty(&t_Te),
  \'ScreenPos': exists('*screenpos') && has('patch-8.2.4204'),
  \'Vim9Script': has('vim9script')}

function! s:ShowDiffChar(...) abort
  " init: 0 = none, 1 = all except wid/bnr, 2 = all
  let init = !exists('t:DChar') ? 2 :
                                \index(values(t:DChar.hlc), {}) != -1 ? 1 : 0
  if (0 < init) && s:InitializeDiffChar(init == 2) == -1 | return | endif
  for ak in [1, 2, 0]
    if ak == 0 | return | endif
    if t:DChar.wid[ak] == win_getid() | break | endif
  endfor
  let lc = {'1': {}, '2': {}}
  for n in !a:0 ? range(len(t:DChar.dfl[ak])) : filter(map(copy(a:1),
                            \'index(t:DChar.dfl[ak], v:val)'), 'v:val != -1')
    let tu = {'1': {}, '2': {}}
    for k in [1, 2]
      let tu[k].l = t:DChar.dfl[k][n]
      if has_key(t:DChar.hlc[k], tu[k].l) | let tu[k].l = 0 | endif
    endfor
    if 0 < tu[1].l && 0 < tu[2].l
      for k in [1, 2]
        let tu[k].b = getbufline(t:DChar.bnr[k], tu[k].l)[0]
        let tu[k].t = t:DChar.opt.ic ? tolower(tu[k].b) : tu[k].b
        let tu[k].u = split(t:DChar.opt.iw == 0 ? tu[k].t :
                \substitute(tu[k].t, (t:DChar.opt.iw == 1) ? '\s\+' : '\s\+$',
                                                      \'', 'g'), t:DChar.upa)
        if t:DChar.opt.iw == 2 && !empty(tu[k].u)
          for s in range(len(tu[k].u) - 1, 1, -1)
            if tu[k].u[s - 1] . tu[k].u[s] =~ '^\s\+$'
              let tu[k].u[s - 1] .= tu[k].u[s]
              unlet tu[k].u[s]
            endif
          endfor
        endif
      endfor
      if tu[1].u !=# tu[2].u
        let uu = [copy(tu[1].u), copy(tu[2].u)]
        if t:DChar.opt.iw == 2
          for k in [1, 2]
            call map(tu[k].u, '(v:val =~ "^\\s\\+$") ? " " : v:val')
          endfor
        endif
        let [tu[1].c, tu[2].c] = s:GetDiffUnitPos(uu,
                          \s:TraceDiffChar(tu[1].u, tu[2].u, t:DChar.opt.ih))
        for k in [1, 2]
          if t:DChar.opt.iw == 1
            if tu[k].t =~ '\s\+'
              let pc = filter(range(1, len(tu[k].t)),
                                              \'tu[k].t[v:val - 1] !~ "\\s"')
              call map(tu[k].c,
                    \'[v:val[0], [pc[v:val[1][0] - 1], pc[v:val[1][1] - 1]]]')
            endif
          endif
          let lc[k][tu[k].l] = tu[k].c
          let t:DChar.cks[k][tu[k].l] = s:ChecksumStr(tu[k].b)
        endfor
      endif
    endif
  endfor
  if 0 < init || !empty(lc[ak])
    call s:HighlightDiffChar(lc)
    if 0 < init
      call s:ToggleDiffCharEvent(1)
      if init == 2 | call s:ToggleDiffHL(1) | endif
    endif
    if 0 < t:DChar.dpv.pv
      call s:ToggleDiffCharPair(1)
      call s:ShowDiffCharPair(ak)
    endif
  endif
endfunction

function! s:ResetDiffChar(...) abort
  " reset: 0 = specified lines, 1 = all lines and events, 2 = all
  let reset = !a:0 ? 1 : type(a:1) == type([]) ? 0 : 2
  if !exists('t:DChar') | return | endif
  for ak in [1, 2, 0]
    if ak == 0 | return | endif
    if t:DChar.wid[ak] == win_getid() | break | endif
  endfor
  let dl = {'1': [], '2': []}
  for n in (0 < reset) ? range(len(t:DChar.dfl[ak])) : filter(map(copy(a:1),
                            \'index(t:DChar.dfl[ak], v:val)'), 'v:val != -1')
    for k in [1, 2]
      let l = t:DChar.dfl[k][n]
      if has_key(t:DChar.hlc[k], l)
        let dl[k] += [l]
        unlet t:DChar.cks[k][l]
      endif
    endfor
  endfor
  if 0 < reset || !empty(dl[ak])
    call s:ClearDiffChar(dl)
    if 0 < t:DChar.dpv.pv
      call s:ClearDiffCharPair(ak)
      call s:ToggleDiffCharPair(0)
    endif
    if 0 < reset
      if reset == 2 | call s:ToggleDiffHL(0) | unlet t:DChar | endif
      call s:ToggleDiffCharEvent(0)
    endif
  endif
endfunction

function! s:InitializeDiffChar(init) abort
  if a:init
    let cw = win_getid()
    let cb = winbufnr(cw)
    let nw = filter(map(range(winnr() + 1, winnr('$')) +
                                  \range(1, winnr() - 1), 'win_getid(v:val)'),
                        \'getwinvar(v:val, "&diff") && winbufnr(v:val) != cb')
    let nb = map(copy(nw), 'winbufnr(v:val)')
    if !getwinvar(cw, '&diff') || empty(nw) || min(nb) != max(nb)
      return -1
    endif
    for tn in filter(range(1, tabpagenr('$')), 'v:val != tabpagenr()')
      let dc = gettabvar(tn, 'DChar')
      if !empty(dc)
        for bn in values(dc.bnr)
          if index([cb, nb[0]], bn) != -1
            call s:EchoWarning('Both or either selected buffer already
                                      \ highlighted in tab page ' . tn . '!')
            return -1
          endif
        endfor
      endif
    endfor
    call s:SetDiffCharHL()
    let t:DChar = {}
    let t:DChar.wid = {'1': cw, '2': nw[0]}
    let t:DChar.bnr = {'1': cb, '2': nb[0]}
  endif
  let t:DChar.dfp = get(t:, 'DiffPages', get(g:, 'DiffPages', 3))
  let t:DChar.opt = s:GetDiffCharOptions()
  let t:DChar.lcc = s:GetLineColCnr()
  let t:DChar.dfl = s:FocusDiffLines(1)
  let t:DChar.upa = s:GetDiffSplitRegExp(t:DChar.opt.ut)
  let t:DChar.dpv = s:GetDiffPairVisible(t:DChar.opt.pv)
  let t:DChar.hgp = s:GetDiffUnitHL(t:DChar.opt.cl)
  let t:DChar.mid = {'1': {}, '2': {}}
  let t:DChar.hlc = {'1': {}, '2': {}}
  let t:DChar.cks = {'1': {}, '2': {}}
endfunction

function! s:GetDiffSplitRegExp(du) abort
  if a:du == 'Char'
    let upa = '\zs'
  elseif a:du == 'Word2' || a:du ==# 'WORD'
    let upa = '\%(\s\+\|\S\+\)\zs'
  elseif a:du == 'Word3' || a:du ==# 'word'
    let upa = '\<\|\>'
  elseif a:du =~ '^\[.\+\]$'
    let s = escape(a:du[1 : -2], ']^-\')
    let upa = '\%([^' . s . ']\+\|[' . s . ']\)\zs'
  elseif a:du =~ '^\([/?]\).\+\1$'
    let upa = a:du[1 : -2]
  else
    let upa = '\%(\w\+\|\W\)\zs'
    if a:du != 'Word1'
      call s:EchoWarning('Not a valid difference unit type.
                                                      \ Use "Word1" instead.')
    endif
  endif
  return upa
endfunction

function! s:GetDiffPairVisible(pv) abort
  let dpv = {'pv': a:pv}
  if (dpv.pv == 3 || dpv.pv == 4) &&
                                \!(s:VF.PopupWindow || s:VF.FloatingWindow) ||
                              \dpv.pv == 4 && !s:VF.GetMousePos || 4 < dpv.pv
    let dpv.pv = 1
  endif
  if 0 < dpv.pv
    let dpv.ch = {}
    if dpv.pv == 3 || dpv.pv == 4
      let dpv.pw = s:VF.PopupWindow ? 0 : s:VF.FloatingWindow ? {} : -1
    endif
  endif
  return dpv
endfunction

function! s:GetDiffUnitHL(dc) abort
  let hgp = [s:DCharHL.T]
  if type(a:dc) == type([])
    let hgp += filter(copy(a:dc), '0 < hlID(v:val)')
  elseif 1 <= a:dc && a:dc <= 3
    let lv = a:dc - 1
    let bx = []
    for nm in values(s:DCharHL)
      let [fc, bc] = map(['fg#', 'bg#'],
                              \'s:ColorClass(synIDattr(hlID(nm), v:val), lv)')
      if !empty(bc) | let bx += [bc] | endif
      if nm == s:DCharHL.n | let fn = fc | endif
    endfor
    let hl = {}
    let id = 1
    while 1
      let nm = synIDattr(id, 'name')
      if empty(nm) | break | endif
      if id == synIDtrans(id) && empty(filter(['underline', 'undercurl',
                          \'strikethrough', 'reverse', 'inverse', 'standout'],
                                            \'!empty(synIDattr(id, v:val))'))
        let [fc, bc] = map(['fg#', 'bg#'],
                                    \'s:ColorClass(synIDattr(id, v:val), lv)')
        if !empty(bc) && index(bx + [!empty(fc) ? fc : fn], bc) == -1
          let wt = !empty(fc) + (!empty(filter(['bold', 'italic'],
                                        \'!empty(synIDattr(id, v:val))'))) * 2
          if !has_key(hl, bc) || hl[bc][0] < wt
            let hl[bc] = [wt, nm]
          endif
        endif
      endif
      let id += 1
    endwhile
    let hgp += map(values(hl), 'v:val[1]')
  elseif a:dc == 100
    let bx = map(values(s:DCharHL), 'synIDattr(hlID(v:val), "bg#")')
    let hl = {}
    let id = 1
    while 1
      let nm = synIDattr(id, 'name')
      if empty(nm) | break | endif
      if id == synIDtrans(id)
        let bg = synIDattr(id, 'bg#')
        if !empty(bg) && index(bx, bg) == -1
          let hl[reltimestr(reltime())[-2 :] . id] = nm
          let bx += [bg]
        endif
      endif
      let id += 1
    endwhile
    let hgp += values(hl)
  elseif -3 <= a:dc && a:dc <= -1
    let hgp += ['SpecialKey', 'Search', 'CursorLineNr',
                \'Visual', 'WarningMsg', 'StatusLineNC', 'MoreMsg',
                \'ErrorMsg', 'LineNr', 'Conceal', 'NonText',
                \'ColorColumn', 'ModeMsg', 'PmenuSel', 'Title']
                              \[: ((a:dc == -1) ? 2 : (a:dc == -2) ? 6 : -1)]
  endif
  return hgp
endfunction

function! s:ColorClass(cn, lv) abort
  if empty(a:cn) | return a:cn | endif
  if a:cn[0] != '#'
    let cn = a:cn % 256
    if cn < 16
      let cv = [[0, 0, 0], [128, 0, 0], [0, 128, 0], [128, 128, 0],
                \[0, 0, 128], [128, 0, 128], [0, 128, 128], [192, 192, 192],
                \[128, 128, 128], [255, 0, 0], [0, 255, 0], [255, 255, 0],
                \[0, 0, 255], [255, 0, 255], [0, 255, 255], [255, 255, 255]]
      let rgb = cv[cn]
    elseif cn < 232
      let cv = [0, 95, 135, 175, 215, 255]
      let cn -= 16
      let rgb = [cv[(cn / 36) % 6], cv[(cn / 6) % 6], cv[cn % 6]]
    else
      let cn = 10 * (cn - 232) + 8
      let rgb = [cn, cn, cn]
    endif
  else
    let rgb = map(split(a:cn[1:], '..\zs'), 'str2nr(v:val, 16)')
  endif
  let cl = [[0, 0, 0, 0, 1, 1, 1, 1], [0, 0, 0, 0, 1, 1, 2, 2],
                                              \[0, 1, 2, 3, 4, 5, 6, 7]][a:lv]
  call map(rgb, 'v:val / 32')
  if max(rgb) == min(rgb)
    return '99' . cl[(rgb[0] + rgb[1] + rgb[2]) / 3]
  else
    return join(map(rgb, 'cl[v:val]'), '')
  endif
endfunction

function! s:SetDiffCharHL() abort
  let hm = (has('gui_running') || has('termguicolors') && &termguicolors) ?
                                                              \'gui' : 'cterm'
  " set or reset original diff hl
  let s:DiffHL = {'A': 'DiffAdd', 'C': 'DiffChange', 'D': 'DiffDelete',
                                                            \'T': 'DiffText'}
  for [hs, hl] in items(s:DiffHL)
    let dh = {}
    let dh.id = hlID(hl)
    let dh.it = synIDtrans(dh.id)       " in case of linked
    let dh.nm = synIDattr(dh.it, 'name')
    " 0 : original and for single color, 1 : for multi color
    let dh[0] = {}
    for hc in ['fg', 'bg']
      let dh[0][hm . hc] = synIDattr(dh.it, hc)
    endfor
    let dh[0][hm] = join(filter(['bold', 'underline', 'undercurl',
                \'strikethrough', 'reverse', 'inverse', 'italic', 'standout'],
                                    \'!empty(synIDattr(dh.it, v:val))'), ',')
    call map(dh[0], '!empty(v:val) ? v:val : "NONE"')
    let dh[1] = map(copy(dh[0]), (hs == 'C') ? 'v:key =~ "bg$" ?
                          \v:val : "NONE"' : (hs == 'T') ? '"NONE"' : 'v:val')
    let s:DiffHL[hs] = dh
  endfor
  " set DChar normal hl
  let s:DCharHL = {}
  let s:DCharHL.n = 'dcNormal'
  call execute('highlight clear ' . s:DCharHL.n)
  call execute('highlight ' . s:DCharHL.n . ' ' . hm . 'fg=fg', 'silent!')
  call execute('highlight ' . s:DCharHL.n . ' ' . hm . 'bg=bg', 'silent!')
  let [fn, bn] = map(['fg#', 'bg#'], 'synIDattr(hlID(s:DCharHL.n), v:val)')
  if empty(fn) | let fn = 'NONE' | endif
  if empty(bn) | let bn = 'NONE' | endif
  " set DChar cursor hl
  let s:DCharHL.c = 'dcCursor'
  let id = hlID(has('nvim') ? 'TermCursor' : 'Cursor')
  let [fg, bg] = map(['fg#', 'bg#'], 'synIDattr(id, v:val)')
  call execute('highlight clear ' . s:DCharHL.c)
  call execute('highlight ' . s:DCharHL.c . ' ' . (!empty(bg) ?
              \join([hm . 'fg=' . (!empty(fg) ? fg : 'NONE'), hm . 'bg=' . bg,
                    \hm . '=' . join(filter(['bold', 'underline', 'undercurl',
                \'strikethrough', 'reverse', 'inverse', 'italic', 'standout'],
                    \'!empty(synIDattr(id, v:val))') + ['nocombine'], ',')]) :
        \join([hm . 'fg=' . fn, hm . 'bg=' . bn, hm . '=reverse,nocombine'])))
  " set DChar diff hl
  for [fs, ts, th, ta] in [['C', 'C', 'dcDiffChange', ''],
                              \['T', 'T', 'dcDiffText', ''],
                              \['A', 'A', 'dcDiffAdd', ''],
                              \['C', 'E', 'dcDiffDelEdge', 'bold,underline'],
    \['D', 'D', 'dcDiffDelete', s:VF.StrikeAttr ? 'strikethrough' : '']]
    let fa = copy(s:DiffHL[fs][0])
    let fa[hm] = ((fa[hm] != 'NONE') ? fa[hm] . ',' : '') .
                                  \(!empty(ta) ? ta . ',' : '') . 'nocombine'
    if fa[hm . 'fg'] == 'NONE' | let fa[hm . 'fg'] = fn | endif
    if fa[hm . 'bg'] == 'NONE' | let fa[hm . 'bg'] = bn | endif
    call execute('highlight clear ' . th)
    call execute('highlight ' . th . ' ' .
                                    \join(map(items(fa), 'join(v:val, "=")')))
    let s:DCharHL[ts] = th
  endfor
endfunction

function! s:GetDiffCharOptions() abort
  let do = split(&diffopt, ',')
  return {'ut': get(t:, 'DiffUnit', g:DiffUnit),
          \'cl': get(t:, 'DiffColors', g:DiffColors),
          \'pv': get(t:, 'DiffPairVisible', g:DiffPairVisible),
          \'cn': get(g:, 'colors_name', 'default'),
          \'ic': index(do, 'icase') != -1,
          \'iw': (index(do, 'iwhiteall') != -1) ? 1 :
                                      \(index(do, 'iwhite') != -1) ? 2 :
                                      \(index(do, 'iwhiteeol') != -1) ? 3 : 0,
          \'ih': index(do, 'indent-heuristic') != -1}
endfunction

function! s:ToggleDiffHL(on) abort
  let id = a:on && 1 < len(t:DChar.hgp)
  for dh in values(s:DiffHL)
    call execute(join(['highlight', dh.nm] +
                                    \map(items(dh[id]), 'join(v:val, "=")')))
  endfor
endfunction

function! s:RefreshDiffCharHL(event) abort
  " a:event : 0 = TabEnter, 1 = ColorScheme
  let on = exists('t:DChar')
  if a:event | call s:SetDiffCharHL() | endif
  call s:ToggleDiffHL(on)
  " redraw DChar units with the latest colorschme
  if on
    let opt = s:GetDiffCharOptions()
    if t:DChar.opt.cn != opt.cn
      let t:DChar.opt.cn = opt.cn
      if 1 < len(t:DChar.hgp)
        let hlc = deepcopy(t:DChar.hlc)
        call s:ClearDiffChar(map(copy(hlc), 'keys(v:val)'))
        let t:DChar.hgp = s:GetDiffUnitHL(t:DChar.opt.cl)
        call s:HighlightDiffChar(hlc)
      endif
    endif
  endif
endfunction

function! s:ToggleDiffCharEvent(on) abort
  call execute(g:DiffCharInitEvent)
  let tv = filter(map(range(1, tabpagenr('$')),
                              \'gettabvar(v:val, "DChar")'), '!empty(v:val)')
  if empty(tv) | return | endif
  let ac = []
  for td in tv
    for k in [1, 2]
      let bl = '<buffer=' . td.bnr[k] . '>'
      let ac += [[s:VF.WinClosed ? 'WinClosed' : 'BufWinLeave', bl,
                                                    \'s:WinClosedDiffChar()']]
      if td.dfp != 0
        let ac += [[s:VF.WinScrolled ? 'WinScrolled' : 'CursorMoved', bl,
                                              \'s:ScrollDiffChar(' . k . ')']]
      endif
      if 0 < td.dpv.pv
        let ac += [['CursorMoved', bl, 's:ShowDiffCharPair(' . k . ')']]
      endif
    endfor
  endfor
  let ac += [['TabEnter', '*', 's:RefreshDiffCharHL(0)']]
  let ac += [['ColorScheme', '*', 's:RefreshDiffCharHL(1)']]
  let ac += [[s:VF.WinClosed ? 'BufWinEnter' : 'BufWinEnter,WinEnter', '*',
                                                      \'s:RepairDiffChar()']]
  let ac += [['DiffUpdated', '*', 's:UpdateDiffChar()']]
  call execute(map(ac, 'join(["autocmd", "diffchar", v:val[0], v:val[1],
                                                        \"call", v:val[2]])'))
endfunction

function! s:TraceDiffChar(u1, u2, ih) abort
  " An O(NP) Sequence Comparison Algorithm
  let [n1, n2] = [len(a:u1), len(a:u2)]
  if a:u1 ==# a:u2 | return repeat('=', n1)
  elseif n1 == 0 | return repeat('+', n2)
  elseif n2 == 0 | return repeat('-', n1)
  endif
  " reverse to be N >= M
  let [N, M, u1, u2, e1, e2] = (n1 >= n2) ?
              \[n1, n2, a:u1, a:u2, '-', '+'] : [n2, n1, a:u2, a:u1, '+', '-']
  let D = N - M
  let fp = repeat([-1], M + N + 1)
  let etree = []    " [next edit, previous p, previous k]
  let p = -1
  while fp[D] != N
    let p += 1
    let epk = repeat([[]], p * 2 + D + 1)
    for k in range(-p, D - 1, 1) + range(D + p, D, -1)
      let [y, epk[k]] = (fp[k - 1] + 1 > fp[k + 1]) ?
                        \[fp[k - 1] + 1, [e1, [(k > D) ? p - 1 : p, k - 1]]] :
                        \[fp[k + 1], [e2, [(k < D) ? p - 1 : p, k + 1]]]
      let x = y - k
      while x < M && y < N && u2[x] ==# u1[y]
        let epk[k][0] .= '=' | let [x, y] += [1, 1]
      endwhile
      let fp[k] = y
    endfor
    let etree += [epk]
  endwhile
  " create a shortest edit script (SES) from last p and k
  let ses = ''
  while 1
    let ses = etree[p][k][0] . ses
    if [p, k] == [0, 0] | break | endif
    let [p, k] = etree[p][k][1]
  endwhile
  let ses = ses[1 :]
  if a:ih
    " follow indent-heuristic, replace =\+ <-> +\+ or -\+ in 2 or more
    " =\++\+ or =\+-\+ blocks (AB vs AxByAB : =+=+++ -> ++++==)
    let [p1, p2, qc, et, ex] = [-1, -1, 0, '', '']
    for ed in reverse(split(ses, '[+-]\+\zs'))
      let es = ed . et
      let qe = count(ed, '=')
      if 0 < qe
        let [q1, q2] = [count(es, e1), count(es, e2)]
        let [uu, pp, qq] = (qe <= q1 && q2 == 0) ? [u1, p1, q1] :
                          \(q1 == 0 && qe <= q2) ? [u2, p2, q2] : [[], -1, -1]
        let [ex, es, p1, p2] = (!empty(uu) &&
                  \uu[pp - qq - qe + 1 : pp - qq] ==# uu[pp - qe + 1 : pp]) ?
                            \[es[: qe - 1] . ex, es[qe :], p1 - qe, p2 - qe] :
                            \[es . ex, '', p1 - qe - q1, p2 - qe - q2]
        let qc += 1
      endif
      let et = es
    endfor
    if 1 < qc | let ses = et . ex | endif
  endif
  return ses
endfunction

function! s:GetDiffUnitPos(uu, es) abort
  if empty(a:uu[0])
    return [[['d', [0, 0]]], [['a', [1, len(join(a:uu[1], ''))]]]]
  elseif empty(a:uu[1])
    return [[['a', [1, len(join(a:uu[0], ''))]]], [['d', [0, 0]]]]
  endif
  let cc = [[], []] | let ll = [1, 1] | let pp = [0, 0]
  for ed in split(a:es, '[+-]\+\zs', 1)[: -2]
    let qe = count(ed, '=')
    let qq = [count(ed, '-'), count(ed, '+')]
    let ee = (qq[0] == 0) ? ['d', 'a'] : (qq[1] == 0) ? ['a', 'd'] :
                                                                  \['c', 'c']
    for k in [0, 1]
      if 0 < qe
        let [ll[k], pp[k]] +=
                        \[len(join(a:uu[k][pp[k] : pp[k] + qe - 1], '')), qe]
      endif
      if 0 < qq[k]
        let hh = [ll[k]]
        let [ll[k], pp[k]] +=
                  \[len(join(a:uu[k][pp[k] : pp[k] + qq[k] - 1], '')), qq[k]]
        let hh += [ll[k] - 1]
      else
        let hh = [ll[k] - ((0 < pp[k]) ? len(strcharpart(a:uu[k][pp[k] - 1],
                                  \strchars(a:uu[k][pp[k] - 1]) - 1, 1)) : 0),
                  \ll[k] + ((pp[k] < len(a:uu[k])) ?
                            \len(strcharpart(a:uu[k][pp[k]], 0, 1)) - 1 : -1)]
      endif
      let cc[k] += [[ee[k], hh]]
    endfor
  endfor
  return cc
endfunction

function! s:HighlightDiffChar(lec) abort
  let hn = len(t:DChar.hgp)
  for k in [1, 2]
    for [ln, ec] in items(a:lec[k])
      if has_key(t:DChar.mid[k], ln) | continue | endif
      let t:DChar.hlc[k][ln] = ec
      let hc = {}
      let cn = 0
      for [e, c] in ec
        if e == 'c'
          let h = t:DChar.hgp[cn % hn] | let cn += 1
        elseif e == 'a'
          let h = s:DCharHL.A
        elseif e == 'd'
          if c == [0, 0] | continue | endif
          let h = s:DCharHL.E
        endif
        if !has_key(hc, h) | let hc[h] = [] | endif
        let hc[h] += [[ln, c[0], c[1] - c[0] + 1]]
      endfor
      let t:DChar.mid[k][ln] = [matchaddpos(s:DCharHL.C, [[ln]], -5, -1,
                                                \{'window': t:DChar.wid[k]})]
      for [h, c] in items(hc)
        let t:DChar.mid[k][ln] += map(range(0, len(c) - 1, 8),
                                \'matchaddpos(h, c[v:val : v:val + 7], -3, -1,
                                                \{"window": t:DChar.wid[k]})')
      endfor
    endfor
  endfor
endfunction

function! s:ClearDiffChar(lines) abort
  for k in [1, 2]
    let wd = win_id2win(t:DChar.wid[k])
    for ln in a:lines[k]
      if wd != 0
        silent! call map(t:DChar.mid[k][ln],
                                        \'matchdelete(v:val, t:DChar.wid[k])')
      endif
      unlet t:DChar.mid[k][ln]
      unlet t:DChar.hlc[k][ln]
    endfor
  endfor
endfunction

function! s:ShiftDiffChar(key, lines, shift) abort
  let [lid, hlc, cks] = [[], {}, {}]
  for ln in filter(copy(a:lines), 'has_key(t:DChar.mid[a:key], v:val)')
    let lid += [[ln, t:DChar.mid[a:key][ln]]]
    let hlc[ln + a:shift] = t:DChar.hlc[a:key][ln]
    let cks[ln + a:shift] = t:DChar.cks[a:key][ln]
    unlet t:DChar.mid[a:key][ln]
    unlet t:DChar.hlc[a:key][ln]
    unlet t:DChar.cks[a:key][ln]
  endfor
  call extend(t:DChar.mid[a:key],
                      \s:ShiftMatchaddLines(t:DChar.wid[a:key], lid, a:shift))
  call extend(t:DChar.hlc[a:key], hlc)
  call extend(t:DChar.cks[a:key], cks)
endfunction

function! s:ShiftMatchaddLines(wid, lid, shift) abort
  let lid = {}
  let gm = getmatches(a:wid)
  for [ln, id] in a:lid
    let mx = filter(copy(gm), 'index(id, v:val.id) != -1')
    call map(copy(mx), 'matchdelete(v:val.id, a:wid)')
    let lid[ln + a:shift] = map(reverse(mx), 'matchaddpos(v:val.group,
                    \map(filter(items(v:val), "v:val[0] =~ ''^pos\\d\\+$''"),
              \"[v:val[1][0] + a:shift] + v:val[1][1 :]"), v:val.priority, -1,
                                                        \{"window": a:wid})')
  endfor
  return lid
endfunction

function! s:ScrollDiffChar(key) abort
  if !exists('t:DChar') || t:DChar.wid[a:key] != win_getid()
    return
  endif
  let lcc = s:GetLineColCnr()
  let scl = 0
  for k in [1, 2]
    " check if a scroll happens in either window with no change on both
    let scl += (t:DChar.lcc[k].cn != lcc[k].cn) ? -1 :
                                  \([t:DChar.lcc[k].tl, t:DChar.lcc[k].bl] !=
                                              \[lcc[k].tl, lcc[k].bl]) ? 1 : 0
    let [t:DChar.lcc[k].tl, t:DChar.lcc[k].bl] = [lcc[k].tl, lcc[k].bl]
  endfor
  if 0 < scl
    let dfl = s:FocusDiffLines(0)
    if t:DChar.dfl != dfl
      " reset/show DChar lines on dfl changes
      if t:DChar.dfp < 0
        let ddl = filter(copy(t:DChar.dfl[a:key]),
                                            \'index(dfl[a:key], v:val) == -1')
        if !empty(ddl) | call s:ResetDiffChar(ddl) | endif
      endif
      let adl = filter(copy(dfl[a:key]),
                                    \'index(t:DChar.dfl[a:key], v:val) == -1')
      let t:DChar.dfl = dfl
      if !empty(adl) | call s:ShowDiffChar(adl) | endif
    endif
  endif
endfunction

function! s:FocusDiffLines(init) abort
  " a:init : initiate dfl (do not use existing dfl)
  let dfl = {}
  if t:DChar.dfp == 0
    if a:init
      for k in [1, 2]
        call win_execute(t:DChar.wid[k], 'let dfl[k] =
                                      \s:GetDiffLines(1, t:DChar.lcc[k].ll)')
      endfor
      return dfl
    else
      return t:DChar.dfl
    endif
  endif
  let init = a:init || index(values(t:DChar.dfl), []) != -1
  let tb = {}
  " 1. get visible dfl in both wins and return existing dfl if no new dfl
  for k in [1, 2]
    call win_execute(t:DChar.wid[k], 'let dfl[k] =
                      \s:GetDiffLines(t:DChar.lcc[k].tl, t:DChar.lcc[k].bl)')
  endfor
  if !init
    let nd = 0
    for k in [1, 2]
      let tb[k] = {'ti': -k, 'bi': -k}
      if !empty(dfl[k])
        let [tb[k].ti, tb[k].bi] = [index(t:DChar.dfl[k], dfl[k][0]),
                                          \index(t:DChar.dfl[k], dfl[k][-1])]
        let nd += (tb[k].ti != -1) && (tb[k].bi != -1) &&
                              \(t:DChar.dfl[k][tb[k].ti : tb[k].bi] == dfl[k])
      endif
    endfor
    if nd == 2 | return t:DChar.dfl | endif
  endif
  " 2. get upper/lower dfl, return empty if not found in both win or
  " return all dfl found on init and set dfp=0 not to check scroll
  let rx = 0
  for k in [1, 2]
    let [fl, tl, bl, ll] = [1, t:DChar.lcc[k].tl, t:DChar.lcc[k].bl,
                                                          \t:DChar.lcc[k].ll]
    if !init && 0 < t:DChar.dfp
      if 0 <= tb[k].ti | let fl = tl | endif
      if 0 <= tb[k].bi | let ll = bl | endif
    endif
    let rc = winheight(t:DChar.wid[k]) * (abs(t:DChar.dfp) - 1)
    let [tx, bx] = [tl - 1, bl + 1]
    while 0 < rc
      let fc = 0
      if fl <= tx
        call win_execute(t:DChar.wid[k], 'let fc = foldclosed(tx)')
        let [tx, rc] = [((fc == -1) ? tx : fc) - 1, rc - 1]
      endif
      if bx <= ll
        call win_execute(t:DChar.wid[k], 'let fc = foldclosedend(bx)')
        let [bx, rc] = [((fc == -1) ? bx : fc) + 1, rc - 1]
      endif
      if fc == 0 | let rx += 1 | break | endif
    endwhile
    call win_execute(t:DChar.wid[k], 'let dfl[k] =
                                    \s:GetDiffLines(tx + 1, tl - 1) + dfl[k] +
                                            \s:GetDiffLines(bl + 1, bx - 1)')
  endfor
  if empty(dfl[1]) && empty(dfl[2])
    return (!init && 0 < t:DChar.dfp) ? t:DChar.dfl : dfl
  endif
  if init && 0 < rx && len(dfl[1]) == len(dfl[2])
    let t:DChar.dfp = 0
    return dfl
  endif
  " 3. find how to relate between top/bottom lines of dfl
  for k in [1, 2]
    let tb[k] = {'tl': 1, 'ti': -k, 'tc': -k, 'bl': t:DChar.lcc[k].ll,
                                                          \'bi': -k, 'bc': -k}
    if !empty(dfl[k])
      if !init
        if t:DChar.dfl[k][0] < dfl[k][0]
          let tb[k].ti = index(t:DChar.dfl[k], dfl[k][0])
          if tb[k].ti == -1
            let tb[k].ti = filter(range(len(t:DChar.dfl[k])),
                                    \'t:DChar.dfl[k][v:val] < dfl[k][0]')[-1]
          endif
          let tb[k].tl = t:DChar.dfl[k][tb[k].ti]
        endif
        if dfl[k][-1] < t:DChar.dfl[k][-1]
          let tb[k].bi = index(t:DChar.dfl[k], dfl[k][-1])
          if tb[k].bi == -1
            let tb[k].bi = filter(range(len(t:DChar.dfl[k])),
                                    \'dfl[k][-1] < t:DChar.dfl[k][v:val]')[0]
          endif
          let tb[k].bl = t:DChar.dfl[k][tb[k].bi]
        endif
      endif
      let tb[k].tc = dfl[k][0] - tb[k].tl
      let tb[k].bc = tb[k].bl - dfl[k][-1]
    endif
  endfor
  " 4. return if dfl in both win have same reference line and distance
  if len(dfl[1]) == len(dfl[2])
    let [tc, bc] = [tb[1].tc == tb[2].tc, tb[1].bc == tb[2].bc]
    if init
      if tc || bc | return dfl | endif
    else
      let [ti, bi] = [tb[1].ti == tb[2].ti, tb[1].bi == tb[2].bi]
      if ti && tc || bi && bc
        if 0 < t:DChar.dfp
          for k in [1, 2]
            let tl = ti && tc ?
                            \t:DChar.dfl[k][: tb[k].ti - (tb[k].tc == 0)] : []
            let bl = bi && bc ?
                            \t:DChar.dfl[k][tb[k].bi + (tb[k].bc == 0) :] : []
            let dfl[k] = tl + dfl[k] + bl
          endfor
        endif
        return dfl
      endif
    endif
  endif
  " 5. get opposite dfl from one win using reference line and distance
  let dfx = {1: [], 2: []}
  for [k1, k2] in [[1, 2], [2, 1]]
    if !empty(dfl[k1])
      if tb[k1].tc == 0
        let [sd, dc] = [1, 0]
      elseif tb[k1].bc == 0
        let [sd, dc] = [0, 0]
      else
        let sd = tb[k1].tc < tb[k1].bc
        call win_execute(t:DChar.wid[k1], 'let dc = len(sd ?
                                \s:GetDiffLines(tb[k1].tl, dfl[k1][0] - 1) :
                                \s:GetDiffLines(dfl[k1][-1] + 1, tb[k1].bl))')
      endif
      let ac = len(dfl[k1])
      call win_execute(t:DChar.wid[k2], 'let dfx[k2] = sd ?
                                    \s:SearchDiffLines(sd, (0 <= tb[k1].ti) ?
                            \t:DChar.dfl[k2][tb[k1].ti] : 1, ac + dc)[-ac :] :
                                    \s:SearchDiffLines(sd, (0 <= tb[k1].bi) ?
                            \t:DChar.dfl[k2][tb[k1].bi] : t:DChar.lcc[k2].ll,
                                                        \ac + dc)[: ac - 1]')
    endif
  endfor
  " 6. join original and opposite dfls
  for k in [1, 2]
    if !empty(dfx[k])
      if !empty(dfl[k])
        let [tl, bl] = [[], []]
        if dfx[k][0] < dfl[k][0]
          let ti = index(dfx[k], dfl[k][0])
          let tl = (ti != -1) ? dfx[k][: ti - 1] : dfx[k]
        endif
        if dfl[k][-1] < dfx[k][-1]
          let bi = index(dfx[k], dfl[k][-1])
          let bl = (bi != -1) ? dfx[k][bi + 1 :] : dfx[k]
        endif
        let dfl[k] = tl + dfl[k] + bl
      else
        let dfl[k] = dfx[k]
      endif
    endif
  endfor
  " 7. merge with existing dfl
  if !init && 0 < t:DChar.dfp
    for k in [1, 2]
      let [tl, bl] = [[], []]
      if t:DChar.dfl[k][0] < dfl[k][0]
        let ti = index(t:DChar.dfl[k], dfl[k][0])
        let tl = (ti != -1) ? t:DChar.dfl[k][: ti - 1] :
                            \filter(copy(t:DChar.dfl[k]), 'v:val < dfl[k][0]')
      endif
      if dfl[k][-1] < t:DChar.dfl[k][-1]
        let bi = index(t:DChar.dfl[k], dfl[k][-1])
        let bl = (bi != -1) ? t:DChar.dfl[k][bi + 1 :] :
                          \filter(copy(t:DChar.dfl[k]), 'dfl[k][-1] < v:val')
      endif
      let dfl[k] = tl + dfl[k] + bl
    endfor
  endif
  return dfl
endfunction

function! s:SearchDiffLines(sd, sl, sc) abort
  " a:sd = direction (1:down, 0:up), a:sl = start line, a:sc = count
  let [dl, sl] = [[], a:sl]
  if a:sd
    while len(dl) < a:sc && sl <= line('$')
      let fl = foldclosedend(sl)
      if fl == -1
        let dl += s:GetDiffLines(sl, sl + a:sc - 1)
        let sl += a:sc
      else
        let sl = fl + 1
      endif
    endwhile
    return dl[: a:sc - 1]
  else
    while len(dl) < a:sc && 1 <= sl
      let fl = foldclosed(sl)
      if fl == -1
        let dl = s:GetDiffLines(sl - a:sc + 1, sl) + dl
        let sl -= a:sc
      else
        let sl = fl - 1
      endif
    endwhile
    return dl[-a:sc :]
  endif
endfunction

function! s:GetDiffLines(sl, el) abort
  return (a:sl > a:el) ? [] : filter(range(a:sl, a:el),
          \'index([s:DiffHL.C.id, s:DiffHL.T.id], diff_hlID(v:val, 1)) != -1')
endfunction

function! s:GetLineColCnr() abort
  let lcc = {}
  for k in [1, 2]
    call win_execute(t:DChar.wid[k], 'let lcc[k] =
                        \{"tl": line("w0"), "bl": line("w$"), "ll": line("$"),
                        \"cl": line("."), "cc": col("."), "cn": changenr()}')
    if lcc[k].bl < lcc[k].ll && (s:VF.ScreenPos ?
                      \screenpos(t:DChar.wid[k], lcc[k].bl + 1, 1).row != 0 :
      \&display =~ 'lastline\|truncate' && getwinvar(t:DChar.wid[k], '&wrap'))
      let lcc[k].bl += 1
    endif
  endfor
  return lcc
endfunction

function! s:UpdateDiffChar() abort
  if mode(1) == 'n' && exists('t:DChar') &&
            \empty(filter(values(t:DChar.wid), '!getwinvar(v:val, "&diff")'))
    let k = (t:DChar.bnr[1] == eval(expand('<abuf>'))) ? 1 : 2
    call win_execute(t:DChar.wid[k], 'call s:RedrawDiffChar(k)')
  endif
endfunction

function! s:RedrawDiffChar(key) abort
  let lcc = t:DChar.lcc[a:key]
  let t:DChar.lcc = s:GetLineColCnr()
  let cfl = s:FocusDiffLines(1)
  if t:DChar.lcc[a:key].cn != lcc.cn
    " compare between previous and current DChar and diff lines
    " using checksum and find ones to be deleted, added, and shifted
    let [ak, bk] = (a:key == 1) ? [1, 2] : [2, 1]
    let lnd = t:DChar.lcc[ak].ll - lcc.ll
    let pfl = t:DChar.dfl
    if pfl[ak] == cfl[ak]
      let ddl = filter(copy(pfl[ak]), 'pfl[bk][v:key] != cfl[bk][v:key] ||
                                              \get(t:DChar.cks[ak], v:val) !=
              \s:ChecksumStr(getbufline(t:DChar.bnr[ak], cfl[ak][v:key])[0])')
      let adl = ddl
      let sdl = []
    else
      let m = min([len(pfl[ak]), len(cfl[ak])])
      let s = 0
      while s < m && pfl[ak][s] == cfl[ak][s] && pfl[bk][s] == cfl[bk][s] &&
                                          \get(t:DChar.cks[ak], pfl[ak][s]) ==
                    \s:ChecksumStr(getbufline(t:DChar.bnr[ak], cfl[ak][s])[0])
        let s += 1
      endwhile
      let m -= s
      let e = -1
      while e >= -m && pfl[ak][e] + lnd == cfl[ak][e] &&
              \pfl[bk][e] == cfl[bk][e] && get(t:DChar.cks[ak], pfl[ak][e]) ==
                    \s:ChecksumStr(getbufline(t:DChar.bnr[ak], cfl[ak][e])[0])
        let e -= 1
      endwhile
      let ddl = pfl[ak][s : e]
      let adl = cfl[ak][s : e]
      let sdl = (lnd != 0 && e < -1) ? pfl[ak][e + 1 :] : []
    endif
    if 0 < t:DChar.dpv.pv | call s:ClearDiffCharPair(ak) | endif
    if !empty(ddl) | call s:ResetDiffChar(ddl) | endif
    let t:DChar.dfl = cfl
    if !empty(sdl) | call s:ShiftDiffChar(ak, sdl, lnd) | endif
    if !empty(adl) | call s:ShowDiffChar(adl) | endif
  else
    " reset dfl and redraw all DChar lines on text unchanged
    " (diffupdate and diffopt changes)
    let opt = s:GetDiffCharOptions()
    if [t:DChar.opt, t:DChar.dfl] != [opt, cfl]
      call s:ResetDiffChar()
      call s:ShowDiffChar()
    endif
  endif
endfunction

function! diffchar#CopyDiffCharPair(dir) abort
  " a:dir : 0 = get, 1 = put
  if !exists('t:DChar') | return | endif
  for ak in [1, 2, 0]
    if ak == 0 | return | endif
    if t:DChar.wid[ak] == win_getid() | break | endif
  endfor
  let bk = (ak == 1) ? 2 : 1
  let un = -1
  if 0 < t:DChar.dpv.pv
    if !empty(t:DChar.dpv.ch) | let [al, un] = t:DChar.dpv.ch.lc | endif
  else
    let [al, co] = [line('.'), col('.')]
    if co == col('$') | let co = 0 | endif
    if has_key(t:DChar.hlc[ak], al)
      let hc = filter(map(copy(t:DChar.hlc[ak][al]), '[v:key, v:val[1]]'),
                                    \'v:val[1][0] <= co && co <= v:val[1][1]')
      if !empty(hc) | let un = hc[0][0] | endif
    endif
  endif
  if un == -1
    call s:EchoWarning('Cursor is not on a difference unit!')
    return
  endif
  let bl = t:DChar.dfl[bk][index(t:DChar.dfl[ak], al)]
  let et = {'a': {}, 'b': {}}
  let [et.a.e, et.a.c] = t:DChar.hlc[ak][al][un]
  let [et.b.e, et.b.c] = t:DChar.hlc[bk][bl][un]
  let [et.a.t, et.b.t] = [getbufline(t:DChar.bnr[ak], al)[0],
                                          \getbufline(t:DChar.bnr[bk], bl)[0]]
  let [x, y] = a:dir ? ['b', 'a'] : ['a', 'b']
  let s1 = (1 < et[x].c[0]) ? et[x].t[: et[x].c[0] - 2] : ''
  let s2 = (et[x].e != 'a') ? et[y].t[et[y].c[0] - 1 : et[y].c[1] - 1] : ''
  if et[x].e == 'd' && et[x].c != [0, 0]
    let ds = split(et[x].t[et[x].c[0] - 1 : et[x].c[1] - 1], '\zs')
    let s2 = ((1 < et[y].c[0]) ? ds[0] : '') . s2 .
                                  \((et[y].c[1] < len(et[y].t)) ? ds[-1] : '')
  endif
  let s3 = (et[x].c[1] < len(et[x].t)) ? et[x].t[et[x].c[1] :] : ''
  let ss = s1 . s2 . s3
  if a:dir
    call setbufline(t:DChar.bnr[bk], bl, ss)
    call win_execute(t:DChar.wid[bk], 'let &undolevels = &undolevels')
    call win_execute(t:DChar.wid[bk], 'call s:RedrawDiffChar(bk)')
  else
    call setbufline(t:DChar.bnr[ak], al, ss)
  endif
endfunction

function! diffchar#JumpDiffChar(dir, pos) abort
  " a:dir : 0 = backward, 1 = forward / a:pos : 0 = start, 1 = end
  if !exists('t:DChar') | return | endif
  for k in [1, 2, 0]
    if k == 0 | return | endif
    if t:DChar.wid[k] == win_getid() | break | endif
  endfor
  let [ln, co] = [line('.'), col('.')]
  if co == col('$')   " empty line
    if !a:dir | let co = 0 | endif
  else
    if a:pos
      let co += len(strcharpart(
                      \getbufline(t:DChar.bnr[k], ln)[0][co - 1 :], 0, 1)) - 1
    endif
  endif
  if has_key(t:DChar.hlc[k], ln) &&
                            \(a:dir ? co < t:DChar.hlc[k][ln][-1][1][a:pos] :
                                        \co > t:DChar.hlc[k][ln][0][1][a:pos])
    let hc = filter(map(copy(t:DChar.hlc[k][ln]), 'v:val[1][a:pos]'),
                                        \a:dir ? 'co < v:val' : 'co > v:val')
    let co = hc[a:dir ? 0 : -1]
  else
    if t:DChar.dfp == 0
      let hl = filter(map(keys(t:DChar.hlc[k]), 'eval(v:val)'),
                                        \a:dir ? 'ln < v:val' : 'ln > v:val')
      if empty(hl) | return | endif
      let ln = a:dir ? min(hl) : max(hl)
    else
      let cp = [line('.'), col('.')]
      while 1
        let dl = s:SearchDiffLines(a:dir, a:dir ? ln + 1 : ln - 1, 1)
        if empty(dl) | noautocmd call cursor(cp) | return | endif
        let ln = dl[0]
        if has_key(t:DChar.hlc[k], ln) | break | endif
        noautocmd call cursor(ln, 0)
        call s:ScrollDiffChar(k)
        if has_key(t:DChar.hlc[k], ln) | break | endif
      endwhile
    endif
    let co = t:DChar.hlc[k][ln][a:dir ? 0 : -1][1][a:pos]
  endif
  " set a dummy cursor position to adjust the start/end
  if 0 < t:DChar.dpv.pv
    call s:ClearDiffCharPair(k)
    if [a:dir, a:pos] == [1, 0]         " forward/start : rightmost
      let [t:DChar.lcc[k].cl, t:DChar.lcc[k].cc] = [ln, col('$')]
    elseif [a:dir, a:pos] == [0, 1]     " backward/end : leftmost
      let [t:DChar.lcc[k].cl, t:DChar.lcc[k].cc] = [ln, 0]
    endif
  endif
  call cursor(ln, co)
endfunction

function! s:ToggleDiffCharPair(on) abort
  if t:DChar.dpv.pv == 3 || t:DChar.dpv.pv == 4
    let oe = [a:on, empty(t:DChar.dpv.pw)]
    if s:VF.PopupWindow
      if oe == [1, 1]
        let t:DChar.dpv.pw = popup_create('', {'hidden': 1, 'scrollbar': 0,
                                        \'wrap': 0, 'highlight': s:DCharHL.c})
      elseif oe == [0, 0]
        let t:DChar.dpv.pw = popup_close(t:DChar.dpv.pw)
      endif
    elseif s:VF.FloatingWindow
      if oe == [1, 1]
        let t:DChar.dpv.pw.fb = nvim_create_buf(0, 1)
        let t:DChar.dpv.pw.fw = nvim_open_win(t:DChar.dpv.pw.fb, 0,
          \{'relative': 'editor', 'row': 0, 'col': 0, 'height': 1, 'width': 1,
                                        \'focusable': 0, 'style': 'minimal'})
        call setbufline(t:DChar.dpv.pw.fb, 1, '')
        call setwinvar(t:DChar.dpv.pw.fw, '&winblend', 100)
        call setwinvar(t:DChar.dpv.pw.fw, '&winhighlight',
                                                    \'Normal:' . s:DCharHL.c)
      elseif oe == [0, 0]
        call nvim_win_close(t:DChar.dpv.pw.fw, 1)
        call nvim_buf_delete(t:DChar.dpv.pw.fb, {'force': 1})
        let t:DChar.dpv.pw = {}
      endif
    endif
  endif
endfunction

function! s:ShowDiffCharPair(key) abort
  if mode(1) != 'n' || !exists('t:DChar') || t:DChar.wid[a:key] != win_getid()
    return
  endif
  let [pl, pc] = [t:DChar.lcc[a:key].cl, t:DChar.lcc[a:key].cc]
  let [cl, cc] = [line('.'), col('.')]
  if cc == col('$') | let cc = 0 | endif
  let [t:DChar.lcc[a:key].cl, t:DChar.lcc[a:key].cc] = [cl, cc]
  if t:DChar.lcc[a:key].cn == changenr()
    if !empty(t:DChar.dpv.ch)
      if t:DChar.dpv.ch.bk == a:key
        " clear if a pair accidentally remains on diffsplit
        call s:ClearDiffCharPair((a:key == 1) ? 2 : 1)
      else
        let [hl, hi] = t:DChar.dpv.ch.lc
        let hc = t:DChar.hlc[a:key][hl][hi][1]
        if cl == hl && hc[0] <= cc && cc <= hc[1] | return | endif
        call s:ClearDiffCharPair(a:key) " outside, clear it
      endif
    endif
    if has_key(t:DChar.hlc[a:key], cl)
      let hu = filter(map(copy(t:DChar.hlc[a:key][cl]), '[v:key, v:val[1]]'),
                                    \'v:val[1][0] <= cc && cc <= v:val[1][1]')
      if !empty(hu)
        " for 2 contineous 'd', check if cursor moved forward/backward
        let ix = (len(hu) == 1) ? 0 : (cl == pl) ? cc < pc : cl < pl
        call s:HighlightDiffCharPair(a:key, cl, hu[ix][0])
      endif
    endif
  endif
endfunction

function! s:HighlightDiffCharPair(key, line, col) abort
  let [ak, bk] = (a:key == 1) ? [1, 2] : [2, 1]
  let [al, bl] = [a:line, t:DChar.dfl[bk][index(t:DChar.dfl[ak], a:line)]]
  let t:DChar.dpv.ch.lc = [al, a:col]
  let t:DChar.dpv.ch.bk = bk
  let bc = t:DChar.hlc[bk][bl][a:col][1]
  if bc != [0, 0]
    let [pos, len] = [bc[0], bc[1] - bc[0] + 1]
    let t:DChar.dpv.ch.id = matchaddpos(s:DCharHL.c, [[bl, pos, len]], -1, -1,
                                                \{'window': t:DChar.wid[bk]})
  else
    let t:DChar.dpv.ch.id = -1  " no cursor hl on empty line
  endif
  call execute('autocmd! diffchar WinLeave <buffer=' . t:DChar.bnr[ak] .
                                    \'> call s:ClearDiffCharPair(' . ak . ')')
  if t:DChar.dpv.pv < 2 | return | endif
  let at = getbufline(t:DChar.bnr[ak], al)[0]
  let bt = getbufline(t:DChar.bnr[bk], bl)[0]
  let [ae, ac] = t:DChar.hlc[ak][al][a:col]
  if ae == 'c'
    let hl = t:DChar.hgp[(count(map(t:DChar.hlc[ak][al][: a:col], 'v:val[0]'),
                                                \'c') - 1) % len(t:DChar.hgp)]
    let [tb, tx, te] = ['', bt[bc[0] - 1 : bc[1] - 1], '']
  elseif ae == 'd'
    let hl = s:DCharHL.A
    let [tb, tx, te] = [(1 < bc[0]) ? '<' : '',
                    \bt[bc[0] - 1 : bc[1] - 1], (bc[1] < len(bt)) ? '>' : '']
  elseif ae == 'a'
    let hl = s:DCharHL.D
    let [tb, tx, te] = [(1 < ac[0]) ? '>' : '',
                      \repeat((t:DChar.dpv.pv == 2 && s:VF.StrikeAttr) ? ' ' :
            \(&fillchars =~ 'diff') ? matchstr(&fillchars, 'diff:\zs.') : '-',
          \strwidth(at[ac[0] - 1 : ac[1] - 1])), (ac[1] < len(at)) ? '<' : '']
  endif
  if t:DChar.dpv.pv == 2
    call execute(['echon tb', 'echohl ' . hl, 'echon tx', 'echohl None',
                                                            \'echon te'], '')
  elseif t:DChar.dpv.pv == 3 || t:DChar.dpv.pv == 4
    if t:DChar.dpv.pv == 4 | let mp = getmousepos() | endif
    if s:VF.PopupWindow
      call popup_move(t:DChar.dpv.pw, (t:DChar.dpv.pv == 3) ?
                                      \{'line': 'cursor+1', 'col': 'cursor'} :
                                \{'line': mp.screenrow, 'col': mp.screencol})
      call popup_settext(t:DChar.dpv.pw, tb . tx . te)
      call popup_show(t:DChar.dpv.pw)
    elseif s:VF.FloatingWindow
      call nvim_win_set_config(t:DChar.dpv.pw.fw,
        \extend((t:DChar.dpv.pv == 3) ?
                                \{'relative': 'cursor', 'row': 1, 'col': 0} :
            \{'relative': 'editor', 'row': mp.screenrow, 'col': mp.screencol},
                                  \{'width': strdisplaywidth(tb . tx . te)}))
      call setbufline(t:DChar.dpv.pw.fb, 1, tb . tx . te)
      call setwinvar(t:DChar.dpv.pw.fw, '&winblend', 0)
    endif
  endif
endfunction

function! s:ClearDiffCharPair(key) abort
  if !exists('t:DChar') | return | endif
  if !empty(t:DChar.dpv.ch)
    let [bk, id] = [t:DChar.dpv.ch.bk, t:DChar.dpv.ch.id]
    if id != -1 && win_id2win(t:DChar.wid[bk]) != 0
      silent! call matchdelete(id, t:DChar.wid[bk])
    endif
    call execute('autocmd! diffchar WinLeave <buffer=' .
                                        \t:DChar.bnr[(bk == 1) ? 2 : 1] . '>')
    let t:DChar.dpv.ch = {}
  endif
  if t:DChar.dpv.pv == 2 | call execute('echo', '')
  elseif t:DChar.dpv.pv == 3 || t:DChar.dpv.pv == 4
    if s:VF.PopupWindow | call popup_hide(t:DChar.dpv.pw)
    elseif s:VF.FloatingWindow
      call nvim_win_set_config(t:DChar.dpv.pw.fw,
                      \{'relative': 'editor', 'row': 0, 'col': 0, 'width': 1})
      call setbufline(t:DChar.dpv.pw.fb, 1, '')
      call setwinvar(t:DChar.dpv.pw.fw, '&winblend', 100)
    endif
  endif
endfunction

function! diffchar#ToggleDiffModeSync(event) abort
  " a:event : 0 = OptionSet diff, 1 = VimEnter
  if !exists('t:DChar') && get(t:, 'NoDiffChar', get(g:, 'NoDiffChar', 0))
    return
  endif
  if a:event || v:option_old != v:option_new
    let cw = win_getid()
    if exists('t:DChar') && ((a:event || v:option_new) ?
                            \index(values(t:DChar.bnr), winbufnr(cw)) == -1 :
                                        \index(values(t:DChar.wid), cw) != -1)
      " diff mode ON on non-DChar buf || OFF on DChar win, try reset
      let dk = filter([1, 2], 'getwinvar(t:DChar.wid[v:val], "&diff")')
      if !empty(dk)
        if empty(filter(copy(dk), 't:DChar.wid[v:val] == cw'))
          let cw = t:DChar.wid[dk[0]]
        endif
        call win_execute(cw, 'call s:ResetDiffChar(1)')
      endif
    endif
    if !exists('t:DChar')
      let aw = win_id2win(cw)
      let dw = filter(map(range(aw, winnr('$')) + range(1, aw - 1),
                            \'win_getid(v:val)'), 'getwinvar(v:val, "&diff")')
      if 1 < len(dw)
        " 2 or more diff mode wins exists, try show
        call win_execute(dw[0], 'call s:ShowDiffChar()')
      endif
    endif
  endif
endfunction

function! s:WinClosedDiffChar() abort
  " reset and show (if possible) DChar on WinClosed or BufWinLeave
  for ti in filter(gettabinfo(), 'has_key(v:val.variables, "DChar")')
    let dc = ti.variables.DChar
    for k in [1, 2]
      if s:VF.WinClosed ? dc.wid[k] == eval(expand('<afile>')) :
                                          \dc.bnr[k] == eval(expand('<abuf>'))
        if !s:VF.WinExecFixed && ti.tabnr != tabpagenr()
          let cw = win_getid()
          noautocmd call win_gotoid(dc.wid[k])
          call s:ResetDiffChar(1)
        else
          let cw = 0
          call win_execute(dc.wid[k], 'call s:ResetDiffChar(1)')
        endif
        let dw = filter(ti.windows, 'v:val != dc.wid[k] &&
                  \winbufnr(v:val) == dc.bnr[k] && getwinvar(v:val, "&diff")')
        if !empty(dw)
          if cw
            noautocmd call win_gotoid(dw[0])
            call s:ShowDiffChar()
          else
            call win_execute(dw[0], 'call s:ShowDiffChar()')
          endif
        endif
        if cw | noautocmd call win_gotoid(cw) | endif
        return
      endif
    endfor
  endfor
endfunction

function! s:RepairDiffChar() abort
  " repair DChar whose win was accidentally closed on BufWinEnter/WinEnter
  if exists('t:DChar')
    let dc = t:DChar
    let dw = filter(copy(dc.wid), 'win_id2win(v:val) != 0 &&
              \winbufnr(v:val) == dc.bnr[v:key] && getwinvar(v:val, "&diff")')
    if len(dw) == 1
      call win_execute(values(dw)[0], ['call s:ResetDiffChar(1)',
                                                    \'call s:ShowDiffChar()'])
    endif
  endif
endfunction

function! s:ChecksumStr(str) abort
  return str2nr(sha256(a:str)[-4 :], 16)
endfunction

function! s:EchoWarning(msg) abort
  call execute(['echohl WarningMsg', 'echo a:msg', 'echohl None'], '')
endfunction

if s:VF.Vim9Script
function! s:DefVim9DiffChar() abort
def! s:TraceDiffChar(u1: list<string>, u2: list<string>, ih: bool): string
  var n1 = len(u1) | var n2 = len(u2)
  if u1 ==# u2 | return repeat('=', n1)
  elseif n1 == 0 | return repeat('+', n2)
  elseif n2 == 0 | return repeat('-', n1)
  endif
  var N: number | var M: number
  var v1: list<string> | var v2: list<string>
  var e1: string | var e2: string
  [N, M, v1, v2, e1, e2] = (n1 >= n2) ?
                      [n1, n2, u1, u2, '-', '+'] : [n2, n1, u2, u1, '+', '-']
  var D = N - M
  var fp = repeat([-1], M + N + 1)
  var etree = []
  var p = -1
  while fp[D] != N
    p += 1
    var epk = repeat([[]], p * 2 + D + 1)
    for k in range(-p, D - 1, 1) + range(D + p, D, -1)
      var x: number | var y: number
      [y, epk[k]] = (fp[k - 1] + 1 > fp[k + 1]) ?
                        [fp[k - 1] + 1, [e1, [(k > D) ? p - 1 : p, k - 1]]] :
                        [fp[k + 1], [e2, [(k < D) ? p - 1 : p, k + 1]]]
      x = y - k
      while x < M && y < N && v2[x] ==# v1[y]
        epk[k][0] ..= '=' | [x, y] += [1, 1]
      endwhile
      fp[k] = y
    endfor
    etree += [epk]
  endwhile
  var k = D
  var ses = ''
  while 1
    ses = etree[p][k][0] .. ses
    if [p, k] == [0, 0] | break | endif
    [p, k] = etree[p][k][1]
  endwhile
  ses = ses[1 :]
  if ih
    var p1 = -1 | var p2 = -1 | var qc = 0 | var et = '' | var ex = ''
    for ed in reverse(split(ses, '[+-]\+\zs'))
      var es = ed .. et
      var qe = count(ed, '=')
      if 0 < qe
        var q1 = count(es, e1) | var q2 = count(es, e2)
        var vv: list<string> | var pp: number | var qq: number
        [vv, pp, qq] = (qe <= q1 && q2 == 0) ? [v1, p1, q1] :
                          (q1 == 0 && qe <= q2) ? [v2, p2, q2] : [[], -1, -1]
        [ex, es, p1, p2] = (!empty(vv) &&
                    vv[pp - qq - qe + 1 : pp - qq] ==# vv[pp - qe + 1 : pp]) ?
                            [es[: qe - 1] .. ex, es[qe :], p1 - qe, p2 - qe] :
                            [es .. ex, '', p1 - qe - q1, p2 - qe - q2]
        qc += 1
      endif
      et = es
    endfor
    if 1 < qc | ses = et .. ex | endif
  endif
  return ses
enddef

def! s:GetDiffUnitPos(uu: list<any>, es: string): list<any>
  if empty(uu[0])
    return [[['d', [0, 0]]], [['a', [1, len(join(uu[1], ''))]]]]
  elseif empty(uu[1])
    return [[['a', [1, len(join(uu[0], ''))]]], [['d', [0, 0]]]]
  endif
  var cc = [[], []] | var ll = [1, 1] | var pp = [0, 0]
  for ed in split(es, '[+-]\+\zs', 1)[: -2]
    var qe = count(ed, '=') | var qq = [count(ed, '-'), count(ed, '+')]
    var ee = (qq[0] == 0) ? ['d', 'a'] : (qq[1] == 0) ? ['a', 'd'] :
                                                                    ['c', 'c']
    for k in [0, 1]
      if 0 < qe
        [ll[k], pp[k]] += [len(join(uu[k][pp[k] : pp[k] + qe - 1], '')), qe]
      endif
      var hh: list<number>
      if 0 < qq[k]
        hh = [ll[k]]
        [ll[k], pp[k]] +=
                      [len(join(uu[k][pp[k] : pp[k] + qq[k] - 1], '')), qq[k]]
        hh += [ll[k] - 1]
      else
        hh = [ll[k] - ((0 < pp[k]) ? len(strcharpart(uu[k][pp[k] - 1],
                                    strchars(uu[k][pp[k] - 1]) - 1, 1)) : 0),
              ll[k] + ((pp[k] < len(uu[k])) ?
                              len(strcharpart(uu[k][pp[k]], 0, 1)) - 1 : -1)]
      endif
      cc[k] += [[ee[k], hh]]
    endfor
  endfor
  return cc
enddef

def! s:HighlightDiffChar(lec: dict<any>)
  var hn = len(t:DChar.hgp)
  for k in [1, 2]
    for [l, ec] in items(lec[k])
      var ln = str2nr(l)
      if has_key(t:DChar.mid[k], ln) | continue | endif
      t:DChar.hlc[k][ln] = ec
      var hc = {} | var h: string | var cn = 0
      for [e, c] in ec
        if e == 'c'
          h = t:DChar.hgp[cn % hn] | cn += 1
        elseif e == 'a'
          h = s:DCharHL.A
        elseif e == 'd'
          if c == [0, 0] | continue | endif
          h = s:DCharHL.E
        endif
        if !has_key(hc, h) | hc[h] = [] | endif
        hc[h] += [[ln, c[0], c[1] - c[0] + 1]]
      endfor
      t:DChar.mid[k][ln] = [matchaddpos(s:DCharHL.C, [ln], -5, -1,
                                                  {'window': t:DChar.wid[k]})]
      for [hh, cc] in items(hc)
        for ic in range(0, len(cc) - 1, 8)
          t:DChar.mid[k][ln] += [matchaddpos(hh, cc[ic : ic + 7], -3, -1,
                                                  {'window': t:DChar.wid[k]})]
        endfor
      endfor
    endfor
  endfor
enddef

def! s:GetDiffLines(sl: number, el: number): list<number>
  return (sl > el) ? [] : filter(range(sl, el),
              'index([DiffHL.C.id, DiffHL.T.id], diff_hlID(v:val, 1)) != -1')
enddef

def! s:ChecksumStr(str: string): number
  return str2nr(sha256(str)[-4 :], 16)
enddef
endfunction
call s:DefVim9DiffChar()
endif

let &cpoptions = s:save_cpo
unlet s:save_cpo

" vim: ts=2 sw=0 sts=-1 et
