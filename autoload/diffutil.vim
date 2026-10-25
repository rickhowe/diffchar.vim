" diffutil: a diff function utility for vim/nvim plugins 
"
" Last Change: 2026/10/25
" Version:     3.4
" Author:      Rick Howe (Takumi Ohtani) <rdcxy754@ybb.ne.jp>
" Copyright:   (c) 2024-2026 Rick Howe
" License:     MIT

let s:save_cpo = &cpoptions
set cpo&vim

function! diffutil#DiffOpt() abort
  let op = #{}
  for [ot, vn] in items(#{b: [['icase', ''],
                            \['iblank', 'ignore_blank_lines'],
                            \['iwhiteall', 'ignore_whitespace'],
                            \['iwhite', 'ignore_whitespace_change'],
                            \['iwhiteeol', 'ignore_whitespace_change_at_eol'],
                            \['indent-heuristic', 'indent_heuristic']],
                          \s: [['algorithm', '']], n: [['linematch', '']]})
    for [vo, no] in vn
      if &diffopt =~ '\<' . vo . '\>'
        if ot == 'b'
          let op[vo] = v:true
        else
          let op[vo] = matchstr(&diffopt, vo . ':\zs\w\+\ze')
          if ot == 'n'
            let op[vo] = str2nr(op[vo])
          endif
        endif
        if has('nvim') && !empty(no)
          let op[no] = op[vo]
          unlet op[vo]
        endif
      endif
    endfor
  endfor
  return op
endfunction

"let s:idxcnt = 1        " index count or shortest edit script

if get(g:, 'BuiltinDiffFunc', 1) &&
                      \(has('nvim') ? type(luaeval('vim.diff')) == v:t_func :
                                    \exists('*diff') && has('patch-9.1.0099'))
" ===========================
" builtin diff function
" ===========================
function! diffutil#DiffFunc(u1, u2, op) abort
  let [n1, n2] = [len(a:u1), len(a:u2)]
  if a:u1 ==# a:u2 | let ic = []
  elseif n1 == 0 | let ic = [[0, 0, 0, n2]]
  elseif n2 == 0 | let ic = [[0, n1, 0, 0]]
  else | let ic = s:BuiltinDiff(a:u1, a:u2, a:op)
  endif
  "if s:idxcnt
    return ic
  "else
    "let es = ''
    "let p1 = 0
    "for [i1, c1, i2, c2] in ic + [[n1, 0, 0, 0]]
      "let es .= repeat('=', i1 - p1) . repeat('-', c1) . repeat('+', c2)
      "let p1 = i1 + c1
    "endfor
    "return es
  "endif
endfunction

if has('nvim')
function! s:BuiltinDiff(u1, u2, op) abort
  let op = copy(a:op)
  let [l1, l2] = [empty(a:u1) ? '' : join(a:u1, "\n") . "\n",
                                  \empty(a:u2) ? '' : join(a:u2, "\n") . "\n"]
  if has_key(op, 'icase')
    let [l1, l2] = [tolower(l1), tolower(l2)]
    unlet op['icase']
  endif
  let op['result_type'] = 'indices'
  return map(v:lua.vim.diff(l1, l2, op),
                            \'[v:val[0] - ((0 < v:val[1]) ? 1 : 0), v:val[1],
                            \v:val[2] - ((0 < v:val[3]) ? 1 : 0), v:val[3]]')
endfunction
else
function! s:BuiltinDiff(u1, u2, op) abort
  let op = copy(a:op)
  let op['output'] = 'indices'
  return map(diff(a:u1, a:u2, op),
          \'[v:val.from_idx, v:val.from_count, v:val.to_idx, v:val.to_count]')
endfunction
endif

else
" ===========================
" plugin diff function
" ===========================
function! diffutil#DiffFunc(u1, u2, op) abort
  let [u1, u2] = [copy(a:u1), copy(a:u2)]
  for uu in [u1, u2]
    if has_key(a:op, 'icase')
      call map(uu, 'tolower(v:val)')
    endif
    if has_key(a:op, 'iwhiteall')
      call map(uu, 'substitute(v:val, "\\s\\+", "", "g")')
    elseif has_key(a:op, 'iwhite')
      call map(uu, 'substitute(v:val, "\\s\\+", " ", "g")')
      call map(uu, 'substitute(v:val, "\\s\\+$", "", "")')
    elseif has_key(a:op, 'iwhiteeol')
      call map(uu, 'substitute(v:val, "\\s\\+$", "", "")')
    endif
  endfor
  let es = s:PluginDiff(u1, u2,
              \has_key(a:op, 'indent-heuristic') && a:op['indent-heuristic'])
  "if s:idxcnt
    let ic = []
    let [i1, i2] = [0, 0]
    for ed in split(es, '[-+]\+\zs', 1)[: -2]
      let [ce, c1, c2] = map(['=', '-', '+'], 'count(ed, v:val)')
      let [i1, i2] += [ce, ce]
      let ic += [[i1, c1, i2, c2]]
      let [i1, i2] += [c1, c2]
    endfor
    return ic
  "else
    "return es
  "endif
endfunction

if has('vim9script') && get(g:, 'vim9script', 1)
" ---------------------------
" vim9script
" ---------------------------
function! s:Vim9PluginDiff() abort
def! s:PluginDiff(u1: list<string>, u2: list<string>, ih: bool): string
  const [eq, n1, n2] = ['=', len(u1), len(u2)]
  var [e1, e2] = ['-', '+']
  if u1 ==# u2 | return repeat(eq, n1)
  elseif n1 == 0 | return repeat(e2, n2)
  elseif n2 == 0 | return repeat(e1, n1)
  endif
  const [N, M, v1, v2] = (n1 >= n2) ? [n1, n2, u1, u2] : [n2, n1, u2, u1]
  if n1 < n2 | [e1, e2] = [e2, e1] | endif
  const D = N - M
  var fp = repeat([-1], M + N + 1)
  var etree = []
  var p = -1
  while fp[D] != N
    p += 1
    var epk: list<list<any>> = repeat([[]], p * 2 + D + 1)
    for k in range(-p, D - 1, 1) + range(D + p, D, -1)
      var x: number | var y: number
      [y, epk[k]] = (fp[k - 1] + 1 > fp[k + 1]) ?
                        [fp[k - 1] + 1, [e1, [(k > D) ? p - 1 : p, k - 1]]] :
                        [fp[k + 1], [e2, [(k < D) ? p - 1 : p, k + 1]]]
      x = y - k
      while x < M && y < N && v2[x] ==# v1[y]
        epk[k][0] ..= eq | [x, y] += [1, 1]
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
  return ih ? s:ReduceDiffHunk(u1, u2, ses) : ses
enddef

def! s:ReduceDiffHunk(u1: list<string>, u2: list<string>, ses: string): string
  # in ==++++/==----, if == units equal to last ++/-- units, swap their SESs
  # (AB vs AxByAB : =+=+++ -> =++++= -> ++++==)
  const [eq, e1, e2] = ['=', '-', '+']
  var [p1, p2] = [-1, -1] | var xes = '' | var ez = ''
  for ed in reverse(split(ses, '[+-]\+\zs'))
    var es = ed .. ez | ez = '' | const qe = count(es, eq)
    if 0 < qe
      const [q1, q2] = [count(es, e1), count(es, e2)]
      const [uu, pp, qq] = (qe <= q1 && q2 == 0) ? [u1, p1, q1] :
                            (q1 == 0 && qe <= q2) ? [u2, p2, q2] : [[], 0, 0]
      if !empty(uu) && uu[pp - qq - qe + 1 : pp - qq] ==# uu[pp - qe + 1 : pp]
        ez = es[-qe :] .. es[qe : -qe - 1] | es = es[: qe - 1]
      else
        [p1, p2] -= [q1, q2]
      endif
    endif
    [p1, p2] -= [qe, qe]
    xes = es .. xes
  endfor
  xes = ez .. xes
  return xes
enddef
endfunction
call s:Vim9PluginDiff()

else
" ---------------------------
" vim8script
" ---------------------------
function! s:PluginDiff(u1, u2, ih) abort
  " An O(NP) Sequence Comparison Algorithm
  let [u1, u2, eq, e1, e2] = [a:u1, a:u2, '=', '-', '+']
  let [n1, n2] = [len(u1), len(u2)]
  if u1 ==# u2 | return repeat(eq, n1)
  elseif n1 == 0 | return repeat(e2, n2)
  elseif n2 == 0 | return repeat(e1, n1)
  endif
  let [N, M, u1, u2] = (n1 >= n2) ? [n1, n2, u1, u2] : [n2, n1, u2, u1]
  if n1 < n2 | let [e1, e2] = [e2, e1] | endif
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
        let epk[k][0] .= eq | let [x, y] += [1, 1]
      endwhile
      let fp[k] = y
    endfor
    let etree += [epk]
  endwhile
  let ses = ''
  while 1
    let ses = etree[p][k][0] . ses
    if [p, k] == [0, 0] | break | endif
    let [p, k] = etree[p][k][1]
  endwhile
  let ses = ses[1 :]
  return a:ih ? s:ReduceDiffHunk(a:u1, a:u2, ses) : ses
endfunction

function! s:ReduceDiffHunk(u1, u2, ses) abort
  " in ==++++/==----, if == units equal to last ++/-- units, swap their SESs
  " (AB vs AxByAB : =+=+++ -> =++++= -> ++++==)
  let [eq, e1, e2] = ['=', '-', '+']
  let [p1, p2] = [-1, -1] | let ses = '' | let ez = ''
  for ed in reverse(split(a:ses, '[+-]\+\zs'))
    let es = ed . ez | let ez = '' | let qe = count(es, eq)
    if 0 < qe
      let [q1, q2] = [count(es, e1), count(es, e2)]
      let [uu, pp, qq] = (qe <= q1 && q2 == 0) ? [a:u1, p1, q1] :
                        \(q1 == 0 && qe <= q2) ? [a:u2, p2, q2] : [[], 0, 0]
      if !empty(uu) && uu[pp - qq - qe + 1 : pp - qq] ==# uu[pp - qe + 1 : pp]
        let ez = es[-qe :] . es[qe : -qe - 1] | let es = es[: qe - 1]
      else
        let [p1, p2] -= [q1, q2]
      endif
    endif
    let [p1, p2] -= [qe, qe]
    let ses = es . ses
  endfor
  let ses = ez . ses
  return ses
endfunction
endif
endif

let &cpoptions = s:save_cpo
unlet s:save_cpo

" vim: ts=2 sw=0 sts=-1 et
