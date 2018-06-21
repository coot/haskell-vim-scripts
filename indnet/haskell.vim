" Haskell indentation script
" Author: Marcin Szamotulski (profunctor@pm.me)
" Maintainer: Marcin Szamotulski (profunctor@pm.me)

setl indentexpr=GetHaskellIndent()
setl indentkeys=!^F,o,O,},=where,=in,=deriving,=::,==,=->,==>,=\|

fun! s:getSynStack(lnum, col)
  return map(synstack(a:lnum, a:col), { key, val -> synIDattr(val, "name") })
endfun

fun! s:isCommentOrString(lnum, col)
  let ss = s:getSynStack(a:lnum, a:col)
  if len(filter(ss, {idx, val -> val == 'hsString' || val == 'hsLineComment' || val == 'hsBlockComment'})) > 0
    return v:true
  else
    return v:false
  endif
endfun

if !exists('g:haskell_indent_case')
  " ```
  " case xs of
  " >>>>[]     -> ...
  " >>>>(y:ys) -> ...
  " ```
  let g:haskell_indent_case = &l:shiftwidth
endif

if !exists('g:haskell_indent_let')
  " ```
  " let x = 0
  " >>>>x
  " ```
  let g:haskell_indent_let = &l:shiftwidth
endif

if !exists('g:haskell_indent_in')
  " ```
  " let x = 0
  " in
  " ```
  let g:haskell_indent_in = 0
endif

if !exists('g:haskell_indent_where')
  " ```
  " where
  " >>>>f :: Int -> Int
  " >>>>f x = x
  " ```
  let g:haskell_indent_where = 4
endif

fun! FindFirstNonClosedBracket(line)
  " Find first non closed '(', '[' or '{' in a single line
  let stackBracket	  = []
  let stackCurlyBracket	  = []
  let stackSquareBracket  = []
  let col = -1
  for chr in split(a:line, '\zs')
    let col += 1
    if chr == '('
      call add(stackBracket, col)
    elseif chr == ')'
      call remove(stackBracket, len(stackBracket) - 1)
    elseif chr == '['
      call add(stackSquareBracket, col)
    elseif chr == ']'
      call remove(stackSquareBracket, len(stackSquareBracket) - 1)
    elseif chr == '{'
      call add(stackCurlyBracket, col)
    elseif chr == '}'
      call remove(stackCurlyBracket, len(stackCurlyBracket) - 1)
    endif
  endfor

  let results = []
  if len(stackBracket)
    call add(results, stackBracket[0])
  endif
  if len(stackCurlyBracket)
    call add(results, stackCurlyBracket[0])
  endif
  if len(stackSquareBracket)
    call add(results, stackSquareBracket[0])
  endif
  if len(results)
    return min(results)
  else
    return -1
  endif
endfun

fun! GetHaskellIndent()
  let col	= v:col
  let lnum	= v:lnum
  let line	= getline(v:lnum)
  let pline	= getline(v:lnum - 1)
  let ppline	= getline(v:lnum - 2)
  let nline     = getline(v:lnum + 1)

  let s = match(line, '\<in\>')
  if s > 0 && !s:isCommentOrString(v:lnum, s)
    let [l, c] = searchpos('\<let\>', 'bnz')
    return c - 1
  endif

  let s = match(pline, '\v^\s*\zs--')
  if s >= 0
    return s
  endif

  if line =~ '^\s*\%(::\|=\)\>'
    return &l:shiftwidth
  endif

  let s = match(pline, '\v%(^\k+\s*::\s*)@<=\([^)]*$')
  if s >= 0
    " ```
    " f :: ( Eq a
    "      , 
    " ```
    let r = 
    let o = 0
    for x in split(pline[s:], '\zs') 
      if x == "("
	let o += 1
      elseif x == ")"
	let o -= 1
      endif
    endfor
    if o == 0
      return s
    endif
  endif

  let s = match(pline, '^\s*)')
  if s >= 0
    " ```
    " f :: ( Eq a
    "      , Ord a
    "      )
    "   ->
    " ```
    let n = v:lnum - 1
    let l = getline(n)
    while n >= 0 && l !~ '^\k*\s*::' && l !~ '^\s*$'
      let n -= 1
      let l = getline(n)
    endwhile
    let s = match(getline(n), '::')
    if s >= 0
      return s
    endif
  endif

  let s = match(pline, '\v%(^\k+\s*)@<=::')
  let r = strpart(pline, s)
  let t = r  =~ '[-=]>$'
  if s >= 0 && (r !~ '[-=]>' || t) && line !~ '='
    if t
      " ```
      " f :: String ->
      "	     String
      " ```
      return match(pline, '\v^\k+\s*::\s*\zs')
    else
      " ```
      " f :: String
      "   -> String
      " ```
      return s
    endif
  endif

  if line =~ '^\s*' && pline =~ '^\s*$' && ppline =~ '^\s*$'
    let n = v:lnum - 2
    while n >= 0 && getline(n) =~ '^\s*$'
      let n -= 1
    endwhile
    return indent(n) - &l:shiftwidth
  endif

  if line =~ '^\s*deriving\>'
    let n = v:lnum - 1
    let l = pline
    while n > 0 && l !~ '^\s*$'
      let s = match(l, '^\s*\zs\%(data\|newtype\)\>')
      if s >= 0
	return s + &l:shiftwidth
      endif
      let n -= 1
      let l = getline(n)
    endwhile
    return 0
  endif

  let s = match(pline, '|')
  if s > 0
    if line =~ '^\s*\%($\||\)'
      " empty line or line that starts with `|`
      return s
    else
      " find first line with `|`
    endif
    let l = v:lnum - 1
    while l >= 0 && getline(l - 1) =~ '|'
      let l -= 1
    endwhile
    return indent(l)
  endif

  if line =~ '^\s*|'
    " line which starts with `|`, but previous line has no `|` (matched by
    " previous group).
    let s = match(pline, '=')
    if s >= 0
      return s
    else
      return indent(v:lnum - 1) + &l:shiftwidth
    endif
  endif

  " ```
  " ( abc
  " , def
  "
  " ( abc,
  "   def,
  "
  " { abc
  " ; 
  "
  " { abc ;
  "   def
  "
  " ( Text (..)
  " , 
  " ```
  let s = FindFirstNonClosedBracket(pline)
  " This pattern skips over `(..)`, `[..]` and `{..}` and matches first non
  " closed `(`, `{` or `[`.
  if s >= 0 && !s:isCommentOrString(v:lnum - 1, s)
    let r = strpart(pline, s, 1)
    let q = strpart(pline, s + 1)
    if r == '{'  && pline =~ ';\s*$'
      return match(pline, '{\s*\zs[^}]\S')
    elseif (r == '(' || r == '[') && pline =~ ',\s*$'
      return s + 1 + match(q, '\s*\zs\S')
    else
      return s
    endif
  endif

  " Find previous line with a smaller indentation if line ends with `)` or `]`
  " which does not close in the current line
  if pline =~ '\%(\%(([^)]\{-}\)\@<!)\|\%(\[[^\]]\{-}\)\@<!\]\)' && pline =~ '^\s*[;,(\[{]'
    let n = v:lnum - 2
    let i = indent(v:lnum - 1)
    let in = indent(n)
    let l = getline(n)
    while n >= 0 && in >= i
      let n -= 1
      let in = indent(n)
    endwhile
    return in
  endif

  let s = match(pline, '::')
  if s >= 0 && line =~ '^\s*[=-]>'
    " When opening a new line with `->`:
    " ```
    " fun :: String
    " >>>>->
    " ```
    return s
  endif

  let s = match(pline, '^\s*\zs\%(::\|=>\|->\)')
  let r = match(pline, '^\s*\zs\.')
  if s >= 0 || (r >= 0 && ppline =~ '::')
    if s >= 0
      if pline =~ '\.\s*$'
	" ```
	" :: forall a .
	" ->  
	" ```
	return s
      elseif line =~ '^\s*\.'
	" ```
	" :: forall a<CR>. a -> a
	" ```
	" i.e. breaking an existing line
	return s + 1
      elseif line !~ '^\s*\%(::\|=>\|->\)' && line !~ '^\s*$' && pline !~ '\.\s*$'
	" first line after multiline type signature
	return s - &l:shiftwidth
      else
	return s
      endif
    elseif r >= 0
      " ```
      " :: forall a
      "  . a
      " ->
      " ```
      return r - r % 2
    endif
  endif

  let s = match(pline, '\<case\>')
  if s >= 0 && !s:isCommentOrString(v:lnum - 1, s)
    if pline =~ '^\s*let\>'
      " TODO: find let block
      return indent(v:lnum - 1) + 2 * &l:shiftwidth
    else
      return indent(v:lnum - 1) + &l:shiftwidth
    endif
  endif

  let s = match(pline, '\<let\>\s\+\zs\S')
  if s >= 0 && !s:isCommentOrString(v:lnum - 1, s)
    return s
  endif

  let s = match(pline, '\<let\>\s*$')
  if s >= 0 && !s:isCommentOrString(v:lnum - 1, s)
    return s + g:haskell_indent_let
  endif

  let s = match(pline, '\<let\>\s\+.\+\(\<in\>\)\?\s*$')
  if s >= 0 && !s:isCommentOrString(v:lnum - 1, s)
    return match(pline, '\<let\>') + g:haskell_indent_let
  endif

  let s = searchpairpos('\%(--.\{-}\)\@<!\<if\>', '\<then\>', '\<else\>.*\zs$', 'bnrc')[0]
  if s > 0
    " this rule ensures that using `=` in visual mode will correctly indent
    " `if then else`, but it does not handle lines after `then` and `else`
    if line =~ '\<\%(then\|else\)\>'
      return match(getline(s), '\<if\>') + &l:shiftwidth
    endif
  endif

  let p = match(pline, '\<if\>\%(.\{-}\<then\>.\{-}\<else\>\)\@!')
  if p > 0
    return p + &l:shiftwidth
  endif

  let s = match(pline, '[)\][:alpha:][:space:]]\zs=\s*$')
  if s >= 0 && !s:isCommentOrString(v:lnum - 1, s)
    " ```
    " fold f as b =
    " >>>>
    " ```
    return match(pline, '\S') + &l:shiftwidth
  endif

  let s = match(pline, '^\s*\zsclass\>')
  if s >= 0
    return s + &l:shiftwidth
  endif

  let s = match(pline, '^\s*\zsmodule.\{-}\%(\<where\s*\)\@<!$')
  if s >= 0
    return s + &l:shiftwidth
  endif

  let s = match(pline, '\<where\>\s*$')
  if s >= 0 && !s:isCommentOrString(v:lnum - 1, s)
    if pline =~ '^\s*module\>'
      return 0
    else
      " module Main (...) where
      " class Eq ... where
      " instance Eq ... where
      " data (GADTs)
      let n = v:lnum - 1
      let l = getline(n)
      while n >= 0 &&  l !~ '^\s*$'
	let s = match(l, '\C^module\>')
	if s >= 0
	  return s
	endif
	let s = match(l, '\C^\s*\zs\%(class\|instance\|data\)\>')
	if s >= 0
	  return s + &l:shiftwidth
	endif
	let n -= 1
	let l = getline(n)
      endwhile
    endif
    return match(pline, '\S') + g:haskell_indent_where
  endif

  let s = match(pline, '\C^\s*\zsinstance\>')
  if s >= 0
    return s + &l:shiftwidth
  endif

  if line =~ '^\s*where\>'
    let n = v:lnum - 1
    let s = match(l, '[^=]\zs=\%([^=]\|$\)')
    while n > 0 && s == -1
      let n -= 1
      let l = getline(n)
      let s = match(l, '[^=]\zs=\%([^=]\|$\)')
      if s >= 0
	return indent(n + 1)
      endif
      let s = match(l, '\C^\s*\zs\%(class\|instance\|data\|module\)\>')
      if s >= 0
	return s + &l:shiftwidth
      endif
      if l =~ '^\s*\%(type\|newtype\|deriving\)\>'
	break
      endif
    endwhile
  endif

  let s = match(pline, '\<where\>\s\+\zs\S\+.*$')
  if s >= 0 && !s:isCommentOrString(v:lnum - 1, s)
    return s
  endif

  let s = match(pline, '\<do\>\s*$')
  if s >= 0 && !s:isCommentOrString(v:lnum - 1, s)
    return match(pline, '\S') + &l:shiftwidth
  endif

  let s = match(pline, '\<do\>\s\+\zs\S\+.*$')
  if s >= 0 && !s:isCommentOrString(v:lnum - 1, s)
    return s
  endif

  let s = match(pline, '^\s*data\>.\{-}\zs=')
  if s >= 0 && nline =~ '^\s*|'
    return s
  endif

  let s = match(pline, '^\s*\zs\%(newtype\|data\)\s\+[^=]\+\s*=\s*\S.*$')
  if s >= 0
    return s
  endif

  let s = match(pline, '^\s*\zs\%(newtype\|data\)\>[^=]*$')
  if s >= 0
    return s + &l:shiftwidth
  endif

  let s = match(pline, '^\s*[}\]]')
  if s >= 0 && !s:isCommentOrString(v:lnum - 1, s)
    return match(pline, '\S') - &l:shiftwidth
  endif

  let n = v:lnum - 1
  let l = pline
  let s = searchpair('(', '', ')', 'bn')
  if s == 0
    while n > 0 && l !~ '^\s*$'
      let s = match(l, '^\s*import\>')
      if s >= 0
	return s
      endif
      let n -= 1
      let l = getline(n)
    endwhile
  endif

  let s = match(line, '^\s*import\>')
  if s >= 0
    return 0
  endif

  return match(pline, '\S')
endfun
