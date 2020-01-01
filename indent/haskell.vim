" Haskell indentation script
" Author: Marcin Szamotulski (profunctor@pm.me)
" Maintainer: Marcin Szamotulski (profunctor@pm.me)

setl indentexpr=GetHaskellIndent()
let &indentkeys="!^F,o,O,},=where,=in\ ,=deriving,=::,==\ ,=->,==>,=|,={"

" TODO:
"
" Indent type signature
" ```
" fun
"   ::
" ```
" it should use `g:haskell_indent_min`
" The same for:
" ```
" Pattern StrictJust a <- Just !a where
"   StrictJust !a = Just a
" ```

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

if !exists('g:haskell_indent_let')
  " ```
  " let x = 0
  " >>>>x
  " ```
  let g:haskell_indent_let = &l:sw
endif

if !exists('g:haskell_indent_in')
  " ```
  " let x = 0
  " in
  " ```
  let g:haskell_indent_in = 0
endif

if !exists('g:haskell_indent_where')
  " TODO: it's just a boolean: if negative then it moves where that much to
  " the left.
  " ```
  " where
  " >>>>f :: Int -> Int
  " >>>>f x = x
  " ```
  let g:haskell_indent_where = 4
endif

if !exists('g:haskell_indent_min')
  " Minimum indentation level for where
  " this also applies to instances and classes
  let g:haskell_indent_min = 4
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

fun! InGADTClause(stop_line)
  return search('^\s*data\>.*\<where\>', 'bnW', a:stop_line) != 0
endfun

fun! InWhereClause(stop_line)
  return search('^\s*where\>', 'bnW', a:stop_line) != 0
endfun

fun! GetLineIdent(line)
  return match(line, '^\s*\zs\i\+')
endfun

fun! GetHaskellIndent()
  let col	= v:col
  let lnum	= v:lnum
  let col_      = getcurpos()[2]
  let line	= getline(v:lnum)
  let pline	= getline(v:lnum - 1)
  let ppline	= getline(v:lnum - 2)
  let nline     = getline(v:lnum + 1)

  " previous non-empty line
  let nelnum    = v:lnum - 1
  while getline(nelnum) =~ '^\s*$'
    let nelnum -= 1
  endwhile
  let neline    = getline(nelnum)


  let stop_line = search('^\S', 'bnW')
  let inGADT    = InGADTClause(stop_line)
  let inWhere   = InWhereClause(stop_line)

  let s = match(line, '\<in\s')
  if s > 0 && !s:isCommentOrString(v:lnum, s)
    let [l, c] = searchpos('\<let\>', 'bnzW')
    return c - 1
  endif

  " Short cut to not reindent current line
  " Otherwise this might happend if the previous line has a non closed
  " bracket.
  if line =~ '^\(instance\|class\|deriving\)'
    return 0
  endif

  let s = match(pline, '\v^\s*\zs--')
  if s >= 0
    return s
  endif

  if line =~ '^\s*\%(::\|=\%($\|\s\)\)'
    return indent(v:lnum - 1) + &l:sw
  endif

  if line[:col_ - 2] =~ '::.*::$'
    return indent(v:lnum)
  endif

  let s = match(pline, '::\s*\zsforall\>.*\.\s*$')
  if s >= 0 && search('^\s*=>', 'nW', (search('^\S', 'nW') || v:lnum + 21) - 1)
    " ```
    " f :: forall a .
    "      Show a
    "   =>
    " ```
    return s
  endif

  let s = match(pline, '\v%(^\s*\i*\s*::\s*)@<=\([^)]*$')
  if s >= 0
    " ```
    " f :: ( Eq a
    "      ,
    " ```
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
    while n >= 0 && l !~ '^\s*\i*\s*::' && l !~ '^\s*$'
      let n -= 1
      let l = getline(n)
    endwhile
    let s = match(getline(n), '::')
    if s >= 0
      return s
    endif
  endif

  let u = match(pline,  '\v%(^\s*\i*\s*)@<=::')
  let r = strpart(pline, u)
  let t = r  =~ '[-=]>$'
  if !inGADT && (u >= 0) && (r !~ '[-=]>' || t) && line !~ '=' && line !~ '}' && searchpair('{', '', '}', 'bnW') == 0
    if t
      " ```
      " f :: String ->
      "	     String
      " ```
      return match(pline, '\v^\s*\i+\s*::\s*\zs')
    else
      " ```
      " f :: String
      "   -> String
      " ```
      return u
    endif
  endif

  " ```
  " f :: a
  "   -> b
  " f -- indent the line after type signature
  " ```
  let u = match(line, '^\s*\zs\i\+')
  if !inGADT && pline =~ '^\s*[-=]>' && line =~ '^\s*\i\+'
    let l = search('^\s*\i*\s*::', 'bnW')
    if getline(l) =~ '^\s*::'
      return indent(l - 1)
    else
      return indent(l)
    endif
  endif

  let s = match(line, '^\s*=>')
  if s >= 0
    let l = search('\(::\|\<instance\>\)', 'bnW', line(".") - 10)
    if l >= 0
      let line = getline(l)
      if line =~ '\<instance\>'
	return max([&l:sw, g:haskell_indent_min])
      else
	return match(getline(l), '::')
      endif
    else
      return indent(lnum - 1)
    endif
  endif

  if line =~ '^\s*deriving\>'
    let n = v:lnum - 1
    let l = pline
    while n > 0 && l !~ '^\s*$'
      let s = match(l, '^\s*\zs\%(data\|newtype\)\>')
      if s >= 0
	return s + &l:sw
      endif
      let n -= 1
      let l = getline(n)
    endwhile
    return 0
  endif

  let s = searchpairpos('\%(--.\{-}\)\@<!\<if\>', '\<then\>', '\<else\>.*\zs$', 'bnrcW')[0]
  if s > 0
    " this rule ensures that using `=` in visual mode will correctly indent
    " `if then else`, but it does not handle lines after `then` and `else`
    if line =~ '\<\%(then\|else\)\>'
      return indent(s) + &l:sw
    endif
  endif

  let p = match(pline, '\<if\>\%(.\{-}\<then\>.\{-}\<else\>\)\@!')
  if p > 0
    return p + &l:sw
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
    elseif pline =~ '^\s*import' && pline =~ ',\s*$'
      return match(pline, '^\s*import\s\+\zs\S') + &l:sw
    elseif (r == '(' || r == '[') && pline =~ ',\s*$'
      return s + 1 + match(q, '\s*\zs\S')
    elseif (r == '(' || r == '[')
      return s
    else
      if pline =~ '{\s*$'
	" Data {
	" --x
	return indent(v:lnum -1) + 2 * &l:sw
      else
	" Data {
	" -----x
	return s
      endif
    endif
  endif

  " Do not change indentation on lines where NamedFieldPuns are used.
  if lastPairCurly == 0 && line[col_ - 2] == '}'
    return indent(v:lnum)
  endif

  " Find previous line with a smaller indentation if line ends with `)` or `]`
  " which does not close in the current line
  "
  " This pattern will not match `{ }` or `{ } { }` but it will match on
  " `{ { } }` (where it should not).
  " ( )
  let matched = v:false
  if pline =~ '[)\]}]\s*$'
    call cursor(v:lnum - 1, len(pline))
    let matched = searchpair('[({\[]', '', '[)}\]', 'bnW', '', v:lnum)
    call cursor(v:lnum, v:col)
  endif

  if matched
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

  " Indent record fields
  if line =~ '^\s*{' && pline =~ '[^=]=\s*\i'
    "  previous line ` = TypeConstructor ...
    return indent(v:lnum - 1) + &l:sw
  endif

  let u = match(ppline, '\v%(^\s*\i*\s*)@<=::')
  if u >= 0 && line =~ '^\s*[=-]>' && indent(v:lnum - 2) < indent(v:lnum - 1)
      " ```
      " f :: forall a b .
      "      a
      "   ->
      " ```
      return u
  endif

  " match `::` but not when it comes after `->` or `=>`
  let s = match(pline, '\([=-]>.*\)\@<!::')
  if s >= 0 && line =~ '^\s*[=-]>'
    " When opening a new line with `->`:
    " ```
    " fun :: String
    " >>>>->
    " ```
    return s
  endif

  let s = match(pline, '\\\@<!\<case\>\(.\{-}\<of\s*{\s*}[[:space:],]*$\)\@!')
  if s >= 0 && !s:isCommentOrString(v:lnum - 1, s)
    if pline =~ '^\s*let\>'
      " TODO: find let block
      return max([indent(v:lnum - 1) + 2 * &l:sw, g:haskell_indent_min])
    else
      return max([indent(v:lnum - 1) + &l:sw, g:haskell_indent_min])
    endif
  endif

  if pline =~ '\\case\>'
    return max([indent(v:lnum - 1) + &l:sw, g:haskell_indent_min])
  endif

  let s = match(pline, '\<let\>\s\+\zs\S')
  if s >= 0 && !s:isCommentOrString(v:lnum - 1, s)
    if line !~ '^\s*$' && indent(v:lnum) == indent(v:lnum - 1)
      " preserve indentation, this is useful in do blocks:
      " ```
      " do 
      "	  let a = 1
      "	  return a  -- avoid reindenint 
      " ```
      return indent(v:lnum)
    else
      return s
    endif
  endif

  let s = match(pline, '\<let\>\s*$')
  if s >= 0 && !s:isCommentOrString(v:lnum - 1, s)
    return s + g:haskell_indent_let
  endif

  let s = match(pline, '\<let\>\s\+.\+\(\<in\>\)\?\s*$')
  if s >= 0 && !s:isCommentOrString(v:lnum - 1, s)
    return match(pline, '\<let\>') + g:haskell_indent_let
  endif

  let s = match(pline, '[)\][:alpha:][:space:]]\zs=\s*$')
  if s >= 0 && !s:isCommentOrString(v:lnum - 1, s + 1)
    " ```
    " fold f as b =
    " >>>>
    " ```
    return max([match(pline, '\S') + &l:sw, g:haskell_indent_min])
  endif

  let s = match(pline, '^\s*\zsclass\>')
  if s >= 0
    return s + max([&l:sw, g:haskell_indent_min])
  endif

  let s = match(pline, '^\s*\zsmodule.\{-}\%(\<where\s*\)\@<!$')
  if s >= 0
    return s + &l:sw
  endif

  let s = match(pline, '^.*\S.*\<where\>\s*$')
  let g = s:isCommentOrString(v:lnum - 1, s + 1)
  if s >= 0 && !s:isCommentOrString(v:lnum - 1, s + 1)
    let s = match(pline, '\C^\s*\zs\%(class\|instance\|data\)\>')
    if pline =~ '^\s*module\>'
      return 0
    elseif s >= 0
      let s = match(pline, '^\s*\zsdata\>')
      return max([s + &l:sw, g:haskell_indent_min])
    endif
    return match(pline, '\S') + &l:sw
  endif

  let s = match(pline, '^\s*\zswhere\s*$')
  if s >= 0 && !s:isCommentOrString(v:lnum - 1, s + 1)
    if g:haskell_indent_where < 0
      return s + abs(g:haskell_indent_where) / 2
    else
      return s + g:haskell_indent_where
    endif
  endif

  let s = match(pline, '\C^\s*\zs\%(instance\|class\)\>')
  if s >= 0
    return max([s + &l:sw, g:haskell_indent_min])
  endif

  if line =~ '^\s*where\>\s*$' && g:haskell_indent_where < 0
    let i = abs(g:haskell_indent_where) / 2
    let s = match(pline, '\S')
    let pos = getpos(".")
    " we need to start search from the start of the line, otherwise we find
    " the current where, not the previous one
    normal |0
    let x = search('^\S', 'Wnb', search('\<where\>', 'Wnb'))
    call setpos(".", pos)
    if x > 0
      return max([i, s - i])
    else
      return s + i
    endif
  endif

  if line =~ '^\s*where\>'
    let n = v:lnum - 1
    let l = pline
    let s = match(l, '\%([^=]\zs=\%([^=]\|$\)\|<-\)')
    while n > 0 && s == -1
      let n -= 1
      let l = getline(n)
      " a line with `=`, but not one that sets fields of a product.
      let s = match(l, '[^=]\zs=\%([^=]\|$\)')
      if s >= 0 && l !~ '^\s*[\[{,]'
	return indent(n + 1)
      endif
      " let and <- in a do context
      let s = match(l , '<-')
      if (s >= 0 && l !~ '^\s*[\[{,]') || l =~ '^\s*let\>'
	return indent(n)
      endif
      let s = match(l, '\C^\s*\zs\%(class\|instance\|data\|module\)\>')
      if s >= 0
	return s + &l:sw
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
    if pline =~ '^\s*,'
      return match(pline, '\S') + max([&l:sw, 4])
    else
      return max([match(pline, '\S') + &l:sw, g:haskell_indent_min])
    endif
  endif

  let s = match(pline, '\<do\>\s\+\zs\S\+.*$')
  if s >= 0 && !s:isCommentOrString(v:lnum - 1, s)
    return s
  endif

  let s = match(neline, '\_s\zs|\_s')
  if s > 0
    if line =~ '^\s*\%($\||\)'
      " empty line or line that starts with `|`
      return s
    else
      " find first line with `|`
    endif
    let l = v:lnum - 1
    while l >= 0 && getline(l - 1) =~ '\_s|\_s' 
      let l -= 1
    endwhile
    return match(getline(l), '\s\zs|\_s')
  endif

  if line =~ '^\s*|'
    let s = match(neline, '->')
    if s >= 0
      return s
    endif
    " line which starts with `|`, but previous line has no `|` (matched by
    " previous group).
    let s = match(pline, '=')
    if s >= 0
      return s
    else
      return indent(v:lnum - 1) + &l:sw
    endif
  endif

  if pline =~ '[^-]->\s*$'
    return indent(v:lnum - 1) + &l:sw
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
    return max([s + &l:sw, g:haskell_indent_min])
  endif

  let s = match(pline, '^\s*[}\]]')
  if s >= 0 && !s:isCommentOrString(v:lnum - 1, s)
    return match(pline, '\S') - &l:sw
  endif

  " Two cases of
  " ```
  " zzzzz {
  "     uuu,
  "     vvv, <- next line, previous ends with ','
  "     www  <-
  "  } <- line of the closing paren
  " ```
  "
  " This rule is one of the last, so that indentation within a record puns
  " takes precedence, e.g.
  " ```
  " ... {
  "	x = \x -> do
  "	  return x,
  " ```
  let stopLine = search('^\S', 'bnW')
  let lastPairCurly = searchpair('{', '', '}', 'bnW', 'getline(".") =~ "{[^{}]*}"', stopLine)
  if pline !~ ',\s*$' && (lastPairCurly > 0 || line =~ '^[^{]*}\s*$')
    let lastComma = lastPairCurly > 0 ? search('\%(^\s*,\|,\s*$\)', 'bnW', lastPairCurly) : 0
    let lastDo = lastComma > 0 || lastPairCurly > 0 ? search('\<do\>', 'bnW', max([lastComma, lastPairCurly])) : 0
    " special treatment for do notation
    if lastDo > 0
      return indent(v:lnum - 1)
    " previous line staring with ','
    elseif pline =~ '^\s*,'
      return match(pline, ',')
    " previous line not ending with ','
    elseif lastPairCurly > 0 && getline(lastPairCurly) =~ '{\s*$'
      return indent(v:lnum - 1) - &l:sw
    else
      let s = match(pline, '[{,]')
      if s >= 0
	return s
      else
	if getline(lastPairCurly) =~ '^\s*{'
	  return indent(lastPairCurly - 1)
	else
	  return indent(lastPairCurly)
	endif
      endif
    endif
  endif

  if lastPairCurly
    " other lines (maybe this block should be moved down, so other things
    " take a precedence over this one)
    let s = search('{\s*$', 'bW')
    return indent((s > 0 ? s + 1 : v:lnum - 1))
  endif

  " Must be after the record puns rule (the above one), otherwise closing
  " bracket of newtype records will not be indented correctly.
  let s = match(pline, '^\s*\%(\f\+\)\?\s*\zs\%(::\|[=-]>\)')
  let r = match(pline, '^\s*\zs\.')
  if s >= 0 || (r >= 0 && ppline =~ '::')
    if s >= 0
      if v:false && line =~ '^\s*\l'
	" first line after a type signature
	" NOTE: this might be confused with a case expression (thus we test
	" against `'^\s*\l'` above, but it's not prefect...)
	let s = search('^\s*\%(::\|[=-]>\)\@<!\s*\w', 'bnW')
	return inWhere || inGADT ? indent(s) : indent(s) - &l:sw
      elseif pline =~ '\.\s*$'
	" ```
	" :: forall a .
	" ->
	" ```
	return s
      elseif pline =~ '^\s*->\s*$'
	" -> in a case expression
	return match(pline, '->\zs') + 1
      elseif pline =~ '->\s*$'
	" -> in a case expression
	return indent(v:lnum - 1) + &l:sw
      elseif line =~ '^\s*\.'
	" ```
	" :: forall a<CR>. a -> a
	" ```
	" i.e. breaking an existing line
	return s + 1
      else
	return indent(v:lnum - 1)
      endif
    elseif r >= 0
      " ```
      " :: forall a
      "  . a
      " ->
      " ```
      if ppline =~ '^\s*\%([=-]>\|::\)'
	return indent(v:lnum - 2)
      elseif ppline =~ '::'
	return match(ppline, '::')
      else
	return indent(v:lnum - 2)
      endif
    endif
  endif

  let n = v:lnum - 1
  let l = pline
  call cursor(v:lnum, 0)
  let ident = GetLineIdent(v:lnum)
  let s = searchpair('(', '', ')', 'bnW')
  if s == 0
    while n > 0 && l !~ '^\s*$'
      let s = match(l, '^\s*\zsimport\>')
      if s >= 0
	return s
      elseif GetLineIdent(l) == ident
	"   someFunction a b =
	"      do ...
	"   somFunction
	return s
      endif
      let n -= 1
      let l = getline(n)
    endwhile
  " elseif getline(s) =~ '^\s*import\>'
    " return match(getline(s), '^\s*\zsimport\>')
  endif

  let s = match(line, '^\s*import\>')
  if s >= 0
    return 0
  endif

  return match(pline, '\S')
endfun
