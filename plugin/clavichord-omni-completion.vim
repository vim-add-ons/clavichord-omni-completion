"
" « Clavichord » Vim Omni Completion For The VimL Language.
" Copyright (c) 2020 Sebastian Gniazdowski.
" License: Gnu GPL v3.
" 
" If using an autocomplete plugin and undecided which one to use, then the
" plugin zphere-zsh/shell-autopop-menu is recommended. It'll integrate nicely
" with this plugin as it is specifically tested and adapted to work with it.
" It started as a (fork-based) companion plugin for the Shell omni completion
" (zphere-zsh/shell-omni-completion), however it works perfectly also with this
" completion for VimL.
" 
" This plugin does no more than setting `omnifunc` (or `completefunc` — see
" below) set variable, which allows to invoke the completion via Ctrl-X Ctrl-O
" (or Ctrl-X Ctrl-U).
" 
" Configuration Variables:
" ------------------------
"
" — g:vichord_use_cfu_setting — whether to use completefunc (^X^U) instead of
"   omnifunc (^X^O) setting (default: 0), example:
"       let g:vichord_use_cfu_setting = 1
"
" — g:vichord_search_in_let — whether to search for variables in "let …"
"   statements (slows down a bit, however it allows to complete local variables
"   even when they're used without the l:… prefix) (default: 0), example:
"       let g:vichord_search_in_let = 1
"

" FUNCTION: VimOmniComplBufInit()
" A function that's called when the buffer is loaded and its filetype is known.
" It initializes the omni completion for the buffer.
function VimOmniComplBufInit()
    let b:vichord_call_count = 0
    let b:vichord_cache_lines_active = 0
    let b:vichord_last_completed_line = ''
    let [ b:vichord_last_fccount, b:vichord_last_pccount, 
                \ b:vichord_last_kccount, b:vichord_last_lccount ] = [ [-1], [-1], [-1], [-1] ]
    let b:vichord_last_ccount_vars = [ b:vichord_last_fccount, b:vichord_last_pccount, 
                \ b:vichord_last_kccount, b:vichord_last_lccount ]
    let g:vichord_search_in_let = get(g:,"vichord_search_in_let", 0)
    if &ft == 'vim'
        if ! get(g:,'vichord_use_cfu_setting','0')
            setlocal omnifunc=VimComplete
        else
            setlocal completefunc=VimComplete
        endif
    endif
endfunction

" FUNCTION: VimComplete()
" The main function of this plugin (assigned to the `omnifunc` set option) that
" has the main task to perform the omni-completion, i.e.: to return the list of
" matches to the text before the cursor.
function VimComplete(findstart, base)
    "echom "::: COMPLETE ::: findstart:" . a:findstart . ", line: " . getbufline(bufnr(), line("."))[0] .", base: " . a:base
    " Prepare the buffers' contents for processing, if needed (i.e.: on every
    " N-th call, when only also the processing-sequence is being initiated).
    "
    " The update of this buffer-lines cache is synchronized with the other
    " processings, i.e.: it depends on the local (to the buffer) call count
    " of the plugin, however the storage variable is s: -session, to limit the
    " memory usage.
    if b:vichord_call_count % 5 == 0
        let s:vichord_all_buffers_lines = []
        for bufnum in range(last_buffer_nr()+1)
            if buflisted(bufnum)
                let s:vichord_all_buffers_lines += getbufline(bufnum, 1,"$")
            endif
        endfor
    endif

    if a:findstart
        let got_winner = 0
        call CompleteVimFunctions(1, a:base)
        if b:vichord_compl_functions_start >= 0
            let result_compl = CompleteVimFunctions(0, a:base)
            "echom 'FUNCTIONS 1/len(result_compl)='.len(result_compl)
            if len(result_compl)
                let result = b:vichord_compl_functions_start
                let winner = 0
                let got_winner = 1
                let to_declare = [ "vichord_compl_parameters_start",
                            \ "vichord_compl_arrays_keys_start",
                            \ "vichord_compl_lines_start" ]
                for var in to_declare | let b:[var] = -3 | endfor
            endif
        endif
        if ! got_winner
            call VimCompleteLines(1, a:base)
            if ( b:vichord_compl_lines_start >= 0 )
                let result_compl = VimCompleteLines(0, a:base)
                "echom 'LINES 2/len(result_compl)='.len(result_compl)
                if len(result_compl)
                    let result = b:vichord_compl_lines_start
                    let winner = 3
                    let got_winner = 1
                    let to_declare = [ "vichord_compl_functions_start",
                                \ "vichord_compl_parameters_start",
                                \ "vichord_compl_arrays_keys_start" ]
                    for var in to_declare | let b:[var] = -3 | endfor
                endif
            endif
        endif
        if ! got_winner 
            let four_results = [ b:vichord_compl_functions_start,
                        \ CompleteVimParameters(1, a:base),
                        \ CompleteVimArrayAndHashKeys(1, a:base),
                        \ VimCompleteLines(1, a:base) ]
            let result = max( four_results )
            let winner = index( four_results, result )
        endif
        " Restart the cyclic renewal of the database variables from the point
        " where the specific object-kind completion finished the cycle in the
        " previous call to VimComplete.
        let b:vichord_call_count = (b:vichord_last_ccount_vars[winner])[0] + 1
        "echom "Returning [a:findstart==1 VimComplete call]: " . string(result)
    else
        let result = []

        let four_results = [ b:vichord_compl_functions_start,
                    \ b:vichord_compl_parameters_start,
                    \ b:vichord_compl_arrays_keys_start,
                    \ b:vichord_compl_lines_start ]

        for id in range(4)
            if four_results[id] < 0 | continue | endif
            let b:vichord_last_ccount_vars[id][0] = b:vichord_call_count
            let result_im = s:completerFunctions[id](0, a:base)
            if id == g:VCHRD_LINE && len(result_im)
                let result = result_im
                break
            endif
            let result += result_im
        endfor
        call uniq(sort(result))
    endif
    return result
endfunction

" FUNCTION: CompleteVimFunctions()
" The function is a complete-function which returns matching Vim-function names.
function CompleteVimFunctions(findstart, base)
    let [line_bits,line] = s:getPrecedingBits(a:findstart)
    " First call — basically return 0. Additionally (it's unused value),
    " remember the current column.
    if a:findstart
        let line_bits_ne = Filtered(function('len'), line_bits)
        "echom "::: FUNS ::: " . string(line_bits) . string(line_bits_ne)
        if len(line_bits_ne)
            let idx = strridx( line, len(line_bits_ne) >= 2 ? line_bits_ne[-2] : line_bits_ne[-1] )
        else
            let idx = 0
        endif
        "echom idx . "← idx"
        if line_bits[-1] !~ '\v\k{1,}$'
            "echom "-3 ← first"
            let b:vichord_compl_functions_start = -3
        elseif len(line_bits_ne) >= 2 && line[idx:] !~ '\v^[[:space:]]*(\||if|elseif|call)[[:space:]]+\k{1,}$'
            "echom "-3 ← second"
            let b:vichord_compl_functions_start = -3
        else
            let b:vichord_compl_functions_start = strridx(line, line_bits[-1])
            " Support the from-void text completing. It's however disabled on
            " the upper level.
            let b:vichord_compl_functions_start += line_bits[-1] =~ '^[[:space:]]$' ? 1 : 0
        endif
        "echom 'b:vichord_compl_functions_start:' . b:vichord_compl_functions_start
        return b:vichord_compl_functions_start
    else
        " Detect the matching Vim function names and return them.
        return s:completeKeywords(g:VCHRD_FUNC, line_bits, line)
    endif
endfunction

" FUNCTION: CompleteVimParameters()
" The function is a complete-function which returns matching Vim-parameter names.
function CompleteVimParameters(findstart, base)
    let [line_bits,line] = s:getPrecedingBits(a:findstart)

    " First call — basically return 0. Additionally (it's unused value),
    " remember the current column.
    if a:findstart
        if line_bits[-1] !~ '\v^%([slgba]:|)[a-zA-Z0-9_]*$'
            let b:vichord_compl_parameters_start = -3
        else
            let b:vichord_compl_parameters_start = strridx(line, line_bits[-1])
            " First try to orientate on the colon…
            let idx = stridx(line[b:vichord_compl_parameters_start:],':') - 1
            " …if it fails, then try to on the alpha-numerics and underscore.
            let idx = idx < 0 ? match(line[b:vichord_compl_parameters_start:],'[a-zA-Z_]') : idx
            let b:vichord_compl_parameters_start = b:vichord_compl_parameters_start + (idx < 0 ? 0 : idx)
            " Support the from-void text completing. It's however disabled on
            " the upper level (above).
            let b:vichord_compl_parameters_start += line_bits[-1] =~ '^[[:space:]]$' ? 1 : 0
        endif
        "echom 'b:vichord_compl_parameters_start:' . b:vichord_compl_parameters_start
        return b:vichord_compl_parameters_start
    else
        " Detect the matching Vim parameter names and return them.
        return s:completeKeywords(g:VCHRD_PARAM, line_bits, line)
    endif
endfunction

" FUNCTION: CompleteVimArrayAndHashKeys()
" The function is a complete-function which returns matching Vim-parameter names.
function CompleteVimArrayAndHashKeys(findstart, base)
    let [line_bits,line] = s:getPrecedingBits(a:findstart)

    " First call — basically return 0. Additionally (it's unused value),
    " remember the current column.
    if a:findstart
        if line_bits[-1] !~ '\v[a-zA-Z0-9_]+\['
            let b:vichord_compl_arrays_keys_start = -3
        else
            let b:vichord_compl_arrays_keys_start = strridx(line, line_bits[-1])
            let b:vichord_compl_arrays_keys_start = b:vichord_compl_arrays_keys_start + stridx(line[b:vichord_compl_arrays_keys_start:],'[') + 1
            " Support the from-void text completing. It's however disabled on
            " the upper level.
            let b:vichord_compl_arrays_keys_start += line_bits[-1] =~ '^[[:space:]]$' ? 1 : 0
        endif
        "echom 'b:vichord_compl_arrays_keys_start:' . b:vichord_compl_arrays_keys_start
        return b:vichord_compl_arrays_keys_start
    else
        " Detect the matching arrays' and hashes' keys and return them.
        return s:completeKeywords(g:VCHRD_KEY, line_bits, line)
    endif
endfunction

" FUNCTION: VimCompleteLines()
" The function is a complete-function which returns matching lines.
function VimCompleteLines(findstart, base)
    let [line_bits,line] = s:getPrecedingBits(a:findstart)

    " First call — basically return 0. Additionally (it's unused value),
    " remember the current column.
    if a:findstart
        " Remember the entry cache-state to verify its change later.
        let enter_cstate = b:vichord_cache_lines_active
        " Line was enriched, extended? Thus, it cannot yield any NEW ↔ DIFFERENT
        " results?
        if len(line) >= len(b:vichord_last_completed_line) && !empty(b:vichord_last_completed_line)
            " Disable the cache invalidation IF a fresh cache has been computed.
            " — 2 — got a fresh cache, invalidation stopped,
            " — -1 — request a fresh cache recomputation before the stop.
            let b:vichord_cache_lines_active = b:vichord_cache_lines_active == 2 ? 2 : -1
        else
            let b:vichord_cache_lines_active = 0
        endif
        "echom (len(line) >= len(b:vichord_last_completed_line) ? "NO, not withdrawed >= (new is longer / same)" : "YES, withdrawed < (new is shorter)") . " →→ " . line . ' ↔ ' . b:vichord_last_completed_line 
        "echom "b:VICHORD_CACHE_LINES_ACTIVE ←← " . b:vichord_cache_lines_active
        let b:vichord_last_completed_line = line
        " A short-path (also a logic- short-path ↔ see the first completer
        " function call) for the locked-in-cache state.
        if b:vichord_cache_lines_active == 2 &&
                    \ enter_cstate == 2 &&
                    \ empty( matchstr( b:vichord_lines_cache, '\v^'.ZshQuoteRegex(line).'.*' ) )
            "echom 'SHORT-PATH (2==2) … →→ 1…2: →→ ' . string(b:vichord_lines_cache[0:1]) . '→→' . matchstr( b:vichord_lines_cache, '\v^'.ZshQuoteRegex(line).'.*' )
            let b:vichord_short_path_taken = 1
            let b:vichord_compl_lines_start = (len(b:vichord_lines_cache) == 0 || !pumvisible())
                        \ ? -3 : b:vichord_compl_lines_start
            "echom '1/b:vichord_compl_lines_start:' . b:vichord_compl_lines_start
            return b:vichord_compl_lines_start
        else
            let b:vichord_short_path_taken = 0
        endif
        if line =~ '\v^[[:space:]]*$'
            "echom "returning -3 here… " . string(line) . '/' b:vichord_last_completed_line
            let b:vichord_compl_lines_start = -3
        else
            let line_bits_ne = Filtered(function('len'), line_bits)
            let idx = stridx(line,line_bits_ne[0])
            let b:vichord_compl_lines_start = idx <= 0 ? 0 : idx
        endif
        "echom '2/b:vichord_compl_lines_start:' . b:vichord_compl_lines_start
        return b:vichord_compl_lines_start
    else
        " Detect the matching arrays' and hashes' keys and return them.
        if b:vichord_cache_lines_active > 0
            "echom 'FROM CACHE [' . b:vichord_cache_lines_active . '], 1…2: → ' . string(b:vichord_lines_cache[0:1])
            let b:vichord_cache_lines_active = b:vichord_cache_lines_active == 2 ? 2 : 0
            if b:vichord_short_path_taken || !pumvisible()
                "echom 'RETURNING FILTERED: ' . string(Filtered2(function('DoesLineMatch'), b:vichord_lines_cache, line)[0:1])
                return Filtered2 ( function('DoesLineMatch'), b:vichord_lines_cache, line )
            else
                return b:vichord_lines_cache
            endif
        else
            " helper var
            let enter_cstate = b:vichord_cache_lines_active
            let b:vichord_cache_lines_active = b:vichord_cache_lines_active == -1 ? 2 : 1
            let b:vichord_lines_cache = s:completeKeywords(g:VCHRD_LINE, line_bits, line)
            "echom 'FROM COMPUTATION [' . enter_cstate . '], 1…2: → ' . string(b:vichord_lines_cache[0:1])
            return b:vichord_lines_cache
        endif
    endif
endfunction

"""""""""""""""""" PRIVATE FUNCTIONS

" FUNCTION: s:completeKeywords()
" A general-purpose, variadic backend function, which obtains the request on the
" type of the keywords (functions, parameters or array keys) to complete and
" performs the operation.
function s:completeKeywords(id, line_bits, line)
    " Retrieve the complete list of Vim functions in the buffer on every
    " N-th call.
    if (b:vichord_call_count == 0) || ((b:vichord_call_count - a:id + 2) % 10 == 0)
        "echom 'CALL: ' . b:vichord_call_count . ' - ' . a:id . ' + 2 % 10 == ' . ((b:vichord_call_count - a:id + 2) % 10)
        call s:gatherFunctions[a:id]()
    endif

    " Ensure that the buffer-variables exist
    let to_declare = filter([ "vichord_functions", "vichord_parameters", "vichord_array_and_hash_keys" ], '!exists("b:".v:val)')
    for bufvar in to_declare | let b:[bufvar] = [] | endfor
    let gatherVariables = [ b:vichord_functions, b:vichord_parameters, 
                \ b:vichord_array_and_hash_keys, s:vichord_all_buffers_lines ]

    " Detect the matching Vim-object names and store them for returning.
    let result = []
    let a:line_bits[-1] = a:line_bits[-1] =~ '^[[:space:]]$' ? '' : a:line_bits[-1]

    "echom "--ckeywords-- →→" . a:id . g:VCHRD_PARAM . ' / '. a:line_bits[-1]
    if a:id == g:VCHRD_PARAM && a:line_bits[-1] =~ '\v^\$.*'
        let a:line_bits[-1] = (a:line_bits[-1])[1:]
        let pfx='$'
    elseif a:id == g:VCHRD_KEY && a:line_bits[-1] =~ '\v^[^\[]+\['
        let a:line_bits[-1] = substitute( a:line_bits[-1], '\v^[^\[]+\[', '', '' )
        let pfx=''
    elseif a:id == g:VCHRD_LINE
        let a:line_bits[-1] = substitute(a:line,'\v^[[:space:]]*(.*)$', '\1', '')
    else
        let pfx=''
    endif
    "echom 'After: '.a:id.' / '.string(a:line_bits)

    let l:count = 0
    for the_key in gatherVariables[a:id]
        let l:count += 1
        if a:id == g:VCHRD_LINE
            let the_key = substitute(the_key,'\v^[[:space:]]*(.*)$', '\1', '')
            if the_key =~# '\v^' . VimQuoteRegex(a:line_bits[-1]). '.*'
                if the_key != a:line_bits[-1]
                    call add(result, the_key)
                endif
            endif
            if l:count >= 250
                break
            endif
        else
            if the_key =~# '\v^' . VimQuoteRegex(a:line_bits[-1]). '.*'
                call add(result, pfx.the_key)
            endif
        endif
    endfor

    return result
endfunction

" FUNCTION: s:gatherFunctionNames()
" Buffer-contents processor for Vim *function* names. Stores all the detected
" Vim function names in the list b:vichord_parameters.
function s:gatherFunctionNames()
    " Prepare, i.e.: zero the buffer collection-variable.
    let b:vichord_functions = []

    " Iterate over the lines in the buffer searching for a function name.
    for line in s:vichord_all_buffers_lines
        let mres = matchlist(line, '\v^[[:space:]]*fu%(n%(c%(t%(i%(o%(n|)|)|)|)|)|)[[:space:]]*\!=[[:space:]]+([^[:space:]]+)[[:space:]]*\(')
        if !empty(mres)
            call add(b:vichord_functions, mres[1])
        endif
    endfor

    " Uniqify the resulting list of Vim function names. The uniquification
    " requires also sorting the input list.
    call uniq(sort(b:vichord_functions))
endfunction

" FUNCTION: s:gatherParameterNames()
" Buffer-contents processor for Vim *parameter* names. Stores all the detected
" Vim parameter names in the list b:vichord_parameters.
function s:gatherParameterNames()
    " Prepare, i.e.: zero the buffer collection-variable.
    let b:vichord_parameters = []

    " Iterate over the lines in the buffer searching for a Vim parameter name.
    for line in s:vichord_all_buffers_lines
        let idx = match(line, '\v[slgba]:[a-zA-Z0-9_]+')
        while idx >= 0
            let res_list = matchlist(line, '\v[slgba]:[a-zA-Z0-9_]+', idx)
            call add(b:vichord_parameters, res_list[0])
            let idx = match(line, '\v[slgba]:[a-zA-Z0-9_]+', idx+len(res_list[0])+2)
        endwhile
        " Try to optimize as much as possible…
        if g:vichord_search_in_let && match(line, '\v^[[:space:]]*let') >= 0
            let mres = matchlist(line, '\vlet[[:space:]]+%([slgba]:)@<!([a-zA-Z0-9_]+)')
            if !empty(mres)
                call add(b:vichord_parameters, mres[1])
            endif
        endif
    endfor

    " Uniqify the resulting list of Vim parameter names. The uniquification
    " requires also sorting the input list.
    call uniq(sort(b:vichord_parameters))
endfunction

" FUNCTION: s:gatherArrayAndHashKeys()
" Buffer-contents processor for Vim *parameter* names. Stores all the detected
" Vim parameter names in the list b:vichord_parameters.
function s:gatherArrayAndHashKeys()
    " Prepare, i.e.: zero the buffer collection-variable.
    let b:vichord_array_and_hash_keys = []

    " Iterate over the lines in the buffer searching for a Vim parameter name.
    for line in s:vichord_all_buffers_lines
        let idx = match(line, '\v([slgba]:|)[a-zA-Z0-9_]+\[[^\]]+\]')
        while idx >= 0
            let res_list = matchlist(line, '\v%([slgba]:|)[a-zA-Z0-9_]+\[([^\]]+)\]', idx)
            
            call add(b:vichord_array_and_hash_keys, substitute(res_list[1],":$","",""))
            " Support the List and string slices: [a:b].
            let res_list2 = matchlist(res_list[1], '\v(%(.{-1,}[^slgba]|[^sglba])):(.+)')
            if len( res_list2 ) > 0
                call add(b:vichord_array_and_hash_keys, res_list2[1])
                call add(b:vichord_array_and_hash_keys, res_list2[2])
            endif
            let idx = match(line, '\v%([slgba]:|)[a-zA-Z0-9_]+\[[^\]]+\]', idx+len(res_list[1])+2)
        endwhile
    endfor

    " Uniqify the resulting list of Vim parameter names. The uniquification
    " requires also sorting the input list.
    call uniq(sort(b:vichord_array_and_hash_keys))
endfunction

" FUNCTION: s:gatherLines()
" Buffer-contents processor for Vim *parameter* names. Stores all the detected
" Vim parameter names in the list b:vichord_parameters.
function s:gatherLines()
endfunction

" FUNCTION: VimQuoteRegex()
" A function which quotes the regex-special characters with a backslash, which
" makes them inactive, literal characters in the very-magic mode (… =~ " '\v…').
function VimQuoteRegex(str)
    return substitute( substitute( a:str, '\v\','\\\\', "g" ), '\v[^0-9A-Za-z_]','\\&',"g" )
endfunction

" The idea of this completion plugin is the following:
" - SomeTextSomeText SomeOtherText
"   ……………………↑ <the cursor>.
" What will be completed, will be:
" - the matching keywords (functions, parameters, etc.) that match:
"   SomeTextSomeText,
" - so the completion takes the whole part in which the cursor currently is
"   being located, not only the preceding part.
function s:getPrecedingBits(findstart)
    if a:findstart
        let line = getbufline(bufnr(), line("."))[0]
        let b:vichord_curline = line
        let curs_col = col(".")
        let b:vichord_cursor_col = curs_col
    else
        let line = b:vichord_curline
        let curs_col = b:vichord_cursor_col
    endif

    let line_bits = split(line,'\v[[:space:]\{\}\(\)\#\%\=\^\!\*\"'."\\'".']')
    let line_bits = len(line_bits) >= 1 ? line_bits : [len(line) > 0 ? (line)[len(line)-1] : ""]

    if len(line_bits) > 1
        " Locate the *active*, *hot* bit in which the cursor is being placed.
        let l:count = len(line_bits)
        let work_line = line
        for bit in reverse(copy(line_bits))
            let idx = strridx(work_line, bit)
            if idx <= curs_col - 2
                " Return a sublist with the preceding elements up to the active,
                " *hot* bit.
                return [line_bits[0:l:count], line]
            endif
            let work_line = work_line[0:idx-1]
            let l:count -= 1
        endfor
    endif
    return [line_bits, line]
endfunction

"""""""""""""""""" THE SCRIPT BODY

let s:gatherFunctions = [ function("s:gatherFunctionNames"),
            \ function("s:gatherParameterNames"),
            \ function("s:gatherArrayAndHashKeys"),
            \ function("s:gatherLines") ]

let s:completerFunctions = [ function("CompleteVimFunctions"),
            \ function("CompleteVimParameters"),
            \ function("CompleteVimArrayAndHashKeys"),
            \ function("VimCompleteLines") ]

augroup VimOmniComplInitGroup
    au FileType * call VimOmniComplBufInit()
augroup END

let [ g:VCHRD_FUNC, g:VCHRD_PARAM, g:VCHRD_KEY, g:VCHRD_LINE ] = [ 0, 1, 2, 3 ]

let g:vichord_omni_completion_loaded = 1
"""""""""""""""""" UTILITY FUNCTIONS

function! Mapped(fn, l)
    let new_list = deepcopy(a:l)
    call map(new_list, string(a:fn) . '(v:val)')
    return new_list
endfunction

function! Filtered(fn, l)
    let new_list = deepcopy(a:l)
    call filter(new_list, string(a:fn) . '(v:val)')
    return new_list
endfunction

function! Filtered2(fn, l, arg)
    let new_list = deepcopy(a:l)
    call filter(new_list, string(a:fn) . '(v:val, "' . a:arg . '")')
    return new_list
endfunction

function! DoesLineMatch(match, line)
    return a:match =~# '\v^' . VimQuoteRegex(a:line) . '.*'
endfunction

function! FilteredNot(fn, l)
    let new_list = deepcopy(a:l)
    call filter(new_list, '!'.string(a:fn) . '(v:val)')
    return new_list
endfunction

function! CreateEmptyList(name)
    eval("let ".a:name." = []")
endfunction

" vim:set ft=vim tw=80 et sw=4 sts=4 foldmethod=syntax:
