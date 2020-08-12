# ClaViChord Omni Completion for VimL

Cla**Vi**Chord is a plugin that implements a smart **VimL** omni completion. The
completion's characteristics are:

1. It'll detect all function-names declared inside `filetype=vim` buffers **and
   complete them only on the command-position**, i.e.: only after a **call …**
   or following a command-separator like **|** (`fun1` **|** `fun2`, etc.),
   a dot **.**, etc.

2. It'll detect all variable names, i.e.: only keywords following a **g:…**,
   **b:…**, **l:…**, etc. and **complete them only after a dot or at command
   position** (like, e.g.: **if b:changetick…**).

3. It'll detect all arrays' and dicts' keys (i.e.: subscripts) **and complete
   them only after a variable[…** — i.e.: it'll complete array keys only when
   you'll be actually entering them. It'll also **mix-in** all the variables
   from (2).

# Installation and Usage

An auto-completion plugin
[**zphere-zsh/shell-auto-popmenu**](https://github.com/zphere-zsh/shell-auto-popmenu/)
is recommended (it's a plugin adapted specifically for this VimL and the other
[**shell**](https://github.com/zphere-zsh/shell-auto-popmenu/) omni completion),
otherwise a regular omni-completion invocation via `Ctrl-X Ctrl-O` is also
possible.

Only sourcing of the plugin is required, no other setup is needed. You can use
your favourite Vim plugin manager or manually copy the script to the
`~/.vim/plugin/autoload` directory.

# Presentation

[![asciicast](https://asciinema.org/a/351814.svg)](https://asciinema.org/a/351814)

<!-- vim:set ft=markdown tw=80 fo+=a1n autoindent: -->
