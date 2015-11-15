se cin
se ai
se ic
" Tell vim to remember certain things when we exit
"  '10  :  marks will be remembered for up to 10 previously edited files
"  "100 :  will save up to 100 lines for each register
"  :20  :  up to 20 lines of command-line history will be remembered
"  %    :  saves and restores the buffer list
"  n... :  where to save the viminfo files
set viminfo='10,\"100,:20,%,n~/.viminfo
function! ResCur()
  if line("'\"") <= line("$")
    normal! g`"
    return 1
  endif
endfunction

augroup resCur
  autocmd!
  autocmd BufWinEnter * call ResCur()
augroup END
"se et
"se ts=4
execute pathogen#infect()
syntax on
filetype plugin indent on

if has("gui_running")
"se guifont=Andale\ Mono\ 10
se guifont=Input\ Mono\ Compressed\ 10
se bg=dark
colo smyck
"colo summerfruit256
set t_Co=256
se rnu
endif
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_python_checkers = ['pyflakes']
se showcmd
se copyindent
nnoremap ; :
let g:indent_guides_enable_on_vim_startup=1
se nocompatible
se tags=/home/shubhro/work/ics/ica/tags
se hlsearch
set guioptions-=m  "remove menu bar
set guioptions-=T  "remove toolbar
set guioptions-=r  "remove right-hand scroll bar
set guioptions-=L  "remove left-hand scroll bar
let g:niji_dark_colours = [
    \ [ '81', '#5fd7ff'],
    \ [ '99', '#875fff'],
    \ [ '1',  '#dc322f'],
    \ [ '76', '#5fd700'],
    \ [ '3',  '#b58900'],
    \ [ '2',  '#859900'],
    \ [ '6',  '#2aa198'],
    \ [ '4',  '#268bd2'],
    \ ]
let g:airline_powerline_fonts=1
set laststatus=2
set ttimeoutlen=50

" ---------------------------------------------------------------------
" emacs follow: scroll bind two windows one screenful apart
nmap <silent> <Leader>ef	:vsplit<bar>wincmd l<bar>exe "norm!  Ljz<c-v><cr>"<cr>:set scb<cr>:wincmd h<cr>:set scb<cr>
