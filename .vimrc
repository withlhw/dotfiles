set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
    Plugin 'gmarik/Vundle.vim'
    Plugin 'Lokaltog/powerline', {'rtp': 'powerline/bindings/vim/'}
    Plugin 'scrooloose/nerdtree'
    Plugin 'kien/ctrlp.vim'
    Plugin 'Valloric/YouCompleteMe'
    Plugin 'taglist.vim'
    Plugin 'Trinity'
    Plugin 'SrcExpl'
    Plugin 'cscope.vim'
    Plugin 'tpope/vim-fugitive'
    Plugin 'mileszs/ack.vim'
call vundle#end()
filetype plugin indent on

syntax enable
colorscheme peachpuff
set nu
set smartindent
set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab
set showmatch
set scrolloff=4
set nobackup
set ruler
set laststatus=2
set cursorline

let g:Powerline_symbols = 'fancy'
let g:NERDTreeWinPos = "right"
let g:ctrlp_custom_ignore = {'dir': '\v[\/]\.(git|hg|svn)$'}
let g:ycm_global_ycm_extra_conf = '~/.vim/bundle/YouCompleteMe/third_party/ycmd/cpp/ycm/.ycm_extra_conf.py'
let g:ycm_confirm_extra_conf = 0
let g:ycm_register_as_syntastic_checker = 0

set completeopt-=preview
set encoding=utf-8
set t_Co=256
set fillchars+=stl:\ ,stlnc:\
set term=xterm-256color
set termencoding=utf-8

" ack.vim
if executable('rg')
    let g:ackprg = "rg --vimgrep -H --no-ignore-vcs -tcpp -tjava -tpy -tc --type-add 'custom:*.{idl,gni,gn,gyp,gypi}' -tcustom"         
    " this ignore files listed in ~/.ignore
    " -t : only search this type
    " -T : exclude this type
endif

let g:ack_default_options =
              \ " -s -H --nocolor --nogroup --follow"

let g:ack_mappings = { 
      \ "s": "<C-W><CR><C-W>K",
      \ "S": "<C-W><CR><C-W>K<C-W>b",
      \ "h": "", 
      \ "H": "", 
      \ }

let g:ack_use_cword_for_empty_search = 1 

nmap <F7>  :Ack<SPACE>
nmap <F8>  :TrinityToggleAll<CR>
nmap <c-o> :NERDTreeToggle<CR>
nmap <c-l> :TlistToggle<CR>

