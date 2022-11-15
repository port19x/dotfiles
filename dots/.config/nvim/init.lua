-- regular nvim settings
vim.g.mapleader = " "
vim.o.shiftwidth = 2
vim.o.number = true
vim.o.relativenumber = true
if (vim.g.neovide)
then
  vim.g.background = dark
  vim.g.gruvbox_contrast_dark = 'hard'
  vim.cmd 'colorscheme gruvbox'
  vim.o.guifont = 'iosevka'
end

-- packer bootstrap
local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
    packer_bootstrap = fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
    vim.cmd [[packadd packer.nvim]]
end

-- plugin installation and configuration
return require('packer').startup(function(use)
    use 'wbthomason/packer.nvim'
    use 'morhetz/gruvbox'

    use {
	'vimwiki/vimwiki',
	config = function()
	vim.g.vimwiki_list = {{
	    path = '~/dl/port19.xyz/wiki/',
	    template_path = '~/dl/port19.xyz/',
	    template_default = 'template',
	    template_ext = '.html'
	}}
	end 
    }

  if packer_bootstrap then
    require('packer').sync()
  end
end)
