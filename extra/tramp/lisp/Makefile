# -*- coding: utf-8; -*-
# Emacs Makefile for TRAMP
# lisp/Makefile.  Generated from Makefile.in by configure.

# Copyright (C) 1998-2016 Free Software Foundation, Inc.

# Author: Kai Großjohann <kai.grossjohann@gmx.net>
#         Michael Albinus <michael.albinus@gmx.de>
# Keywords: comm, processes

# This file is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This file is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

# This Makefile requires GNU make.

EMACS		= emacs
EMACS_GVFS	= no
INSTALL		= install -m644
MKDIR		= @mkdir -p
RM		= -rm -f

builddir        = $(dir $(lastword $(MAKEFILE_LIST)))
prefix		= /usr/local
datarootdir	= ${prefix}/share
datadir		= ${datarootdir}
lispdir		= ${datadir}/emacs/site-lisp
DESTDIR		=

CLEAN_FILES	= .\\\#* \\\#* .*~ *~ *.aux *.cp *.cps *.diff *.dvi *.elc *.fn *.fns *.html *.info *.ky *.log *.pdf *.pg *.tmp *.toc *.tp *.vr *.vrs

# trampver.el and tramp.el must be handled first due to dependencies.
LISP_FILES	= trampver.el tramp.el tramp-adb.el tramp-cache.el	\
		  tramp-cmds.el tramp-compat.el tramp-ftp.el		\
		  tramp-gw.el tramp-sh.el tramp-smb.el tramp-uu.el

# tramp-gvfs.el needs Emacs 23 with dbus.
ifeq ($(EMACS_GVFS), yes)
  LISP_FILES += tramp-gvfs.el
endif

ELC_FILES	= $(addsuffix .elc, $(basename $(LISP_FILES)))
INSTALL_FILES	= $(LISP_FILES) tramp-loaddefs.el $(ELC_FILES)
EM		= $(EMACS) --no-site-file -batch

.PHONY: all autoloads install clean distclean tramp

.SUFFIXES: .elc .el

all: autoloads $(ELC_FILES)

autoloads: $(builddir)/tramp-loaddefs.el
$(builddir)/tramp-loaddefs.el: $(LISP_FILES)
	$(EM) -l autoload						    \
	  --eval "(setq generate-autoload-cookie \";;;###tramp-autoload\")" \
	  --eval "(setq generated-autoload-file				    \
		    (expand-file-name \"tramp-loaddefs.el\"))"		    \
	  --eval "(setq make-backup-files nil)"				    \
	  -f batch-update-autoloads .

%.elc: %.el
	$(EM) -l bytecomp \
	  --eval "(add-to-list 'load-path default-directory)"		\
	  -f batch-byte-compile $?

install: all
	$(MKDIR) $(DESTDIR)$(lispdir)
	$(INSTALL) $(INSTALL_FILES) $(DESTDIR)$(lispdir)

clean:
	$(RM) $(CLEAN_FILES)

distclean: clean
	$(RM) tramp-loaddefs.el

# Maintainer targets.

tramp: all

### TODO:

## * Update Tramp's autoladed symbols in loaddefs.el during install.
