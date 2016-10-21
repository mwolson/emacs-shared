# -*- coding: utf-8; -*-
# Emacs Makefile for TRAMP
# Makefile.  Generated from Makefile.in by configure.

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

# This Makefile requires GNU make and GNU tar.


EMACS		= emacs
RM		= -rm -f
AUTOCONF	= autoconf

DESTDIR		=
DIRS		= lisp texi

CONFIG_FILES	= Makefile lisp/Makefile lisp/trampver.el texi/Makefile texi/trampver.texi
CLEAN_FILES	= .\\\#* \\\#* .*~ *~ *.aux *.cp *.cps *.diff *.dvi *.elc *.fn *.fns *.html *.info *.ky *.log *.pdf *.pg *.tmp *.toc *.tp *.vr *.vrs

# To be used by maintainer only.
VERSION		= $(subst .,-,2.3.0)
TARNAME		= tramp-2.3.0
EMACS_TARGET	= ../emacs

# Can't use wildcard tramp-*.el since that would catch tramp-efs.el,
# too, which isn't included in Emacs.
# lisp/trampver.el is not listed here since it is a generated file,
# but the files listed here are subjected to "git diff".  We just copy
# lisp/trampver.el.
EMACS_EL_FILES = lisp/tramp.el lisp/tramp-adb.el lisp/tramp-cache.el	\
		lisp/tramp-cmds.el lisp/tramp-compat.el			\
		lisp/tramp-ftp.el lisp/tramp-gvfs.el lisp/tramp-gw.el	\
		lisp/tramp-smb.el lisp/tramp-uu.el

# texi/trampver.texi is not listed here since it is a generated file,
# but the files listed here are subjected to "git diff".  We just copy
# texi/trampver.texi.
EMACS_TEXI_FILES = texi/tramp.texi

# ftp upload program to be used.
ifeq (no, yes)
  # Michaels flavor.
  FTP_UPLOAD = ftp-upload --host ftp-upload.gnu.org \
			  --dir /incoming/ftp
else
  # Kais flavor.
  FTP_UPLOAD = ftp -u ftp://ftp-upload.gnu.org/incoming/ftp
endif

# Tramp test arguments.
TRAMP_TEST_ARGS	=

# Decrease noise.
.SILENT: all lisp info install clean

.PHONY: all install lisp info check clean distclean 	\
	tags test maintainer-clean gittag MANIFEST tar	\
	savannah

all install: $(CONFIG_FILES)
	for a in ${DIRS}; do				\
	    $(MAKE) -C $$a				\
	    "EMACS=$(EMACS)" "DESTDIR=$(DESTDIR)" 	\
	    $(MAKECMDGOALS);				\
	done

lisp: $(CONFIG_FILES)
	$(MAKE) -C lisp					\
	    "EMACS=$(EMACS)" "DESTDIR=$(DESTDIR)" all

info: $(CONFIG_FILES)
	$(MAKE) -C texi					\
	    "EMACS=$(EMACS)" "DESTDIR=$(DESTDIR)" all

check test: $(CONFIG_FILES) lisp
	$(MAKE) -C test					\
	    "EMACS=$(EMACS)" "DESTDIR=$(DESTDIR)"	\
	    "TRAMP_TEST_ARGS=$(TRAMP_TEST_ARGS)" all

clean:
	$(RM) $(CLEAN_FILES)
	for a in ${DIRS} test; do			\
	    $(MAKE) -C $$a clean;			\
	done

distclean: clean
	for a in ${DIRS} test; do			\
	    $(MAKE) -C $$a distclean;			\
	done
	$(RM) $(CONFIG_FILES) MANIFEST *.tar.gz*
	$(RM) -r autom4te.cache info

tags:
	etags lisp/*.el test/*.el texi/*.texi

# Maintainer targets.

maintainer-clean: distclean
	$(RM) configure config.status

gittag:
	git tag -f V-$(VERSION)
	git push --tags

# Depends on configure in order to run autoconf.  All files
# which shall be produced on customer host should be removed.
MANIFEST: configure distclean
	find . \( -name .git     -prune \)		\
	    -o \( -name info     -prune \)		\
	    -o \( -name obsolete -prune \)		\
	    -o \( -name tmp      -prune \)		\
	    -o \( -type f \!     -name .gitignore \)	\
	    -print > MANIFEST
	./config.status

tar: MANIFEST
	rm -rf $(TARNAME)
	mkdir $(TARNAME)
	tar cpfT - MANIFEST | ( cd $(TARNAME) ; tar xpf - )
	chmod -R a+r $(TARNAME)
	tar cvpfz $(TARNAME).tar.gz $(TARNAME)
	$(RM) -r $(TARNAME)
	gpg --detach-sign $(TARNAME).tar.gz
	echo "version: 1.1\ndirectory: tramp\nfilename: $(TARNAME).tar.gz"   \
		>$(TARNAME).tar.gz.directive
	gpg --clearsign $(TARNAME).tar.gz.directive
	$(RM) $(TARNAME).tar.gz.directive
	chmod a+r $(TARNAME).tar.gz*

savannah: tar gittag
	$(FTP_UPLOAD) $(TARNAME).tar.gz*
	./configure
	$(MAKE) -C texi savannah

# Experimental stuff.

# Make "Version foo released." ChangeLog entries.
cl-release:
	for cl in ChangeLog */ChangeLog; do \
	    emacs -batch -l ~/.emacs $$cl --eval \
	    "(progn (add-change-log-entry nil nil nil t) \
	    (backward-delete-char 2) \
	    (insert \"Version 2.3.0 released.\") \
	    (save-buffer) (kill-emacs))" ; \
	done
