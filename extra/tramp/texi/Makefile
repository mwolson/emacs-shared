# -*- coding: utf-8; -*-
# Emacs Makefile for TRAMP
# texi/Makefile.  Generated from Makefile.in by configure.

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

# This Makefile requires GNU make and GNU texinfo.  If there is an
# insufficient version of makeinfo, autoconf refuses to generate
# Makefile.  Problems of texi2dvi or install-info result in a warning.

EMACS		= emacs
INSTALL_CHAPTER	= $(if $(findstring yes, yes), \
			installchapter, noinstallchapter)
MAKEINFO	= makeinfo --no-warn --no-split -D $(INSTALL_CHAPTER)
TEXI2DVI	= no
INSTALL		= install -m644
INSTALL_INFO	= install-info --quiet
CP		= cp
MKDIR		= @mkdir -p
RM		= -rm -f

prefix		= /usr/local
datarootdir	= ${prefix}/share
datadir		= ${datarootdir}
infodir		= ${prefix}/share/info
localinfodir	= ../info
DESTDIR		=

TEXI_FILES	= tramp.texi trampver.texi doclicense.texi docstyle.texi \
		  $(if $(findstring yes, yes), trampinst.texi)
INFO_FILES	= $(localinfodir)/tramp
INSTALL_FILES	= $(DESTDIR)$(infodir)/tramp
CLEAN_FILES	= .\\\#* \\\#* .*~ *~ *.aux *.cp *.cps *.diff *.dvi *.elc *.fn *.fns *.html *.info *.ky *.log *.pdf *.pg *.tmp *.toc *.tp *.vr *.vrs $(wildcard $(localinfodir)/*)

# To be used by maintainer only.
NONGNU		= ../../tramp-web

.PHONY: all install clean distclean tramp dvi html pdf savannah \
	$(INSTALL_FILES) $(localinfodir)/dir $(DESTDIR)$(infodir)/dir

all: tramp dvi html pdf

tramp: $(INFO_FILES) $(localinfodir)/dir

install: tramp $(INSTALL_FILES) $(DESTDIR)$(infodir)/dir

clean:
	$(RM) $(CLEAN_FILES)

distclean: clean

$(INFO_FILES): $(TEXI_FILES)
	$(MKDIR) $(dir $@)
	$(MAKEINFO) --footnote-style=end -o $@ $<

$(INSTALL_FILES): $(INFO_FILES)
	$(MKDIR) $(dir $@)
	$(INSTALL) $(INFO_FILES) $(DESTDIR)$(infodir)

# Don't call install-info if it failed in configure.
$(localinfodir)/dir $(DESTDIR)$(infodir)/dir: $(INFO_FILES)
	$(MKDIR) $(dir $@)
	$(if $(findstring dir, $(shell find $(dir $@) -name dir)),, \
		$(CP) dir_sample $@)
	$(if $(findstring no, $(INSTALL_INFO)),, \
		$(foreach file, $(INFO_FILES), \
			$(if $(shell grep -q -s "($(notdir $(file)))\." $@),, \
			    $(INSTALL_INFO) --info-dir=$(dir $@) \
				$(subst $(dir $(file)), $(dir $@), $(file)))))

dvi html pdf: $(TEXI_FILES)
	-$(MAKEINFO) --$@ -o tramp.$@ $<

# Maintainer targets.

savannah: html
	cp tramp.html $(NONGNU)
	cp tramp.html $(NONGNU)/index.html
	@echo ""
	@echo "Now use cvs checkin in $(NONGNU)"
	@echo ""
