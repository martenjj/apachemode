#########################################################################
#									#
#  This is the Makefile for apache-mode, an add-on for Emacs.		#
#									#
#  Author:    Jonathan Marten <jjm@keelhaul.me.uk>			#
#  Last edit: 04-Nov-16							#
#									#
#  It is free software; you can redistribute it and/or modify it	#
#  under the terms of the GNU General Public License as published by	#
#  the Free Software Foundation; either version 2, or (at your option)	#
#  any later version.							#
#									#
#  It is distributed in the hope that it will be useful, but		#
#  WITHOUT ANY WARRANTY; without even the implied warranty of		#
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU	#
#  General Public License for more details.				#
#									#
#  You should have received a copy of the GNU General Public License	#
#  along with your copy of Emacs; see the file COPYING.  If not,	#
#  see <http://www.gnu.org/licenses/>.					#
#									#
#########################################################################

LISP=		apache-mode.el apache-mode-autoloads.el
CSV=		keywords.csv sections.csv values.csv

PKW=		./makekeywords.pl
PERL=		perl
EMACS=		emacs

all:		$(LISP)

apache-mode.el:	apache-mode.in $(CSV) $(PKW)
		$(RM) $@; $(PERL) $(PKW) $< >$@

apache-mode-autoloads.el: apache-mode.el
		$(EMACS) -batch -eval "(let ((generated-autoload-file \"`pwd`/$@\")) (if (boundp 'running-xemacs) (update-autoload-files \"`pwd`\" \"apache-mode\" generated-autoload-file) (update-directory-autoloads \"`pwd`\")))"

clean:;		$(RM) $(LISP)
