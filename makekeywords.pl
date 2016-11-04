#########################################################################
#									#
#  This Perl code is used to build apache-mode, an add-on for Emacs.	#
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
#									#
#  This program takes a prototype Lisp file and outputs it with lists	#
#  of keywords inserted in the appropriate place.  The keywords are	#
#  taken from CSV files, which therefore can be manipulated by any	#
#  spreadsheet or other utility.					#
#									#
#  The format of the input files should be two columns, the first	#
#  being the keyword and the second being the module that it belongs	#
#  to.  The second field is only used for commenting the generated	#
#  Lisp code.  The fields should be separated by a single comma and	#
#  there should be no quotes or other characters included in or		#
#  around the fields - this will be the format if LibreOffice is used	#
#  to edit the files.							#
#									#
#  For sensible module grouping and comments, the file should be	#
#  sorted on the second	column followed by the first column.		#
#  Duplicate keywords will be eliminated.				#
#									#
#  The keywords are read from the CSV file and inserted into the	#
#  generated Lisp file when a comment of the form			#
#									#
#     ; @KEYWORDS@ variable-name input-file				#
#									#
#  is seen.  There may be any number of leading semicolons and		#
#  optional white space after them.  The VARIABLE-NAME is the name	#
#  of the Lisp variable that will be set, enclosed in an appropriate	#
#  'defconst' form.  The INPUT-FILE is the CSV file to be read.		#
#									#
#########################################################################

require 5;						# some functions only this version
use strict qw(refs subs);				# disciplined programming

use File::Basename;					# file name parsing

#########################################################################
#  Reporting								#
#########################################################################

sub error(@)
{
    die "$myname (ERROR): ",@_,"\n";
}

sub infomsg(@)
{
    warn "$myname (INFO): ",@_,"\n";
}

#########################################################################

$myname = basename($0, (".pl"));			# get my program name
while (<>)
{
    chomp();						# trim trailing newline

    if (/^;+\s*\@KEYWORDS\@\s+(\S+)\s+(\S+)/)		# look for insertion directive
    {
	my ($var) = $1;					# Lisp variable name
	my ($file) = $2;				# input CSV file

	open(CSV, "<$file") || &error("Cannot read '$file', $!");
	&infomsg("Reading '$file' to set '$var'");

	print ";; Generated from '",$file,"' by the '",$myname,"' script","\n";
							# inserted file header
	&outline("(defconst ".$var." (purecopy");	# start of defconst
	&outline("  (delete-dups");			# start of delete-dups
	&outline("    '(");				# start of keyword list

	$oldmodule = "";				# previous module name

	while (<CSV>)					# read from input file
	{
	    chomp();					# trim trailing newline
	    s/^\s+//;					# trim leading whitespace
	    s/^,+//;					# trim leading comma
	    s/\s+$//;					# trim trailing whitespace
	    s/,+$//;					# trim trailing comma
	    next if (/^$/);				# ignore blank lines
	    next if (/^#/);				# ignore comments

	    my ($keyword, $module) = split(/\s*,\s*/);
	    if ($module ne $oldmodule)			# start of a new module set
	    {
		&outline("      \"".$keyword."\"", $module);
		$oldmodule = $module;
	    }
	    else
	    {
		&outline("      \"".$keyword."\"");
	    }
	}

	&outline("    ))");				# end of list/delete-dups
	&outline("))");					# end of purecopy/defconst
	print "\n";					# end of insertion
	close(CSV);					# finished with input file
    }
    else						# just a normal line
    {
	print $_,"\n";
    }
}

exit(0);						# all done

#########################################################################

sub outline($;$)
{
    my ($line) = shift;					# line to output
    my ($comment) = shift || "";			# comment for line

    if ($comment ne "")					# there is a comment
    {
	printf("%-54s  ; %s", $line, $comment);
    }
    else
    {
	print $line;
    }

    print "\n";
}
