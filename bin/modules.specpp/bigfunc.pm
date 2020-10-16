########################################################################
#
# bigfunc is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; see the file COPYING.  If not, write to
# the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
#
########################################################################
#
#  Project      :  File Preprocessor - bigfunc module
#  Filename     :  $RCSfile: bigfunc.pm,v $
#  Author       :  $Author: darren $
#  Maintainer   :  Darren Miller: darren@cabaret.demon.co.uk
#  File version :  $Revision: 1.6 $
#  Last changed :  $Date: 2003/07/04 15:42:15 $
#  Description  :  This allows last minute processing of stuff
#  Licence      :  GNU copyleft
#
########################################################################
# THIS IS A FILEPP MODULE, YOU NEED FILEPP TO USE IT!!!
# usage: filepp -m bigfunc.pm <files>
########################################################################
package Bigfunc;

use strict;

########################################################################
# WARNING - this module is still in development and may behave strangely,
# please let me know if it misbehaves - darren@cabaret.demon.co.uk
########################################################################

# version number of module
my $VERSION = '0.9.0';

require "function.pm";

my %Defines;
my %DefineArgs;

########################################################################
# Bigfunc keyword - same as bigdef - only difference is any keywords
# in the macro are evaluated when the macro is called rather than
# when the macro is defined.
########################################################################
sub Bigfunc
{
    my $input = shift;
    my $macrodefn = "";
    my $keywordchar = Filepp::GetKeywordchar();

    # check there are brackets () in the function name
    if($input !~ /\(/) {
	Filepp::Error("bigfunc: macro must have brackets [use macro() for 0 arguments]");
      }
    
    # find end of bigfunc
    while($input !~ /^\s*$keywordchar\s*endbigfunc\s*/) {
	$macrodefn = $macrodefn.$input;
	$input = Filepp::GetNextLine();
	if(!$input) { Filepp::Error("bigfunc: End of file found in bigfunc"); }
    }
    # last line of input is #endbigfunc which can be ignored
    
    ########################################################################
    # split macro up into macro and define
    ########################################################################
    # find end of macroword - assume separated by space or tab
    my $i = Filepp::GetNextWordEnd($macrodefn);
    
    # separate macro and defn (can't use split, doesn't work with '0')
    my $macro = substr($macrodefn, 0, $i);
    my $defn  = substr($macrodefn, $i);
    
    # strip leading whitespace from $defn
    if($defn) {
	$defn =~ s/^[ \t]*//;
    }
    else {
	$defn = "";
    }

    # split up macro, args and defn - delimiters = space, (, ), ','
    my @arglist = split(/([\s,\(,\),\,])/, $macro." ".$defn);
    my $macroargs = "";
    my $arg;
    
    # macro is first element in list, remove it from list
    $macro = $arglist[0];
    $arglist[0] = "";
    # loop through list until ')' and find all args
    foreach $arg (@arglist) {
	if($arg) {
	    # end of arg list, leave loop
	    if($arg eq ")") {
		$arg = "";
		last;
	    }
	    # ignore space, ',' and '('
	    elsif($arg =~ /([\s,\,,\(])/) {
		$arg = "";
	    }
	    # argument found, add to ',' separated list
	    else {
		$macroargs = $macroargs.",".$arg;
		$arg = "";
	    }
	}
    }
    $macroargs = Filepp::Strip($macroargs, ",", 1);
    # store args
    $DefineArgs{$macro} = $macroargs;
    
    Filepp::Debug("Define: macro ".$macro." has args (".$macroargs.")");
    # put rest of defn back together
    $defn = join('',@arglist);
    $defn = Filepp::CleanStart($defn);

    # define the macro defn pair
    $Defines{$macro} = $defn;
    
    # add a function which will call this macro
    Function::AddFunction($macro, "Bigfunc::Run");
}
Filepp::AddKeyword("bigfunc", "Bigfunc::Bigfunc");


########################################################################
# Function to parse the bigfunc
########################################################################
sub Run
{
    my $macro = Filepp::FunctionMacro();
    my @Argvals = @_;
    my @Argnames = split(/\,/, $DefineArgs{$macro});
    
    my $parseline = Filepp::GetParseLineEnd();
    # split macro into single lines
    my @Input = split(/\n/, $Defines{$macro});
    my $output = "";
    my $i = 0;
    
    # process all lines in macro
    while($i <= $#Input)  {
	my ($more, $line) = $parseline->($Input[$i]);
	$i++;
	while($more && $i <= $#Input) {
	    ($more, $line) = $parseline->($line.$Input[$i]);
	    $i++;
	}
	$output = $output."\n".Filepp::RunProcessors($line, 1);
    }
    
    # replace any arguments

    # check if last arg ends in ... (allows any number of args in macro)
    if($#Argnames >= 0 && $Argnames[$#Argnames] =~ s/\.\.\.\Z//o) {
	# concatanate all extra args into final arg
	while($#Argvals > $#Argnames) {
	    my $arg1 = pop(@Argvals);
	    my $arg2 = pop(@Argvals);
	    push(@Argvals, $arg2.", ".$arg1);
	}
	# check for ## at start of macro name in args list
	if($output =~ /\#\#$Argnames[$#Argnames]/) {
	    # if last argument is empty remove preciding ","
	    if($#Argvals == $#Argnames && $Argvals[$#Argnames] eq "") {
		$output =~ s/\,\s*\#\#$Argnames[$#Argnames]//g;
	    }
	    else {
		$output =~
		    s/\#\#$Argnames[$#Argnames]/$Argnames[$#Argnames]/g;
	    }
	}
    }
    
    my $j;
    # The next step replaces argnames with argvals.  Once a bit of string
    # has been replaced it is removed from further processing to avoid
    # unwanted recursive macro replacement.
    my @InString = ( $output ); # string to be replaced
    my @InDone   = ( 0 );       # flag to say if string section replaced
    my @OutString;              # output of string sections after each
    # macro has been replaced
    my @OutDone;                # output flags
    my $k = 0;
    for($i=0; $i<=$#Argnames; $i++) {
	for($j=0; $j<=$#InString; $j++) {
	    if($InDone[$j] == 0) {
		# replace macros and split up string so replaced part
		# is flagged as done and rest is left for further
		# processing
		while($InString[$j] =~ /$Argnames[$i]/) {
		    $OutString[$k] = $`;            $OutDone[$k] = 0;
		    $k++;
		    $OutString[$k] = $Argvals[$i];  $OutDone[$k] = 1;
		    $k++;
		    $InString[$j] = $';     # one more quote for emacs '
		}
	    }
	    $OutString[$k] = $InString[$j];   $OutDone[$k] = $InDone[$j];
	    $k++;
	}
	@InString = @OutString;   @InDone = @OutDone;
	$k = 0;
    }
    # rebuild string
    $output = join('', @InString);
    
    return $output;
}


return 1;
########################################################################
# End of file
########################################################################
