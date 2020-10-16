########################################################################
#
# foreach is free software; you can redistribute it and/or modify
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
#  Project      :  File Preprocessor - foreach loop module
#  Filename     :  $RCSfile: foreach.pm,v $
#  Author       :  $Author: darren $
#  Maintainer   :  Darren Miller: darren@cabaret.demon.co.uk
#  File version :  $Revision: 1.4 $
#  Last changed :  $Date: 2003/07/02 22:23:15 $
#  Description  :  Implements a simple foreach loop
#  Licence      :  GNU copyleft
#
########################################################################
# THIS IS A FILEPP MODULE, YOU NEED FILEPP TO USE IT!!!
# usage: filepp -m foreach.pm <files>
########################################################################

package Foreach;

use strict;

# version number of module
my $VERSION = '1.0.1';

# delimiter used to split list
my $delim = "\\s*,\\s*";

# contains foreach structure:
# $macro = macro which stores value
# $comp = comparison
# $end = max (or min) value
# $inc = increment
# $pos = position of start of foreach loop
my @Foreachloops = ();

# set when end of foreach loop reached which did not evaluate to true
# foreach first loop, at this point parsing skips to end foreachloop.
# If they are inner loops the inner foreach is skipped, but the inner
# endforeach is not leading to more endforeach's than foreach's.  This
# flag says it is ok to ignore them.
my $ignore_end = 0;


##############################################################################
# foreachdelim keyword, allows user to set list delimiter, input:
# /delimiter/
##############################################################################
sub ForeachDelim
{
    my $newdelim = shift;
    # remove initial /
    $newdelim =~ s/\A\s*\///;
    # remove final /
    $newdelim =~ s/\/\s*\Z//;
    $delim = $newdelim;
    Filepp::Debug("Foreach: set delimiter to <$delim>");
}
Filepp::AddKeyword("foreachdelim", "Foreach::ForeachDelim");

##############################################################################
# foreach keyword, input:
# macro start comparison end increment
# loop of foreachm:
# foreach(macro, @List)
#   ops
# endforeach
##############################################################################
sub Foreach
{
    my $input = shift;
    # split up line
    my $macro;
    my $list;
    my $i;
    
    # find end of macroword - assume separated by space or tab
    $i = Filepp::GetNextWordEnd($input);
    
    # separate macro and defn (can't use split, doesn't work with '0')
    $macro = substr($input, 0, $i);
    $list  = substr($input, $i);
    
    # strip leading whitespace from $val
    if($list) {
	$list =~ s/^[ \t]*//;
    }
    $list = Filepp::RunProcessors($list);
    
    # split up list
    my @List = split(/$delim/, $list);
    # reverse the list so pop will give first element
    @List = reverse(@List);

    Filepp::Debug("Foreach: $macro, /$delim/, $list\n");
    
    # get current position in input file
    my $pos = tell(Filepp::INPUT);
    
    # get next value and increment it
    my $val = pop(@List);
    
    # add data structure to current foreach list
    my @ThisForeach = ($pos, $macro, @List);
    push(@Foreachloops, \@ThisForeach);

    # foreach loop ok, set up data structure and go
    if(defined($val)) {
	Filepp::Define("$macro $val");
	Filepp::Debug("Foreach: $macro loop started with value $val");
	# in a valid foreach loop, make sure all endforeach's are
	# treated as valid
	$ignore_end = 0;
	# in foreach loop - return 1 to Parse
	return 1;
    }
    # have not entered loop, be ready foreach excess endforeach's
    $ignore_end = 1;
    # foreach loop comparison failed, skip to endforeach - return 0 to Parse
    Filepp::Debug("Foreach: $macro loop not entered");
    return 0;
}

##############################################################################
# add foreach keyword - also an ifword
##############################################################################
Filepp::AddKeyword("foreach", "Foreach::Foreach");
Filepp::AddIfword("foreach");


##############################################################################
# endforeach keyword
# no input, used to terminate foreachloop
##############################################################################
sub EndForeach
{
    # check endforeach is at a valid position - otherwise ignore it
    # (filepp will give an error if it is out of place)
    if($#Foreachloops < 0) { return 1; }
    
    # pop current foreach loop info off top of list
    my @ThisForeach = @{pop(@Foreachloops)};
    my ($pos, $macro, @List) = @ThisForeach;

    # get next value and increment it
    my $val = pop(@List);

    # test if current foreachloop should continue
    if(defined($val)) {
	Filepp::Define("$macro $val");
	Filepp::Debug("Foreach: $macro loop continuing with value $val");
	# continuing, push foreach loop info back onto list
	@ThisForeach = ($pos, $macro, @List);
	push(@Foreachloops, \@ThisForeach);
	# return file to start of foreach loop
	if(seek(Filepp::INPUT, $pos, 0) == 0) {
	    Filepp::Error("Foreach: Internal error in foreach loop - fseek failed");
	}	
	# return 0 to Parse, tells it we are still in the current 
	# "if" (foreach) block
	return 0;
    }
    # end of loop, return 1 to Parse so it moves on
    Filepp::Debug("Foreach: $macro loop end");
    return 1;
}

##############################################################################
# add foreach keyword - also an endif word
##############################################################################
Filepp::AddKeyword("endforeach", "Foreach::EndForeach");
Filepp::AddEndifword("endforeach");

return 1;

########################################################################
# End of file
########################################################################
