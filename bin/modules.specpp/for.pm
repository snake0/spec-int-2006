########################################################################
#
# for is free software; you can redistribute it and/or modify
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
#  Project      :  File Preprocessor - for loop module
#  Filename     :  $RCSfile: for.pm,v $
#  Author       :  $Author: darren $
#  Maintainer   :  Darren Miller: darren@cabaret.demon.co.uk
#  File version :  $Revision: 1.12 $
#  Last changed :  $Date: 2003/07/02 22:23:15 $
#  Description  :  Implements a simple for loop
#  Licence      :  GNU copyleft
#
########################################################################
# THIS IS A FILEPP MODULE, YOU NEED FILEPP TO USE IT!!!
# usage: filepp -m for.pm <files>
########################################################################

package For;

use strict;

# version number of module
my $VERSION = '1.2.1';


# contains for structure:
# $macro = macro which stores value
# $comp = comparison
# $end = max (or min) value
# $inc = increment
# $pos = position of start of for loop
my @Forloops = ();

# set when end of for loop reached which did not evaluate to true for first
# loop, at this point parsing skips to end forloop.  If they are inner loops
# the inner for is skipped, but the inner endfor is not leading to more
# endfor's than for's.  This flag says it is ok to ignore them.
my $ignore_end = 0;

##############################################################################
# TestFor - tests current for loop to see if it should continue in
# loop or break out (returns 1 for continue, 0 for stop)
##############################################################################
sub TestFor
{
    my ($val, $compare, $end) = @_;
    return Filepp::If(("$val $compare $end"));
}


##############################################################################
# for keyword, input:
# macro start comparison end increment
# loop of form:
# for(macro=start; macro comparison end; macro += increment)
#   ops
# endfor
##############################################################################
sub For
{
    my $input = shift;
    
    # split up line
    my ($macro, $start, $compare, $end, $inc) = split(/\s+/, $input);

    # check if $start is a macro
    $start = Filepp::RunProcessors("$start");

    Filepp::Debug("For: $macro = $start, $macro $compare $end, $macro += $inc\n");
    
    # define the macro to have the starting value
    Filepp::Define("$macro $start");

    # get current position in input file
    my $pos = tell(Filepp::INPUT);

    # add data structure to current for list
    my @ThisFor = ($macro, $compare, $end, $inc, $pos);    
    push(@Forloops, \@ThisFor);

    # for loop ok, set up data structure and go
    if(TestFor($start, $compare, $end)) {
	Filepp::Debug("For: $macro loop started with value $start");
        # in a valid for loop, make sure all endfor's are treated as valid
	$ignore_end = 0;
	# in for loop - return 1 to Parse
	return 1;
    }
    # have not entered loop, be ready for excess endfor's
    $ignore_end = 1;
    # for loop comparison failed, skip to endfor - return 0 to Parse
    Filepp::Debug("For: $macro loop not entered");
    return 0;
}

##############################################################################
# add for keyword - also an ifword
##############################################################################
Filepp::AddKeyword("for", "For::For");
Filepp::AddIfword("for");


##############################################################################
# endfor keyword
# no input, used to terminate forloop
##############################################################################
sub EndFor
{
    # check endfor is at a valid position - otherwise ignore it
    # (filepp will give an error if it is out of place)
    if($#Forloops < 0) { return 1; }
    
    # pop current for loop info off top of list
    my @ThisFor = @{pop(@Forloops)};
    my ($macro, $compare, $end, $inc, $pos) = @ThisFor;

    # get macro val and increment it
    my $val = Filepp::RunProcessors($macro);
    $val += Filepp::RunProcessors("$inc");
    Filepp::Define("$macro $val");

    # test if current forloop should continue
    if(TestFor($val, $compare, $end)) {
	Filepp::Debug("For: $macro loop continuing with value $val");
	# continuing, push for loop info back onto list
	push(@Forloops, \@ThisFor);
	# return file to start of for loop
	if(seek(Filepp::INPUT, $pos, 0) == 0) {
	    Filepp::Error("For: Internal error in for loop - fseek failed");
	}
	
	# return 0 to Parse, tells it we are still in the current 
	# "if" (for) block
	return 0;
    }
    # end of loop, return 1 to Parse so it moves on
    Filepp::Debug("For: $macro loop end");
    return 1;
}

##############################################################################
# add for keyword - also an endif word
##############################################################################
Filepp::AddKeyword("endfor", "For::EndFor");
Filepp::AddEndifword("endfor");

return 1;

########################################################################
# End of file
########################################################################
