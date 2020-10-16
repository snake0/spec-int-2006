#!/bin/perl

# This is a sample sysinfo program.  
#
# Copyright (C) 2003-2005 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# The program should output lines of the form
#
# name=value
#
# Standard config file comments are also allowed.
#
# Only informational fields (hw_*, sw_*, notes*, test_sponsor, machine_name,
# license_num, tester, test_date, prepared_by) may be set; these
# settings will be inserted into the 'default=default=default=default'
# section, and will not be able to override any per-benchmark settings.
# Illegal lines will be silently discarded.  Because the line checking is
# fairly simple-minded, some config file features such as line continuation,
# block quotes, and line appending are not available.
#
# The lines should be output to standard output.  There's no requirement
# that the sysinfo program be a perl (or any other kind) of script.
#
# The output from the sysinfo program will be included in the final config
# as if it had been part of the original config file.
#
# To use the sysinfo program, insert a line in the header section of your
# config file (before any section markers) that looks like
#
# sysinfo_program = <path to program or script to run>
#
# For example, assuming that you have a default installation in /cpu2006,
# the following line would run this example:
#
# sysinfo_program = /cpu2006/Docs/sample-sysinfo-program.pl
#
# Arguments, etc, can be given to the command if necessary.  Note that because
# the sysinfo_proram setting is evaluated before configuration file processing
# is complete, config file variable expansion is not done on its value.  So:
#
# sysinfo_program = ${top}/Docs/sample-sysinfo-program  # XX DOES NOT WORK XX
#
# will not work.  Preprocessor macro expansion is performed, however.
# It's not absolutely necessary that the argument to sysinfo_program be an
# absolute path; it is HIGHLY recommended that it be in order to ensure that
# it works as expected.  If for any reason the sysinfo_program can't be run,
# NO warnings will be logged.
########################################################################

# This example has been written for one specific environment (Solaris 10).
# Nevertheless, it is hoped that it might be a useful as an illustration of
# techniques. 

# First, a comment
printf "# Sysinfo program ($0)\n";

# The form of date requested here is SPEC's required form: mmm-YYYY
printf   "test_date          = %s", `date +%b-%Y`;

print "\n# Recall that 'prepared by' can be used to tag your rawfile, but\n"; 
print "# is not printed in reports\n";
printf   "prepared_by        = %s\n", `whoami`;

# The "prtdiag" command is a handy place from which to pick out the memory
# size.  The perl below runs the prtdiag command, does pattern matching to
# look for the actual size, capturing it with the (.*), and returns
# the matched string (in list context) so it can be stuffed into $memory.

($memory) = (`/usr/sbin/prtdiag | grep 'Memory size'` =~ /Memory size: (.*)/);
printf   "hw_memory          = $memory\n";

# The file /etc/release includes useful information about the OS version on
# the first line
($os) = (`head -1 /etc/release` =~ /\s*(.*)/);
print    "sw_os              = $os\n";

# The following command returns the name and MHz of the CPU, in this form:
#     UltraSPARC-IV+ (portid 0 impl 0x19 ver 0x21 clock 1350 MHz)
$info = `/usr/sbin/psrinfo -pv | head -2 | tail -1`;
($cpuname, $mhz) = ($info =~ /\s*(\S+).* (\d+) MHz/);
printf   "hw_cpu_name        = %s\n", $cpuname;
printf   "hw_cpu_mhz         = %4d\n", $mhz;

# This command returns the number of chips
($nchips) = (`/usr/sbin/psrinfo -p` =~ /(\d+)/);
printf   "hw_nchips          = %4d\n", $nchips;

if ($cpuname =~ /UltraSPARC-I/) {
   # For anything in the series UltraSPARC-II, III, IV, IV+, the following
   # command should produce something like:
   #   The physical processor has 2 virtual processors (0, 512)
   ($procsper) = (`/usr/sbin/psrinfo -pv | head -1` =~ /has (\d+) virtual processor/);
   printf "hw_ncoresperchip   = %4d\n", $procsper;
   printf "hw_ncores          = %4d\n", `/usr/sbin/psrinfo | wc -l`;
   printf "hw_nthreadspercore = %4d\n", 1;
} elsif ($cpuname =~ /UltraSPARC-T1/) {
   # Unfortunately, we don't have a handy command to tell us about the chips
   # vs. cores vs. threads on the UltraSPARC-T1; so if we're on that
   # processor, we'll do a basic sanity check, and if that passes, print a
   # pretty good guess.
   ($checktotal) = (`/usr/sbin/psrinfo | wc -l` =~ /(\d+)/);
   if ($checktotal == 32*$nchips) {
      printf "hw_ncoresperchip   = %4d\n", 8;
      printf "hw_ncores          = %4d\n", 8 * $nchips;
      printf "hw_nthreadspercore = %4d\n", 4;
   } else {
      $idunno = 1;
   }
} else {
   $idunno = 1;
}
if (defined $idunno && $idunno) {
   printf "hw_ncoresperchip   = %4s\n", "?";
   printf "hw_ncores          = %4s\n", "?";
   printf "hw_nthreadspercore = %4s\n", "?";
}

