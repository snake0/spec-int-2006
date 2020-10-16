#
# vars.pl
#
# Copyright (C) 1995-2006 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: vars.pl 4655 2006-07-24 12:48:49Z cloyce $

##############################################################################
# Initialize Variables
##############################################################################

use strict;
use Config;
use Cwd;
use UNIVERSAL;
use vars qw($nonvolatile_config $default_config);

# Customize the name of the suite and the multipliers here
$::year = '2006';
$::suite = 'CPU'.$::year;

# lcsuite should match the name of at least one of your benchsets.  If not,
# be sure to edit resolve_user_selection in parse.pl to do the right thing.
$::lcsuite = lc($::suite);

# Ratio multipliers vs. reference machine
if ($::suite eq 'CPU2006') {
  $::speed_multiplier = 1;	# See osgcpu-14317
  $::rate_multiplier = 1;	# See osgcpu-14317
} else {
  $::speed_multiplier = 1;	# A reasonable default
  $::rate_multiplier = 1;	# A reasonable default
}

# Public location of benchmark and suite flag descriptions
$::spec_flags_base = "http://www.spec.org/auto/$::lcsuite/flags/";

# List of SPEC(r) trademarked strings, for use by the formatters
# Values are "r" (registered trademark), "t" (trademark), and "s" (service mark)
%::trademarks = (
                  'SPEC'           => 'r',
                  'SPEC Logo'      => 'r',
                  'SPECapc'        => 's',
                  'SPECchem'       => 'r',
                  'SPECclimate'    => 'r',
                  'SPECenv'        => 'r',
                  'SPECfp'         => 'r',
                  'SPECglperf'     => 'r',
                  'SPEChpc'        => 'r',
                  'SPEChpc'        => 'r',
                  'SPECint'        => 'r',
                  'SPECjAppServer' => 'r',
                  'SPECjbb'        => 'r',
                  'SPECjvm'        => 'r',
                  'SPECmail'       => 'r',
                  'SPECmark'       => 'r',
                  'SPECmedia'      => 's',
                  'SPEComp'        => 'r',
                  'SPECopc'        => 's',
                  'SPECrate'       => 'r',
                  'SPECseis'       => 'r',
                  'SPECsfs'        => 'r',
                  'SPECviewperf'   => 'r',
                  'SPECweb'        => 'r',
                );

# Config items that may not be changed by the user
$nonvolatile_config = {
    'benchdir'   => 'benchspec',     # benchmark directory
    'resultdir'  => 'result',        # result directory
    'configdir'  => 'config',        # configuration directory
    'log'        => $::suite,        # extra error logging file
    'dirprot'    => 0777,            # directory protection
    'workdir'    => 'run',           # Where to actually do the runs
    'worklist'   => 'list',          # List of runs in workdir
    'exitvals'   => 'spec_exit',     # File containing return codes
    'datadir'    => 'data',          # directory containing input sets
    'inputdir'   => 'input',         # dir under datadir containing input
    'outputdir'  => 'output',        # dir under datadir containing output
    'reftime'    => 'reftime',       # file containing reference for input
    'bindir'     => 'exe',           # directory containing executables
    'srcdir'     => 'src',           # directory containing source
    'specmake'   => 'Makefile.YYYtArGeTYYYspec', # Override file for Makefile
    'specdiff'   => 'specdiff',      # Number of lines of difference
    'min_report_runs' => 3,          # Minimum number of runs to be valid
    'rawfile'    => '',	             # Hint to the formatter
    'orig_argv'  => [ @ARGV ],
    'orig_env'   =>  { %ENV },
    'help'       => 0,
    'version'    => 0,
    'output'     => 'asc',
    'valid_actions'    => [ qw(buildsetup runsetup setup
                               build only_run onlyrun
                               run validate
                               configpp
                               clean realclean trash clobber scrub
                               report) ],
    'valid_tunes'      => [ qw(base peak) ],
    'vendor_makefiles' => 0,
    'specrun'          => 'specinvoke',
    'check_integrity'  => $main::check_integrity,
    'flag_url_base'    => $::spec_flags_base,
    'realuser'         => 'your name here'
};

$default_config = {
    'compile_error'       => 0,               # had problems compiling?
    'reportable'          => 0,               # run follows all reporting rules
    'check_md5'           => 1,               # check saved md5 sums
    'prefix'              => '',              # prefix to prepend to files
    'sigint'              =>  undef,          # Signal # of SIGINT
    'ignore_sigint'       =>  0,              # Ignore SIGINT
    'make'                => 'specmake',      # Name of make executable (override in benchmark.pm)
    'vendor'              => 'anon',          # Vendor/Version for filenames
    'action'              => 'validate',      # Default action
    'run'                 => 'all',           # What benchmarks to run
    'config'              => 'default.cfg',   # configuration file
    'mach'                => 'default',       # Vendor/Version for filenames
    'ext'                 => 'none',          # default extension for executable
    'size'                => 'ref',           # Size of input set
    'tune'                => 'base',          # default tuning level
    'output_format'       => 'default',       # output format
    'calctol'             => 0,               # calculate tolerances
    'rawformat'           => 0,               # just do rawformat
    'nc'                  => 0,               # format as non-compliant
    'srcalt'              => '',              # approved source mods
    'fake'                => 0,               # send -n to spec{make,invoke}
    'expand_notes'        => 0,               # do var expansion in notes
    'max_active_compares' => 0,               # Max # of parallel compares
    'difflines'           => 10,              # Number of lines of difference
    'subworkdir'          => 'work',          # prefix for dir under workdir
    'endian'              => $Config{'byteorder'},
    'ignore_errors'       => 0,               # Ignore certain errors
    'mean_anyway'         => 0,               # Calculate mean even if invalid
    'setprocgroup'        => 1,               # Set the process group
    'verbose'             => 5,               # Verbosity level
    'deletework'          => 0,               # Delete existing working dirs
    'deletebinaries'      => 0,               # Delete existing binaries
    'rate'                => 0,     # Throughput run (or convert speed to rate)
    'speed'               => 0,               # Convert 1-copy rate to speed
    'unbuffer'            => 1,               # Unbuffer STDOUT
    'line_width'          => 0,               # line wrap width
    'log_line_width'      => 0,               # line wrap width for logfile
    'feedback'            => 1,               # Default to allow feedback
    'copies'              => 1,               # Number of copies
    'uid'                 => $<,              # User ID of the user
    'rebuild'             => 0,               # Rebuild binaries even if they
                                              # already exist
    'env_vars'            => 0,               # Allow environment to be
                                              # overriden by ENV_*
    'locking'             => 1,               # Try to lock files
    'os_exe_ext'          => '',	      # Some OSes (NT) create
                                              # executables with specific
				              # extesions
    'makeflags'           => '',	      # Extra flags for make (like -j)
    'OS'                  => 'unix',	      # OS type
    'teeout'              => 0,               # Run output through 'tee' so
				              # you can see it on the screen
    'minimize_rundirs'    => 0,		      # Try to keep working disk size
				              # down as small as possible
    'minimize_builddirs'  => 0,		      # Try to keep working disk size
				              # down as small as possible
    'backup_config'       => 1,		      # Whether to keep backup config
				              # file left over from updating
    				              # MD5s
    'make_no_clobber'     => 0,               # Don't blow away directory when
				              # building executables
    'basepeak'            => 0,		      # Use base binary for peak
				              # measurements
    'iterations'          => 3,		      # Number of iterations to run
    'commandfile'         => 'speccmds.cmd',  # Name of command file
    'commanderrfile'      => 'speccmds.err',  # Name of command error file
    'commandstdoutfile'   => 'speccmds.stdout',# Name of command stdout file
    'commandoutfile'      => 'speccmds.out',  # Name of command output file
    'comparefile'         => 'compare.cmd',   # Name of compare file
    'compareerrfile'      => 'compare.err',   # Name of compare error file
    'comparestdoutfile'   => 'compare.stdout',# Name of compare stdout file
    'compareoutfile'      => 'compare.out',   # Name of compare output file
    'table'               => 1,               # Produce a table of results
    'safe_eval'           => 1,               # Very strict opcode mask for
				              # string expansion
    'section_specifier_fatal' => 1,           # Is a typo in a section specifier fatal?
    'delay'               => 0,	# Sleep a bit before and after each benchmark run?
    'command_add_redirect'=> 0,
    'use_submit_for_speed'=> ($::lcsuite eq 'mpi2006') ? 1 : 0,
    'mailto'              => '',
    'mailserver'          => '127.0.0.1',
    'mailmethod'          => 'smtp',
    'mailport'            => 25,
    'mailcompress'        => 0,
    'sendmail'            => '/usr/sbin/sendmail',
    'mail_reports'        => 'all',
    'no_monitor'          => '',
    'plain_train'         => ($::lcsuite eq 'cpu2006') ? 1 : 0,
    'info_wrap_columns'   => 50,# Wrap non-notes at 50 columns by default
    'notes_wrap_columns'  => 0,	# Do not wrap notes lines by default
    'notes_wrap_indent'   => '    ', # Indent continuation 4 spaces
    'shrate'              => 0,	# Do a staggered homogenous rate run?
    'stagger'             => 10, # Stagger delay for the above
    'strict_rundir_verify'=> 1,  # Make sure that the MD5s of file in the
                                 # rundir match the MANIFEST _and_ the original
                                 # source file.
    'sysinfo_program'     => '',
    'no_input_handler'    => 'close',
    'flagsurl'            => '',# Flags file URL
    'check_version'       => 1, # Check suite version at SPEC?
    'version_url'         => "http://www.spec.org/auto/$::lcsuite/current_version",
    'http_timeout'        => 30, # How long to wait for HTTP responses
    'http_proxy'          => '', # HTTP proxy (if any)
    'update-flags'        => 0,
    'review'              => 0, # Format for review?
    'allow_extension_override' => 0,    # Ext with no settings okay?
    'output_root'         => '',
    'expid'               => '',
};

my $version = '$LastChangedRevision: 4655 $ '; # Make emacs happier
$version =~ s/^\$LastChangedRevision: (\d+) \$ $/$1/;
$::tools_versions{'vars.pl'} = $version;

sub initialize_variables {
    my ($config) = @_;

    for (keys %$default_config) {
	$config->{$_} = $default_config->{$_};
    }
    for (keys %$nonvolatile_config) {
	$config->{$_} = $nonvolatile_config->{$_};
    }

    $config->{'runspec'} = join(' ', ($0, @ARGV));

    my $name = '';
    $name = $ENV{'SPECUSER'}     if ($name eq '') && (exists $ENV{'SPECUSER'});
    $name = $ENV{'USER'}         if ($name eq '') && (exists $ENV{'USER'});
    $name = $ENV{'USERNAME'}     if ($name eq '') && (exists $ENV{'USERNAME'});
    $name = $ENV{'LOGNAME'}      if ($name eq '') && (exists $ENV{'LOGNAME'});
    $name = eval q/getlogin || getpwuid $config->{'uid'}/ if ($name eq '');
    $name = $config->{'uid'}     if ($name eq '');
    $name = 'default'            if ($name eq '');
    $config->{'realuser'} = $name;      # Not mungable
    $config->{'username'} = $name;      # May be changed by command line

    # Check to see if OS was specified in environment
    $config->{'OS'} = lc($ENV{'OS'}) if exists($ENV{'OS'}) && ($ENV{'OS'} ne '');
    if ($config->{'OS'} =~ /^windows/) {
	$config->{'os_exe_ext'} = '.exe';
	$config->{'ignore_sigint'} = 1;
    }

    # See where the top of the SPEC tree is
    $config->{'top'} = $ENV{'SPEC'};
    $config->{'top'} = cwd if $ENV{'SPEC'} eq '' || ! -d $ENV{'SPEC'};
    $config->{'specrun'} = jp($config->top, $config->specrun);

    # Check to see if sigint is defined in the Config data
    {
	my @nums = split(" ", $Config{'sig_num'});
	my @names = split(" ", $Config{'sig_name'});
	while (@nums && @names) {
	    my $num = shift @nums;
	    my $name = shift @names;
	    if ($name eq 'INT') {
		$config->{'sigint'} = $num;
		last;
	    }
	}
    }
}

sub finalize_config {
    my ($config, $cl_opts) = @_;
# Command line options override config file options
    for ( keys %$cl_opts) {
	next if $_ eq 'ref' || $_ eq 'refs';
	$config->{$_} = $cl_opts->{$_};
    }

# Make sure none of the unchangeble constants changed
    for ( keys %$nonvolatile_config) {
	$config->{$_} = $nonvolatile_config->{$_};
    }
}

1;

__END__
