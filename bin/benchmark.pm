#
# benchmark.pm
# Copyright (C) 1999-2006 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: benchmark.pm 4647 2006-07-21 20:23:27Z cloyce $
#

package Spec::Benchmark;
use strict;
use File::Path;
use File::Basename;
use File::stat;
use IO::File;
use IO::Dir;
use Cwd;
use IO::Scalar;
use Compress::Bzip2 qw(:all);
use Digest::MD5;
use Carp;
use MIME::Base64;
use UNIVERSAL qw(isa);
use vars '@ISA';

@ISA = (qw(Spec::Config));

my $version = '$LastChangedRevision: 4647 $ '; # Make emacs happier
$version =~ s/^\$LastChangedRevision: (\d+) \$ $/$1/;
$::tools_versions{'benchmark.pm'} = $version;

# List of things to *not* include in the MD5 hash of options
my %option_md5_ignorelist = (
			     'test_sponsor'   =>  'Test Sponsor Company Name',
			     'license_num'    =>  'License Number',
			     'tester'         =>  'Tested By',
			     'test_date'      =>  'Test Date',
			     'config'         =>  'Config',
			     'ext'            =>  'Should not matter',
			     'mach'           =>  'likewise',
			     'abstol' => '',
			     'action' => 'build',
                             'allow_extension_override' => 0,
			     'basepeak' => '0',
                             'output_root' => '',
			     'benchdir' => 'benchspec',
			     'benchmark' => '999.notabenchmark',
			     'binary' => 0,
                             'bind' => [],
			     'bindir' => 'exe',
			     'calctol' => '',
			     'check_integrity' => 1,
			     'delay' => 0,
			     'commanderrfile' => 'speccmds.err',
			     'commandfile' => 'speccmds.cmd',
			     'commandstdoutfile' => 'speccmds.stdout',
			     'commandoutfile' => 'speccmds.out',
			     'compareerrfile' => 'compare.err',
			     'comparefile' => 'compare.cmd',
			     'compareoutfile' => 'compare.out',
			     'comparestdoutfile' => 'compare.stdout',
			     'compwhite' => '',
			     'configdir' => 'config',
			     'datadir' => 'data',
			     'debug' => 0,
			     'deletebinaries' => '0',
			     'deletework' => '0',
			     'difflines' => '10',
			     'dirprot' => '511',
			     'exitvals' => 'spec_exit',
			     'expand_notes' => 0,
			     'endian' => 0,
			     'fail' => 0,
			     'fail_run' => 0,
			     'fail_build' => 0,
			     'fake' => 0,
			     'flags' => '',
			     'flagsurl' => '',
			     'flag_url_base' => "http://www.spec.org/auto/$::lcsuite/flags/",
			     'floatcompare' => '',
                             'graph_min' => '',
                             'graph_max' => '',
                             'graph_auto' => 0,
			     'help' => '0',
			     'ignorecase' => '0',
			     'ignore_errors' => '0',
			     'ignore_sigint' => '0',
			     'inputdir' => 'input',
			     'iterations' => '1',
			     'iterlist' => '1',
			     'line_width' => '0',
			     'locking' => '1',
			     'log' => 'log',
			     'log_line_width' => '0',
			     'lognum' => '016',
			     'logname' => $::suite.'.000.log',
			     'makeflags' => '',
			     'max_active_compares' => '10',
			     'mean_anyway' => '0',
			     'min_report_runs' => '3',
			     'minimize_builddirs' => '0',
			     'minimize_rundirs' => '0',
			     'obiwan' => '',
			     'output' => 'asc',
			     'output_format' => 'default',
			     'outputdir' => 'output',
			     'path' => "/spec/${main::lcsuite}",
                             'nc' => 0,
			     'nobuild' => '0',
                             'expid' => '',
			     'rate' => '0',
			     'rawfile' => '',
			     'rawformat' => 0,
			     'rebuild' => '1',
			     'reftime' => 'reftime',
			     'reltol' => '',
			     'resultdir' => 'result',
			     'run' => 'all',
			     'safe_eval' => '1',
			     'setpgrp_enabled' => '1',
			     'ref_added' => 'never',
			     'section' => 'default:default:default:default',
			     'section_specifier_fatal' => 1,
			     'setprocgroup' => '1',
			     'sigint' => '2,',
			     'size' => 'ref',
			     'skiptol' => '',
			     'skipabstol' => '',
			     'skipreltol' => '',
			     'skipobiwan' => '',
			     'tune' => 'base',
			     'command_add_redirect' => '0',
			     'specrun' => 'specinvoke',
			     'specmake' => 'specmake',
			     'specdiff' => 'specdiff',
			     'srcdir' => 'src',
			     'subworkdir' => 'work',
			     'table' => '1',
			     'teeout' => 'yes',
			     'timefile' => 'time',
			     'top' => "/spec/${main::lcsuite}",
			     'uid' => '95621',
			     'unbuffer' => '0',
			     'realuser' => 'cloyce',
			     'username' => 'cloyce',
			     'clcopies' => '1',
			     'copies' => '1',
			     'use_submit_for_speed' => '0',
			     'vendor' => 'anon',
			     'verbose' => '99',
			     'version' => '0',
			     'workdir' => 'run',
			     'workdirinfo' => 'spec.dir',
			     'worklist' => 'list',
			     'optmd5' => '',
			     'exemd5' => '',
			     'compile_options' => '',
			     'rawcompile_options' => '',
			     'reportable' => 0,
                             'no_input_handler' => 'close',
			     'check_md5' => '',
			     'configpath' => '',
			     'oldmd5' => '',
			     'changedmd5' => '',
			     'backup_config' => '',
			     'mailmethod' => '',
			     'mailto' => '',
			     'mailserver' => '',
			     'mailport' => '',
			     'mailcompress' => '',
			     'sendmail' => '',
			     'mail_reports' => '',
			     'monitor_pre' => '',
			     'monitor_pre_run' => '',
			     'monitor_pre_bench' => '',
			     'monitor_post_bench' => '',
			     'monitor_post' => '',
			     'monitor_pre_perl' => '',
			     'monitor_pre_run_perl' => '',
			     'monitor_pre_bench_perl' => '',
			     'monitor_post_bench_perl' => '',
			     'monitor_post_perl' => '',
			     'monitor_wrapper' => '',
			     'monitor_specrun_wrapper' => '',
			     'no_monitor' => '',
			     'plain_train' => 0,
			     'shrate' => '',
			     'stagger' => '',
			     'valid_build' => 0,
			     'runspec' => '',
                             'check_version'       => 1,
                             'version_url'         => undef,
                             'http_timeout'        => 30,
                             'http_proxy'          => undef,
                             'review'              => 0,
                             'sysinfo_program'     => '',
# Items for benchmarks with special settings that do _NOT_ affect the build
'wrf_data_header_size' => 4,
);

sub new {
    no strict 'refs';
    my ($class, $topdir, $config, $num, $name) = @_;
    my $me       = bless {}, $class;

    $me->{'name'}        = ${"${class}::benchname"};
    $me->{'num'}         = ${"${class}::benchnum"};
    if (!defined(${"${class}::need_math"}) ||
        (${"${class}::need_math"} eq '0') ||
        (lc(${"${class}::need_math"}) eq 'no')) {
        $me->{'need_math'} = '';
    } else {
	$me->{'need_math'} = ${"${class}::need_math"};
    }
    if (@{"${class}::sources"} > 0) {
	$me->{'sources'} = [ @{"${class}::sources"} ];
    } else {
	$me->{'sources'} = { %{"${class}::sources"} };
    }
    $me->{'deps'}        = { %{"${class}::deps"} };
    $me->{'srcdeps'}     = { %{"${class}::srcdeps"} };

    if ($me->{'name'} ne  $name || $me->{'num'} != $num) {
	Log(0, "Benchmark name (".$me->{'num'}.".".$me->{'name'}.") does not match directory name '$topdir'.  Ignoring benchmark\n");
	return undef;
    }

    # Here are the settings that get passed to specdiff.  If these are changed, don't
    # forget to add a new sub for each new item below.
    $me->{'abstol'}      = ${"${class}::abstol"};
    $me->{'reltol'}      = ${"${class}::reltol"};
    $me->{'compwhite'}   = ${"${class}::compwhite"};
    $me->{'floatcompare'}= ${"${class}::floatcompare"};
    $me->{'obiwan'}      = ${"${class}::obiwan"};
    $me->{'skiptol'}     = ${"${class}::skiptol"};
    $me->{'skipabstol'}  = ${"${class}::skipabstol"};
    $me->{'skipreltol'}  = ${"${class}::skipreltol"};
    $me->{'skipobiwan'}  = ${"${class}::skipobiwan"};
    $me->{'binary'}      = ${"${class}::binary"};
    $me->{'ignorecase'}  = ${"${class}::ignorecase"};

    if (!defined(${"${class}::benchlang"}) || ${"${class}::benchlang"} eq '') {
        %{$me->{'BENCHLANG'}} = %{"${class}::benchlang"};
        @{$me->{'allBENCHLANG'}}= ();
        # Fix up the benchlang lists (so that they're lists), and make the
        # full list of all benchlangs
        foreach my $exe (keys %{$me->{'BENCHLANG'}}) {
            if (ref($me->{'BENCHLANG'}->{$exe}) eq 'ARRAY') {
                push @{$me->{'allBENCHLANG'}}, @{$me->{'BENCHLANG'}->{$exe}};
            } else {
                my @langs = split(/[\s,]+/, $me->{'BENCHLANG'}->{$exe});
                $me->{'BENCHLANG'}->{$exe} = [ @langs ];
                push @{$me->{'allBENCHLANG'}}, @langs;
            }
        }
    } else {
        @{$me->{'BENCHLANG'}}= split(/[\s,]+/, ${"${class}::benchlang"});
        @{$me->{'allBENCHLANG'}}= @{$me->{'BENCHLANG'}};
    }
    if ($::lcsuite eq 'cpu2006' &&
        grep { $_ eq 'F77' } @{$me->{'allBENCHLANG'}}) {
      # CPU2006 uses F variables for F77 codes
      push @{$me->{'allBENCHLANG'}}, 'F';
    }

    # Set up the language-specific benchmark flags
    foreach my $blang ('', 'c', 'f', 'cxx', 'f77', 'fpp') {
      if (defined ${"${class}::bench_${blang}flags"}) {
        $me->{'BENCH_'.uc($blang).'FLAGS'} = ${"${class}::bench_${blang}flags"};
      }
    }
    $me->{'benchmark'}   = $me->{'num'}.'.'.$me->{'name'};
    $me->{'path'}        = $topdir;
    $me->{'base_exe'}    = [@{"${class}::base_exe"}];
    $me->{'EXEBASE'}     = [@{"${class}::base_exe"}];
    $me->{'config'}      = $config;
    $me->{'refs'}        = [ $me, $config ];
    $me->{'result_list'} = [ ];
    $me->{'added_files'} = { };
    for (qw( abstol reltol compwhite obiwan skiptol binary
             skipabstol skipreltol skipobiwan
             floatcompare ignorecase )) {
	$me->{$_} = '' if !defined $me->{$_};
    }

    if ($me->{'name'} eq '' || $me->{'num'} eq '' || !@{$me->{'base_exe'}}) {
	return undef;
    }
    $me->{'srcalts'} = { };

    return $me;
}

sub per_file_param_val {
    my ($me, $param, $size, $tune, $file) = @_;
    my $val = $me->{$param};
    my $result;
    if (ref($val) eq 'HASH') {
	if (exists($val->{$size}) && ref($val->{$size}) eq 'HASH') {
	    $val = $val->{$size};
	}
	if (exists($val->{$tune}) && ref($val->{$tune}) eq 'HASH') {
	    $val = $val->{$tune};
	}
	if (exists $val->{$file}) {
	    $result = $val->{$file};
	} elsif (ref ($val->{'default'}) eq 'HASH' && 
		 exists $val->{'default'}{$file}) {
	    $result = $val->{'default'}{$file};
	}
	if (!defined $result) {
	    if (exists $val->{$tune} && ref($val->{$tune}) eq '') {
		$result = $val->{$tune};
	    } elsif (exists $val->{$size} && ref($val->{$size}) eq '') {
		$result = $val->{$size};
	    } elsif (exists $val->{$file} && ref($val->{$file}) eq '') {
		$result = $val->{$file};
	    } else {
		$result = $val->{'default'};
	    }
	}
    } else {
	$result = $val;
    }
    return $result;
}
sub per_file_param {
    my $val = per_file_param_val(@_);
    return istrue($val)?1:undef;
}

sub compwhite    { shift->per_file_param('compwhite', @_); };
sub floatcompare { shift->per_file_param('floatcompare', @_); };
sub abstol       { shift->per_file_param_val('abstol', @_); };
sub reltol       { shift->per_file_param_val('reltol', @_); };
sub obiwan       { shift->per_file_param('obiwan', @_); };
sub skiptol      { shift->per_file_param_val('skiptol', @_); };
sub skipreltol   { shift->per_file_param_val('skipreltol', @_); };
sub skipabstol   { shift->per_file_param_val('skipabstol', @_); };
sub skipobiwan   { shift->per_file_param_val('skipobiwan', @_); };
sub binary       { shift->per_file_param('binary', @_); };
sub ignorecase   { shift->per_file_param_val('ignorecase', @_); };

sub instance {
    my ($me, $config, $tune, $size, $ext, $mach, $copies) = @_;
    my $child = bless { %$me }, ref($me);
    $child->{'config'} = $config;
    $child->{'tune'}  = $tune;
    $child->{'ext'}   = $ext;
    $child->{'size'}  = $size;
    $child->{'mach'}  = $mach;
    $child->{'result_list'} = [];
    $child->{'iteration'} = -1;
    my $bench = $child->benchmark;
    my @sets = $config->benchmark_in_sets($bench);
    $child->{'refs'} = [ $child,
			 reverse ($config,
				  $config->ref_tree('', ['default', @sets, $bench],
						    ['default', $tune],
						    ['default', $ext],
						    ['default', $mach])) ];

    $child->{'srcalts'} = $me->srcalts;
    if (defined($copies) && $copies < 0) {
        # Get the count of copies via the ref tree.
        $copies = $child->accessor_nowarn('copies');
        $copies = ($config->copies)[0] if (!defined($copies));
    }
    $copies = ($config->copies)[0] if ($tune eq 'base'); # The number of
                                # copies is not allowed to vary on a
                                # per-benchmark basis for base runs
    if (defined($copies) && ($copies > 0)) {
        $child->{'copylist'} = [$copies];
    } else {
        $copies = $child->accessor_nowarn('copylist');
        if (defined($copies) && ref($copies) eq 'ARRAY') {
            $child->{'copylist'} = $copies;
        } else {
            $child->{'copylist'} = [ 1 ];
        }
    }
    if ($child->basepeak == 2 &&
	!exists($child->{'basepeak'})) {
	# We've inherited this weird setting from the top level, so ignore
	# it.
	$child->{'basepeak'} = 0;
    } else {
	$child->{'basepeak'} = istrue($child->basepeak);
    }
    if (istrue($child->{'basepeak'})) {
	$child->{'smarttune'} = 'base';
	$child->{'refs'} = [ $child,
			     reverse ($config,
				      $config->ref_tree('', ['default', @sets, $bench],
							['default', 'base'],
							['default', $ext],
							['default', $mach])) ];
    } else {
	$child->{'smarttune'} = $tune;
    }

    return $child;
}

sub descmode {
    my $me = shift;
    return join(' ', $me->benchmark, $me->size, $me->tune, $me->ext, $me->mach);
}

sub input_files_hash {
    my ($me, $size, $showbz2) = @_;
    my $head = jp($me->path, $me->datadir);
    $size = $me->size if ($size eq '');

    my @dirs = ();
    for my $dir (jp($head, $size, $me->inputdir),
                 jp($head, 'all', $me->inputdir)) {
        unshift (@dirs, $dir) if -d $dir;
    }
    my ($files, $dirs) = main::build_tree_hash($me, \%::file_md5, @dirs);

    # Change the names of the compressed files if showbz2 is false, in order
    # to give some clients (like invoke()) a picture of the input files as
    # they'll be presented to the benchmarks.
    if (!$showbz2) {
	foreach my $file (sort keys %$files) {
	    if ($file =~ s/\.bz2$//o) {
		$files->{$file} = $files->{$file.'.bz2'};
		delete $files->{$file.'.bz2'};
	    }
	}
    }

    # In the case of working in the working tree, it's necessary to weed out
    # sources of compressed files.
    foreach my $file (sort keys %$files) {
	if (exists $files->{$file.'.bz2'}) {
	    delete $files->{$file};
	}
    }
    return ($files, $dirs);
}

sub copy_input_files_to {
    my ($me, $fast, $size, @path) = @_;
    my ($files, $dirs) = $me->input_files_hash($size, 1);
    if (!defined($files) || !defined($dirs)) {
	Log(0, "ERROR: couldn't get file list for $size input set\n");
	return 1;
    }

    # Make sure there's something to do
    return 0 if (!@path);

    for my $dir (@path) {
	# Create directories
	main::mkpath($dir, 0, 0755);
	for my $reldir (sort keys %$dirs) {
	    main::mkpath(jp($dir, $reldir), 0, 0755);
	}
    }
    # Copy files
    for my $file (sort keys %$files) {
	if (!main::copy_file($files->{$file}, $file, \@path, !$fast)) {
	    Log(0, "ERROR: couldn't copy $file for $size input set\n");
	    return 1;
	}
    }
    return 0;
}

sub input_files_base {
    my $me = shift;
    my ($hash) = $me->input_files_hash(@_);

    return undef unless ref($hash) eq 'HASH';
    return sort keys %$hash;
}

sub input_files {
    my $me = shift;
    my ($hash) = $me->input_files_hash(@_);

    return undef unless ref($hash) eq 'HASH';
    return sort map { $hash->{$_} } keys %$hash;
}

sub input_files_abs {
    my $me = shift;
    my $head   = jp($me->path, $me->datadir);
    my ($hash) = $me->input_files_hash(@_);

    return undef unless ref($hash) eq 'HASH';
    return sort map { jp($head, $hash->{$_}) } keys %$hash;
}

sub output_files_hash {
    my ($me, $size) = @_;
    my $head = jp($me->path, $me->datadir);

    $size = $me->size if ($size eq '');

    my @dirs = ();
    foreach my $dir (jp($head, $size, $me->outputdir),
                     jp($head, 'all', $me->outputdir)) {
      unshift (@dirs, $dir) if -d $dir;
    }

    return main::build_tree_hash($me, \%::file_md5, @dirs);
}

sub output_files_base {
    my $me = shift;
    my ($hash) = $me->output_files_hash;

    return undef unless ref($hash) eq 'HASH';
    return sort keys %$hash;
}

sub output_files {
    my $me = shift;
    my ($hash) = $me->output_files_hash;

    return undef unless ref($hash) eq 'HASH';
    return sort map { $hash->{$_} } keys %$hash;
}

sub output_files_abs {
    my $me = shift;
    my $head   = jp($me->path, $me->datadir);
    my ($hash) = $me->output_files_hash;

    return undef unless ref($hash) eq 'HASH';
    return sort map { jp($head, $hash->{$_}) } keys %$hash;
}

sub added_files_hash {
  my ($me) = @_;
  if (defined($me->{'added_files'}) && ref($me->{'added_files'}) eq 'HASH') {
    return $me->{'added_files'};
  } else {
    return {};
  }
}

sub added_files_base {
    my $me = shift;
    my ($hash) = $me->added_files_hash;

    return undef unless ref($hash) eq 'HASH';
    return sort keys %$hash;
}

sub added_files {
    my $me = shift;
    my ($hash) = $me->added_files_hash;

    return undef unless ref($hash) eq 'HASH';
    return sort map { $hash->{$_} } keys %$hash;
}

sub added_files_abs {
    my $me = shift;
    my $head   = jp($me->path, $me->datadir);
    my ($hash) = $me->added_files_hash;

    return undef unless ref($hash) eq 'HASH';
    return sort map { jp($head, $hash->{$_}) } keys %$hash;
}

sub exe_files {
    my $me    = shift;
    my $tune  = $me->smarttune;
    my $ext   = $me->ext;
    my $fdocommand = $me->accessor_nowarn('fdocommand');
    if (defined($fdocommand) && ($fdocommand ne '')) {
	return @{$me->base_exe};
    } else {
#	return map { "${_}_$tune$mach.$ext" } @{$me->base_exe};
	return map { "${_}_$tune.$ext" } @{$me->base_exe};
    }
}

sub exe_file {
    my $me = shift;
    return ($me->exe_files)[0];
}

sub exe_files_abs {
    my $me = shift;
    my $path = $me->path;
    if ($me->output_root ne '') {
      my $oldtop = ::make_path_re($me->top);
      my $newtop = $me->output_root;
      $path =~ s/^$oldtop/$newtop/;
    }
    my $subdir = $me->expid;
    $subdir = undef if $subdir eq '';
    my $head   = jp($path, $me->bindir, $subdir);
    return sort map { jp($head, $_) } $me->exe_files;
}

sub reference {
    my $me = shift;
    my $file = jp($me->path, $me->datadir, $me->size, 'reftime');
    if (!-f $file) {
	Log(0, "No file $file\n");
	return 1;
    };
    my $ref = (main::read_file($file))[1];
    chomp($ref);
    if (($me->size eq 'ref') && ($ref == 0)) {
	Log(0, "ref reference time for ".$me->descmode." == 0\n");
	return 1;
    };
    return $ref;
}


sub Log     { main::Log(@_); }
sub jp      { main::joinpaths(@_); }
sub istrue  { main::istrue(@_); }
sub src     { my $me = shift; jp($me->path, $me->srcdir); }
sub apply_diff { main::apply_diff(@_); }
sub md5scalardigest { main::md5scalardigest(@_); }

sub check_md5 {
    my $me = shift;
    return 1 if istrue($me->reportable);
    return $me->accessor_nowarn('check_md5');
}

sub make {
    my $me = shift;
    return 'specmake' if istrue($me->reportable);
    return $me->accessor_nowarn('make');
}

sub make_no_clobber {
    my $me = shift;
    return 0 if istrue($me->reportable);
    return $me->accessor_nowarn('make_no_clobber');
}

# Check to make sure that the input set exists.
sub check_size {
    my ($me) = @_;
    my $datadir = jp($me->path, $me->datadir);
    my $dir = jp($datadir, $me->size);

    return -d $dir
        && ($me->size ne 'ref' || -f jp($dir, $me->reftime))
	&& (-d jp($dir, $me->inputdir) || -d jp($datadir, 'all', $me->inputdir))
        && (-d jp($dir, $me->outputdir) || -d jp($datadir, 'all', $me->outputdir));
}

sub check_exe {
    my ($me) = @_;
    my $check_md5 = istrue($me->check_md5) || istrue($me->reportable);

    # If there are no MD5 hashes then, we will definitely fail the compare
    if ($check_md5 && ($me->accessor_nowarn('optmd5') eq '')) {
        Log(130, "When checking options for ".join(',', $me->exe_files_abs).", no MD5 sums were\n  found in the config file.  They will be installed after build.\n");
	return 0;
    }
    if ($check_md5 && ($me->accessor_nowarn('exemd5') eq '')) {
        Log(130, "When checking executables (".join(',', $me->exe_files_abs)."), no MD5 sums were\n  found in the config file.  They will be installed after build.\n");
	return 0;
    }

    # Build a hash of the executable files
    my $ctx = new Digest::MD5;

    for my $name (sort $me->exe_files_abs) {
        if ((! -e $name) || ($^O !~ /MSWin/i && ! -x $name)) {
          if (!-e $name) {
            Log(190, "$name does not exist\n");
          } elsif (!-x $name) {
            Log(190, "$name exists, but is not executable\n");
            Log(190, "stat for $name returns: ".join(', ', @{stat($name)})."\n") if (-e $name);
          }
          return 0;
        }
	my $fh = new IO::File "<$name";
	if (!defined $fh) {
	    Log (0, "Can't open file '$name' for reading: $!\n");
	} else {
            $ctx->addfile($fh);
        }
    }
    if ($check_md5) {
	my $md5exe = $ctx->hexdigest;
	my $md5opt = option_md5($me);
	if ($md5opt ne $me->accessor_nowarn('optmd5')) {
	    Log(130, "MD5 mismatch for options (stored: ".$me->accessor_nowarn('optmd5').")\n");
	    return 0;
	}
	if ($md5exe ne $me->accessor_nowarn('exemd5')) {
	    Log(130, "MD5 mismatch for executables (stored: ".$me->accessor_nowarn('exemd5').")\n");
	    return 0;
	}
    }
    return 1;
}

sub form_makefiles {
    my $me = shift;
    my %vars;
    my @deps;

    my $tune  = $me->smarttune;
    my $ext   = $me->ext;
    my $bench = $me->benchmark;

    my $srcref = $me->sources;
    my %sources;
    my @targets;

    if (ref($srcref) eq 'ARRAY') {
	$sources{$me->EXEBASE->[0]} = [ @{$srcref} ];
	@targets = ($me->EXEBASE->[0]);
    } else {
	%sources = %{$srcref};
	@targets = sort keys %{$srcref};
    }

    foreach my $exe (@targets) {
	$vars{$exe} = [];
	push @{$vars{$exe}}, "TUNE=". $tune;
	push @{$vars{$exe}}, "EXT=".  $ext;
	# Do the stuff that used to be in src/Makefile
	push @{$vars{$exe}}, "NUMBER=". $me->num;
	push @{$vars{$exe}}, "NAME=". $me->name;
	push @{$vars{$exe}}, ::wrap_join(75, ' ', "\t ", " \\",
					     ('SOURCES=', @{$sources{$exe}}));
	push @{$vars{$exe}}, "EXEBASE=$exe";
	push @{$vars{$exe}}, 'NEED_MATH='.$me->need_math;
        my @benchlang;
        if (ref($me->BENCHLANG) eq 'HASH') {
            if (!exists($me->BENCHLANG->{$exe})) {
                Log(0, "ERROR: No benchlang is defined for target $exe\n");
                do_exit(1);
            }
            if (ref($me->BENCHLANG->{$exe}) eq 'ARRAY') {
                @benchlang = @{$me->BENCHLANG->{$exe}};
            } else {
                @benchlang = split(/[\s,]+/, $me->BENCHLANG->{$exe});
            }
            push @{$vars{$exe}}, 'BENCHLANG='.join(' ', @benchlang);
        } else {
            @benchlang = @{$me->BENCHLANG};
            push @{$vars{$exe}}, 'BENCHLANG='.join(' ', @benchlang);
        }
        # Do ONESTEP (or not)
        foreach my $chance ('ONESTEP', $benchlang[0].'ONESTEP') {
          my $onestep = $me->accessor_nowarn($chance);
          if ($::lcsuite eq 'cpu2006' && $tune eq 'base' && istrue($onestep)) {
            Log(0, "ERROR: $chance is not allowable in a base build.  Ignoring $chance setting\n");
            Log(0, "       for $bench $tune $ext\n");
            push @{$vars{$exe}}, $chance.'=';
          } else {
            push @{$vars{$exe}}, $chance.'='.$onestep;
          }
        }
	push @{$vars{$exe}}, '';

	foreach my $var (sort $me->list_keys) {
	    my $val = $me->accessor($var);
	    # Don't want references in makefile
	    if (ref ($val) eq '' && $var !~ /^(?:(?:pp|raw)txtconfig|oldmd5|cfidx_|toolsver|baggage|compile_options|rawcompile_options|exemd5|optmd5|flags(?:url)?|(?:C|F|F77|CXX)?ONESTEP|ref_added)/) {
		# Escape the escapes
		$val =~ s/\\/\\\\/go;
		$val =~ s/(\r\n|\n)/\\$1/go;
		$val =~ s/\#/\\\#/go;
		push (@{$vars{$exe}}, sprintf ('%-16s = %s', $var, $val));
	    }
	}

	# Add vendor makefile stuff at the end
	if ($me->accessor_nowarn('vendor_makefile') ne '') {
	    push (@{$vars{$exe}}, $me->vendor_makefile);
	}
	$vars{$exe} = join ("\n", @{$vars{$exe}}) . "\n";
    }


    # Add in dependencies, if any
    push @deps, '','# These are the build dependencies', '';
    # Object dependencies are for things like F9x modules which must actually
    # be built before the object in question.
    foreach my $deptarget (keys %{$me->deps}) {
	my $deps = $me->deps->{$deptarget};
        my (@normaldeps, @ppdeps);

        # Coerce the dependencies into a form that we like
        if (ref($deps) eq '') {
	   # Not an array, just a single entry
           $deps = [ $deps ];
        } elsif (ref($deps) ne 'ARRAY') {
	    Log(0, "WARNING: Dependency value for $deptarget is not a scalar or array; ignoring.\n");
            next;
	}

        # Figure out which will need to be preprocessed and which won't
        foreach my $dep (@{$deps}) {
          if ($dep =~ /(\S+)\.F(90|95|)$/o) {
            push @ppdeps, "$1.fppized";
          } else {
            push @normaldeps, $dep;
          }
        }

        # Change the name of the target, if necessary
        my ($ppname, $fulltarget) = ($deptarget, '');
	if ($deptarget =~ /(\S+)\.F(90|95|)$/o) {
          $fulltarget = "$1.fppized";
          $ppname = "$fulltarget.f$2";
        } else {
          $fulltarget = "\$(basename $deptarget)";
        }

        # The end result
        push @deps, "\$(addsuffix \$(OBJ), $fulltarget): $ppname \$(addsuffix \$(OBJ),\$(basename ".join(' ', @normaldeps).") ".join(' ', @ppdeps).")";
    }

    # Source dependencies are for things like #include files for C
    foreach my $deptarget (keys %{$me->srcdeps}) {
	my $deps = $me->srcdeps->{$deptarget};
	if (ref($deps) eq '') {
	    push @deps, "\$(addsuffix \$(OBJ), \$(basename $deptarget)): $deptarget $deps";
	} elsif (ref($deps) eq 'ARRAY') {
	    push @deps, "\$(addsuffix \$(OBJ), \$(basename $deptarget)): $deptarget ".join(' ', @{$deps});
	} else {
	    Log(0, "WARNING: Dependency value for $deptarget is not a scalar or array; ignoring.\n");
	}
    }
    push @deps, '# End dependencies';

    my $deps = join ("\n", @deps) . "\n";
    return ($deps, %vars);
}

sub write_makefiles {
    my ($me, $path, $varname, $depname, $no_write) = @_;
    my @files = ();
    my ($deps, %vars) = $me->form_makefiles;
    my ($filename, $fh);

    if (!$no_write) {
      # Dump the dependencies
      $filename = jp($path, $depname);
      Log (150, "Wrote to makefile '$filename':\n", \$deps);
      $fh = new IO::File;
      if (!$fh->open(">$filename")) {
          Log(0, "Can't write makefile '$filename': $!\n");
          main::do_exit(1);
      }
      $fh->print($deps);
      $fh->close();
      if (-s $filename < length($deps)) {
        Log(0, "\nERROR: $filename is short!\n       Please check for sufficient disk space.\n");
        main::do_exit(1);
      }
    }

    my @targets = sort keys %vars;
    foreach my $target (sort keys %vars) {
	# Dump the variables
	$filename = jp($path, $varname);
	# Benchmarks with a single executable get 'Makefile.spec'; all
	# others get multiple makefiles with the name of the target
	# in the filename.
	if ($target eq $me->baseexe && (@targets+0) == 1) {
	    $filename =~ s/YYYtArGeTYYYspec/spec/;
	} else {
	    $filename =~ s/YYYtArGeTYYYspec/${target}.spec/;
	}
        if (!$no_write) {
          $fh = new IO::File;
          if (!$fh->open(">$filename")) {
              Log(0, "Can't write makefile '$filename': $!\n");
              main::do_exit(1);
          }
          Log (150, "Wrote to makefile '$filename':\n", \$vars{$target});
          $fh->print($vars{$target});
          $fh->close();
          if (-s $filename < length($vars{$target})) {
            Log(0, "\nERROR: $filename is short!\n       Please check for sufficient disk space.\n");
            main::do_exit(1);
          }
        }
	push @files, $filename;
    }
    return @files;
}

sub option_md5 {
    my $me = shift;
    my $md5 = new Digest::MD5;

    # Binaries built using make_no_clobber aren't usable for reportable
    # runs, so make sure that we get a value of '0' for make_no_clobber
    # if reportable is set.
    if (istrue($me->reportable)) {
        $me->{'make_no_clobber'} = 0;
    }

    Log(30, "option_md5 list contains ------------------------------------\n");
    for my $key (sort $me->list_keys) {
        # Don't allow descriptive text
	next if $key =~ m/^(hw_|sw_|notes|submit|cfidx_|prepared_by)/; 
	next if exists $option_md5_ignorelist{$key};
        next if $key =~ /^(?:ref|test|train)addedbytools\d+$/;

	my $val = $me->accessor($key);
	if (($key eq 'srcalt') && ref($val) eq 'ARRAY') {
	    # Make sure that src.alts count
	    $val = join(',', sort @{$val});
	}
	next if ref($val) ne '';    # Don't allow references
        next if ($key =~ /^(?:pp|raw)txtconfig/);
        # Squash whitespace in keys and values
        $key =~ tr/ \012\015\011/ /s;
        $val =~ tr/ \012\015\011/ /s;
	Log(30, "  $key=$val\n");
	$md5->add($key, '=', $val, "\n")
    }
    Log(30, "------------------------------------ end option_md5 list\n");
    return $md5->hexdigest;
}

sub build {
    my ($me, $directory, $setup) = @_;
    my ($fdo);
    my $rc;
    my $bench = $me->benchmark;
    my @pass;
    my $makefile_md5;
    my $valid_build = 1;
    my $compile_options = '';
    my $path = $directory->path;
    my $ownpath = $me->path;
    if ($me->output_root ne '') {
      my $oldtop = ::make_path_re($me->top);
      my $newtop = $me->output_root;
      $ownpath =~ s/^$oldtop/$newtop/;
    }
    my $subdir = $me->expid;
    $subdir = undef if $subdir eq '';
    my $no_clobber = istrue($me->make_no_clobber);

    $valid_build = 0 if istrue($me->fake);

    # Get a pointer to where we update our build status info
    my $md5ref    = $me->config;
    for my $key ($me->benchmark, $me->smarttune, $me->ext, $me->mach) {
	if (!exists $md5ref->{$key} || ref($md5ref->{$key} ne 'HASH')) {
	    $md5ref->{$key} = {};
	}
	$md5ref = $md5ref->{$key};
    }
    $md5ref->{'changedmd5'} = 0;
    my $baggage;
    if (defined($md5ref->{'baggage'})) {
	$baggage = $md5ref->{'baggage'};
    } else {
	$baggage = '';
    }
    $md5ref->{'baggage'} = '';

    if (!istrue($me->fake) && !istrue($me->make_no_clobber)) {
      # First things first, remove any existing binaries with these names,
      # this makes sure that if the build fails any pre-existing binaries are
      # erased
      for my $file ($me->exe_files_abs) {
          if (-f $file && !unlink $file) {
              Log(0, "Can't remove file '$file': $!\n");
              main::do_exit(1);
          }
      }
    }

    if (istrue($me->accessor_nowarn('fail')) ||
        istrue($me->accessor_nowarn('fail_build'))) {
        Log(0, "ERROR: fail or fail_build set for this benchmark\n");
        $me->release($directory);
        $me->compile_error_result('CE', 'failed by request');
        return 1;
    }

    my $langref = {};
    if ($me->smarttune eq 'base') {
	$langref->{'FPBASE'} = 'yes';
    }
    $langref->{'commandexe'} = ($me->exe_files)[0];
    $langref->{'baseexe'} = ($me->base_exe)[0]->[0];
    $me->unshift_ref($langref);

    if ( $setup ||
         ! istrue($me->make_no_clobber) || 
	 ! -f jp ( $path, 'Makefile' )) {
        $no_clobber = 0;        # Must turn this off for makefiles to be made
	if (!rmpath($path)) {	# It's probably not there
            main::mkpath($path);
	}

        if (! -d $me->src()) {
            Log(0, "ERROR: src subdirectory (".$me->src().") for ".$me->benchmark." is missing!\n");
            $me->shift_ref;
            $me->release($directory);
            $me->compile_error_result('CE', 'MISSING src DIRECTORY');
            return 1;
        }
        # Copy the src directory, but leave out the src.alts
        if (!main::copy_tree($me->src(), $directory->path(), undef, [qw(src.alt CVS .svn)], !istrue($me->strict_rundir_verify))) {
            Log(0, "ERROR: src directory for ".$me->benchmark." contains corrupt files.\n");
            Log(0, "       Is your SPEC $::suite distribution corrupt, or have you changed any\n");
            Log(0, "       of the files listed above?\n");
            $me->shift_ref;
            $me->release($directory);
            $me->compile_error_result('CE', 'CORRUPT src DIRECTORY');
            return 1;
        }
        if ($me->pre_build($directory->path(), 1)) {
            Log(0, "ERROR: pre-build setup failed for ".$me->benchmark."\n");
            $me->shift_ref;
            $me->release($directory);
            $me->compile_error_result('CE', 'pre-build FAILED');
            return 1;
        }
        my $srcalts;
        if (ref($me->srcalt) eq 'ARRAY') {
            $srcalts = [ grep { defined($_) && $_ ne '' } @{$me->srcalt} ];
        } elsif (ref($me->srcalt) eq '') {
            if (defined($me->srcalt) && $me->srcalt ne '') {
                $srcalts = [ $me->srcalt ];
            } else {
                $srcalts = [];
            }
        }

# This is where we apply src.alts!
        # This happens in several stages.  First, make sure that all the
        # src.alts that have been asked for are available.
        my $srcalt_applied = 0;
        foreach my $srcalt (@{$srcalts}) {
            if (!exists($me->srcalts->{$srcalt})) {
                Log(103, "ERROR: Requested src.alt ".$me->srcalts->{$srcalt}." does not exist!  Build failed.\n");
                $me->shift_ref;
                $me->release($directory);
                $me->compile_error_result('CE', "src.alt \"".$me->srcalts->{$srcalt}."\" not found");
                return 1;
            }
        }
        my %touched = ();
        # Next, copy all of the _new_ files from all of the src.alts into
        # the source directory.  Don't just copy blindly; do only the ones
        # listed as new in the src.alt.  (This is to not cause errors
        # when testing against the original src.alt directory.)
        # Though it should NOT be possible for a src.alt to modify a file
        # introduced by another (since the file won't have been in the
        # original src directory when the src.alt was made), let's be sort
        # of safe and mark all of the new ones as touched.
        foreach my $srcalt (@{$srcalts}) {
            my $saref = $me->srcalts->{$srcalt};
            my $srcaltpath = jp($me->src(), 'src.alt', $saref->{'name'});
            my $dest = $directory->path();
            foreach my $newfile (grep { m{^benchspec[/\\]}o } sort keys %{$saref->{'filesums'}}) {
                my $shortpath = $newfile;
                $shortpath =~ s{$srcaltpath}{};
                # Each "new" file's path will start will benchspec/
                if (!main::copy_file($newfile, $shortpath, [ $dest ],
                                     $::check_integrity && istrue($me->strict_rundir_verify), $saref->{'filesums'})) {
                    Log(0, "ERROR: src.alt \'$saref->{'name'}\' contains corrupt files.\n");
                    $me->shift_ref;
                    $me->release($directory);
                    $me->compile_error_result('CE', 'CORRUPT src.alt DIRECTORY');
                    return 1;
                } else {
                    $touched{jp($dest, $shortpath)}++;
                }
            }
        }

        # Now that all the files have been copied in, apply the diffs to
        # the existing files.
        foreach my $srcalt (@{$srcalts}) {
            my $saref = $me->srcalts->{$srcalt};
            my $srcaltpath = jp($me->src(), 'src.alt', $saref->{'name'});
            my $dest = $directory->path();
            foreach my $difffile (sort keys %{$saref->{'diffs'}}) {
                my $difftext = decode_base64($saref->{'diffs'}->{$difffile});
                if ($::check_integrity) {
                    my $diffsum = md5scalardigest($difftext);
                    if ($diffsum ne $saref->{'diffsums'}->{$difffile}) {
                        Log(0, "ERROR: src.alt \'$saref->{'name'}\' contains corrupt difference information.\n");
                        $me->shift_ref;
                        $me->release($directory);
                        $me->compile_error_result('CE', 'CORRUPT src.alt CONTROL FILE DIFFS (SUMS)');
                        return 1;
                    }
                }
                my $hunks;
                eval $difftext;
                if ($@) {
                    Log(0, "ERROR: src.alt \'$saref->{'name'}\' has corrupted control file.\n");
                    $me->shift_ref;
                    $me->release($directory);
                    $me->compile_error_result('CE', 'CORRUPT src.alt CONTROL FILE DIFFS (SYNTAX)');
                    return 1;
                }

                my ($newsum, $offset, $ok) = apply_diff(jp($dest, $difffile), $hunks);
                # Application failed if application of diff
                # 1. failed (duh)
                # 2. succeeded with offset and file not previously touched
                # 3. succeeded with no offset and MD5 mismatch
                if (!$ok ||
                    (!$touched{$difffile} && ($offset ||
                    ($newsum ne $saref->{'filesums'}->{$difffile})))) {
                    if (!$ok) {
                        Log(0, "ERROR: application of diff failed\n");
                    } elsif (!$touched{$difffile} && $offset) {
                        Log(0, "ERROR: diff application offsets needed for previously untouched file\n");
                    } elsif (!$touched{$difffile} && ($newsum ne $saref->{'filesums'}->{$difffile})) {
                        Log(0, "ERROR: MD5 sum mismatch for previously untouched file\n");
                    }
                    Log(0, "ERROR: application of src.alt \'$saref->{'name'}\' failed!\n");
                    $me->shift_ref;
                    $me->release($directory);
                    $me->compile_error_result('CE', 'src.alt APPLICATION FAILED');
                    return 1;
                }
                $touched{$difffile}++;
            }
            # If we get to here, the src.alt was applied successfully
            my $tmpstr = $me->benchmark.' ('.$me->tune."): Applied ".$saref->{'name'}." src.alt.";
            if ($md5ref->{'baggage'} !~ /\Q$tmpstr\E/) {
                if ($md5ref->{'baggage'} ne '' &&
                    $md5ref->{'baggage'} !~ /\n$/) {
                    $md5ref->{'baggage'} .= "\n";
                }
                $md5ref->{'baggage'} .= $tmpstr;
            }
            Log(0, "$tmpstr\n");
        }
	my $origmakefile = jp($me->src,"Makefile.${main::lcsuite}");
	$origmakefile = jp($me->src,'Makefile') if (!-f $origmakefile);
	if (!main::copy_file($origmakefile, 'Makefile', [$path], istrue($me->strict_rundir_verify))) {
	  Log(0, "ERROR: Failed copying makefile into build directory!\n");
	  $me->shift_ref;
	  $me->release($directory);
	  $me->compile_error_result('CE', 'Build directory setup FAILED');
	  return 1;
	}
    } else {
	$valid_build = 0;
    }

    if (!chdir($path)) {
	Log(0, "Couldn't chdir to $path: $!\n");
    }

    main::monitor_shell('build_pre_bench', $me);

    my @makefiles = $me->write_makefiles($path, $me->specmake,
					 'Makefile.deps', $no_clobber);
    my @targets = map { basename($_) =~ m/Makefile\.(.*)\.spec/o; $1 } @makefiles;

    if ($setup) {
      $me->release($directory);
      return 0;
    }

    my $compile_start = time;  ## used by the following log statement
    Log(160, "  Compile for '$bench' started at: ".::ctime($compile_start)." ($compile_start)\n");

    my $make = $me->make;
    $make .= ' -n' if istrue($me->fake);
    $make .= ' '.$me->makeflags if ($me->makeflags ne '');
# Check to see if feedback is being used
    if (istrue($me->feedback)) {
	for ($me->list_keys) {
	    if (m/^fdo_\S+?(\d+)/    && $me->accessor_nowarn($&) ne '') { 
		$pass[$1] = 1; $fdo = 1; 
	    }
	    if (m/^PASS(\d+)_(\S+)/ && $me->accessor_nowarn($&) ne '')    { 
		my ($pass, $what) = ($1, $2);
		$what =~ m/^(\S*)(FLAGS|OPTIMIZE)/;
		if ($1 ne 'LD' && !grep { $_ eq $1 } @{$me->allBENCHLANG}) {
		    next;
		}
		$pass[$pass] = 1; $fdo = 1; 
	    }
	}
    }

    # Feedback is only allowed in peak
    if ($fdo && $me->smarttune eq 'base') {
        Log(0, "ERROR: Feedback-directed optimization is not allowed for base tuning\n");
        $me->release($directory);
        $me->compile_error_result('CE', 'FDO used for base build');
        return 1;
    }

# Set up some default values for FDO, don't set these if the user has
# overridden them
    my (@commands) = ('fdo_pre0');
    my $tmp = {
	    'fdo_run1'         => '$command',
	};
    for (my $i = 1; $i < @pass; $i++) {
	if ($pass[$i]) {
	    if ($i != 1) {
              foreach my $target (@targets) {
                my $targetflag = ($target ne '') ? " TARGET=$target" : '';
		$tmp->{"fdo_make_clean_pass$i"} = "$make fdoclean FDO=PASS$i$targetflag";
              }
	    }
	    if (($i < (@pass-1)) && !exists($tmp->{"fdo_run$i"})) {
		$tmp->{"fdo_run$i"} = '$command';
	    }
	    push (@commands, "fdo_pre_make$i", "fdo_make_clean_pass$i");
	    foreach my $target (@targets) {
		my $exe = ($target ne '') ? "_$target" : '';
                my $targetflag = ($target ne '') ? " TARGET=$target" : '';
		$tmp->{"fdo_make_pass${i}${exe}"} ="$make build FDO=PASS$i$targetflag";
		push @commands, "fdo_make_pass${i}${exe}";
	    }
	    foreach my $thing ("fdo_make_pass$i", "fdo_post_make$i",
			       "fdo_pre$i", "fdo_run$i", "fdo_post$i") {
		if (!grep { /^$thing/ } @commands) {
		    push @commands, $thing;
		}
	    }
	}
    }
    $me->push_ref($tmp);
    my %replacements = (
	    'benchmark' => $me->benchmark,
	    'benchtop'  => $me->path,
	    'benchnum'  => $me->num,
	    'benchname' => $me->name,
	    'spectop'   => $me->top,
    );

    foreach my $target (@targets) {
        next if istrue($no_clobber);
	my $targetflag = ($target ne '') ? " TARGET=$target" : '';
	my $file = ($target ne '') ? "make.clean.$target" : 'make.clean';
	if (main::log_system("$make clean$targetflag", $file, 0, $me, \%replacements)) {
	    $tmp = "Error with make clean!\n";
	    Log(0, "  $tmp") if $rc;
	    $me->pop_ref;
	    $me->shift_ref;
	    $me->release($directory);
	    $me->compile_error_result('CE', $tmp);
	    return 1;
	}
    }

    if ($fdo) {
	my $reason = undef;
	$me->unshift_ref({ 'size'          => 'train' });
	$rc = $me->copy_input_files_to(!istrue($me->strict_rundir_verify), 'train', $path);
        if ($me->post_setup($path)) {
            Log(0, "training post_setup for " . $me->benchmark . " failed!\n");
        };

	$me->shift_ref();
	if ($rc) {
	    Log(0, "  Error setting up training run!\n");
	    $reason = 'FE';
	} else {
	    for my $cmd (@commands) {
		my $val = $me->accessor_nowarn($cmd);
		my $pass = '';
                my $target = '';
                if ($cmd =~ m/^fdo_make_pass(\d+)_(.+)$/) {
                  $pass = $1;
                  $target = $2;
                } elsif ($cmd =~ m/(\d+)$/) {
                  $pass = $1;
                }
		next if $val =~ m/^\s*$/;
		if ($cmd =~ /^fdo_run/) {
		    $me->unshift_ref({
			'size'          => 'train',
			'dirlist'       => [ $directory ],
			'fdocommand'    => $val });
		    Log(3, "Training ", $me->benchmark, "\n");
		    $rc = $me->run_benchmark(1, 0, 1, undef, 1);
		    $me->shift_ref();
		    $compile_options .= "RUN$pass: $val\n";
		    if ($rc->{'valid'} ne 'S' && !istrue($me->fake)) {
			Log(0, "  Error with train!\n");
			$reason = 'FE';
			last;
		    }
		} else {
                    my $really_fake = 0;
                    my $fake_cmd = substr($val, 0, 35);
                    $fake_cmd .= '...' if length($fake_cmd) >= 35;
                    $fake_cmd = "$cmd ($fake_cmd)";
                    if (istrue($me->fake) && $val !~ /$make/) {
                      $really_fake = 1;
                      Log(0, "\n%% Fake commands from $fake_cmd:\n");
                    }
		    $rc = main::log_system($val, $cmd, $really_fake,
					   $me, \%replacements);
                    Log(0, "%% End of fake output from $fake_cmd\n\n") if $really_fake;

		    $compile_options .= "$cmd: $val\n";
		    if (($rc == 0) && $cmd =~ m/^fdo_make_pass/) {
                        # Since only one target was built, it's not necessary
                        # to generate options for all of them.
                        my $targetflag = ($target ne '') ? " TARGET=$target" : '';
                        my $file = "options$pass";
                        $file .= ".$target" if ($target ne '');
                        $rc = main::log_system("$make options$targetflag FDO=PASS$pass",
                                               $file,
                                               $really_fake,
                                               $me, \%replacements);
                        my $fh = new IO::File "<${file}.out";
                        if (defined $fh) {
                            while (<$fh>) {
                                if ($^O =~ /MSWin/) {
                                    # Strip out quotes that Windows echo left in
                                    s/^"//;
                                    s/"$//;
                                    s/\\"/"/;
                                    s/\\"(?!.*\\")/"/;
                                }
                                # Knock out unused variables (shouldn't be any)
                                next if (/^[CPO]: _/o);
                                # Ignore empty variables
                                next if (m/^[CPO]: \S+="\s*"$/o);
                                # Fix up "funny" compiler variables
                                s/^C: (CXX|F77)C=/C: $1=/o;
                                # Add the current pass number
                                s/:/$pass:/;
                                $compile_options .= $_;
                            }
                            $fh->close();
                        }
                        if ($rc && !istrue($me->fake)) {
                            Log(0, "  Error with $cmd!\n");
                            $reason = 'CE';
                            last;
                        }
		    } elsif ($rc && !istrue($me->fake)) {
                        $reason = 'CE';
                        last;
                    }
		}
	    }
	    if ($rc && !istrue($me->fake)) {
		$me->pop_ref;
		$me->shift_ref;
		$me->release($directory);
		$me->compile_error_result($reason, $tmp);
		log_finish($bench, $compile_start);
		return 1;
	    }
	}
    } else {
	foreach my $target (@targets) {
	    my $targetflag = ($target ne '') ? " TARGET=$target" : '';
	    my $exe = ($target ne '') ? ".$target" : '';
	    $rc = main::log_system("$make build$targetflag", "make$exe", 0, $me, \%replacements);
	    last if $rc;
	    if (!$rc || istrue($me->fake)) {
		$rc = main::log_system("$make options$targetflag", "options$exe", 0, $me, \%replacements);
		last if $rc;
		my $fh = new IO::File "<options${exe}.out";
		if (defined $fh) {
		    while (<$fh>) {
			if ($^O =~ /MSWin/) {
			    # Strip out quotes that Windows echo left in
			    s/^"//;
			    s/"$//;
			    s/\\"/"/;
			    s/\\"(?!.*\\")/"/;
			}
                        # Knock out unused variables (shouldn't be any)
                        next if (/^[CPO]: _/o);
                        # Ignore empty variables
			next if (m/^[CPO]: \S+="\s*"$/o);
                        # Fix up "funny" compiler variables
                        s/^C: (CXX|F77)C=/C: $1=/o;
			$compile_options .= $_;
		    }
		    $fh->close();
		}
	    }
	}

	if ($rc && !istrue($me->fake)) {
	    $tmp = "Error with make!\n";
	    Log(0, "  $tmp") if $rc;
	    $me->pop_ref;
	    $me->shift_ref;
	    $me->release($directory);
	    $me->compile_error_result('CE', $tmp);
            log_finish($bench, $compile_start);
	    return 1;
	}
    }

    main::monitor_shell('build_post_bench', $me);

    $me->pop_ref;
    $me->shift_ref;

    log_finish($bench, $compile_start);

    my @unmade = ();
    my $os_ext = $me->os_exe_ext;
    for my $name (@{$me->base_exe}) {
	if (! -x $name && ! -x "$name$os_ext") {
          my $tmpfname = ( -e $name ) ? $name : ( -e "$name$os_ext" ) ? "$name$os_ext" : "DOES NOT EXIST";
          Log(90, "$tmpfname exists, but ") if (-e $tmpfname);
          Log(90, "$tmpfname is not executable\n");
          Log(99, "stat for $tmpfname returns: ".join(', ', stat($tmpfname))."\n");
          push (@unmade, $name);
        }
    }
    if (@unmade && !istrue($me->fake)) {
	my $tmp = "Some files did not appear to be built: ". join(', ',@unmade). "\n";
	Log(0, "  $tmp");
	$me->release($directory);
	$me->compile_error_result('CE', $tmp);
	return 1;
    }


    # Well we made it all the way here, so the executable(s) must be built
    # But are they executable? (Thank you, HP-UX.)
    # Copy them to the exe directory if they are.
    my $tune  = $me->smarttune;
    my $ext   = $me->ext;

    my $ctx = new Digest::MD5;
    my $head = jp($ownpath, $me->bindir, $subdir);
    if ( ! -d $head ) {
	main::mkpath($head, 0, 0777);
    }
    for my $name (sort @{$me->base_exe}) {
        if (! -x $name && ! -x "$name$os_ext" && !istrue($me->fake)) {
          my $tmpfname = ( -e $name ) ? $name : ( -e "$name$os_ext" ) ? "$name$os_ext" : "DOES NOT EXIST";
          if (-e $tmpfname) {
            Log(5, "$tmpfname exists, but is not executable.  Skipping...\n"); 
          }
          next;
        }
	my $sname = $name;
	$sname .= $os_ext if ! -f $name && -f "$name$os_ext";
	if (!istrue($me->fake) &&
	    !main::copy_file($sname, "${name}_$tune.$ext", [$head], istrue($me->strict_rundir_verify))) {
	  Log(0, "ERROR: Copying executable from build dir to exe dir FAILED!\n");
	  $me->release($directory);
	  $me->compile_error_result('CE', $tmp);
	  return 1;
	}
	my $fh = new IO::File "<$sname";
	if (!defined $fh) {
	    Log(0, "Can't open file '$sname': $!\n") unless istrue($me->fake);
	} else {
            $ctx->addfile($fh);
        }
    }
    my $md5exe = $ctx->hexdigest;

    $md5ref->{'valid_build'} = $valid_build ? 'yes' : 'no';
    ($md5ref->{'rawcompile_options'}, undef, $md5ref->{'compile_options'}) = 
        main::compress_encode($compile_options);
    my $md5opt = option_md5($me);

    if ($md5ref->{'optmd5'} ne $md5opt) {
	$md5ref->{'optmd5'} = $md5opt;
	$md5ref->{'changedmd5'}++;
    }
    if ($md5ref->{'exemd5'} ne $md5exe) {
	$md5ref->{'exemd5'} = $md5exe;
	$md5ref->{'changedmd5'}++;
    }
    if ($md5ref->{'baggage'} ne $baggage) {
	$md5ref->{'changedmd5'}++;
    }

    # Do _not_ overwrite possibly good executable signatures when faking
    # a run.
    $md5ref->{'changedmd5'} = 0 if istrue($me->fake);

    $me->{'dirlist'} = [] unless (ref($me->{'dirlist'}) eq 'ARRAY');
    if ((istrue($me->minimize_rundirs) && ($directory->{'type'} eq 'run')) ||
	(istrue($me->minimize_builddirs) && ($directory->{'type'} eq 'build'))) {
	push @{$me->{'dirlist'}}, $directory;
    } else {
	$me->release($directory);
    }

    return 0;
}

sub log_finish {
    my ($bench, $compile_start) = @_;

    my $compile_finish = time;  ## used by the following log statement
    my $elapsed_time = $compile_finish - $compile_start;
    ::Log(160, "  Compile for '$bench' ended at: ".::ctime($compile_finish)." ($compile_finish)\n");
    ::Log(160, "  Elapsed compile for '$bench': ".::to_hms($elapsed_time)." ($elapsed_time)\n");
}

sub compile_error_result {
    my $me = shift @_;
    my $result = Spec::Config->new(undef, undef, undef, undef);

    $result->{'valid'}     = shift(@_);
    $result->{'errors'}    = [ @_ ];
    $result->{'tune'}      = $me->tune;
    $result->{'mach'}      = $me->mach;
    $result->{'ext'}       = $me->ext;
    $result->{'benchmark'} = $me->benchmark;
    $result->{'reference'} = $me->reference;

    $result->{'reported_sec'}  = '--';
    $result->{'reported_nsec'} = '--';
    $result->{'reported_time'} = '--';
    $result->{'ratio'}         = '--';
    $result->{'selected'}  = 0;
    $result->{'iteration'} = -1;
    $result->{'basepeak'}  = 0;
    $result->{'copies'}    = 1;
    $result->{'rate'}      = 0;

    push (@{$me->{'result_list'}}, $result);

    # Remove the options read in from the MD5 section in the config file
    # (if any); they're not valid for this failed build.
    my $md5ref    = $me->config;
    for my $key ($me->benchmark, $me->smarttune, $me->ext, $me->mach) {
	if (!exists $md5ref->{$key} || ref($md5ref->{$key} ne 'HASH')) {
	    $md5ref->{$key} = {};
	}
	$md5ref = $md5ref->{$key};
    }
    delete $md5ref->{'compile_options'};
    delete $md5ref->{'rawcompile_options'};

    return $me;
}

sub link_rundirs {
    my ($me, $owner) = @_;
    $me->{'dirlist'} = $owner->{'dirlist'};
    $me->{'dirlist_is_copy'} = 1;
}

sub setup_rundirs {
    my ($me, $numdirs) = @_;
    my $rc;
    my $tune  = $me->smarttune;
    my $ext   = $me->ext;
    my $mach   = $me->mach;
    my $size   = $me->size;
    my $nodel  = exists($ENV{"SPEC_${main::suite}_NO_RUNDIR_DEL"}) ? 1 : 0;
    my (@dirs) = $me->reserve($nodel, $numdirs,
			      'type'=>'run', 'tune'=>$tune, 'ext' => $ext,
			      'mach' => $mach, 'size'=>$size,
			      'username' => $me->username);
    if (!@dirs) {
       Log(0, "\nERROR: Could not reserve run directories!\n");
       return undef;
    }

    my $sizepath = jp($me->path, $me->datadir, $me->size, 'input');
    my $allpath  = jp($me->path, $me->datadir, 'all', 'input');

    # Quick check for "bad" directories
    for my $dir (@dirs) {
	# They're bad if we say they are
	if (istrue($me->deletework)) {
	    $dir->{'bad'} = 1;
	}
	# Any directories that don't exist are obviously bad
	if (!-d $dir->path) {
	    main::mkpath($dir->path);
	    $dir->{'bad'} = 1;
	}
    }

    # Check to see which directories are ok
    my $fast = !istrue($me->strict_rundir_verify) || istrue($me->fake);
    my @input_files = $me->input_files_abs($me->size, 1);
    if (@input_files+0 == 0 ||
	!defined($input_files[0])) {
	Log(0, "Error during setup for ".$me->benchmark.": No input files!?\n");
	return(undef);
    }

    # This _only_ checks to see whether files are okay or not
    for my $reffile (@input_files) {
	next if istrue($me->fake);
	# We can't just do basename here, because sometimes there are files
	# in subdirs
	my $refsize  = stat($reffile)->size;
        my $isbz2 = 0;
        $isbz2 = 1 if ($reffile =~ s/\.bz2$//);
	my $short    = $reffile;
	my $refdigest = $::file_md5{$short};
	$short       =~ s%^($sizepath|$allpath)/%%i;
        if (!$isbz2) {
	    if (!$fast && $refdigest eq '') {
		$refdigest = main::md5filedigest($reffile);
		$::file_md5{$reffile} = $refdigest;
	    }
        } else {
	    if (exists($::file_md5{$reffile})) {
		$refdigest = $::file_md5{$reffile};
	    }
	    if (exists($::file_size{$reffile}) && $refdigest ne '') {
		$refsize = $::file_size{$reffile};
	    } else {
		# Uncompress the file
		# This should not happen, because the MD5 of the uncompressed
		# file should be in SUMS.data.
		Log(10, "MD5 hash of ${reffile} being generated.  SUMS.data be damaged or incomplete.\n");
		my $bz = bzopen("${reffile}.bz2", 'rb');
		if (defined($bz)) {
		    my $tmp = '';
		    my ($size, $sum) = (0, undef);
		    my $md5 = new Digest::MD5;
		    while ($bz->bzread($tmp, 262144) > 0) {
			$size += length($tmp);
			$md5->add($tmp);
		    }
		    $size += length($tmp);
		    $md5->add($tmp);
		    my $rc = $bz->bzerror + 0;
		    $bz->bzclose();
		    if ($rc == BZ_STREAM_END || $rc == BZ_OK) {
			# Since the decompression takes by far the longest time,
			# the MD5 hash will be calculated for later use even if
			# $fast is set.
			$::file_size{$reffile} = $refsize = $size;
			$::file_md5{$reffile} = $refdigest = $md5->hexdigest if ($refdigest eq '');
		    }
		}
	    }
	}

	for my $dir (@dirs) {
	    next if $dir->{'bad'} || istrue($me->fake);
	    my $target = jp($dir->path, $short);
	    if (!-f $target) {
		Log(10, "$short not found in run dir ".$dir->path."; marking as bad.\n");
		$dir->{'bad'} = 1;
	    } elsif (-s $target != $refsize) {
		Log(10, "Size of $short does not match reference; marking rundir as bad.\n");
		$dir->{'bad'} = 1;
	    } elsif (!$fast) {
                if (istrue($me->strict_rundir_verify)) {
                    Log(10, "Doing REALLY slow MD5 tests for $target\n");
                } else {
                    Log(10, "Doing slow MD5 tests for $target\n");
                }
                my $shortmd5 = main::md5filedigest($target);
                if ($refdigest ne $shortmd5) {
                    Log(10, "MD5 sum of $short does not match cached sum of reference file; rundir is bad.\n");
                    $dir->{'bad'} = 1;
                } elsif (istrue($me->strict_rundir_verify) &&
                         main::md5filedigest($reffile) ne $shortmd5) {
                    Log(10, "MD5 sum of $short does not match reference; marking rundir as bad.\n");
                    $dir->{'bad'} = 1;
                }
            }
	}
    }

    # Remove output and other files from directories which are ok
    for my $dir (@dirs) {
	next if $dir->{'bad'};
	my $basepath = $dir->path;
	$dir->{'bad'} = $me->clean_single_rundir($basepath, 0);
	my $dh = new IO::Dir $dir->path;
	if (!defined $dh) {
	    $dir->{'bad'} = 1;
	    next;
	}
	# This should never have anything to do.
	while (defined(my $file = $dh->read)) { 
	    next if ($file !~ m/\.(out|err|cmp|mis)$/);
	    my $target = jp($dir->path, $file);
	    if (!unlink ($target)) {
		$dir->{'bad'} = 1;
		last;
	    }
	}
    }

    my $needed_setup = 0;
    my @dirnum = ();
    # Now rebuild all directories which are not okay
    my @copy_dirs = ();
    for my $dir (@dirs) {
	my $path = $dir->path();
	push @dirnum, File::Basename::basename($path);

	next if !$dir->{'bad'};

	$needed_setup = 1;
	delete $dir->{'bad'};
	if (!rmpath($path)) {
	    main::mkpath($path);
	}
	push @copy_dirs, $path;
    }

    # Copy input files to dirs that need them
    if (@copy_dirs) {
	if ($me->copy_input_files_to($fast, $me->size, @copy_dirs)) {
	    Log(0, "ERROR: Copying input files to run directory FAILED\n");
	    Log(0, "\nError during setup of ".$me->benchmark."\n");
	    return(undef);
	}
    }

    # Copy executables to first directory
    if (!istrue($me->fake)) {
	for my $file ($me->exe_files_abs) {
	  if (!main::copy_file($file, undef, [$dirs[0]->path], istrue($me->strict_rundir_verify))) {
	    Log(0, "ERROR: Copying executable to run directory FAILED\n");
	    Log(0, "\nError during setup of ".$me->benchmark."\n");
	    return(undef);
	  }
	}
    }

    if ($me->post_setup(map { $_->path } @dirs)) {
      Log(0, "ERROR: post_setup for " . $me->benchmark . " failed!\n");
      Log(0, "\nError during setup of ".$me->benchmark."\n");
      return(undef);
    };

    $me->{'dirlist'} = [ @dirs ];
    return ($needed_setup, @dirnum);
}

sub cleanup_rundirs {
  my ($me, $numdirs) = (shift @_, shift @_);
  my $rc = 0;

  return 0 if istrue($me->fake);

  $numdirs = @{$me->{'dirlist'}}+0 if ($numdirs <= 0);
  my (@dirs) = @_;


  for (my $i = 0; $i < $numdirs; $i++) {
    my $dir = $me->{'dirlist'}[$i]->path;
    for my $file ($me->exe_files_abs) {
      my $fullpath = jp($dir, basename($file));
      if (-e $fullpath) {
	# Make an effort to make the executable be less-easily identifiable
	# as the _same_ executable that we used last time.
	rename $fullpath, "${fullpath}.used.$$";
	my $ofh = new IO::File ">${fullpath}.used.$$";
	$ofh->print("#!/bin/sh\necho This is a non-functional placeholder\n");
	# Make sure that we maintain an open filehandle to the placeholder,
	# so that when the file is unlinked its inode doesn't get reallocated.
	push @{$me->{'open_files'}}, $ofh;
	# clean_single_rundir will take care of the placeholder file for us
	if (!main::copy_file($file, undef, [$dir], 1)) {
	  Log(0, "ERROR: Copying executable to run directory FAILED in cleanup_rundirs\n");
	  return 1;
	}
      }
    }
    $rc |= $me->clean_single_rundir($dir, 1);
    push @dirs, $dir;
  }
  if (!$rc) {
    # All the files are copied now, so go ahead and kill the temps
    my $fh = shift @{$me->{'open_files'}};
    while(defined($fh) && ref($fh) eq 'IO::File') {
        $fh->close();
        $fh = shift @{$me->{'open_files'}};
    }
    foreach my $dir (@dirs) {
      $rc |= $me->clean_single_rundir($dir, 0);
    }
  }
  return $rc;
}

sub clean_single_rundir {
  my ($me, $basepath, $presetup) = @_;
  my $sizepath = jp($me->path, $me->datadir, $me->size, 'input');
  my $allpath  = jp($me->path, $me->datadir, 'all', 'input');
  my @tmpdir = ($basepath);
  my @files = ();

  while (defined(my $curdir = shift(@tmpdir))) {
    my $dh = new IO::Dir $curdir;
    next unless defined $dh;
    foreach my $file ($dh->read) {
      next if ($file eq '.' || $file eq '..');
      $file = jp($curdir, $file);
      if ( -d $file ) {
	push @tmpdir, $file;
      } else {
	push @files, $file;
      }
    }
  }
  # Strip the top path from the list of files we just discovered
  @files = sort map { s%^$basepath/%%i; jp(dirname($_), basename($_, '.bz2')) } @files;

  # Make a list of the files that are allowed to be in a run directory
  # before a run starts.  This could be (and was) done as a much more
  # concise and confusing one-liner using map.  Hey, not everyone has
  # 1337 p3r1 skillz.
  my %okfiles = ();
  foreach my $okfile ($me->exe_files,
		      $me->input_files_base,
		      $me->added_files_base) {
    $okfile =~ s%^($sizepath|$allpath)/%%i;
    $okfiles{jp(dirname($okfile), basename($okfile, '.bz2'))}++;
  }

  # The "everything not mandatory is forbidden" enforcement section
  for my $reffile (@files) {
    next if exists($okfiles{$reffile});
    next if ($presetup && $reffile =~ /\.used\.${$}$/);
    my $target = jp($basepath, $reffile);
    next if !-f $target;
    if (!unlink($target)) {
        Log(0, "\nERROR: Failed to unlink $target\n");
        return 1;
    }
  }

  return 0;
}

sub rmpath {
    # Remove the contents of a given directory.  Doesn't actually remove the
    # directory itself.
    my ($path) = @_;
    # Remove the contents of the given path
    my $dh = new IO::Dir $path;
    return 0 if (!defined $dh);	# Fail quietly; there's nothing to do.
    while (defined($_ = $dh->read)) { 
	my $target = jp($path, $_);
	if (-d $target) {
	    next if ($target =~ /\/\.{1,2}/o);
	    rmpath($target);
	    rmdir($target);
	} else {
	    if (!unlink ($target)) {
		Log(0, "Couldn't unlink $target - tree removal aborted\n");
		return 0;
	    }
	}
    }
    return 1;
}

sub delete_binaries {
    my ($me, $all) = @_;
    my $path = $me->path;
    if ($me->output_root ne '') {
      my $oldtop = ::make_path_re($me->top);
      my $newtop = $me->output_root;
      $path =~ s/^$oldtop/$newtop/;
    }
    my $subdir = $me->expid;
    $subdir = undef if $subdir eq '';

    my $head = jp($path, $me->bindir, $subdir);
    if ($all) {
	rmpath($head);
    } else {
	my $tune  = $me->smarttune;
	my $ext   = $me->ext;
#	my $mach  = $me->mach;
#	if ($mach eq 'default') {
#	    $mach = '';
#	} elsif ($mach ne '') {
#	    $mach = "_$mach";
#	}
	for my $name (@{$me->base_exe}) {
#	    unlink(jp($head, "${name}_$tune$mach.$ext"));
	    unlink(jp($head, "${name}_$tune.$ext"));
	}
    }
}

sub delete_rundirs {
    my ($me, $all) = @_;
    my $path = $me->{'path'};
    my $top = $me->top;
    if ($me->output_root ne '') {
      my $oldtop = ::make_path_re($top);
      $top = $me->output_root;
      $path =~ s/^$oldtop/$top/;
    }
    my $subdir = $me->expid;
    $subdir = undef if $subdir eq '';

    my @attributes = ();

    if ($all) {
	my $dir = jp($path, $me->workdir, $subdir);
	rmpath($dir);
    } else {
	@attributes = ([
	    'username'=>$me->username, 'size'=>$me->size, 'ext'=>$me->ext,
	    'tune'=>$me->smarttune, 'mach'=>$me->mach,
	], [
	    'username'=>$me->username, 'type'=>'build', 'ext'=>$me->ext,
	]);

	my $file = $me->lock_listfile();
	my $entry;
	for my $attr (@attributes) {
	    while (1) {
		$entry = $file->find_entry($top, @$attr);
		last if !$entry;
		rmpath($entry->path);
		rmdir($entry->path);
		$entry->remove();
	    }
	}
	$file->update();
	$file->close();
    }
}

sub remove_rundirs {
    my ($me) = @_;

    if ($me->{'dirlist_is_copy'}) {
	delete $me->{'dirlist_is_copy'};
    } else {
	if (ref($me->{'dirlist'}) eq 'ARRAY') {
	    my @dirs = @{$me->{'dirlist'}};
	    for my $dirobj (@dirs) {
		rmpath($dirobj->path);
	    }
	    $me->release(@dirs);
	} else {
	    Log(3, "No list of directories to remove for ".$me->descmode."\n");
	}
    }
    $me->{'dirlist'} = [];
}

sub release_rundirs {
    my ($me) = @_;

    if ($me->{'dirlist_is_copy'}) {
	delete $me->{'dirlist_is_copy'};
    } elsif (ref($me->{'dirlist'}) eq 'ARRAY') {
	my @dirs = @{$me->{'dirlist'}};
	$me->release(@dirs);
    }
    $me->{'dirlist'} = [] unless (istrue($me->minimize_rundirs));
}

sub reserve {
    my ($me, $nodel, $num, %attributes) = @_;
    my $top = $me->top;
    if ($me->output_root ne '') {
      $top = $me->output_root;
    }

    $num = 1 if ($num eq '');
    if (keys %attributes == 0) {
	%attributes = ( 'username' => $me->username,  'ext'  => $me->ext,
			'tune'     => $me->smarttune, 'mach' => $me->mach );
    }
    my $name;
    my %temp;
    foreach my $thing (qw(type tune size ext)) {
        ($temp{$thing} = $attributes{$thing}) =~ tr/-A-Za-z0-9./_/cs;
    }
    if ($attributes{'type'} eq 'run') {
        $name = sprintf("%s_%s_%s_%s", $temp{'type'}, $temp{'tune'},
                                       $temp{'size'}, $temp{'ext'});
    } elsif ($attributes{'type'} eq 'build') {
        $name = sprintf("%s_%s_%s", $temp{'type'}, $temp{'tune'}, $temp{'ext'});
    } else {
        $name = sprintf("UNKNOWN_%s_%s_%s", $temp{'tune'}, $temp{'size'},
                                            $temp{'ext'});
    }

    my $file = $me->lock_listfile();
    my @entries;

    for (my $i = 0; $i < $num; $i++ ) {
	my $entry = $file->find_entry($top, 'lock'=>0, %attributes);
	if (!$entry || $nodel) {
	    $entry = $file->new_entry($name, 'lock'=>0, 'username'=>$me->username, %attributes);
	}
	push (@entries, $entry);
	$entry->lock($me->username);
    }
    $file->update();
    $file->close();
    push (@{$me->{'entries'}}, @entries);

    return @entries;
}

sub release {
    my ($me, @dirs) = @_;

    my $file = $me->lock_listfile();

    for my $dir (@dirs) {
	my $entry = $file->find_entry_name($dir->name);
	if ($entry) {
	    $entry->unlock($dir->name);
	} else {
	    Log(0, "WARNING: release: Bogus entry in entries list\n");
	}
    }

    $file->update();
    $file->close();
}

sub make_empty_result {
    my ($me, $num_copies, $iter, $add_to_list) = @_;

    my $result = Spec::Config->new();
    $result->{'valid'}         = 'S';
    $result->{'errors'}        = [];
    $result->{'tune'}          = $me->tune;
    $result->{'mach'}          = $me->mach;
    $result->{'ext'}           = $me->ext;
    $result->{'selected'}      = 0;
    $result->{'rate'}          = istrue($me->rate);
    $result->{'benchmark'}     = $me->benchmark;
    $result->{'basepeak'}      = 0;
    $result->{'iteration'}     = $iter;
    $result->{'clcopies'}      = $num_copies;
    $result->{'rc'}            = 0;
    $result->{'reported_sec'}  = 0;
    $result->{'reported_nsec'} = 0;
    $result->{'reported_time'} = 0;
    $result->{'selected'}      = 0;
    if ($me->size eq 'ref') {
        $result->{'ratio'}     = 0;
        $result->{'reference'} = $me->reference;
    } else {
        $result->{'ratio'}     = '--';
        $result->{'reference'} = '--';
    }

    if ($add_to_list) {
        push @{$me->{'result_list'}}, $result;
    }

    return $result;
}

sub assemble_submit {
    # Assemble a hash (keyed by executable name) of submit commands.
    # Ones that were multiply-valued will be joined into a single string.
    my ($me) = @_;
    my %submit = ();
    
    foreach my $line (grep { /^submit_\S+\d*$/ } $me->list_keys) {
      my ($exe, $idx) = $line =~ m/^submit_(\S+)(\d*)$/;
      my $val = $me->accessor($line);
      $submit{$exe}->[$idx] = $val;
    }

    foreach my $exe (sort keys %submit) {
      $submit{$exe} = join('', @{$submit{$exe}});
    }
    return %submit;
}

sub run_benchmark {
    my ($me, $num_copies, $setup, $is_build, $iter, $is_training) = @_;
    my ($start, $stop, $elapsed);
    my %err_seen = ();
    my $specperl = ($^O =~ /MSWin/) ? 'specperl.exe' : 'specperl';

    my $submit = '$command';
    my %submit = $me->assemble_submit();
    Log(40, "Submit command(s) for ".$me->descmode.":\n  ".join("\n  ", map { "$_ => $submit{$_}" } sort keys %submit)."\n");
    my $origwd = main::cwd();
    my $do_monitor =    !(istrue($me->plain_train) && $is_training)
                     && !check_list($me->no_monitor, $me->size);
    my $tune = $me->tune;
    my $ext = $me->ext;
    my $env_vars = istrue($me->env_vars) && ($::lcsuite !~ /^cpu2/ || !istrue($me->reportable));

    my @dirs = @{$me->dirlist}[0..$num_copies-1];
    my $error = 0;

    my $result = $me->make_empty_result($num_copies, $iter, 0);

    if (istrue($me->accessor_nowarn('fail')) ||
        istrue($me->accessor_nowarn('fail_run'))) {
        Log(0, "ERROR: fail or fail_run set for this benchmark\n");
        $result->{'valid'} = 'RE';
        push (@{$result->{'errors'}}, $me->benchmark.": failed by request\n");
        return $result;
    }

    my $path = $dirs[0]->path;
    chdir($path);

    if ($me->pre_run(map { $_->path } @dirs)) {
        Log(0, "ERROR: pre-run failed for ".$me->benchmark."\n");
        $result->{'valid'} = 'TE';
        push (@{$result->{'errors'}}, $me->benchmark.": pre_run failed\n");
        return $result;
    }

    $me->unshift_ref({ 'iter' => 0, 'command' => '', 'commandexe' => '',
		       'copynum' => 0, });
    $me->push_ref   ({ 'fdocommand' => '', 'monitor_wrapper' => '',
		       'monitor_specrun_wrapper' => '', });
    my @newcmds;

    push @newcmds, '-S '.$me->stagger if istrue($me->shrate);
    my $bindval = $me->accessor_nowarn('bind');
    my @bindopts = isa($bindval, 'ARRAY') ? @{$bindval} : ();
    my $do_binding = defined($bindval) && @bindopts;
    for(my $i = 0; $i < @dirs; $i++) {
	my $dir = $dirs[$i];
        if ($do_binding) {
	    $bindval = $bindopts[$i % ($#bindopts + 1)];
	    $bindval = '' unless defined($bindval);
	    push (@newcmds, "-b $bindval");
	}
	push (@newcmds, '-C ' . $dir->path);
    }
    if (istrue($me->fake)) {
      Log(0, "\nBenchmark invocation\n");
      Log(0, "--------------------\n");
    }
    for my $obj ($me->invoke) {
	my $command = jp('..', basename($path), $obj->{'command'});
        my $shortexe = $obj->{'command'};
        $shortexe =~ s/_$tune.$ext//;
        $submit = exists($submit{$shortexe}) ? $submit{$shortexe} : $submit{'default'};
	$me->accessor_nowarn('commandexe', $command);
	$command .= ' ' . join (' ', @{$obj->{'args'}}) if @{$obj->{'args'}};
	if (istrue($me->command_add_redirect)) {
	    $command .= ' < '.$obj->{'input'} if ($obj->{'input'} ne '');
	    $command .= ' > '.$obj->{'output'} if ($obj->{'output'} ne '');
	    $command .= ' 2>> '.$obj->{'error'} if ($obj->{'error'} ne '');
	}
	$me->command($command);

	## expand variables and values in the command line
	if ($me->fdocommand ne '') {
	    $command = ::command_expand($me->fdocommand, $me, { 'iter', $iter });
	    $me->command($command);
	} elsif ($me->monitor_wrapper ne '' && $do_monitor) {
	    $command = ::command_expand($me->monitor_wrapper, $me, { 'iter', $iter });
	    $me->command($command);
	}

	$me->copynum(0);
	if ($submit
            &&
            # Submit should only be used for training runs when plain_train is
            # unset and use_submit_for_speed is set.
            (!$is_training ||
              (!istrue($me->plain_train) &&
               istrue($me->use_submit_for_speed))
            )
            &&
	    (istrue($me->rate) ||
             istrue($me->shrate) ||
	     istrue($me->use_submit_for_speed))) {
	    $command = ::command_expand($submit, $me, { 'iter', $iter });
	    $me->command($command);
	}
	my $opts = '';
	$opts .= '-i '. $obj->{'input'}  .' ' if (exists $obj->{'input'});
	$opts .= '-o '. $obj->{'output'} .' ' if (exists $obj->{'output'});
	$opts .= '-e '. $obj->{'error'}  .' ' if (exists $obj->{'error'});
	$command =~ s/[\r\n]+/;/go;
	push (@newcmds, "$opts$command");
    }

    Log(150, "Commands to run: \n    ", join ("\n    ", @newcmds), "\n") unless istrue($me->fake);

    my $absrunfile = jp($path, $me->commandfile);
    my $resfile    = jp($path, $me->commandoutfile);
    {
	my $fh = new IO::File ">$absrunfile";
	if (defined($fh)) {
	    print $fh join ("\n", @newcmds), "\n";
	    $fh->close;
            my $expected_length = length(join("\n", @newcmds));
            $expected_length++ unless $expected_length;
            if (-s $absrunfile < $expected_length) {
              Log(0, "\n$absrunfile is short; evil is afoot,\n  or the benchmark tree is corrupted or incomplete.\n");
              main::do_exit(1);
            }
	} else {
	    Log(0, "Error opening $absrunfile for writing!\n");
	    main::do_exit(1);
	}
    }

    if (!$setup) {
	# This is the part where the benchmark is actually run...
	my @specrun = (jp($me->top, 'bin', $me->specrun),
		       '-d', $path,
		       '-e', $me->commanderrfile,
		       '-o', $me->commandstdoutfile,
		       '-f', $me->commandfile,
		       );
	push @specrun, '-r' if istrue($me->command_add_redirect);
	push @specrun, '-nn' if istrue($me->fake);
	if ($me->no_input_handler =~ /null/io) {
	    push @specrun, '-N';
	} elsif ($me->no_input_handler =~ /(?:zero|file)/io) {
	    push @specrun, '-Z';
	} else {
	    push @specrun, '-C';
	}
	my $command =join (' ', @specrun);
	$me->command($command);
	if ($me->monitor_specrun_wrapper ne '' && $do_monitor) {
	    $command = ::command_expand($me->monitor_specrun_wrapper, $me, { 'iter', $iter });
	    $command = "echo \"$command\"" if istrue($me->fake);
	}
	main::monitor_pre_bench($me) if $do_monitor;
	if ($me->delay > 0 && !istrue($me->reportable)) {
	    Log(190, "Entering user-requested pre-invocation sleep for ".$me->delay." seconds.\n");
	    sleep $me->delay;
	}

	my %oldENV = %ENV;
	main::munge_environment($me) if $env_vars;
	Log(191, "Specinvoke: $command\n") unless istrue($me->fake);

	$start = time;
	my $rc;
        my $outname = istrue($me->fake) ? 'benchmark_run' : undef;
	if ($me->monitor_specrun_wrapper ne '' && $do_monitor) {
	    $rc = ::log_system_noexpand($command, $outname);
	} else {
	    $rc = ::log_system_noexpand(join(' ', @specrun), $outname);
	}
	$stop = time;
	$elapsed = $stop-$start;
	%ENV = %oldENV if $env_vars;

	if ($me->delay > 0 && !istrue($me->reportable)) {
	    Log(190, "Entering user-requested post-invocation sleep for ".$me->delay." seconds.\n");
	    sleep $me->delay;
	}

	main::monitor_post_bench($me) if $do_monitor;

	$me->pop_ref();
	$me->shift_ref();
	if (defined($rc) && $rc) {
	    $result->{'valid'} = 'RE';
	    Log(0, "\n".$me->benchmark.': '.$me->specrun.' non-zero return code (rc='.(($rc & 0xff00) >> 8).', signal='.($rc & 0x7f).")\n\n");
	    push (@{$result->{'errors'}}, $me->benchmark.': '.$me->specrun.' non-zero return code (rc='.(($rc & 0xff00) >> 8).', signal='.($rc & 0x7f).")\n");
	    log_err_files($path, 1, \%err_seen);
	}

	my $fh = new IO::File "<$resfile";
	if (defined $fh) {
	    my $error = 0;
	    my @child_times = ( );
	    my @counts = ();
	    while (<$fh>) {
		if (m/child finished:\s*(\d+),\s*(\d+),\s*(\d+),\s*(?:sec=)?(\d+),\s*(?:nsec=)?(\d+),\s*(?:pid=)?\d+,\s*(?:rc=)?(\d+)/) {
		    my ($num, $ssec, $snsec, $esec, $ensec, $rc) =
			($1, $2, $3, $4, $5, $6);
		    $counts[$num]++;
		    if ($rc != 0) {
			$error = 1;
			$result->{'rc'} = $rc;
			$result->{'valid'} = 'RE';
			Log(0, "\n".$me->benchmark.": copy #$num non-zero return code (rc=".(($rc & 0xff00) >> 8).', signal='.($rc & 0x7f).")\n\n");
                        push (@{$result->{'errors'}}, $me->benchmark.": copy #$num non-zero return code (rc=".(($rc & 0xff00) >> 8).', signal='.($rc & 0x7f).")\n");
                        log_err_files($path, 0, \%err_seen);
		    }
		    Log(110, "Workload elapsed time ($num:$counts[$num]) = ".($esec + ($ensec/1000000000))." seconds\n");
		    $child_times[$num] = { 'time' => 0, 'num' => $num } unless defined($child_times[$num]);
		    $child_times[$num]->{'lastline'} = "copy $num finished \@ ".main::ctime($ssec+($snsec/1000000000)).".  Total elapsed time: ";
		    $child_times[$num]->{'time'} += $esec + ($ensec/1000000000);
		    # Make a check for extra stupid times
		    my $lifetime = time - $main::runspec_time;
		    $lifetime++ unless ($lifetime);
		    if ($child_times[$num]->{'time'} > $lifetime) {
			# Something stupid has happened, and an elapsed time
			# greater than the total amount of time in the run so far
			# has been claimed.  This is obviously big B.S., so just
			# croak.
			Log(0, "\nERROR: Claimed elapsed time of ".$child_times[$num]->{'time'}." for ".$me->benchmark." is longer than\n       total run time of $lifetime seconds.\nThis is extremely bogus and the run will now be stopped.\n");
			main::do_exit(1);
		    }
		}
		if (m/runs elapsed time:\s*(\d+),\s*(\d+)/) {
		    $result->{'reported_sec'}  = $1;
		    $result->{'reported_nsec'} = $2;
		}
	    }
	    foreach my $ref (@child_times) {
		next unless defined($ref);
		if (ref($ref) ne 'HASH') {
		    Log(0, "Non-HASH ref found in child stats: $ref\n");
		} else {
		    Log(125, $ref->{'lastline'}.$ref->{'time'}."\n");
                    $result->{'copytime'}->[$ref->{'num'}] = $ref->{'time'};
		}
	    }
	    $fh->close;
	} elsif (!istrue($me->fake)) {
	    $result->{'valid'} = 'RE';
	    Log(0, "couldn't open specrun result file '$resfile'\n");
	    push (@{$result->{'errors'}}, "couldn't open specrun result file\n");
	}

	return $result if (($error || $result->{'valid'} ne 'S'
			    || @{$result->{'errors'}}+0 > 0)
			   && $me->accessor_nowarn('fdocommand') ne '');
    }

# Now make sure that the results compared!
    if ($me->action eq 'only_run') {
	$result->{'valid'} = 'R?' if $result->{'valid'} eq 'S';
    } else {
	my $size        = $me->size;
	my $tune        = $me->tune;
	my $comparedir  = $dirs[0]->path;
	my $comparename = jp($comparedir, $me->comparefile);

        
        if (istrue($me->fake)) {
          Log(0, "\nBenchmark verification\n");
          Log(0, "----------------------\n");
        }

	if (!$setup) {
	    # If we're just setting up, there won't be any output files
	    # to fix up.
	    if ($me->pre_compare(@dirs)) {
		Log(0, "pre_compare for " . $me->benchmark . " failed!\n");
	    }
	}

	my $comparecmd = new IO::File ">$comparename";
	if (!defined $comparecmd) {
	    $result->{'valid'} = 'TE';
	    push (@{$result->{'errors'}}, "Unable to open compare commands file for writing");
	} else {
	    my $num_output_files = 0;
            my $expected_length = 0;
	    for my $obj (@dirs) {
		my $path = $obj->path;
		my $basecmd = "-c $path ";

		Log(145, "comparing files in '$path'\n") unless istrue($me->fake);
		for my $absname ($me->output_files_abs) {
		    my $relname = basename($absname, '.bz2');
		    my $opts = { 'cw'         => $me->compwhite($size, $tune, $relname),
				 'abstol'     => $me->abstol ($size, $tune, $relname),
				 'floatcompare' => $me->floatcompare,
				 'calctol'    => $me->calctol,
				 'reltol'     => $me->reltol ($size, $tune, $relname),
				 'obiwan'     => $me->obiwan ($size, $tune, $relname),
				 'skiptol'    => $me->skiptol($size, $tune, $relname),
				 'skipabstol' => $me->skipabstol($size, $tune, $relname),
				 'skipreltol' => $me->skipreltol($size, $tune, $relname),
				 'skipobiwan' => $me->skipobiwan($size, $tune, $relname),
				 'binary'     => $me->binary ($size, $tune, $relname),
				 'ignorecase' => $me->ignorecase($size, $tune, $relname),
			     };

		    Log(150, "comparing '$relname' with ".join(', ', map { "$_=$opts->{$_}" } sort keys %$opts)."\n") unless istrue($me->fake);

		    my $cmd = $basecmd . "-o $relname.cmp " . "$specperl " . jp($me->top, 'bin', $me->specdiff) . 
			    ' -m -l ' . $me->difflines . ' ';
		    # Add options that have skip- variants and take args
		    foreach my $cmptype (qw(abstol reltol skiptol)) {
			if (defined($opts->{$cmptype}) &&
			    ($opts->{$cmptype} ne '')) {
			    $cmd .= " --$cmptype $opts->{$cmptype} ";
			}
			if (defined($opts->{"skip$cmptype"}) &&
			    ($opts->{"skip$cmptype"} ne '')) {
			    $cmd .= qq/ --skip$cmptype $opts->{"skip$cmptype"} /;
			}
		    }
		    # skipobiwan is special because obiwan is a switch
		    if (defined($opts->{'skipobiwan'}) &&
			($opts->{'skipobiwan'} ne '')) {
			$cmd .= " --skipobiwan $opts->{'skipobiwan'} ";
		    }
		    # Add options for switches
		    foreach my $cmptype (qw(calctol obiwan binary cw
                                            floatcompare ignorecase)) {
			if (defined($opts->{$cmptype}) && $opts->{$cmptype}) {
			    $cmd .= " --$cmptype ";
			}
		    }
		    $cmd .= $absname . ' ' . $relname;
		    print $comparecmd "$cmd\n";
                    $expected_length += length($cmd);
		    $num_output_files++;
		}
	    }
	    $comparecmd->close();
	    return $result if $setup;
            if ($num_output_files == 0) {
              Log(0, "\nNo output files were found to compare!  Evil is afoot, or the benchmark\n  tree is corrupt or incomplete.\n\n");
              main::do_exit(1);
            }
            
            if (-s $comparename < $expected_length) {
              Log(0, "\nERROR: The compare commands file ($comparename) is short!\n       Please make sure that the filesystem is not full.\n");
              $result->{'valid'} = 'TE';
              push (@{$result->{'errors'}}, "Compare commands file was short");
            } else {
              my $num_compares = $me->max_active_compares;
              # If max_active_compares isn't set, this will ensure that we
              # do one compare (at a time) per run directory
              $num_compares = @dirs+0 if $num_compares == 0;
              # If we try to run more compares than the total number of output
              # files to compare (copies * output files), then one will exit,
              # and the compare will fail, even if everything else is okay.
              if ($num_compares > $num_output_files) {
                  $num_compares = $num_output_files;
              }
              my @specrun = (jp($me->top, 'bin', $me->specrun),
                          '-E',
                          '-d', $comparedir,
                          '-c', $num_compares,
                          '-e', $me->compareerrfile,
                          '-o', $me->comparestdoutfile,
                          '-f', $me->comparefile,
                          );
              push @specrun, '-nn' if istrue($me->fake);
              Log(191, 'Specinvoke: ', join (' ', @specrun), "\n") unless istrue($me->fake);
	      my $outname = istrue($me->fake) ? 'compare_run' : undef;
              my $rc = ::log_system_noexpand(join(' ', @specrun), $outname);
              if (defined($rc) && $rc) {
                  log_err_files($path, 1, \%err_seen);
              }
	      # Scan the specdiff output files for indications of completed
	      # runs.
	      my @misfiles = ();
	      my %specdiff_errors = ();
	      for my $obj (@dirs) {
		  my $file;
		  my $dh = new IO::Dir $obj->path;
		  while (defined($file = $dh->read)) {
		      next if $file !~ m/\.(mis|cmp)$/i;
		      if ($1 eq 'mis') {
			  # Remember it for later
			  push @misfiles, jp($obj->path, $file);
			  next;
		      }
		      my ($basename) = $file =~ m/(.*)\.cmp$/;
		      my ($cmpname) = jp($obj->path, $file);
		      my $diff_ok = 0;
		      my $fh = new IO::File "<$cmpname";
		      if (!defined($fh)) {
			  Log(0, "*** specdiff error on $basename; no output was generated\n");
			  $rc = 1 unless $rc;
		      } else {
			  # Just read it in to make sure specdiff said "all ok"
			  while(<$fh>) {
			      $diff_ok = 1 if /^specdiff run completed$/o;
			      last if $diff_ok;
			  }
			  $fh->close();
		      }
		      if ($diff_ok == 0) {
			  $specdiff_errors{$cmpname}++;
			  $rc = 1 unless $rc;
		      }
		  }
	      }
              if ($rc) {
                  $result->{'valid'} = 'VE' if $result->{'valid'} eq 'S';
                  push (@{$result->{'errors'}}, "Output miscompare");
                  my $logged = 0;
		  while (defined(my $misname = shift(@misfiles))) {
		      my $cmpname = $misname;
                      $cmpname =~ s/\.mis$/.cmp/o;
		      my $basename = basename($misname, '.mis');

		      Log (0, "*** Miscompare of $basename, see $misname\n");
		      $logged = 1;
		      my $fh = new IO::File "<$misname";
		      if (!defined $fh) {
			  if ($specdiff_errors{$basename}) {
			      Log(0, "specdiff did not complete successfully!\n");
			  } else {
			      Log (0, "Can't open miscompare file!\n");
			  }
		      } else {
			  while (<$fh>) {
			      Log (120, $_);
			  }
			  $fh->close();
		      }
		      delete $specdiff_errors{$cmpname};
		  }
		  foreach my $diff_error (sort keys %specdiff_errors) {
		      Log(0, "*** Error comparing $diff_error: specdiff did not complete\n");
		  }
                  Log(0, "Compare command returned $rc!\n") unless $logged;
	      }
	  }
	}
    }

    return $result if ($me->accessor_nowarn('fdocommand') ne '');

    my $reported_sec  = $result->{'reported_sec'};
    my $reported_nsec = $result->{'reported_nsec'};
    my $reported = $reported_sec + $reported_nsec / 1000000000;
    if ($me->size eq 'ref') {
	my $reference = $me->reference;

	$result->{'ratio'}         = (defined($reported) && $reported) ? $reference / $reported : 0;
	if (istrue($me->rate)) {
	    $result->{'ratio'} *= $num_copies * $::rate_multiplier;
	} else {
	    $result->{'ratio'} *= $::speed_multiplier;
	}
    }
    $result->{'reported_time'} = $reported;

    if (!istrue($me->fake)) {
      Log (155, "Benchmark Times:\n",
               '  Start:    ', ::ctime($start), " ($start)\n",
               '  Stop:     ', ::ctime($stop),  " ($stop)\n",
               '  Elapsed:  ', ::to_hms($elapsed), " ($elapsed)\n",
               '  Reported: ', "$reported_sec $reported_nsec $reported\n");
    }

    push (@{$me->{'result_list'}}, $result);
    chdir($origwd);
    return $result;
}

sub pre_build {
    # Placeholder function
    return 0;
}

sub post_setup {
    # Placeholder function
    return 0;
}

sub pre_compare {
    return 0;
}

sub pre_run {
    # Placeholder function
    return 0;
}

sub result_list {
    my ($me, $copies) = @_;
    if (defined $copies) {
	return grep ($_->copies == $copies, @{$me->{'result_list'}});
    } else {
	return @{$me->{'result_list'}};
    }
}

sub ratio {
    my ($me, $num_copies) = @_;
    my @res = @{$me->{'result_list'}};
    if (defined $num_copies) {
	@res = grep ($_->{'copies'} == $num_copies, @res);
    }
    @res = sort { $a->{'ratio'} <=> $b->{'ratio'} } @{$me->{'result_list'}};
    if (@res % 2) {
	return $res[(@res-1)/2]; # Odd # results, return the median ratio
    } else {
        # For even # of results, return the lower median.
        # See chapter 9 of Cormen, Thomas, et al. _Introduction to Algorithms,
        #   2nd Edtion_. Cambridge: MIT Press, 2001
	return $res[@res/2-1];   # Return the lower median
    }
}

sub lock_listfile {
    my $me = shift;
    my $subdir = $me->expid;
    $subdir = undef if ($subdir eq '');
    my $path = $me->{'path'};
    if ($me->output_root ne '') {
      my $oldtop = ::make_path_re($me->top);
      my $newtop = $me->output_root;
      $path =~ s/^$oldtop/$newtop/;
    }

    my $dir       = jp($path, $me->workdir, $subdir);
    my $file      = jp($dir,  $me->worklist);
    my $obj = Spec::Listfile->new($dir, $file);
    $me->{'listfile'} = $obj;
    return $obj;
}

sub log_err_files {
    my ($path, $specinvoke_problem, $already_done) = @_;

    # Read the contents of the error files (other than speccmds.err and
    # compare.cmd) and put them in the log file
    my $dh = new IO::Dir $path;
    if (!defined($dh)) {
	Log(0, "\nCouldn't log contents of error files from $path: $!\n\n");
	return;
    }
    while (defined(my $file = $dh->read)) {
	next unless ($file =~ /\.err$/o);
	next if exists($already_done->{$file});
	next if (!$specinvoke_problem && $file =~ /(?:compare|speccmds)\.err$/);
	next unless (-f $file);
        next unless (-s $file);
	my $fh = new IO::File "<$file";
	next unless defined($fh);
	my $eol = $/;
	$/ = undef;
	Log(100, "\n****************************************\n");
	Log(100, "Contents of $file\n");
	Log(100, "****************************************\n");
	Log(100, <$fh>."\n");
	Log(100, "****************************************\n");
	$/ = $eol;
        $already_done->{$file}++;
    }
}

sub check_list {
  # Check to see if $item appears in the text of $list.  For some reason,
  # $list =~ /$item/ does not always work...
  my ($list, $item) = @_;

  foreach my $listitem (split(/[,[:space:]]+/, $list)) {
    return 1 if ($listitem eq $item);
  }
  return 0;
}
1;
