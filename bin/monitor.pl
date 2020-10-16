#
# monitor.pl
#
# Copyright (C) 1999-2006 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: monitor.pl 3626 2006-01-17 14:16:50Z cloyce $

use strict;

my $version = '$LastChangedRevision: 3626 $ '; # Make emacs happier
$version =~ s/^\$LastChangedRevision: (\d+) \$ $/$1/;
$::tools_versions{'monitor.pl'} = $version;

sub monitor_shell {
    my ($name, $config) = @_;
    my $cmd = $config->accessor_nowarn($name);
    my $rc = undef;

    if (defined($cmd) && ($cmd ne '')) {
	Log(0, "Executing $name: $cmd\n");
	#$rc = log_system_noexpand($cmd, $name, $config);
	$rc = log_system($cmd, $name, istrue($config->accessor_nowarn('fake')), $config);
	if ($rc) {
	    Log(0, "$name returned non-zero exit code ($rc)\n");
	    do_exit(1);
        }
    }

    $cmd = $config->accessor_nowarn("${name}_perl");
    if (defined($cmd) && ($cmd ne '')) {
	my $s = new Safe 'tmp';
	if (istrue($main::runconfig->safe_eval())) {
            $s->permit_only(':base_core', ':base_mem', 'padany', 'padsv',
                            'padav', 'padhv', 'sprintf');
	} else {
	    $s->deny_only();
	}
	$s->share('%ENV', '$main::runconfig');
	if (istrue($config->accessor_nowarn('fake'))) {
	    Log(0, "_NOT_ executing the following perl code:\n---------------\n$cmd\n-----------------\n");
	} else {
	    $s->reval($cmd);
	}
	if ($@) {
	    Log(0, "Error executing ${name}_perl:\n$@\n");
	}
	no strict 'refs';
	%{*{"main::".$s->root."::"}{HASH}} = ();
    }
}

sub monitor_pre        { monitor_shell('monitor_pre',        @_); }
sub monitor_pre_run    { monitor_shell('monitor_pre_run',    @_); }
sub monitor_pre_bench  { monitor_shell('monitor_pre_bench',  @_); }
sub monitor_post_bench { monitor_shell('monitor_post_bench', @_); }
sub monitor_post       { monitor_shell('monitor_post',       @_); }

1;
