#
# log.pm
#
# Copyright (C) 1999-2006 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: log.pl 4160 2006-04-22 22:21:59Z cloyce $

use strict;
use IO::File;

my $version = '$LastChangedRevision: 4160 $ '; # Make emacs happier
$version =~ s/^\$LastChangedRevision: (\d+) \$ $/$1/;
$::tools_versions{'log.pl'} = $version;

my $log_handle = new IO::File ">&STDOUT";
my $log_opened = 0;
my @saved_logs = ();    # Things logged before the log file was opened
my %partial_lines = ('screen' => undef, 'log' => undef);

sub open_log {
    my ($config) = @_;
    my $top = $config->top;
    if ($config->output_root ne '') {
      $top = $config->output_root;
    }
    my $subdir = $config->expid;
    $subdir = undef if $subdir eq '';

    my $locked = 0;

    # Find a name
    my $dir  = jp($top, $config->resultdir, $subdir);
    mkpath($dir);
    my $name = jp($dir, $config->prefix . $config->log);
    my $lockfile = "${name}.lock";

    # We've got to be careful about races here, too!
    # Some systems lack the ability to lock files that aren't opened for
    # writing.  So we open a file for writing in $SPEC/result, and make sure
    # that everyone can write to it.
    my $origumask = umask;
    my ($rc, $what, $stuff) = (0, undef, undef);
    umask 0000;			# Make sure that everyone can write it
    my $fh = new IO::File;
    my $num = undef;
    $rc = $fh->open($lockfile, O_RDWR|O_CREAT, 00666);
    umask $origumask;		# Now put it back
    if (!$rc) {
	Log(0, "Couldn't open $lockfile for update:\n  $!\nThis makes selecting a run number impossible, so I'm going to bail now.\n");
        return 0;
    } else {
        ($rc, $what, $stuff) = lock_file($fh);
        if (!defined($rc) || $what ne 'ok') {
            if ($what eq 'unimplemented') {
                Log(0, "LOCK ERROR: Your system doesn't seem to implement file locking.  Perl said\n");
                Log(0, "-------\n$stuff\n-------\n");
                Log(0, "Because of this, it is not possible to guarantee that the run number (which\n");
                Log(0, "determines the names of the log and results files) will be unique.  This is\n"); 
                Log(0, "only an issue if there are concurrent runs happening using this copy of the\n");
                Log(0, "benchmark tree.\n");
                $locked = 0;
            } elsif ($what eq 'error') {
                Log(0, "LOCK ERROR: Could not lock log lock file \"$lockfile\".\n");
                Log(0, "Perl said\n-------\n$stuff\n-------\n");
                Log(0, "  It is not safe to attempt to generate a run number, so I'll bail now.\n");
                return 0;
            }
        } else {
            $locked = 1;
        }
    }
    $fh->seek(0, 0);	# Make sure to only read the first line
    chomp($num = <$fh>);
    $num += 0;  # Make it into a real number
    $name = $lockfile;  # This is just to make the following test succeed
    while (-e $name) {
        if (!defined($num) || $num <= 0) {
          $num  = find_biggest_ext($dir, '.log') + 1;
        } else {
          $num++;
        }
        $num = sprintf("%03d", $num);
        $fh->seek(0, 0);	# Make sure to only write the first line
        $fh->printflush("$num\n");
        $name = jp($dir, $config->prefix . $config->log . ".${num}.log" );
    }
    $config->{'lognum'} = $num;
    if (!defined($log_handle)) {	# Default open of STDOUT must've failed
	$log_handle = new IO::File;
    }
    if (!$log_handle->open(">$name")) {
	Log(0, "Couldn't open log file '$name' for writing: $!\n");
    } else {
      $config->{'logname'} = $name;
      $log_opened = 1;
      # Unbuffer the log file
      $log_handle->autoflush;

      # Output saved logs, if any
      if (@saved_logs) {
        $log_handle->print(join('', @saved_logs))."\n";
        @saved_logs = ();
      }
    }

    if ($locked) {
      # Unlock the file so that others may proceed.
      ($rc, $what, $stuff) = unlock_file($fh);
      if (!defined($rc) || $what ne 'ok') {
        if ($what eq 'unimplemented') {
          # If locking is really unimplemented, $locked shouldn't be set.
          Log(0, "UNLOCK ERROR: Tried to unlock a file on a system that claims to not support\n");
          Log(0, "        locking.  Please report this error to ${main::lcsuite}support\@spec.org\n");
          Log(0, "        at SPEC.  Please include the output from 'runspec -V' in your report.\n");
          Log(0, "About this error, Perl said\n");
          Log(0, "-------\n$stuff\n-------\n");
        } elsif ($what eq 'error') {
          Log(0, "UNLOCK ERROR: Could not unlock log lock file \"$lockfile\".\n");
          Log(0, "The error might have been\n");
          Log(0, "-------\n$stuff\n-------\n");
          Log(0, "Because of this, other runs may wait indefinitely.\n");
        }
      }
    }
    $fh->close();		# It'd close when we leave this sub anyway

    return 1;
}

sub close_log {
  # Flush pending output (if any) here
  if ($log_opened && defined($partial_lines{'log'})) {
    $log_handle->print($partial_lines{'log'}."\n");
    $partial_lines{'log'} = undef;
  }
  if (defined($partial_lines{'screen'})) {
    print $partial_lines{'screen'}."\n";
    $partial_lines{'screen'} = undef;
  }
}

## ############
## SUB                   LOG
## ############

## The first argument is the log level associated with the call.
## The following arguments hold the information that needs to be
## logged. The global $verbose level needs to be set to determine
## if the message is to be logged. This function does some
## remedial screen wrapping--breaking on character postion of
## the width rather than whitespace.

sub Log {			## The multi-level multi-type log writer.
				## The log statement will output if
				## the global verbosity level is equal to or
				## greater than the level associated with the
				## call to Log. There are messages that
				## are formatted both for the screen
				## and (separately) for the log file.
				## All messages with a level greater than
				## 90 are generated regardless of the
				## verbosity level.
    my ($level, @data) = @_;
    my ($type, @output);
    my $verbose    = $main::global_config->verbose;
    my $line_width = $main::global_config->line_width;
    my $log_line_width = $main::global_config->log_line_width;
    my $log_output    = "";
    my $screen_output = "";
    my $logfile = 0;
    for (@data) {		## handle each data type reference
	my $type = ref($_);
	my $data;

	if ($type eq "ARRAY") {	## arrays referenced are joined w/o separator
				## and then appended to the output
	    $data = join("", @$_);
	} elsif ($type eq "SCALAR") { ## scalars reference are just appended
	    $data = $$_;
	} else {		## if item is not a reference, but only a
				## explicit value, then just append that
				## value
	    $data = $_;
	}
	if ($level >= 100) {
	    $level   -= 100;
	    $logfile  = 1;
	}
	$log_output    .= $data if (($verbose >= $level) || $logfile);
	$screen_output .= $data if ($verbose >= $level);
    }
    
    print split_line('screen', $screen_output, $line_width);

    $log_output = split_line('log', $log_output, $log_line_width);
    if ($log_opened != 0) {
      print $log_handle $log_output;	  ## log file
    } elsif ($log_output eq "\n" || $log_output !~ /^\s*$/) {
      push @saved_logs, $log_output;
    }

}

sub split_line {
  my ($dest, $line, $width) = @_;

  if (exists($partial_lines{$dest}) && defined($partial_lines{$dest})) {
    $line = $partial_lines{$dest}.$line;
    $partial_lines{$dest} = undef;
  }
  if ($width > 0) {
    # log line wrapping
    my @output = split ("\n", $line, -1);

    # Save a trailing newline, if any
    $line =~ s/.*?(\n)?$/$1/s;

    # Split things up
    my @log_output = ();
    for (@output) {		
        while ($_) {
            push @log_output, substr($_, 0, $width);
            substr($_, 0, $width) = '';
        }
    }

    # Add the trailing newline (if any) back in
    if (@log_output) {
      if ($line ne '') {
        # There was a newline on the end, so this will be a complete line.
        $log_output[$#log_output] .= $line;
      } else {
        # No newline means that we need to save the last "line" (actually
        # line fragment) in partial_lines so that its length can be properly
        # accounted for against the width constraints next time around.
        # If there _is_ no "next time around" it'll get kicked out by
        # close_log; it's always guaranteed to be less than width characters
        # anyway.
        $partial_lines{$dest} = pop(@log_output);

        # Ensure that the final line has a newline attached; if there was
        # a part without a newline, it's now in partial_lines.
        $log_output[$#log_output] =~ s/([^\n])$/$1\n/ if (@log_output);
      }
    } elsif ($line ne '') {
      # This just handles the special case of a single newline
      @log_output = ($line);
    }

    # Put it all back together
    return join("\n", @log_output);
  } else {
    return $line;
  }
}

sub log_header {
    my ($config) = @_;
    my $tune       = join (',', @{$config->tunelist});
    my $action     = $config->action;
    my $verbose    = $config->verbose;
    my $ext        = join (',', @{$config->extlist});
    my $size       = join (',', @{$config->sizelist});
    my $mach       = join (',', @{$config->machlist});
    my $benchmarks = join (',', map {$_->benchmark} @{$config->runlist});
    my $outputs    = join (',', map {$_->name}      @{$config->formatlist});
    my $username   = $config->username;

    Log(140, <<EOV);
Verbosity = $verbose
Action    = $action
Tune      = $tune
Ext       = $ext
Size      = $size
Machine   = $mach
benchmarks= $benchmarks
outputs   = $outputs
username  = $username
EOV
}



1;
