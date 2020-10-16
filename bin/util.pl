#
# util.pl
#
# Copyright (C) 1999-2006 Standard Performance Evaluation Corporation
#  All Rights Reserved
#
# $Id: util.pl 4670 2006-07-25 19:19:49Z cloyce $

use strict;
use Safe;
use IO::Dir;
use File::Copy;
use File::Basename;
use File::Path;
use File::Spec;
use Compress::Bzip2 qw(:constants);
use Compress::Zlib;
use MIME::Base64;
use Digest::MD5;
use Text::Wrap;
use Text::Tabs;
use Storable qw(dclone);
use Carp;
use IO::Handle;
use IO::File;
use POSIX qw(:sys_wait_h);
use Fcntl qw(:flock);
use UNIVERSAL qw(isa);
use LWP::UserAgent;
#use LWP::Debug qw(+);

use vars qw($exited $timeout $ua);

my $logged_vars = 0;
my $diff_debug = 0;

my $version = '$LastChangedRevision: 4670 $ '; # Make emacs happier
$version =~ s/^\$LastChangedRevision: (\d+) \$ $/$1/;
$::tools_versions{'util.pl'} = $version;

sub md5fhdigest {
    my ($fh) = @_;
    my $md5 = new Digest::MD5;
    my $tmp = '';
    $fh->read($tmp, stat($fh)->size);
    $md5->add($tmp);
    return $md5->hexdigest();
}

sub md5refdigest {
    my ($ref) = @_;
    my $md5 = new Digest::MD5;
    $md5->add($$ref);
    return $md5->hexdigest();
}

sub md5diffdigest {
    # Like md5filedigest, but with guaranteed consistent line endings
    # (as will be produced by applying the diff)
    my ($file) = @_;
    my $md5 = new Digest::MD5;

    my $fh  = new IO::File $file, O_RDONLY|O_BINARY;
    if (!defined($fh)) {
        die "$0: can't open '$file' for reading: $!\n";
    } else {
	local $/ = undef;
	my $contents = <$fh>;
	my @file = map { tr{\012\015}{\012\012}s; $_ } split(/(?:\n|\r\n)/, $contents);
        $md5->add(join("\012", @file)."\012");
        $fh->close();
    }
    return $md5->hexdigest();
}

sub md5scalardigest {
    my ($string) = @_;
    my $md5 = new Digest::MD5;
    $md5->add($string);
    return $md5->hexdigest();
}

sub copy_file {
    my ($source, $targetfile, $dirs, $verify, $sumhash) = @_;
    my $isbz2 = 0;  # Is the source file compressed?
    $sumhash = \%::file_md5 unless ref($sumhash) eq 'HASH';

    if (!defined($targetfile)) {
	$targetfile = basename($source);
    }
    if (ref($dirs) ne 'ARRAY') {
	if ($dirs eq '') {
	    $dirs = [ dirname($targetfile) ];
	    $targetfile = basename($targetfile);
	} else {
	    $dirs = [ $dirs ];
	}
    }

    # Do some pre-copy checks
    my $oldmode = 0644;
    foreach my $dir (@{$dirs}) {
	my $target = jp($dir, $targetfile);
	if (-e $target) {
	    # Sheesh!  Make sure that the *target* isn't read-only
	    # Thanks very much, Microsoft!
	    $oldmode = (stat($target))[2];
	    $oldmode |= 0644;
	    chmod $oldmode, $target;
	}
    }

    # Copy the file into all the directories
    if ($source =~ s/\.bz2$//) {
	$isbz2 = 1;
	copy_bz2_file($source, $targetfile, $dirs, $verify, $sumhash);
    } else {
	foreach my $dir (@{$dirs}) {
	    my $target = jp($dir, $targetfile);
	    if (!copy($source, $target)) {
		Log(0, "\nERROR: Copying $source to $target failed: $!\n");
		Log(0, "  in copy_file:\n".Carp::longmess()."\n\n");
		return 0;
	    }
	}
    }

    # Do the post-copy cleanup and file verification
    # Get the hash from the MANIFEST if the source is compressed (because
    # there's no real file to generate it _from_) or if check_integrity is
    # is on.
    my $refhash = $sumhash->{$source};
    if ($verify) {
	if (!defined($refhash)) {
	    # Okay, it's not in the existing set, so generate it
	    $refhash = md5filedigest(($isbz2) ? "${source}.bz2" : $source, $isbz2);
	}
    }
    foreach my $dir (@{$dirs}) {
	my $target = jp($dir, $targetfile);
	# Ensure that the copied file is not read-only
	$oldmode = (stat($source))[2];
	$oldmode |= 0644;
	chmod $oldmode, $target;
	# Set the modification time on the target to match the source
	utime ((stat(_))[8,9], $target);

	if ($verify) {
	    my $targhash = md5filedigest($target);
	    Log(89, "Comparing MD5 hashes:\n  $refhash $source\n  $targhash $target\n");
	    my $errcount = 0;
	    while (($refhash ne $targhash) && ($errcount < 5)) {
		$errcount++;
		Log(0, "Target file ($target) hash doesn't match\nafter copy from $source in copy_file ($errcount tr".(($errcount > 1)?'ies':'y')." total)!  Sleeping 2 seconds...\n");
		sleep(2);
		$targhash = md5filedigest($target);
	    }
	    if ($errcount >= 5) {
		# The files continued to mismatch; it's an error
		Log(0, "\nERROR: MD5 file mismatch while copying $source to $target\n");
		Log(0, "  in copy_file:\n".Carp::longmess()."\n\n");
		unlink $target;
		return 0;
	    }
	}
    }
    return 1;
}

sub copy_tree {
    my ($source, $target, $sumhash, $ignore, $fast) = @_;
    $sumhash = \%::file_md5 unless ref($sumhash) eq 'HASH';
    if (ref($ignore) eq 'ARRAY') {
	$ignore = { map { $_ => 1 } @{$ignore} };
    } elsif (ref($ignore) ne 'HASH') {
	$ignore = { };
    }
    my $dir = new IO::Dir $source;
    while (defined(my $file = $dir->read)) {
	next if $file eq '.' || $file eq '..';
	next if exists $ignore->{$file};
	my $sf = jp($source, $file);
        $file =~ s/\.bz2$//;
        my $tf = jp($target, $file);
        my $oldmode = 0644;
        if (-e $tf) {
            # Make sure that the target isn't read-only.  See bile above
            $oldmode = (stat($tf))[2];
	    $oldmode |= 0644;
	    chmod $oldmode, $tf;
        }
	if (-f $sf) {
	    if ($::check_integrity && !$fast) {
		if (!exists $sumhash->{$sf}) {
		    Log(0, "\n$sf has no stored checksum!\n");
		    return 0;
		}
		if ($sumhash->{$sf} ne md5filedigest($sf)) {
		    Log(0, "\n$sf is corrupt!\n");
		    return 0;
		}
	    }
            if ($sf =~ /\.bz2$/) {
              copy_bz2_file($sf, $file, [$target], 0);
            } else {
		if (!copy($sf, $tf)) {
		    Log(0, "ERROR: Copying $sf to $tf failed: $!\n");
		    Log(0, "  in copy_tree:\n".Carp::longmess());
		    return 0;
		}
            }
	    $oldmode = (stat($sf))[2];
	    # Ensure that the copied file is not read-only
	    $oldmode |= 0644;
	    chmod $oldmode, $tf;
	    utime ((stat(_))[8,9], $tf);
	} elsif (-d $sf && $file ne 'CVS' && $file ne '.svn') {
	    mkpath($tf);
	    copy_tree($sf, $tf, $sumhash, $ignore, $fast);
	}
    }
    return 1;
}

sub copy_bz2_file {
    my ($source, $destfile, $dirs, $verify, $sumhash) = @_;
    # Source file is compressed; dest file must be decompressed
    $sumhash = \%::file_md5 unless ref($sumhash) eq 'HASH';

    if (ref($dirs) ne 'ARRAY') {
	if ($dirs ne '') {
	    $dirs = [ $dirs ];
	} else {
	    $dirs = [ '' ];
	}
    }

    if ($source !~ /\.bz2$/) {
	$source .= '.bz2';
    }

    if ($::check_integrity && $verify) {
	# Check the MD5 sum of the compressed file; if it doesn't match,
	# what's the point of trying to decompress it?
        my $refhash = $sumhash->{$source};
	if (!defined($refhash)) {
	    Log(0, "No MD5 sum for $source; aborting bz2 copy\n");
	    return 0;
	}
	my $filehash = md5filedigest($source);
	if ($refhash ne $filehash) {
	    Log(0, "ERROR: MD5 sum mismatch on compressed source file $source\n");
	    return 0;
	}
    }

    # Trim the extension, if it's still there
    $destfile =~ s/\.bz2$//;

    # Get a copy of the destinations that we can munge
    my @dirs = @{$dirs};
    my $firstdest = jp(shift(@dirs), $destfile);
    my $oldmode = 0644;
    if (-e $firstdest) {
	# Make sure that the destination isn't read-only
	$oldmode = (stat($firstdest))[2];
	chmod $oldmode|0644, $firstdest;
    }
    my $fh = new IO::File ">$firstdest";
    binmode $$fh, ':raw';
    if (!defined($fh)) {
	Log(0, "Couldn't open \"$firstdest\" for writing: $!\n");
	return 0;
    }

    # Read the source file a bit at a time.  In order to save memory, the
    # compressed file is read bit by bit and written out to the first
    # destination.  Then _that_ file is copied to the rest of the destinations.
    # Overall there's more I/O, but it should take less time than doing
    # multiple decompressions, and (more importantly) MUCH less memory than
    # reading the WHOLE thing into memory and just writing it multiple times.
    my $bz = bzopen($source, 'rb');
    if (!defined($bz)) {
	Log(0, "Couldn't open \"$source\" for reading: $Compress::Bzip2::bzerrno\n");
	return 0;
    }
    my $tmp;

    while($bz->bzread($tmp, 262144) > 0) {
	$fh->print($tmp);
    }
    my $rc = $bz->bzerror + 0;
    $tmp = ''.$bz->bzerror;
    $bz->bzclose();
    $fh->close();
    $oldmode = (stat($source))[2];
    chmod $oldmode, $firstdest;
    utime ((stat(_))[8,9], $firstdest);
    if ($rc != BZ_STREAM_END && $rc != BZ_OK) {
	Log(0, "Error reading from $source: $tmp ($rc)\n");
	unlink $firstdest;
	return 0;
    }

    # Now copy it into the list of directories
    my @written = $firstdest;
    foreach my $dir (@dirs) {
	my $dest = jp($dir, $destfile);
	if (-e $dest) {
	    # Make sure that the destination isn't read-only
	    $oldmode = (stat($dest))[2];
	    chmod $oldmode|0644, $dest;
	    unlink $dest;
	}
	if (!copy $firstdest, $dest) {
	    Log(0, "Copy from $firstdest to $dest failed: $!\n");
	    unlink @written;
	    unlink $dest;
	    return 0;
    	}
	push @written, $dest;
	# This should be unnecessary
	$oldmode = (stat($source))[2];
	chmod $oldmode, $dest;
	utime ((stat(_))[8,9], $dest);
    }
    return 1;
}

sub istrue {
    my $val = shift @_;
    return 0 unless defined($val);
    $val = lc($val);
    return ($val eq 'y' || $val eq 'yes' || $val eq 't' || $val eq 'true' || 
	    $val eq 'o' || $val eq 'on'  || $val+0 != 0) ? 1 : 0;
}

sub find_biggest_ext {	## find the file with the highest suffix
    my $dir = shift;
    my $ext = shift;
    $ext = '' unless defined($ext);
    my $dh = new IO::Dir $dir;
    my $num = 0;
    if (!defined $dh) {
	Log(0, "find_biggest_num: Can't open directory '$dir': $!\n");
    } else {
	while (defined($_ = $dh->read)) { 
	    $num = $1 if m/\.(\d+)${ext}$/ && $1 > $num;
	}
    }
    return $num;
}

sub choose_string {
    my($string, @choices) = @_;
    my($match);
    # This could be done with a map, generating a hash, and then an existence
    # check, but the loop is faster for the found case and not any slower
    # for the not-found case.
    foreach my $choice (@choices) {
        return $choice if lc($choice) eq lc($string);
    }
    for (@choices) {
	if (m/^$string/i) {
	    return undef if defined $match;
	    $match = $_;
	}
    }
    return $match if defined $match;
    undef;
}

sub choose_strings {
    my ($name, $string, $default_choices, $all_choices) = @_;
    my %seen;
    my @results = ();
    my @temp = split(/\s*[,\s]\s*/, $string);

    for (@temp) {
	my $selection = choose_string($_, @{$default_choices}, @{$all_choices}, "all");
	if (!defined $selection) {
	    Log(0, "$name does not support '$_'\n");
	} elsif ($selection eq "all") {
	    for (@{$default_choices}) {
		if (!$seen{$_}++) {
		    push (@results, $_);
		}
	    }
	} else {
	    if (!$seen{$_}++) {
		push (@results, $_);
	    }
	}
    }
    @results;
}

# List files in a directory.
# Note that the directory handle is closed upon exit from the subroutine
sub list_files {
    my $dir = new IO::Dir "$_[0]";
    return sort grep { !/^\.\.?$/ && ($_[0] eq '.' || s%^%$_[0]/%) } $dir->read() if (defined $dir);
    return undef;
}

sub build_tree_hash {
    my ($os, $sumhash, @absdirs) = @_;
    my ($files, $dirs) = ({}, {});

    $os = $os->OS if (ref($os) ne '');
    my @work;
    for my $dir (@absdirs) {
	push (@work, [$dir, '', ''])
    }

    while (@work) {
	my ($absdir, $absroot, @paths) = @{shift @work};
	while (@paths) {
	    my $path    = shift(@paths);
	    my $root    = jp($absroot, $path);
	    my $dir     = jp($absdir, $path);
	    my $dh = new IO::Dir $dir;
	    my $file;

	    if (! defined $dh ) {
		Log(0, "Can't open path '$dir: $!\n");
	        return(undef, undef);
	    }
	    while (defined($file = $dh->read)) {
		my $absfile = jp($dir, $file);
		my $relfile = jp($root, $file);
		if (-d $absfile) {
		    if ($file eq '.' || $file eq '..' || 
                        $file eq 'CVS' || $file eq '.svn') {
		    } elsif ($file =~ m/^OS_(.*)(-|$)/i) {
			if ($1 eq $os) {
			    push (@work, [ $absfile, $root, '' ]);
			}
		    } else {
			$dirs->{$relfile} = '';
			push (@paths, $relfile);
		    }
		} elsif (-f $absfile) {
		    if ($::check_integrity &&
			defined($sumhash) && ref($sumhash) eq 'HASH') {
			if (!exists($sumhash->{$absfile})) {
			    Log(0, "build_tree_hash: $absfile not found in MD5 sum list\n");
			    return (undef, undef);
			}
		    }
		    $files->{$relfile} = $absfile;
		} else {
		    Log(0, "build_tree_hash: Can't tell what $absfile is!\n");
		}
	    }
	}
    }
    return ($files, $dirs);
}

# Run a command and log the action
sub log_system {
    log_system_raw(1,@_);
}
sub log_system_noexpand {
    log_system_raw(0,@_);
}
sub log_system_raw {
    my ($expand, $cmd, $outn, $fake, @repl) = @_;
    ## $cmd   -- initially, the unexpanded command string; eventually the whole
    ## $outn  -- basename for output and errors files
    ## $ses   -- session data
    ## @repl  -- the hash(es) of replacement variables and values

    my $errname = "$outn.err";
    my $outname = "$outn.out";
    my $config  = $main::global_config;
    my $rc;
    my $desc;
    my $printcmd;
    my $env_vars = istrue($config->env_vars) && ($::lcsuite !~ /^cpu2/ || !istrue($config->reportable));

    $desc = " $outn" if $outn ne '';

    $cmd = command_expand($cmd, @repl) if $expand;
    $printcmd = [ split (/[\r\n]+/, $cmd) ];

    my $fake_cmd = substr($cmd, 0, 40);
    $fake_cmd .= '...' if (length($fake_cmd) >= 40);

    if ($outn ne '') {
	$printcmd = redirect_cmd($cmd, $outname, $errname, $config);
	unlink $errname, $outname;
        $fake_cmd = "$outn ($fake_cmd)";
    }

    ## ready -- make a log entry if required
    Log (120, "Issuing$desc command '$cmd'\n") unless istrue($config->fake);

    ## give user some indication of what is happening if she is
    ## is about to get some tee output
    print join('; ', @{$printcmd})."\n" if (istrue($config->teeout) && !$fake);

    my %oldENV = %ENV;
    main::munge_environment(@repl) if $env_vars;
    ## go -- this is it.. issue the command and grab the result
    my $start = time;
    Log (125, "Start$desc command: ", ctime($start), " ($start)\n") unless istrue($config->fake);
    my $loglvl = 180;
    $loglvl = 0 if (istrue($config->teeout) || istrue($config->fake));
    if ($fake) {
	Log(0, "$cmd\n");
	$rc = 0;
    } else {
        if ($^O =~ /MSWin32/) {
	  # If $config->fake is true, but $fake is not, then this is an example
	  # command (such as 'make -n') which should be sent to the screen
	  # as well as the log.
	  foreach my $subcmd (@{$printcmd}) {
	    $rc = system $subcmd;
	    last if $rc;
	  }
          Log($loglvl, "\n%% Fake commands from $fake_cmd:\n") if istrue($config->fake);
	  # Log the contents of the output and errors files
	  Log($loglvl, "Command output:\n".join('', ::read_file($outname))."\n") if (-s $outname);
	  Log($loglvl, "Command errors:\n".join('', ::read_file($errname)."\n")) if (-s $errname);
          Log($loglvl, "%% End of fake output from $fake_cmd\n\n") if istrue($config->fake);
        } else {
          Log($loglvl, "\n%% Fake commands from $fake_cmd:\n") if istrue($config->fake);
	  $rc = my_system(istrue($config->teeout), $outn, $outname, $errname, $cmd);
          Log($loglvl, "%% End of fake output from $fake_cmd\n\n") if istrue($config->fake);
          # Sometimes this is polluted
          $? = $rc;
        }
    }
    my $stop = time;
    my $elapsed = $stop-$start;
    if (!istrue($config->fake)) {
      Log (125, "Stop$desc command: ", ctime($stop), " ($stop)\n");
      Log (125, "Elapsed time for$desc command: ", to_hms($elapsed), " ($elapsed)\n");
    }
    %ENV=%oldENV if $env_vars;

    if ($rc) { ## if there is a non-zero result from the $cmd
	if ($rc == $config->sigint && !$config->ignore_sigint) {
	    ## the command was interrupted
	    Log(0, "Exit on SIGINT\n"); 
	    do_exit(1);
	}

	## $temp holds the path for the error file
	my $temp = cwd() . '/' . $errname;
	Log(0, "Error with$desc '$cmd': check file '$temp'\n");
    }
    return $rc;
}

sub my_system {
  # Run a command, taking care of teeing and logging and everything
  # Log messages and errors at 180; 200 if teeout is on (since it will already
  # be sent to the screen)
  my ($tee, $basename, $outfile, $errfile, $cmd) = @_;

  my $loglevel = ($tee) ? 201 : 180;
  $exited = 0;
  $timeout = undef;

  local $SIG{'CHLD'} = sub { $exited = 1; $timeout = 1; };

  my ($rin, $rout, $ein, $eout) = ((undef) x 4);

  my ($stdout, $stderr) = (undef, undef);

  if ($basename ne '') {
    $stdout = new IO::File ">$outfile";
    if (!defined($stdout)) {
      Log(0, "\nCouldn't open $outfile for writing: $!\n");
      return -1;
    }
    $stderr = new IO::File ">$errfile";
    if (!defined($stderr)) {
      Log(0, "\nCouldn't open $errfile for writing: $!\n");
      return -1;
    }
  }

  pipe(CHILD_STDOUT_IN, CHILD_STDOUT_OUT);
  pipe(CHILD_STDERR_IN, CHILD_STDERR_OUT);

  my $pid = undef;
  if ($pid = fork()) {
    # Parent
    close(CHILD_STDOUT_OUT);
    close(CHILD_STDERR_OUT);
    $rin = $ein = '';
    vec($rin, fileno(CHILD_STDOUT_IN), 1) = 1;
    vec($rin, fileno(CHILD_STDERR_IN), 1) = 1;
    $ein = $rin;
    # Wait for I/O
    my $nfound;
    my $exitcode = undef;
    my $backoff = 0;
    while (($nfound = select($rout = $rin, undef, $eout = $ein, $timeout)) ||
           !$exited) {
      my $read_something = 0;
      if (vec($rout, fileno(CHILD_STDOUT_IN), 1)) {
        my $linebuf = undef;
        my $readlen = sysread(CHILD_STDOUT_IN, $linebuf, 131072);
        while ($readlen == 131072) {
          $read_something++;
          print "$linebuf" if $tee;
          Log($loglevel, "$linebuf");
          $stdout->print("$linebuf") if defined($stdout);
          # Just in case
          $linebuf = '';
	  $readlen = sysread(CHILD_STDOUT_IN, $linebuf, 131072);
        }
        $read_something++ if $linebuf ne '';
        print "$linebuf" if $tee;
        Log($loglevel, "$linebuf");
        $stdout->print("$linebuf") if defined($stdout);
      }
      if (vec($rout, fileno(CHILD_STDERR_IN), 1)) {
        my $linebuf = undef;
        my $readlen = sysread(CHILD_STDERR_IN, $linebuf, 131072);
        while ($readlen == 131072) {
          $read_something++;
          print "$linebuf" if $tee;
          Log($loglevel, "$linebuf");
          $stderr->print("$linebuf") if defined($stderr);
          # Just in case
          $linebuf = '';
	  $readlen = sysread(CHILD_STDERR_IN, $linebuf, 131072);
        }
        $read_something++ if $linebuf ne '';
        print "$linebuf" if $tee;
        Log($loglevel, "$linebuf");
        $stderr->print("$linebuf") if defined($stderr);
      }
      my $rc = waitpid($pid, WNOHANG);
      if ($rc == $pid) {
        # The child was harvested at this point
        $exitcode = $?;
        $exited = 1;
        $timeout = 1;   # Arrange for select to not block forever
      }
      last if $exited > 1;        # Go through one extra time
      $exited++ if $exited;
      if ($read_something == 0 && $nfound != 0) {
        # select() may be broken; slow things down for a bit.  This won't affect
        # the timing of benchmarks, which is done by specinvoke.
        sleep(2**$backoff);
        $backoff++ if $backoff < 3;    # Never sleep more than 8 seconds at a time
      } else {
        $backoff = 0;
      }
    }
    if (!defined($exitcode)) {
      waitpid($pid, 0);
      $exitcode = $?;
    }
#    Log(0, "Child exited with code $exitcode (rc=".(($exitcode & 0xff00) >> 8).', signal='.($exitcode & 0x7f).")\n");
    return $exitcode;
  } else {
    # Child
    close(CHILD_STDOUT_IN);
    close(CHILD_STDERR_IN);
    open(STDOUT, '>&CHILD_STDOUT_OUT') or die "Can't dup STDOUT: $!\nStopped";
    open(STDERR, '>&CHILD_STDERR_OUT') or die "Can't dup STDERR: $!\nStopped";
    autoflush STDOUT 1;
    autoflush STDERR 1;
    autoflush CHILD_STDOUT_OUT 1;
    autoflush CHILD_STDERR_OUT 1;
    if (!exec $cmd) {
      # Command didn't exist _and_ no shell was involved
      print STDERR "exec() failed: $!\n";
      exit 1;
    }
  }

}

sub munge_environment {
    my (@refs) = @_;
    no strict 'refs';

    while (@refs && ref($refs[0]) ne '') {
	my $ref = shift @refs;
	if (isa($ref, 'Spec::Config')) {
	    foreach my $key ($ref->list_keys) {
                if ($key =~ m/^ENV_(\S+)/) {
                    my $name = $1;
                    my $val = $ref->accessor($key);
                    Log(35, "Setting(config) environment variable \"$name\" to \"$val\"\n");
                    if ($val =~ /\s+$/) {
                        Log(0, "WARNING: Value for environment variable \"$name\" has trailing whitespace!\n");
                    }
                    $ENV{$name} = $val;
                }
	    }
	} elsif (isa($ref, 'HASH')) {
	    foreach my $key (keys %$ref) {
                if ($key =~ m/^ENV_(\S+)/) {
                    my $name = $1;
                    my $val = $ref->{$key};
                    Log(35, "Setting(hash) environment variable \"$name\" to \"$val\"\n");
                    if ($val =~ /\s+$/) {
                        Log(0, "WARNING: Value for environment variable \"$name\" has trailing whitespace!\n");
                    }
                    $ENV{$name} = $val;
                }
	    }
	}
    }
    while (@refs > 1) {
	my $name = shift @refs;
	my $val = shift @refs;
        if ($name =~ m/^ENV_(\S+)/) {
            Log(35, "Setting(param) environment variable \"$name\" to \"$val\"\n");
            if ($val =~ /\s+$/) {
                Log(0, "WARNING: Value for environment variable \"$name\" has trailing whitespace!\n");
            }
            $ENV{$name} = $val;
        }
    }
}

sub command_expand {
    my ($pattern, $s, @refs) = @_;

    # If the string does not contain any $, then there will not be anything
    # to expand.  But if the verbosity is sufficiently high, do it anyway.
    return $pattern unless $pattern =~ m/\$/ || $::global_config->verbose >= 35;

    my $last_string;
    my $do_setup = 1;
    if (defined($s)) {
        if (ref($s) eq 'Safe') {
            # The caller has provided a presumably pre-setup Safe compartment
            # with variables, etc, populated.
            $do_setup = 0;
        } else {
            # It's not a value, but it's defined, so put it back on the
            # list of "manual" refs
            unshift @refs, $s;
            $s = undef;
        }
    }
    $s = new_safe_compartment('tmp') unless defined($s);

    my $string = $pattern;
    my %paths = ();		# Path variables that may need
				# post-interpolation fixup if they contain \

    no strict 'refs';

    if ($do_setup) {
        while (@refs && ref($refs[0]) ne '') {
            my $ref = shift @refs;
            my $reftype = ref($ref);
            if ($reftype->isa('Spec::Config')) {
                for my $key ($ref->list_keys) {
                    my $val = $ref->accessor($key);
                    safe_store($key, $val, $s, 'config', \%paths);
                }
            } elsif ($reftype eq 'HASH') {
                for my $key (keys %$ref) {
                    my $val = $ref->{$key};
                    safe_store($key, $val, $s, 'hash', \%paths);
                }
            }
        }
        # These are expanded by specinvoke
        my $safevarref = $s->varglob('SPECCOPYNUM');
        $$safevarref = '$SPECCOPYNUM';
        $safevarref = $s->varglob('SPECUSERNUM');    # Deprecated.
        $$safevarref = '$SPECUSERNUM';               # Very.
        $safevarref = $s->varglob('BIND');
        $$safevarref = '$BIND';
    }
    $logged_vars = 1;

    $string = $pattern;
    for (my $i = 0; ; $i++) {
	$last_string = $string;
	$string =~ s/([\\])(.)/($2 eq '#')?"\\$1$2":"\\$1\\$2"/eg;
	$string =~ s/\#/\\\#/g;
	$string = $s->reval("qq#$string#;");
	if ($@) {
            Log(0, "expansion of '$pattern' in stage '$last_string' caused an eval error: $@\n");
            do_exit(1);
        }
	last if $string eq $last_string;
	if ($i >= 100) {
            Log(0, "expansion of '$pattern' resulted in $i recursions!\n");
            do_exit(1);
        }
    } 
    $string =~ s/([\\])(\#)/($2 eq '#')?"\\$1$2":"\\$1\\$2"/eg;
    $string =~ s/\#/\\\#/g;
    $string = $s->reval("qq#$string#;");
    # Now that all of the interpolations have happened, go back and make the
    # paths right.
    # We reverse sort by length of path to avoid screwing up more specific
    # paths by replacing their prefixes first.
    foreach my $pathkey (sort {	length($paths{$b}->[1]) <=> length($paths{$a}->[1]) } keys %paths) {
	$string =~ s/$paths{$pathkey}->[0]/$paths{$pathkey}->[1]/g;
    }

    return wantarray ? ($string, $s) : $string;
}

sub safe_store {
    my ($key, $val, $compartment, $label, $pathref) = @_;

    my $valtype = ref($val);
    # Only put scalars and arrays into the Safe compartment.
    return if ($valtype ne '' && $valtype ne 'ARRAY');

    # Some variables should not be put into the Safe compartment.
    return if ($key =~ /^(?:(?:raw|pp)txtconfig(?:all)?|oldmd5|compile_options)$/);

    my $safevarref = $compartment->varglob($key);
    if ($valtype eq 'ARRAY') {
	# There are a few arrays that should not be propagated
	return if ($key =~ /^(?:refs|formatlist|benchconf|benchsets|result_list|entries|runlist|setobjs|orig_argv)/);
	@{$safevarref} = @{$val};
	Log(35, "Setting($label) \"\@$key = (".join(', ', @{$val}).")\" for command substitution\n") unless ($logged_vars);
    } else {
	$$safevarref = $val;
	Log(35, "Setting($label) \"\$$key = $val\" for command substitution\n") unless ($logged_vars);
	if ($key =~ /(\S*(?:path|top))/o) {
	    # Transform any backslashes in the path to forward
	    # slashes, and stow the original result so that it
	    # can be put back after the eval (which would destroy
	    # the backslashes in the path)
	    my $fixpath = $val;
	    $fixpath =~ s/\\/\//go;
	    $pathref->{$key} = [ $fixpath, $val ];
	    $$safevarref = $fixpath;
	    Log(35, "  Storing $key for later fixup (new value: $fixpath)\n") unless ($logged_vars);
	}
    }
}

sub new_safe_compartment {
    my ($ns) = @_;

    my $s = new Safe $ns;
    if (istrue($main::global_config->safe_eval())) {
	# The line below is sufficient for operation, but because the
	# definition of the optags may change, we'll just list them all
	# out.  This list is from the Opcode module as shipped in Perl 5.8.7
        #$s->permit_only(':base_core', ':base_mem', 'padany', 'padsv', 'padav',
        #                'padhv', 'sprintf', 'localtime', 'gmtime');

	$s->permit_only(
			qw(
			   padany padsv padav padhv sprintf
			   localtime gmtime

			   null stub scalar pushmark wantarray const
			   defined undef
			   rv2sv sassign
			   rv2av aassign aelem aelemfast aslice av2arylen
			   rv2hv helem hslice each values keys exists delete
			   preinc i_preinc predec i_predec
			   postinc i_postinc postdec i_postdec
			   int hex oct abs pow
			   multiply i_multiply divide i_divide
			   modulo i_modulo add i_add subtract i_subtract
			   left_shift right_shift bit_and bit_xor bit_or
			   negate i_negate not complement
			   lt i_lt gt i_gt le i_le ge i_ge eq i_eq ne i_ne
			   ncmp i_ncmp slt sgt sle sge seq sne scmp
			   substr vec stringify study pos length index rindex
			   ord chr ucfirst lcfirst uc lc quotemeta trans chop
			   schop chomp schomp
			   match split qr
			   list lslice splice push pop shift unshift reverse
			   cond_expr flip flop andassign orassign and or xor
			   warn die lineseq nextstate scope enter leave setstate
			   rv2cv anoncode prototype
			   entersub leavesub leavesublv return
			   method method_named
			   leaveeval

			   concat repeat join range
			   anonlist anonhash
			 )
			);
    }
    return $s;
}

# Munge the command to redirect the output correctly for the OS
# also handle teeout
sub redirect_cmd {
    my ($cmd, $out, $err, $config) = @_;

    # Split the $cmd string on CR or LF because it's important that *all*
    # of the commands have the redirection applied to them.
    my @cmds = split(/[\r\n]+/, $cmd);

    if ((!defined($out) || $out eq '') &&
        (!defined($err) || $err eq '')) {
      return \@cmds;
    }

    if (istrue($config->teeout)) {
      if ($^O =~ /MSWin32/) {
	# Windows doesn't have tee(1); treat it as a normal output and let
	# log_system_raw dump the output files to the log and screen after
	# it's run.
	$cmd = [ shift(@cmds)." > $out 2> $err" ];
	push @{$cmd}, map { "$_  >> $out 2>> $err" } @cmds if (@cmds+0);
      } else {
	  $cmd = [ shift(@cmds)." 2> $err | tee $out" ];
	  push @{$cmd}, map { "$_  2>> $err | tee -a $out" } @cmds if (@cmds+0);
      }
    } else {
      $cmd = [ shift(@cmds)." > $out 2> $err" ];
      push @{$cmd}, map { "$_  >> $out 2>> $err" } @cmds if (@cmds+0);
    }
    return $cmd;
}

## ############
## SUB                   FROM_HMS
## ############

## takes strings of hh:mm:ss (or mm:ss) and returns number of seconds

sub from_hms {
    my ($time) = @_;
    my (@vals) = split (":", $time);
    $time = 0;
    for (@vals) {
        $time *= 60;
        $time += $_;
    }
    $time;
}

## ############
## SUB                   TO_HMS
## ############

## takes seconds and returns a string of hh:mm:ss
## optionally can take a second argument of decimal seconds

sub to_hms {
    my ($t, $t2) = @_;
    my ($h,$m,$s);
    $s = $t % 60;
    $t = int($t/60);
    $m = $t % 60;
    $h = int($t/60);
    if ($t2) {
	sprintf ("%02d:%02d:%02d.%06d", $h, $m, $s, $t2);
    } else {
	sprintf ("%02d:%02d:%02d", $h, $m, $s);
    }
}

## ############
## SUB                   READ_FILE
## ############
# IO::File will close the file when $fh goes out of scope

sub read_file {
    my($name) = @_;
    my (@temp);
    my $fh = new IO::File "<$name";
    return () if !defined $fh;
    return <$fh>;
}

sub lock_file {
    my ($fh) = @_;
    my $rc = undef;
    my $tries = 0;
    my $max_tries = 10;

    return (undef, 'badfh') unless defined($fh);

    while($tries < $max_tries && !(defined($rc) && $rc)) {
        eval '$rc = flock($fh, LOCK_EX);';
        return (undef, 'unimplemented', $@) if $@;
        if ($rc) {
            return($rc, 'ok');
        } else {
            Log(170, "LOCK ERROR: flock($fh, LOCK_EX) returned \"$!\"\n");
            sleep int(rand(5)) + 1;
        }
        $tries++;
    }
    return(undef, 'error', $!);
}

sub unlock_file {
    my ($fh) = @_;
    my $rc = undef;
    my $tries = 0;
    my $max_tries = 10;

    return (undef, 'badfh') unless defined($fh);

    $fh->flush();
    while($tries < $max_tries && !(defined($rc) && $rc)) {
        eval '$rc = flock($fh, LOCK_UN);';
        return (undef, 'unimplemented', $@) if $@;
        if ($rc) {
            return ($rc,   'ok');
        } else {
            Log(170, "LOCK ERROR: flock($fh, LOCK_UN) returned $!\n");
            sleep int(rand(5)) + 1;
            $tries++;
        }
    }
    return (undef, 'error', $!);
}

## ############
## SUB                   MIN
## ############

## takes a list of values and returns the least of them (numeric only)
sub min {
    my ($val) = @_;
    for (@_) {
        $val = $_ if $_ < $val;
    }
    return $val;
}

## ############
## SUB                   MAX
## ############

## takes a list of values and returns the greatest of them
sub max {
    my ($val) = @_;
    for (@_) {
        $val = $_ if $_ > $val;
    }
    return $val;
}

## ############
## SUB                   EXPAND_RANGES
## ############

sub expand_ranges {
    my (@data) = @_;
    my (@rc, $start, $stop, $step, $i);

    for (@data) {
	if (($start, $stop, $step) = m/^(\d+)-(\d+)(?:x(\d+))?$/) {
	    $step = 1 if $step eq '';
	    if ($start < $stop) {
		for ($i = $start; $i <= $stop; $i += $step) {
		    push (@rc, $i);
		}
	    } else {
		for ($i = $start; $i >= $stop; $i -= $step) {
		    push (@rc, $i);
		}
	    }
	} else {
	    push (@rc, $_);
	}
    }
    @rc;
}

sub uniq {
    my (@data) = @_;
    my %u = map { $_ => 1 } @data;
    return keys %u;
}

sub deep_copy {
    my ($objref) = @_;

    # Well, I had written one, but this is better:
    return $objref if (ref($objref) eq '');
    my $tmpref;
    eval { local($SIG{__WARN__}) = sub { 1; }; $tmpref = dclone($objref) };
    if ($@ && ($@ !~ /Can.t store item CODE/ || !$Storable::forgive_me)) {
	Log(0, "Error during deep_copy:\n  $@\n");
    }
    return $tmpref;
}

sub squeeze_undef {
    # Squeeze undefined values from an array
    my ($ary) = @_;

    return [] unless @{$ary};

    for(my $i = 0; $i < @{$ary}; $i++) {
        next if defined($ary->[$i]);
        # undef => remove it
        splice @{$ary}, $i, 1;
        redo if $i < @{$ary};   # $ary->[$i] is now the next element
    }

    return $ary;
}

sub center {
    my ($text, $width) = @_;
    my $len = length $text;
    $width = 78 if !defined $width;
    $width = $len if $width < $len;
    ' ' x int(($width - length $text) / 2) . $text;
}

sub wrap_lines {
    my ($lines, $columns, $indent) = @_;

    my @orig = @$lines;
    $lines = [];
    foreach my $line (@orig) {
	if (length($line) <= $columns) {
	    push @$lines, $line;
	} else {
	    my ($baseindent) = $line =~ /^(\s*)/o;
	    $Text::Wrap::columns = $columns;
	    $Text::Wrap::huge = 'die';
	    my $newline = '';
	    eval '$newline = Text::Wrap::wrap($baseindent, $baseindent.$indent, $line);';
	    if ($@) {
		# There was an element that was _so_ large that it was
		# larger than the maximum number of columns.  So log a
		# warning and leave the line unchanged.
		Log(0, "WARNING: the following line:\n-----\n$line\n-----\ncontains an element longer than $columns columns and could not be split.\n");
		push @$lines, $line;
	    } else {
		my @newlines = split(/\n+/, $newline);
		push @$lines, @newlines;
	    }
	}
    }

    return @{$lines};
}

sub wrap_join {
    # Take a list of things and return a list of strings with as many items
    # as can fit in $maxlen columns, indented with $ident and terminated by
    # $eol, except for the last string, which will be unterminated.
    my ($maxlen, $sep, $indent, $eol, @items) = @_;

    my @lines = ();
    my $currstring = shift @items;
    foreach my $item (@items) {
	if ($currstring ne '' &&
	    length($currstring) + length($sep) + length($item) >= $maxlen ||
	    length($currstring) + length($eol)                 >= $maxlen) {
	    push @lines, $currstring.$eol;
	    $currstring = $indent.$item;
	} else {
	    $currstring .= $sep.$item;
	}
    }

    push @lines, $currstring if ($currstring ne '');
    return @lines;
}

sub apply_diff {
    my ($path, $hunks) = @_;
    my ($all_ok, $offset_used) = (1, 0);

    my $fh = new IO::File '<'.$path;
    die "Couldn't open $path for reading: $!\nStopped" unless defined($fh);

    # This will take more memory (two copies of each file instead of just one),
    # but is pretty necessary to ensure that we have arrays of lines no matter
    # which OS happens to be running the script.
    local $/ = undef;		# Slurp mode
    my $contents = <$fh>;
    undef $fh;

    # Clean the line endings.
    # Don't use [\r\n]+ for the split because that removes sequences of
    # blank lines.
    my @oldfile = map { tr{\012\015}{\012\012}s; $_ } split(/(?:\n|\r\n)/, $contents);

    # The basic workings are from apply_diff in Algorithm::Apply::Diff

    my $delta = 0;
    my $hunk_count = 0;
    foreach my $changeref (@{$hunks}) {
	$hunk_count++;
        if ($diff_debug) {
            Log(0, "\n\n---------------------------------------------------------------\n");
            Log(0, "New hunk (#$hunk_count): base = ".$changeref->{'base'}."; context = ".$changeref->{'clines'}."\n");
            Log(0, "Current delta is $delta\n");
            Log(0, "\nCalling match_context($changeref->{'base'} + $delta, $changeref->{'context'}, \@oldfile)\n");
        }
	my $base = match_context($changeref->{'base'} + $delta,
			     $changeref->{'context'}, \@oldfile);
	my $offset = $base - ($changeref->{'base'} + $delta);
        Log(0, "base == $base\noffset == $offset\n") if $diff_debug;
	if (!defined($base)) {
	    Log(0, "ERROR: hunk $hunk_count FAILED at $changeref->{'base'} for $path\n");
	    $all_ok = 0;
	    next;
	}
	$offset_used++ if $offset;

	# The context finder effectively gets rid of any "delta" that we
	# might need to keep track of.  It would always be 0 if you could
	# ensure that the original file was the same as the file against
	# which the diff was generated.  But if you could guarantee that,
	# all of this context crap wouldn't be necessary.
	$delta -= ($base - $changeref->{'base'});	# Probably 0
        Log(0, "(context adj) delta == $delta\n") if $diff_debug;

	# Because finding the context effectively changes the offset into
	# the "new" file, it's necessary to adjust the index for new insertions
	# to take into account the "help" that we got from the context finder.
	my $newdelta = $changeref->{'base'} - $base + $offset;
        Log(0, "newdelta == $newdelta\n") if $diff_debug;
	foreach my $change (@{$changeref->{'diffs'}}) {
	    if (ref($change) eq 'ARRAY') {
		my ($pos, $line, $repl) = @{$change};
                my $del_line = $repl + $base + $delta + $offset;
		if (defined($repl)) {
		    # Do the removal
		    my @oldlines = splice(@oldfile, $del_line, 1);
                    Log(0, "Removed line at ($repl + $base + $delta + $offset = $del_line):\n  ".join("\n  ", @oldlines)."\n") if $diff_debug;
		    --$delta;	# Because it will be incremented again shortly
		}
		# Do the add
		splice(@oldfile, $pos + $base + $newdelta, 0, $line);
                Log(0, "Added line at ($pos + $base + $newdelta = ".($pos + $base + $newdelta).") (delta == $delta):\n  $line\n") if $diff_debug;
		++$delta;
	    } elsif (ref($change) eq '' && ($change <= 0)) {
		# It's a delete
                my $del_line = (-1 * $change) + $base + $delta + $offset;
                if ($del_line <= $#oldfile) {
                    my @oldlines = splice(@oldfile, $del_line, 1);
                    Log(0, "Deleted lines at ((-1 * $change) + $base + $delta + $offset = $del_line):\n  ".join("\n  ", @oldlines)."\n") if $diff_debug;
                    --$delta;
                } elsif ($diff_debug) {
                    Log(0, "Can't delete line $del_line of ".$#oldfile.": out of range\n");
                }
	    } else {
		die "Huh?  Bad diff ($change)\n";
	    }
	}
	$delta += ($base - $changeref->{'base'});	# Probably 0
        if ($diff_debug) {
            Log(0, "(context REadj) delta == $delta\n");
            print_range(0, \@oldfile, "Finished hunk:\n  ", "\n  ", ($changeref->{'base'} - 5), ($changeref->{'base'} + 10)); Log(0, "\n");
            Log(0, "(post apply) delta == $delta\n");
        }
	if ($offset) {
	    Log(125, "    hunk $hunk_count offset ".pluralize($offset, 'line')." for $path\n");
	} else {
            Log(125, "    hunk $hunk_count applied cleanly to $path\n");
	}
    }

    # Rewrite the file now that it has been transformed into its new form...
    $fh = new IO::File '>'.$path;
    $fh->print(join("\012", map { defined($_) ? $_ : "HEY! UNDEF LINE!" } @oldfile)."\012");
    $fh->close();

    return (md5diffdigest($path), $offset_used, $all_ok);
}

sub match_context {
    my ($base, $context, $lines) = @_;
    my $rc = undef;

    # Find and return the index into @$lines where all of @$context can be
    # found.  Return undef if not found.

    return undef if (ref($context) ne 'ARRAY' || ref($lines) ne 'ARRAY');

    $rc = search_context($base, 1, $context, $lines);
    $rc = search_context($base, -1, $context, $lines) if (!defined($rc));

    return $rc;
}

sub search_context {
    my ($base, $inc, $context, $lines) = @_;
    # This actually does the work that you thought match_context would do

    if ($diff_debug) {
        Log(0, "search_context($base, $inc, $context, $lines) called\n");
        print_range(0, $context, "Context:\n  ", "\n  "); Log(0, "\n");
        print_range(0, $lines, "Lines[$base..]:\n  ", "\n  ", $base, $base + $#{$context}); Log(0, "\n");
    }

    for(; $base < @{$lines}+0 && $base >= 0; $base += $inc) {
	my $idx = 0;
	while(defined($lines->[$base + $idx]) && defined($context->[$idx]) &&
	      ($lines->[$base + $idx] eq $context->[$idx])) {
	    $idx++;
	}
	if ($idx >= @{$context}+0) {
	    # Finished!
            Log(0, "Found at $base\n") if $diff_debug;
	    return $base;
	}
    }
    Log(0, "Not found\n") if $diff_debug;
    return undef;
}

sub print_range {
    my ($loglevel, $aref, $head, $eol, $start, $end) = @_;
    $start = 0 unless defined $start;
    $end = $#{$aref} unless defined $end;
    Log($loglevel, $head) if defined($head);
    $eol = "\n" unless defined($eol) && $eol ne '';
    while($start < 0) {
        Log($loglevel, sprintf("%05d: undef$eol", $start));
        $start++;
    }
    while($start < $end) {
        if ($start <= $#{$aref}) {
            Log($loglevel, sprintf("%05d: %s$eol", $start, defined($aref->[$start]) ? $aref->[$start] : 'undef'));
            $start++;
        } else {
            Log($loglevel, sprintf("%05d: *** out of range ***$eol", $start, ));
            $start++;
        }
    }
}

sub new_user_agent {
    my $newua = LWP::UserAgent->new;
    $newua->agent("${main::suite}/${main::suite_version} ");
    $newua->protocols_allowed([qw( file http ftp )]);
    $newua->env_proxy;

    return $newua;
}

sub update_manifest {
    my (@files) = @_;
    my %files = map { $_ => 1 } @files;
    my @lines;

    # First read in the contents of the MANIFEST
    my $fh = new IO::File '<'.jp($ENV{'SPEC'}, 'MANIFEST');
    if (!defined($fh)) {
        Log(0, "\nERROR: Could not open MANIFEST for reading: $!\n\n");
        do_exit(1);
    }
    while (my $line = <$fh>) {
        my ($stuff, $file) = ($line =~ m/^(.* [0-9A-F]{8}) (.*)$/);
        next if exists $files{$file};
        push @lines, $line;
    }
    $fh->close();

    # Now rewrite it with the just the lines that aren't the files we want
    $fh = new IO::File '>'.jp($ENV{'SPEC'}, 'MANIFEST');
    if (!defined($fh)) {
        Log(0, "\nERROR: Could not open MANIFEST for writing: $!\n\n");
        do_exit(1);
    }
    $fh->print(@lines);

    # Now run specmd5sum to generate sums for the new files
    if (!open(MD5, '-|', 'specmd5sum', '-e', @files)) {
        $fh->close();   # Flush what we got
        do_exit(1);
    }
    $fh->print(<MD5>);
    $fh->close();
    close MD5;

    # Also update MANIFEST.pl
    system("specperl", jp($ENV{'SPEC'}, qw(bin scripts.misc convert_manifest)));
}

sub check_version {
    # Phone home (to SPEC) to see if a newer version of the suite has been
    # released.
    my ($url, $timeout, $proxy, $will_continue) = @_;
    my ($ver, $date, $pause) = (undef, undef, 0);
    $url = $::default_config->{'version_url'} unless defined($url);
    $timeout ||= 30;

    Log(1, "Loading \"$url\" for version check: ");
    $ua = new_user_agent() unless defined($ua);
    $ua->timeout($timeout);
    $ua->proxy(['http', 'ftp'], $proxy) if (defined($proxy) && $proxy ne '');
    my $res = $ua->get($url);
    if ($res->is_success) {
        Log(1, "OK\n");
        ($ver, $date) = split(/\s+/, $res->content);
        my $suitever = $::suite_version;
        $suitever /= 1000 if $suitever > 10; # Deal with pre-release
        if ($ver < $suitever) {
            Log(0, "\nERROR: You are running a version of the suite that is newer than the newest\n         available from SPEC.  How did you do that?\n\n");
            $pause = 1;
        } elsif ($ver > $suitever) {
            Log(0, "\nNOTICE: There is a newer version of the suite available from SPEC.\n  Version $ver was released on ".scalar(CORE::localtime($date))."\n\n");
            $pause = 1;
        }
    } else {
        Log(1, "FAILED\n");
        Log(0, "\nNOTICE: Suite version checking failed; got\n  ".$res->status_line."\n");
        Log(0, "          A connection to the internet is not required in order to run\n");
        Log(0, "          $::suite.  However, if one is available, several components will\n");
        Log(0, "          be checked to see if they are current.  If you need to use an\n");
        Log(0, "          HTTP proxy to access the Internet, please see the 'http_proxy'\n");
        Log(0, "          entry in config.html.\n\n");
        $pause = 1;
    }
    if ($pause && $will_continue) {
        my $spaces = " " x 23;
        Log(0, "${spaces}----------------------------------\n");
        Log(0, "${spaces}The run will continue in 5 seconds\n");
        Log(0, "${spaces}----------------------------------\n");
        sleep 5;
    }

    return($ver, $date);
}

sub shift_indices {
    my ($obj, $index, $delta) = @_;

    return if $delta == 0;

    my @idxkeys = grep { /^cfidx_/ } keys %{$obj};
    foreach my $idxkey (@idxkeys) {
	if ($obj->{$idxkey} >= $index) {
	    $obj->{$idxkey} += $delta;
	}
    }

}

sub update_stored_config {
    my ($obj, $oldtag, $newtag, $text, $updateline, $no_backup) = @_;

    # Make versions of the tags suitable for printing in error messages
    my $printold = $oldtag;
    $printold =~ s/(.)/chr(ord($1) & 0x7f)/goe;
    $printold .= '(+high bits)' if $printold ne $oldtag;

    my $printnew = $newtag;
    $printnew =~ s/(.)/chr(ord($1) & 0x7f)/goe;
    $printnew .= '(+high bits)' if $printnew ne $newtag;

    # Fix up the $tag item in the array $obj->{'txtconfig'} (which should
    # be the text of a config file).  The index into the array is expected
    # to be found in cfidx_$oldtag, unless $oldtag is undef, in which case
    # it'll be added to the end of the config file, in a 'default:' section
    # which is automatically added.

    if (!exists $obj->{'txtconfig'}) {
	Log(0, "ERROR(update_stored_config): $obj does not contain a txtconfig member!\n");
	return 0;
    }
    if (ref($obj->{'txtconfig'}) ne 'ARRAY') {
	Log(0, "ERROR(update_stored_config): The txtconfig member in $obj is not an array!\n");
	return 0;
    }
    if (!defined($newtag) || $newtag eq '') {
	if (!defined($oldtag) || $oldtag eq '') {
	    Log(0, "ERROR(update_stored_config): Both new and old tags are empty!\n");
	    return 0;
	} else {
	    $newtag = $oldtag;
            $printnew = $printold;
	}
    }
    if (!defined($text)) {
	# The line pointed to by $oldtag must be removed
	if (!defined($oldtag) || $oldtag eq '' ||
	    !exists($obj->{'cfidx_'.$oldtag})) {
	    Log(0, "ERROR(update_stored_config): Can't delete line '$printold'; index not found.\n");
	    return 0;
	}

        # Do a rudimentary check to ensure that the line being deleted
        # actually contains the text of $oldtag.  This protects against
        # corruption of the cfidx list, which should never happen.
        if ($obj->{'txtconfig'}->[$obj->{'cfidx_'.$oldtag}] !~ /$oldtag/) {
	    Log(0, "ERROR(update_stored_config): Won't delete line ".$obj->{'cfidx_'.$oldtag}."; line does not contain '$printold'.\n");
            # Get rid of the faulty index
            delete $obj->{'cfidx_'.$oldtag};
	    return 0;
        }

	# Save a copy of the old config (maybe)
        if (!$no_backup) {
            @{$obj->{'orig_raw_config'}} = @{$obj->{'txtconfig'}} unless exists($obj->{'orig_raw_config'});
            $obj->{'orig_raw_good'} = 1;
        }

	# Delete the offending line
	splice(@{$obj->{'txtconfig'}}, $obj->{'cfidx_'.$oldtag}, 1);

	# Adjust the indices of lines that used to follow
	::shift_indices($obj, $obj->{'cfidx_'.$oldtag}, -1);

	# Get rid of the old index
	delete $obj->{'cfidx_'.$oldtag};

	return 1;

    }

    if (!defined($oldtag)) {
	# This must be an insertion!
	my $idx = undef;
	if ($newtag =~ s/:(\d+)//) {
	    $idx = $1;
	}
	if (exists($obj->{'cfidx_'.$newtag})) {
	    Log(0, "ERROR(update_config_file): Cannot insert new line with same tag\n                            ($printnew) as existing line!\n");
	    return 0;
	}

	# Save a copy of the old config (maybe)
        if (!$no_backup) {
            @{$obj->{'orig_raw_config'}} = @{$obj->{'txtconfig'}} unless exists($obj->{'orig_raw_config'});
            $obj->{'orig_raw_good'} = 1;
        }

	if (defined($idx)) {
	    # There's a specific place to put it
	    splice @{$obj->{'txtconfig'}}, $idx, 0, "$newtag = $text";
	    # Fix up all the indices that have just been bumped
	    ::shift_indices($obj, $idx, 1);

	    $obj->{'cfidx_'.$newtag} = $idx;
	} else {
	    # Just put it in at the end
	    if (!exists($obj->{'cfidx_tools_added_default_section'}) ||
		$obj->{'cfidx_tools_added_default_section'} <= 0) {
		push @{$obj->{'txtconfig'}}, '', '# The following section was added automatically, and contains settings that', '# did not appear in the original configuration file, but were added to the', '# raw file after the run.', 'default:';
		$obj->{'cfidx_tools_added_default_section'} = $#{$obj->{'txtconfig'}};
	    }
	    push @{$obj->{'txtconfig'}}, "$newtag = $text";
	    $obj->{'cfidx_'.$newtag} = $#{$obj->{'txtconfig'}};
	}

	return 1;
    }

    # If it's not an insertion and not a deletion, it must be a change.  The
    # fun begins... now!
    if (!exists($obj->{'cfidx_'.$oldtag})) {
	Log(0, "ERROR(update_config_file): No index found for '$printold'; cannot change line!\n");
	return 0;
    }
    my $idx = $obj->{'cfidx_'.$oldtag};
    if (!defined($obj->{'txtconfig'}->[$idx])) {
	Log(0, "ERROR(update_config_file): No config file line found at index $idx!\n");
	return 0;
    }
    if (exists($obj->{'cfidx_'.$newtag}) &&
	$obj->{'cfidx_'.$newtag} != $obj->{'cfidx_'.$oldtag}) {
	Log(0, "ERROR(update_config_file): Cannot change \"$printold\" \@ $idx;\n                             \"$printnew\" already exists \@ ".$obj->{'cfidx_'.$newtag}."!\n");
	return 0;
    }

    # Save a copy of the old config (maybe)
    if (!$no_backup) {
        @{$obj->{'orig_raw_config'}} = @{$obj->{'txtconfig'}} unless exists($obj->{'orig_raw_config'});
        $obj->{'orig_raw_good'} = 1;
    }

    # Make things easier by expanding tabs
    $tabstop = 8;
    my $tmpline = expand($obj->{'txtconfig'}->[$idx]);
    my ($initial_whitespace, $found_tag, $post_tag_whitespace, $pre_text_whitespace, $found_text);
    if ($tmpline =~ /^(\s*)(\S+)(\s*)=(\s*)(.*)$/) {
	$initial_whitespace  = $1;
	$found_tag           = $2;
	$post_tag_whitespace = $3;
	$pre_text_whitespace = $4;
	$found_text          = $5;
    } else {
	Log(0, "ERROR(update_config_file): Config file line at $idx does not look like\n                            a variable setting!\n");
	return 0;
    }
    if ($found_tag ne $oldtag) {
        my $printfound = $found_tag;
        $printfound =~ s/(.)/chr(ord($1) & 0x7f)/goe;
        $printfound .= '(+high bits)' if $printfound ne $found_tag;
	Log(0, "ERROR(update_config_file): Config file line at $idx ($printfound) does not match $printold!\n");
	return 0;
    }
    # Adjust the length of the variable section, preserving indentation where
    # possible.
    my $origlen = length($found_tag);
    my $newlen = length($newtag);
    my $newline;
    if ($newlen == $origlen) {
	# Easy!  Just replace $found_tag with $newtag
	$newline = $initial_whitespace.$newtag.$post_tag_whitespace.'=';
    } elsif ($origlen > $newlen) {
	# Also easy! Just pad out $post_tag_whitespace
	$newline = $initial_whitespace.$newtag.$post_tag_whitespace.
	    sprintf '%*s=', ($origlen - $newlen), ' ';
    } elsif ($origlen + length($post_tag_whitespace) > $newlen) {
	# Easy easy! Just remove some from $post_tag_whitespace
	$newline = $initial_whitespace.$newtag.
	    sprintf '%*s=', (($origlen + length($post_tag_whitespace)) - $newlen), ' ';
    } else {
	# Okay, also not too hard; just make the total length the same (if
	# possible), and have no whitespace before the equals
	$newline = sprintf '%*s=', (length($initial_whitespace) + $origlen + length($post_tag_whitespace)), $newtag;
    }

    if ($updateline) {
	# Replace the text, preserving whitespace around the tag, the equals,
	# and (if the tag does not contain "notes") the beginning of the text.
	$pre_text_whitespace = '' if ($oldtag =~ /notes/);
	$newline .= $pre_text_whitespace.$text;
    } else {
	$newline .= $pre_text_whitespace.$found_text;
    }
    $obj->{'txtconfig'}->[$idx] = $newline;
    delete $obj->{'cfidx_'.$oldtag};
    $obj->{'cfidx_'.$newtag} = $idx;
    return 1;
}

sub renumber_notes {
    my ($result, $format, $backup) = @_;

    foreach my $section (grep { /^notes/ } keys %{$result}) {
        next unless ref($result->{$section}) eq 'HASH';
        foreach my $key (sort keys %{$result->{$section}}) {
            next unless ref($result->{$section}->{$key}) eq 'ARRAY';
            my $notesref = $result->{$section}->{$key};
            my @newnotes = ();

            # Do the notes renumbering in two passes to avoid trashing
            # unrenumbered lines with newly renumbered lines.
            my $cfline = undef;
            for(my $i = 0; $i < @{$notesref}; $i++) {
                next unless ref($notesref->[$i]) eq 'ARRAY';
                my ($updateline, $oldtag, $note);
                if ($format == 3) {
                    ($updateline, $oldtag, $note) = @{$notesref->[$i]};
                } else {
                    $updateline = 1;
                    ($oldtag, $note) = @{$notesref->[$i]};
                }
                my $newtag = sprintf("%s%s_%03d", $section,
                                                  ($key ne '') ? "_$key" : '',
                                                  ($i * 5));
                # The rename tag needs to be guaranteed unique, but it also
                # needs to be the same length as $newtag, so just set the
                # high bits on all the characters.  It's easy to undo, and
                # because of the regexp in the config file reader, it's
                # guaranteed to not clash with any "natural" name.
                my $renametag = $newtag;
                $renametag =~ s/(.)/chr(ord(uc($1)) | 0x80)/goe;

                if (defined($oldtag)) {
                    $cfline = $result->{'cfidx_'.$oldtag};
                    $cfline = undef if ($cfline eq '');
                } else {
                    $renametag .= ":$cfline" if (defined($cfline));
                }

                # Fix up the tag (and maybe the line itself) in the stored
                # config file, and do _not_ create a backup.
                if (::update_stored_config($result, $oldtag, $renametag, $note, $updateline, !$backup)) {
                    push @newnotes, [ $newtag, $note ];
                    $cfline++;
                } else {
                    ::Log(0, "ERROR: Edit of stored config file failed;\n        Could not change '$oldtag' to '$newtag' (1st pass).\n");
                }
            }

            # The final rename pass.
            @{$notesref} = ();
            foreach my $noteref (@newnotes) {
                my ($tag, $note) = @$noteref;
                my $renametag = $tag;
                $renametag =~ s/(.)/chr(ord(uc($1)) | 0x80)/goe;
                if (::update_stored_config($result, $renametag, $tag, $note, 0, !$backup)) {
                    push @{$notesref}, [ $tag, $note ];
                } else {
                    ::Log(0, "ERROR: Edit of stored config file failed;\n        Could not change temporary tag to '$tag' (2nd pass).\n");
                }
            }
        }
    }

}

sub compress_encode {
    # Compress and Base-64 encode the input string.  Returns the original
    # string, the compressed version, and the encoded version.  An undef in
    # a return slot indicates a failure of the operation.
    my ($input) = @_;
    my @tries = (['*', $input]);

    my ($compressed, $encoded) = (undef, undef);
    eval '$compressed = Compress::Zlib::compress($input, 9)';
    if (!$@ && defined($compressed)) {
	push @tries, [ '@', $compressed ];
    }
    eval '$compressed = Compress::Bzip2::memBzip($input)';
    if (!$@ && defined($compressed)) {
	push @tries, [ '&', $compressed ];
    }

    # Sort them in order of length, so as to use the shortest one
    @tries = sort { length($a->[1]) <=> length($b->[1]) } @tries;
#print "Chose $tries[0]->[0] (".join(' < ', map { length($_->[1]) } @tries).")\n";

    $encoded = $tries[0]->[0].encode_base64($tries[0]->[1]);

    return wantarray ? ($input, $tries[0]->[1], $encoded) : $encoded;

}

sub decode_decompress {
    # Base-64 decode and uncompress the input string.  Returns the original
    # string, the decoded version, and the decompressed version.  An undef in
    # a return slot indicates a failure of the operation.
    my ($input, $gzip) = @_;
    my ($uncompressed, $decoded) = (undef, undef);
    my $copy = undef;
    my $type = '';
    if ($input =~ /^\s*([\*\&\@])?([A-Za-z0-9+\/=\012\015]+)$/os) {
        $copy = $decoded = decode_base64($2);
	$type = $1 || '#';
    } else {
        $decoded = undef;
        $copy = $input;
    }

    if ($type eq '*') {
	$uncompressed = $decoded;
    } elsif ($type eq '@') {
	eval '$uncompressed = Compress::Zlib::uncompress($copy)';
    } elsif ($type eq '#') {
	eval '$uncompressed = Compress::Zlib::memGunzip($copy)';
    } elsif ($type eq '&') {
	eval '$uncompressed = Compress::Bzip2::memBunzip($copy)';
    }
    $uncompressed = undef if ($@);

    return wantarray ? ($input, $decoded, $uncompressed) : $uncompressed;
}

sub check_elem {
    my ($endtype, $start, @keys) = @_;
    my $curr = $start;
    my $lastkey = pop @keys;

    return 0 unless isa($start, 'HASH');

    # For a chained hash, check to see that each element exists and is a
    # HASH ref, except for the end, which must be of $endtype type.
    foreach my $key (@keys) {
	return 0 unless exists($start->{$key}) && isa($start->{$key}, 'HASH');
	$start = $start->{$key};
    }
    return 0 unless exists($start->{$lastkey});

    if (defined($endtype) && $endtype ne '') {
	return 0 unless isa($start->{$lastkey}, $endtype);
    }
    return 1;
}

sub find_benchobj {
    my ($bench, $tune, $ext, $mach, $objref) = @_;

    return {} unless ref($objref) eq 'ARRAY';
    foreach my $obj (@{$objref}) {
        next unless isa($obj, 'HASH');
        next unless ($obj->num.'.'.$obj->name eq $bench);
        next unless ($obj->tune eq $tune);
        next unless ($obj->ext eq $ext);
        next unless ($obj->mach eq $mach);
        return $obj;
    }
    return {};
}

sub basepeak_munge {
    my ($result, $bp_only, @benchmarks) = @_;

    return unless $result->{'basepeak'} == 1 || $result->{'basepeak'} == 2;

    # It's stupid to do base->peak substitution if there's no base to
    # substitute in!
    my $hasbase = 1 if grep { /^base$/ } @{$result->{'tunelist'}};

    if ($result->{'basepeak'} == 1) {
	$bp_only = 1;
	@benchmarks = sort keys %{$result->benchmarks};
    }

    if ($bp_only || $::lcsuite eq 'cpu2006') {
        if ($bp_only) {
          Log(10, "Doing base -> peak substitution for all benchmarks\n");
        } else {
          Log(10, "Doing base -> peak substitution for some benchmarks (per-benchmark basepeak)\n");
        }
        # Per run rules section 3.5, the base result is used for all
        # per-benchmark basepeak munging.
	for my $bench (@benchmarks) {
	    my $benchref = $result->{'results'}->{$bench};
	    # Make sure the data structures exist
	    foreach my $tmptune ('base', 'peak') {
		if (!isa($benchref->{$tmptune}, 'HASH')) {
		    $benchref->{$tmptune} = {'data' => [] };
		}
                foreach my $iterref (@{$benchref->{$tmptune}{'data'}}) {
                    $iterref->{'basepeak'} = 1;
                }
	    }
            if ($hasbase) {
              @{$benchref->{'peak'}{'data'}} = @{$benchref->{'base'}{'data'}};
            }
	}
    } elsif (@benchmarks) {
        Log(10, "Doing lowest median substitution for some benchmarks (per-benchmark basepeak)\n");
        for my $bench (@benchmarks) {
	    next unless exists($result->{'results'}->{$bench});
	    Log(10, "  basepeak lowest median sub for $bench\n");
	    my %tmpmedian = ();
	    my $benchref = $result->{'results'}->{$bench};
	    for my $tune (keys %{$benchref}) {
		my @tmpdata = ();
		for my $obj (@{$benchref->{$tune}{'data'}}) {
		    $obj->{'basepeak'} = 1;
		    next unless ($obj->valid eq 'S');
		    push (@tmpdata, $obj);
		}
		$tmpmedian{$tune} = median_ratio(0, @tmpdata);
	    }
	    my @sortres = sort { $tmpmedian{$a} <=> $tmpmedian{$b} } keys %tmpmedian;
	    for (my $i = 1; $i < @sortres; $i++) {
		Log(89, "Setting $sortres[$i] to $sortres[0] for $bench\n");
		dupe_results($benchref->{$sortres[$i]}->{'data'},
			     $benchref->{$sortres[0]}->{'data'});
	    }
        }
    }
}

sub dupe_results {
    my ($dest, $src) = @_;

    return unless (ref($dest) eq 'ARRAY' && ref($src) eq 'ARRAY');

    # Do a deep copy, except for the 'tune', 'ref', and 'refs' members
    for (my $i = 0; $i < @{$src}; $i++) {
	my $href = $src->[$i];
	for my $key (keys %{$href}) {
	    next if ($key =~ /^(tune|ref|refs)$/o);
	    $dest->[$i]->{$key} = $src->[$i]->{$key};
	}
    }
}
    
# SIDE EFFECT:  Sets the selected bit for the result if select is nonzero
sub median_ratio {
    my ($select, @objs) = @_;
    my $numobjs = @objs+0;
    return undef if $numobjs <= 0;
    Log(70, "median_ratio(select=$select) for ".$objs[0]->benchmark.'='.$objs[0]->tune.'='.$objs[0]->ext.'='.$objs[0]->mach."\n");
    Log(70, "         ratios and runtimes:\n             ".join("\n             ", map { '[ '.$_->reported_time.' = '.$_->ratio.' ]' } @objs)."\n");
    # Sort by runtime and not ratio; for non 'ref' runs, ratio is always '--'!
    @objs = sort { $a->reported_time <=> $b->reported_time } @objs;
    Log(70, "  sorted ratios and runtimes:\n             ".join("\n             ", map { '[ '.$_->reported_time.' = '.$_->ratio.' ]' } @objs)."\n");
    my $sel;
    if (($numobjs > 1) && (($numobjs % 2) == 0)) {
        # For even # of runs, return the lower median.
        # See chapter 9 of Cormen, Thomas, et al. _Introduction to Algorithms,
        #   2nd Edtion_. Cambridge: MIT Press, 2001
        $sel = int($numobjs / 2) - 1;
	Log(70, "  Even number of runs! Selecting the lower median (run #".($sel + 1)." of $numobjs)\n");
    } else {
        $sel = int($numobjs / 2);
	Log(70, "  Odd number of runs! Choosing #".($sel + 1)." of $numobjs\n");
    }
    $objs[$sel]->selected(1) if $select;
    Log(70, "  Returned ratio = ".$objs[$sel]->ratio."\n");
    return $objs[$sel]->ratio;
}

sub expand_all {
    my (@selection) = @_;

    # Convert 'all' to ('int', 'fp') as decided by the committee.
    # This makes things nicer in several ways, but less general
    # Position is important, so we search for the index of 'all' so we can
    # use splice.
    for (my $i = 0; $i < @selection; $i++) {
	if ($selection[$i] =~ /(\^)*(all|${main::lcsuite})\b/io) {
	    my $not = $1;
            my $what = $2;
	    splice @selection, $i, 1, ("${not}int", "${not}fp");
	    Log(24, "'${not}${what}' replaced by ('${not}int', '${not}fp') at position $i of selection list\n");
	}
    }

    return @selection;
}

sub pluralize {
    my ($count, $word, @verb) = @_;

    my $rc = $count.' '.$word;
    $rc .= 's' if ($count != 1);
    $rc .= ' '.$verb[($count == 1) ? 0 : 1] if (@verb);
    return $rc;
}

sub assemble_cpu_description {
    my ($config) = @_;
    my $hw_ncpu = '';

    # Given these four fields:
    #  hw_ncpu  (n chips)
    #  hw_ncores (n cores)
    #  hw_coresperchip (n cores/chip)
    #  hw_nthreadspercore (n threads/core)
    # This routine assembles a nice text field which is VERY similar to what
    # we have in CPU2000.

    my $hw_nchips = firstof($config->accessor_nowarn('hw_nchips'));
    $hw_nchips = -1 if (!defined($hw_nchips) || $hw_nchips eq '--');

    my $hw_ncores = firstof($config->accessor_nowarn('hw_ncores'));
    $hw_ncores = -1 if (!defined($hw_ncores) || $hw_ncores eq '--');

    my $hw_ncoreschip = firstof($config->accessor_nowarn('hw_ncoresperchip'));
    $hw_ncoreschip = -1 if (!defined($hw_ncoreschip) || $hw_ncoreschip eq '--');

    my $hw_nthreadspercore = firstof($config->accessor_nowarn('hw_nthreadspercore'));
    $hw_nthreadspercore = -1 if (!defined($hw_nthreadspercore) || $hw_nthreadspercore eq '--');

    $hw_ncpu  = ::pluralize($hw_ncores, 'core').', ';
    $hw_ncpu .= ::pluralize($hw_nchips, 'chip').', ';
    $hw_ncpu .= ::pluralize($hw_ncoreschip, 'core').'/chip';
    if ($hw_nthreadspercore != 1) {
        $hw_ncpu .= ', '.::pluralize($hw_nthreadspercore, 'thread').'/core';
    }

    return $hw_ncpu;
}

sub store_flags_file {
  # Given some flags text, look in the flags storage area for an pre-existing
  # copy and point to it if it exists.  Otherwise write it there and return
  # a URL pointing to it.
  my ($flagtext, $flags) = @_;
  my $name = $flags->{'filename'};

  # We'll need the MD5 no matter what...
  my $md5 = new Digest::MD5;
  $md5->add($flagtext);
  $md5 = $md5->hexdigest();
  ::Log(15, "Storing or referencing a flags file with sum $md5\n");

  # ...and the flag file's name.
  if (!defined($name) || $name eq '') {
    $name = $::suite.'_flags';
  }
  ::Log(15, "  User-requested name is \"$name\"\n");
  # Clean it up, if necessary
  $name =~ tr/-a-zA-Z0-9./_/cs;
  ::Log(15, "  Sanitized name is \"$name\"\n");

  # Make a list file
  my $listfile = Spec::Listfile->new($::flag_report_base, 'flagidx');
  # Change the permissions back to something reasonable; this isn't the
  # happy world of a running benchmark, after all.
  chmod 0664, ::jp($::flag_report_base, 'flagidx');

  # Look for a match
  my $entry = $listfile->find_entry($::flag_report_base, 'md5' => $md5);
  $entry = $listfile->new_entry($name,
                                'md5' => $md5,
                                'num_width' => 2,
                                'num_optional' => 1,
                                'num_is_date' => 1) unless $entry;

  # At this point, there's an entry that points to where our flags file
  # should go.  If the size doesn't match (or it doesn't exist at all),
  # write it out.
  my $filename = $entry->path.'.xml';
  if ( -s $filename != length($flagtext) ) {
    my $fh = new IO::File '>'.$filename;
    if (!defined($fh)) {
      ::Log(0, "\nERROR: Could not open $filename for writing: $!\n");
      $entry->delete();
      $listfile->remove_entry('md5' => $md5);
      $listfile->update();
      $listfile->close();
      do_exit(1);
    }
    $fh->print($flagtext);
    $fh->close();
    
    # Now dump it
    if ($fh->open('>'.$entry->path.'.html')) {
      # It's okay if this fails, since it can be re-done by hand
      Spec::Format::flags::flags_to_html($flags, $fh,
                                         !$::format_for_publication);
      $fh->close();
    } else {
      ::Log(0, "\nWARNING: Could not open ".$entry->path.".html for writing: $!\n");
    }
  }
  $listfile->update();
  $listfile->close();

  ::Log(15, "  Final flags URL is \"".$::flag_report_url_base.$entry->name.".xml\"\n");
  return $::flag_report_url_base.$entry->name.'.xml';
}

sub get_format {
    # Return a format object (from formatlist) based on the name (which must
    # be one of the synonyms that the format lists for itself).
    my ($formatlist, $name) = @_;

    return { } unless isa($formatlist, 'HASH');

    foreach my $format (keys %{$formatlist}) {
        return $formatlist->{$format} if exists($formatlist->{$format}->{'synonyms'}->{lc($name)});
    }
    return { };
}

sub make_path_re {
    my ($path) = @_;

    # Given a path, turn it into a regular expression where either kind of
    # slash matches, and where the match is not case-sensitive on Windows.
    $path =~ s#[\\/]#\[\\/\]#g;
    if ($^O =~ /MSWin/) {
        return qr/$path/i;	# Not case-sensitive
    } else {
        return qr/$path/;
    }
}

sub unrel_path {
  my ($path) = @_;

  # Given a path, strip out explicit references to parent directories:
  # /a/b/c/../../d/e => /a/d/e
  # Always returns an absolute path.
  if ($path !~ m#^[\\/]#) {
    $path = File::Spec->rel2abs($path);
  }
  $path = File::Spec->canonpath($path);
  my @components = split(/[\/\\]+/, $path);
  for(my $i = 1; $i < @components; $i++) {
    if ($components[$i] eq '..' && $i > 0) {
      # Get rid of it and the directory it would back up into
      splice @components, $i-1, 2;
      $i -= 2;
    }
  }

  return File::Spec->catdir(@components);
}

sub trademark_lines {
  my ($indent, %trademarks_done) = @_;

  my @tms_used = sort { length($a) <=> length($b) } keys %trademarks_done;
  my $line = $indent;
  if (@tms_used) {
      my @types = ();
      my %seen = ();
      my $types = '';
      foreach my $tm (@tms_used) {
          my $type = $::trademarks{$tm};
          next if $seen{$type};
          my $what = undef;
          if ($type eq 'r') {
              $what = 'registered trademark';
          } elsif ($type eq 't') {
              $what = 'trademark';
          } elsif ($type eq 's') {
              $what = 'service mark';
          }
          $what .= "s" if (defined($what) && @tms_used > 1);
          push @types, $what;
          $seen{$type}++;
      }
      if (@types) {
          my $last_type = pop @types;
          if (@types) {
              $types = join(', ', @types);
              $types .= ',' if @types > 1;
              $types .= ' or '.$last_type;
          } else {
              $types = $last_type;
          }
      }
      if (@tms_used > 1) {
          my $last_tm = pop @tms_used;
          $line .= join(', ', @tms_used);
          $line .= ',' if @tms_used > 1;
          $line .= " and $last_tm are $types";
      } else {
          $line .= $tms_used[0].' is a '.$types;
      }
      $line .= " of the Standard Performance Evaluation Corporation.  All other";
  } else {
    $line .= "All";
  }
  $line .= " brand and product names appearing in this result are trademarks or registered trademarks of their respective holders.\n";

  return ::wrap_lines([$line], 75, $indent);
}

sub allof {
    my ($thing) = @_;

    if (isa($thing, 'ARRAY')) {
        return @{$thing};
    } else {
        return ($thing);
    }
}

sub firstof {
    my ($thing) = @_;

    if (isa($thing, 'ARRAY')) {
        return $thing->[0];
    } else {
        return $thing;
    }
}

sub escape_HTML {
  my ($str) = @_;

  $str =~ s/&/&amp;/gso;
  $str =~ s/</&lt;/gso;
  $str =~ s/>/&gt;/gso;

  return $str;
}

sub byformat {
    # Sort formats lexically, except for raw, which always goes first,
    # and screen, which always goes last.  Oh yeah... mail and check go before
    # screen and after everything else.  Finally, flags goes second (and
    # isn't optional).

    # Make sure that we won't have a run-time error below
    return -1 if (!isa($a, 'HASH') &&  isa($b, 'HASH'));
    return  1 if ( isa($a, 'HASH') && !isa($b, 'HASH'));
    return  0 if (!isa($a, 'HASH') && !isa($b, 'HASH'));

    my ($an, $bn) = (lc($a->{'name'}), lc($b->{'name'}));

    foreach my $format (qw(raw flags)) {
      return -1 if ($an eq $format);
      return  1 if ($bn eq $format);
    }
    return  1 if ($an eq 'screen'          && $bn =~ /(?:mail|check)/i);
    return -1 if ($an =~ /(?:mail|check)/i && $bn eq 'screen'         );
    foreach my $format (qw(screen mail check)) {
      return  1 if ($an eq $format);
      return -1 if ($bn eq $format);
    }
    return $an cmp $bn;
}

sub bytag ($$) {
    my ($a, $b) = @_;		# This is slower but works around the package
    				# variable problem.
    # Sort multi-valued things (like notes) properly, even in the face of
    # notes1 = foo
    # notes002 = bar
    # etc.
    if ($a !~ /\d$/ || $b !~ /\d$/) {
	# Both need trailing numbers to get special attention
	return $a cmp $b;
    }
    my ($atxt, $anum) = ($a =~ /^(.*?)0*(\d+)$/);
    my ($btxt, $bnum) = ($b =~ /^(.*?)0*(\d+)$/);
    return $atxt cmp $btxt || ($anum+0) <=> ($bnum+0);
}

sub bytrailingnum {
    my ($aname, $anum) = $a =~ m/^(\S+?)([\d.]*)$/;
    my ($bname, $bnum) = $b =~ m/^(\S+?)([\d.]*)$/;
    return $aname cmp $bname || ($anum+0) <=> ($bnum+0);
}

sub bytune {
    # Sort a list of tunings into the prescribed order.
    # Currently, we just ensure that base comes first and peak comes last.
    # Otherwise, it's lexical.
    return -1 if $a eq 'base';
    return 1 if $a eq 'peak';
    return $a cmp $b;
}

sub bylang($$) {
  # Sort a list of "language strings" (as seen in the reduced flags structure).
  # The idea is that the "mixed" languages some come last, or at least after
  # the "simple" language lists.
  my ($a, $b) = @_;
  my ($la, $lb) = (length($a), length($b));
  return $a cmp $b if ($la < 5 && $lb < 5);     # Single-language
  return $la <=> $lb;
}

sub pdf_ok {
    # Here's where we decide if it's okay to have PDF output

# XXX Can't do this because PS hasn't been loaded yet when this is
# evaluated.  Instead, move all the code to the PDF module and put this
# check in ps_ok().
#    return 0 unless ($Spec::Format::ps::name eq 'PostScript');
    return 1 if $ENV{'SPEC_TRY_PDF'};
    return 0 if $ENV{'SPEC_NEVER_TRY_PDF'};

    $::pdf_ok = 1 unless defined($::pdf_ok);
    return $::pdf_ok;
}

sub csv_ok {
    # Here's where we decide if it's okay to have CSV output

    return 1 if $ENV{'SPEC_TRY_CSV'};
    return 0 if $ENV{'SPEC_NEVER_TRY_CSV'};

    eval 'use Text::CSV_XS;';

    return $@ eq '';
}

sub ps_ok {
    # Here's where we decide if it's okay to have PostScript output

    return 1 if $ENV{'SPEC_TRY_PS'};
    return 0 if $ENV{'SPEC_NEVER_TRY_PS'};

    return 1;
}

sub html_ok {
    # Here's where we decide if it's okay to have HTML output

    return 1 if $ENV{'SPEC_TRY_HTML'};
    return 0 if $ENV{'SPEC_NEVER_TRY_HTML'};

    return 1;
}

sub asc_ok {
    # Here's where we decide if it's okay to have ASCII output

    return 1 if $ENV{'SPEC_TRY_TXT'};
    return 0 if $ENV{'SPEC_NEVER_TRY_TXT'};

    return 1;
}

sub screen_ok {
    # Here's where we decide if it's okay to have ASCII output to the screen

    return 0 unless ($Spec::Format::asc::name eq 'ASCII');
    return 1 if $ENV{'SPEC_TRY_SCREEN'};
    return 0 if $ENV{'SPEC_NEVER_TRY_SCREEN'};

    return 1;
}

sub config_ok {
    # Here's where we decide if it's okay to have config file output

    return 1 if $ENV{'SPEC_TRY_CONFIG'};
    return 0 if $ENV{'SPEC_NEVER_TRY_CONFIG'};

    return 1;
}

sub mail_ok {
    # Here's where we decide if it's okay to mail results

    return 1 if $ENV{'SPEC_TRY_MAIL'};
    return 0 if $ENV{'SPEC_NEVER_TRY_MAIL'};

    return 1;
}

sub raw_ok {
    # raw output is *always* okay
    return 1;
}

1;
