#!/usr/bin/perl
# harness.pl     doom@kzsu.stanford.edu     2009/04/29 07:43:52

use warnings;
use strict;
$|=1;
use Data::Dumper;
use FindBin qw( $Bin );  # ~/End/Cave/DesktopAutosave/lib/emacs/t/

my $test_config_pl = "$Bin/etc/config.pl";


# can control these settings locally, but can over-ride them with a shell
# setting of the environment variables.
my $DEBUG   = 1;
my $VERBOSE = 1;
$ENV{ TEST_EMACS_DEBUG }   = $DEBUG   unless defined ( $ENV{ TEST_EMACS_DEBUG } );
$ENV{ TEST_EMACS_VERBOSE } = $VERBOSE unless defined ( $ENV{ TEST_EMACS_VERBOSE } );

my $config;
open my $fh, '<', $test_config_pl or die "$!"; # TODO maybe just default not die
{ local $/;
  my $config_code = <$fh>;
  eval "$config_code";
  warn "$@" if $@;
}
($DEBUG) && print STDERR "config: ", Dumper( \$config ), "\n";

chdir( $Bin );
my @tests = sort glob <*.t>;

($DEBUG) && print STDERR "tests: ", join "\n", @tests, "\n";

foreach my $c ( @{ $config } ) {
  $ENV{ TEST_EMACS_BINARY } = $c->[0];
  $ENV{ TEST_EMACS_INIT }   = $c->[1];

  print "Running harness.pl with envars: \n" .
    "  TEST_EMACS_BINARY: " . $ENV{ TEST_EMACS_BINARY } . "\n" .
    "  TEST_EMACS_INIT: " .   $ENV{ TEST_EMACS_INIT }   . "\n";

  chdir( $Bin );

  foreach my $test (@tests) {
    my $cmd = "perl $test";
    ($DEBUG) && print STDERR "cmd: $cmd\n";
    my $ret = qx{ $cmd };
    print "$ret\n";
  }
  sleep(1);
}







__END__

=head1 NAME

harness.pl

=head1 SYNOPSIS



=head1 DESCRIPTION

Stub documentation for harness.pl,
created by template.el.

It looks like the author of this script was negligent
enough to leave the stub unedited.

=head1 AUTHOR

Joseph Brenner, E<lt>doom@finelineE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2009 by Joseph Brenner

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.2 or,
at your option, any later version of Perl 5 you may have available.

=head1 BUGS

None reported... yet.

=cut
