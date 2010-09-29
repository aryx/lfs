#!/usr/bin/perl

use diagnostics;
use strict;
use warnings;

use Common;

my $path = $ARGV[0];

my @aux = ();

mapf (sub { my ($s) =@_;  if($s =~ /^([^\s]+)\s+function\s/) { push @aux, "function:$1"; }}, 
      cat("ctags -x --language-force=C $path|"));  

print (join "/", @aux); print "\n";

