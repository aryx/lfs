#!/usr/bin/perl

use diagnostics;
use strict;
use warnings;

my $path = $ARGV[0];

my @aux = ();

sub mems { my ($e, $xs) = @_;  foreach (@{$xs}) { $e eq $_ and return 1 } 0 }

#mapf (sub { my ($s) =@_;  if($s =~ /^([^\s]+)\s+function\s/) { push @aux, "function:$1"; }},
#      cat("ctags -x --language-force=C $path|"));


my $state = {};
open(FILE, "$path") or die "$!";
while(<FILE>) {
    my $s = $_;

#    if($state->{isincomment}) {
#	if($s =~ /\*\//) { $state->{isincomment} = 0 }
#    }
#    elsif($state->{isinbody}) {
#	if($s =~ /^}/) { $state->{isinbody} = 0 }
#    }
#    else {
#	if($s =~ /^\/\*/) {
#	    $state->{isincomment} = 1;
#	}
#	elsif($s =~ /(\w+)\s*\(/) {
#	    push @aux,"function:$1" if (!mems("function:$1", [@aux]));
#	}
#    }
    if ($s =~/^(\w+)\s*\(/) { push @aux,"function:$1" if (!mems("function:$1", [@aux])); }
}

print (join "/", @aux); print "\n";
