#!/usr/bin/perl

use strict;
use diagnostics;

$| = 1;

sub is_formula { my ($p) = @_;   if($p =~ /.*[\?\*\+\^\$\[\]\(\)].*/) { 1 } else { 0 };}

sub print_bool { my ($b) = @_;   if($b) { print "yes\n" } else { print "no\n" }}


while (<STDIN>) {
    #my $p1 = <STDIN>;
    my $p1 = $_;
    my $p2 = <STDIN>;

    if ($p2 eq "IS_FORMULA?\n") {
        print_bool(is_formula($p1));
    } else {
        print_bool(0) if (is_formula($p1) && is_formula($p2));
        print_bool(0) if (is_formula($p1) && !is_formula($p2));
        print_bool($p1 eq $p2) if (!is_formula($p1) && !is_formula($p2));
        print_bool($p1 =~ /^$p2/) if (is_formula($p2) && !is_formula($p1));
    }
    

}
