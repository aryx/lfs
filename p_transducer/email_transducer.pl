#!/usr/bin/perl

use diagnostics;
use strict;
use warnings;

my $path = $ARGV[0];

sub words { my ($s) = @_;  
    my @words = (); 
    while($s =~ /([\w\d_]+)/g) {
	my $x = $1;
	push @words,$x if (length($x) > 3);
    }
    \@words;
}

my @aux = ();

open(FILE, "$path") or die "$!";

my $done_from = 0; # a mail may contain a ref to another email, it is a useful info, but prefer stop

while(<FILE>) {
    my $s = $_;
    

    #todo: surname lastname (normally done via solver)
    #ex:  from:"kassie elga" <kale7369iona@yahoo.com>
    if($s =~ /^From: (.*)/ && !$done_from)    { push @aux, "from:$1"; $done_from = 1; }
    if($s =~ /^From:.*@((\w|\.)+)/) { push @aux, "from_domain:$1"; }

    #todo: split(/\s*,\s*/,...)   for  to:, cc:
    if($s =~ /^To: (.*)/) { push @aux, "to:$1"; }

    if($s =~ /^Subject: (.*)/) { 
	my $s = $1;
	push @aux, "subject:$s";
	if ($s =~ /Re: (.*)/) { push @aux, "thread:$1" }
	else { push @aux, "thread:$s" }
	if ($s =~ /\[(.*?)\]/) { push @aux, "mailing_list:$1"; }
    }
    if($s =~ /^Subject:.*SPAM.*/) { push @aux, "SPAM"; }
    if($s =~ /^Subject:(.*)/) { 
        map { push @aux, "thema:$_" } @{words($1)};
    }


    #todo: a real solver on date 
    #ex:  Date: Thu, 18 Dec 2003 07:22:40 +0100
    #TODO can have too Date: 19 Mar 2004 15:50:19 +0100
    if($s =~ /^Date: (\w+), (\d+) (\w+) (\d+)(.*)/) {
        push @aux, "year:$4", "month:$3";
    }
}

print (join "/", @aux); print "\n";

