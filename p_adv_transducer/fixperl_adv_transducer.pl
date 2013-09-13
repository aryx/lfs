#!/usr/bin/perl

use diagnostics;
use strict;
use warnings;

sub maybe { my ($Uv) = @_;    (defined($Uv))  ?  ($Uv)  :  () ;}

my $state = {};

my @data = ();
while(<STDIN>) {
    my $s = $_;
    push @data, $s;
}
my $i = 0;
map {
    my $s = $_;
    my @aspects = ();

    if (($s =~ /^#####/) && ($data[$i+1] =~ /^# ([A-Z].*)/)) { $state->{section}  = "section:$1"; push @aspects, "synchro" ;}

    if($s =~ /^Fun/)            { push @aspects, "header"}
    elsif($s =~ /^\s+raise /)   { push @aspects, "error" }
    elsif($s =~ /^\s*#/)        { push @aspects, "comment:"}
    elsif($s =~ /^\s*Example/)  { push @aspects, "example"}
    elsif($s =~ /^\s*Assert/)   { push @aspects, "assert"}
    elsif($s =~ /^\s*Pre/)      { push @aspects, "pre"}
    elsif($s =~ /^\s*Post/)     { push @aspects, "post"}
    else                        { push @aspects, "algo" }

    $state->{function} = "function:$1" if $s =~ /^Fun\s+(\w+)/;

#    $state->{bloc} = undef when $s =~ /^\s*(#.*)?$/;
#    if($s =~ /^(\s+)[\#\s]/) {
#	 my $current_indent = length($1);
#	 if (defined($state->{last_if_indent}) && $current_indent == $state->{last_if_indent}) {
#	     $state->{stateif} = "else";
#	     if($s =~ /^(\s*)}/)       { $state->{currentif} = ["blocifnotthen:$state->{if}"]}
#	     fi($s =~ /(\s*)else\s*{/) { $state->{currentif} = ["blocifnotthen:$state->{if}"]}
#	     fi($s =~ /(\s*)}\s*else\s*{/) { $state->{currentif} = ["blocifnotthen:$state->{if}"]}
#	     else {
#		 $state->{if} = undef; $state->{last_if_indent} = undef; $state->{currentif} = undef;
#	     }
#	 }
#	 fi(defined($state->{last_if_indent}) && $current_indent < $state->{last_if_indent}) {
#	     $state->{if} = undef; $state->{last_if_indent} = undef; $state->{currentif} = undef; }
#	 fi(defined($state->{last_if_indent}) && $current_indent > $state->{last_if_indent}) {
#	     $state->{currentif} = ["blocifthen:$state->{if}"] when $state->{stateif} eq "then";
#	     $state->{currentif} = ["blocifnotthen:$state->{if}","blocifelse:$state->{if}"] when $state->{stateif} eq "else";
#
#	 }
#	 else { }
#    }
#
#    if(!(defined($state->{if})) && ($s =~ /(\s+).*if\((.+?)\)/)) {
#	 $state->{if} = "$2";
#	 $state->{last_if_indent} = length($1);
#	 $state->{currentif} = ["blocifheader:$state->{if}", "blocifnotthen:$state->{if}"];
#	 $state->{stateif} = "then";
#    };
#
#
#    # TODO4: could put this in generic
#    if (!defined($state->{bloc}) && !($s =~ /^\s*(#.*)?$/)) {
#	 $state->{bloc} = words($s) +> uniqs() +> sorts() +> mapf(Fun($e) { "bloc:$e" });
#    }

    #TODO2: type, const, ...

    my $res =
      [
       @aspects,
       maybe($state->{function}),
       maybe($state->{section}),
#       @{$state->{currentif} || []},
#       @{$state->{bloc} || ["bloc"]},
      ];
    print (join "/", @{$res}); print "\n";
    $i++;
} @data;
