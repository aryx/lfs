#!/usr/bin/perl

sub maybe { my ($Uv) = @_;    (defined($Uv))  ?  ($Uv)  :  () ;}

my $state = {};

#if have to see forward then uncoment this  and replace
#forward:    my @data = ();
#forward:    while(<STDIN>) {
#forward:        my $s = $_;
#forward:        push @data, $s;
#forward:    }
#forward:    my $i = 0;

#forward:  map {
while(<STDIN>) {
    my $s = $_;
    my @aspects = ();

    if($s =~ /class\s*(\w+)/) {
        push @aspects, "synchro"; $state = {};
        $state->{class} = "class:$1";
        $state->{where} = "implementation";
    }

    if($s =~ /interface\s*(\w+)/) {
        push @aspects, "synchro"; $state = {};
        $state->{class} = "class:$1";
        $state->{where} = "interface";
    }

    if($s =~ /^}/) {
        push @aspects,  maybe($state->{class}),  maybe($state->{method}),  maybe($state->{where}), maybe($state->{function});
        $state = {};

    }

    if($s =~ /^\s*public\s*.*?(\w+)\s*\(/) {
        $state->{method} = "method:$1";
        $state->{visibility} = "visibility:public";
        $state->{where} eq "implementation";
    }

    if($s =~ /^\s*\/\*(.*)\*\/\s*$/) { push @aspects, "comment:" }

    if($s =~ /(\w+)\s*\(/ && ($state->{where} eq "implementation")) {
        push @aspects, "call:$1";
    }

    my $res =
      [
       @aspects,
       maybe($state->{class}),
       maybe($state->{method}),
       maybe($state->{where}),
       maybe($state->{function}),
       maybe($state->{visibility}),
      ];
    print (join "/", @{$res}); print "\n";

    #forward:  $i++;
}
#forward:  @data;



