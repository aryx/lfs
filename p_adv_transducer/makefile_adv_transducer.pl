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

    if($s =~ /^(\w+):/) { 
        push @aspects, "synchro"; $state = {}; 
        $state->{make} = "make:$1";
    }
    if($s =~ /^\s*$/) { 
        $state = {}; 
    }


    my $res = 
      [
       @aspects,
       maybe($state->{make}),
      ];
    print (join "/", @{$res}); print "\n";

    #forward:  $i++;
}
#forward:  @data;



