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
        $state->{where} = "interface";
    }

    if($s =~ /(\w+)::(\w+)\s*\(/) {
        push @aspects, "synchro"; $state = {}; 
        $state->{method} = "method:$2";
        $state->{class} = "class:$1";
        $state->{where} = "implementation";
    }
    if($s =~ /(\w+)\s*\(/ && ($state->{where} eq "interface")) { 
        push @aspects, "method:$1";
    }
    if($s =~ /^}/) { 
        push @aspects,  maybe($state->{class}),  maybe($state->{method}),  maybe($state->{where}), maybe($state->{function});
        $state = {}; 

    }

    if($s =~ /^(\w+)\s+(\w+)\s*\(/) { 
        push @aspects, "synchro"; $state = {}; 
        $state->{function} = "function:$2";
        $state->{where} = "implementation";
    }

    if($s =~ /^\s*\/\*(.*)\*\/\s*$/) { push @aspects, "comment:" }

    if($s =~ /^public:/) { $state->{visibility} = "visibility:public"; }
    if($s =~ /^private:/) { $state->{visibility} = "visibility:private"; }


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



