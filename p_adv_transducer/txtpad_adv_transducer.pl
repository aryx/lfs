#!/usr/bin/perl
use diagnostics;
use strict;
use warnings;


sub maybe { my ($Uv) = @_;    (defined($Uv))  ?  ($Uv)  :  () ;}
sub maybe2 { my ($Uv, $s) = @_;    (defined($Uv))  ?  ("$s$Uv")  :  () ;}
sub take_until { my ($f, $xs) = @_;
    my @res = ();
    foreach $a (@{$xs}) {
	if (&$f($a)) { return \@res }
	else { push @res, $a }
    }
    return \@res;
}

sub words { my ($s) = @_;
    my @words = ();

    while($s =~ /([\w\d_]+)/g) {
	my $x = $1;
        push @words,$x;
	#push @words,$x if ((length($x) > 3)||($x=~ /[A-Z][A-Z]/)); #if put > 2, then pass from 1 to 4sec
	#push @words,$x if length($x) <= 3;
        #zarb cos if use >3 or <= 3 alone, then fast, but when want both very slow
    }
    \@words;
}


my $state = {};

my @data = ();
while(<STDIN>) {
    my $s = $_;
    push @data, $s;
}
my $i = 0;
map {

#while(<STDIN>) {
    my $s = $_;
    my @aspects = ();

    ####################################################
    # synchro points,  the structure/hierarchy of a doc
    ####################################################

    do { push @aspects, "synchro"; $state = {}; } if ($s =~ /^#/);
    do { push @aspects, "synchro"; $state = {}; } if ($s =~ /^---/);

    if($s =~ /^#(\w.*)/) {
	my @xs = split /\s*,\s*/, $1;
        my @ys = map { if ($_ =~ /([\w:]+)/) { $1 } else { $_ } } @xs;
	my @zs =  map { if ($_ =~ /\w+:.*/) { $_ } else { "index:$_"} } @ys;

	$state->{index}  = [@zs];
    }

    if($s =~ /^\s*(\w+):/) {
        my $aux = $s;
        my @aux = ();
        while($aux =~ /^\s*(\w+):(.*)/) {
            push @aux, "index:$1";
            $aux = $2;
        }
        $state->{resetwithnewline} = [ (defined($state->{resetwithnewline}) ? @{$state->{resetwithnewline}} : ()), @aux];
    }
    if($s =~ /^\s*$/) { delete $state->{resetwithnewline} }

    ##########################################
    # paracontain
    ##########################################

    if($s =~ /^\s*$/) { delete $state->{deadline} }
    elsif(!defined($state->{deadline})) {
	my $para = take_until(sub { my ($e) = @_; (!defined($e)) ? 1 : ($e =~ /^\s*$/) }, [ @data[$i..$i+300] ]);
	my @aux = ();
	map {
            if($_ =~ /deadline:(.*)/) { push @aux, "index:deadline_$1" }
            #map { push @aux, "paracontain:$_" } @{words($_)}
        } @{$para};
	$state->{deadline} = [@aux];
    }

    ##########################################
    my $res =
      [
       @aspects,
       #maybe($state->{resetwithnewline}),
       (defined($state->{resetwithnewline}) ? @{$state->{resetwithnewline}} : ()),

       (defined($state->{index}) ? @{$state->{index}} : ()),
       (defined($state->{deadline}) ? @{$state->{deadline}} : ()),

      ];

    print (join "/", @{$res}); print "\n";
  $i++;
}

@data;
