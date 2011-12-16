#!/usr/bin/perl
use diagnostics;
use strict;
use warnings;

#use Common; but words already defined

##############################################################################
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


#my $forbiddens = [qw(the of a to for and in an or from on with s as)];

sub words { my ($s) = @_;
    my @words = ();

    while($s =~ /([\w\d_éèàêçÉÈÀÊÇ]+)/g) {
	my $x = $1;
        push @words,$x;
	#push @words,$x if ((length($x) > 3)||($x=~ /[A-Z][A-Z]/)); #if put > 2, then pass from 1 to 4sec
	#push @words,$x if length($x) <= 3;
        #zarb cos if use >3 or <= 3 alone, then fast, but when want both very slow

    }
    \@words;
}






##############################################################################
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

    ##############################################################################

    if($s =~ /^\s*$/) { delete $state->{paracontain} }
    elsif(!defined($state->{paracontain})) {
	my $para = take_until(sub { my ($e) = @_; (!defined($e)) ? 1 : ($e =~ /^\s*$/) }, [ @data[$i..$i+300] ]);
	my @aux = ();
	map {
            map { push @aux, "paracontain:$_" } @{words($_)}
        } @{$para};
	$state->{paracontain} = [@aux];
    }

    ##############################################################################

    my $res =
      [
       (map { "contain:$_" } @{words($s)}),
       (defined($state->{paracontain}) ? @{$state->{paracontain}} : ()),
      ];


    print (join "/", @{$res}); print "\n";
    $i++;
}
@data;
