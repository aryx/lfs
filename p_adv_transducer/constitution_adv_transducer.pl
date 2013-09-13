#!/usr/bin/perl

sub maybe { my ($Uv) = @_;    (defined($Uv))  ?  ($Uv)  :  () ;}

my $state = {};

#if have to see forward then uncoment this  and replace
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

    if(/ARTICLE/) { push @aspects, "synchro"; $state = {}; }
    if(/ARTICLE (.*)-(\d+)/) {
        my ($part, $num) = ($1, $2);
        my $next = $data[$i+2];
        my $title = ($next =~ /^\s+(.*?)\s*$/) ? $1 : "notitle";
        $state->{article} = ["section:$part-$num", "title:$title"];

    }


    my $res =
      [
       @aspects,
#       maybe($state->{PUT_HERE}),
       (defined($state->{article}) ? @{$state->{article}} : ()),

      ];
    print (join "/", @{$res}); print "\n";

    $i++;
}
@data;



