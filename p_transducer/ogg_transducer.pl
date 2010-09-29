#!/usr/bin/perl

#require: ogginfo from ogg package

#print "@ARGV\n";
my $path = $ARGV[0];

# you can use xmms or the command vorbiscomment to set the tags repeatidly
# as in:    for i in trac* { vorbiscomment -w $i -t "ARTIST=Etienne Daho" -t "GENRE=Pop" }

open(FILE, "ogginfo '$path' |") or die "$!";

#print "HERE\n";
my @aux = ();

while(<FILE>) {
    if(/artist=(.*)/i) { push @aux, "artist:$1" }
    if(/genre=(.*)/i) { 
	my @genres = split /\//, $1;
	map { push @aux, "genre:$_" } @genres;
    }
    if(/title=(.*)/i) { push @aux, "title:$1" }
    if(/date=(.*)/i) { push @aux, "year:$1" }
    if(/album=(.*)/i) { push @aux, "album:$1" }

    if(/Playback length: (\d+)m:(\d+)s/i) { push @aux, "time:$1:$2" }
    if(/playtime:(\d+):(\d+)/i) { push @aux, "time:$1:$2" }

    if(/^\s*=(.*)/i) { push @aux, "comment:$1" }
}



print (join "/", @aux); print "\n";
