#!/usr/bin/perl

#require: perl mp3 API, which require in turns Compress-Zlib perl API, 
#  tagged,  zlib (quite standard), and zlib-devel package (not very standard)

# could as for ogg call an mpg123info program but he does not exist
# could parse the ouput of mpg123 but need in that case install latest mpg321, 
# libmad, libid3tag, ...

use MP3::Info;
use MP3::Tag;

#example of output:
# title:Zero/artist:Astral Projection/album:The Astral Files/comment:/year:/genre:Psychadelic/time:05:55/bitrate:128/frequency:44.1

#print "@ARGV\n";
my $path = $ARGV[0];

#note that this code is quite slow
#it seems that it takes time to get the metadata from the mp3 (perhaps cos need decompress stuff ?)

 
my $tag = MP3::Tag->new($path);
if($tag) {
    $tag->get_tags();
    if (exists $tag->{ID3v1}) {
	my $last = $tag->{ID3v1};
	push @aux, 
	  ("title:"  .  $last->song, 
	   "artist:" .  $last->artist,
	   "album:"  .  $last->album,
	   "comment:" . $last->comment,
	   "year:"   .  $last->year,
	   "genre:" .   $last->genre,
	  );
    } else {
	my $tag = get_mp3tag($path);
	if($tag) { map { my $e = $_; push @aux, (lc $e . ":$tag->{$e}") } qw(TITLE ARTIST ALBUM YEAR COMMENT GENRE) }
    }
}

my $tag2 = get_mp3info($path);
if($tag2) { map { my $e = $_; push @aux, (lc $e . ":$tag2->{$e}") } qw(TIME BITRATE FREQUENCY)	}

print (join "/", @aux); print "\n";
