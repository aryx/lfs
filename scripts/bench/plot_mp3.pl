#!/usr/bin/perl

#time sudo ./plot_mp3 /home/pad/archive/orig/medias/*
use Time::HiRes;

my @files =  reverse  @ARGV;
print "@files\n";

#exit;

my $SRC = "/home/pad/work/lfs/code";

open (CP, "> /tmp/CP") or die $!;

`mkfs.lfs /sfl-meta/mp3/`;
`mount.lfs /sfl-meta/mp3/ /sfl/mp3/`;
`cd /sfl/mp3/; sh -x $SRC/demos/lfs-mp3.sh`;

my $i=0;
my $len = scalar(@files);

my $total=0;

map {
    my $current = $_;
    $i++;
    print "Testing:$i/$len: $current\n";

    my $time = Time::HiRes::time();
#    `cp "$current" /sfl/mp3/mine/.ext/`; #quadratic ls pb with mp3 ?
    `cp "$current" /sfl/mp3/.relaxed/mine/`;
    my $time2 = Time::HiRes::time();
    print "res = " . ($time2 - $time) ."\n";
    print CP ($time2 - $time);    print CP "\n";
    $total+= ($time2 - $time);

#    exit if $i == 30;

} @files;

print ("total = " . $total/60 . "minutes \n");

# LS (in mymusic^/.relaxed
#  artist:
#  genre:Disco/artist:
#  genre:Comedy/artist:
#  genre:Soundtrack/.strict


`umount /sfl/mp3`;
`sleep 2; sudo pkill -9 LfsOcaml; sudo pkill -9 LfsOcaml; sudo pkill -9 LfsOcaml`;

system("cd /sfl-meta/mp3/; du -c children parents iprop_prop prop_iprop extfiles filesdb");
system("cat /sfl-meta/mp3/stat_context");
