#!/usr/bin/perl

#require: jpeginfo from jpeginfo package

#TODO exif format ? google: jpeg metadata ?
# exist programs to add stuff to jpg ? cf fradet ?

#print "@ARGV\n";
my $path = $ARGV[0];

open(FILE, "jpeginfo '$path' |") or die "$!";
#ex of output: jpeginfo vge.jpg =>
#vge.jpg  393 x 207  24bit JFIF  N    8176


my @aux = ();

while(<FILE>) {
    if(/.*?(\d+) x (\d+)/i) { push @aux, "resolution:$1x$2" }
    #could extract more but who cares
}

print (join "/", @aux); print "\n";
