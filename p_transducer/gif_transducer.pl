#!/usr/bin/perl

#require: gifinfo  from libungif package

#print "@ARGV\n";
my $path = $ARGV[0];

open(FILE, "gifinfo '$path' |") or die "$!";
#ex of output: gifinfo tech_t.gif =>
#Size: 779x615
#Comment: 

my @aux = ();

while(<FILE>) {
    if(/Size:\s*(\d+)x(\d+)/i) { push @aux, "resolution:$1x$2" }
    if(/Comment:(.*)/i) { push @aux, "comment:$1" }
}

print (join "/", @aux); print "\n";
