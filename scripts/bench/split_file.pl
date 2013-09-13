#!/usr/bin/perl

my $file =    $ARGV[0];
my $numline = $ARGV[1];
print "$file\n";

`rm -f genSplit*`;
#old: `split -l 1000 $file genSplit`;

open(FILE, "$file") or die "$!";

my $cur_f_idx = 0;
my $cur_line = 0;


open(CURRENT, "> genSplit$cur_f_idx") or die $!;

while(<FILE>) {
    my $line = $_;
    print CURRENT $line;
    $cur_line++;
    if($cur_line == $numline) {
	close(CURRENT);
	my $next = $cur_f_idx+1;

	`cp genSplit$cur_f_idx genSplit$next`; # if want cumulative
	$cur_f_idx++;
	open(CURRENT, ">> genSplit$cur_f_idx") or die $!;
	$cur_line = 0;
    }
}
close(CURRENT);
