#!/usr/bin/perl

#time sudo ./plot_man /var/cache/man/whatis
use Time::HiRes;

my $file =  $ARGV[0];
print "$file\n";

#exit;

my $SRC = "/home/pad/work/lfs/code";

#TODO
#synopsis:
# type des synoptis
# options
#
#extract info that glimpse cant
# such as section, ...


open (CP, "> /tmp/CP") or die $!;

`mkfs.lfs /sfl-meta/man/`; #XX
`mount.lfs /sfl-meta/man/ /sfl/man/`; #XX
`cd /sfl/man/; sh -x $SRC/demos/basicenv.sh`; #XX
`cd /sfl/man; mkdir -p manprop; mkdir -p manprop/keyword:;  mkdir -p manprop/section:;  `; #XX
`cd /sfl/man; cd logic:keyword:; cp $SRC/p_logic/string_logic .`;

use Common;
my @files = @{cat_chomped("$file")};

my $i=0;
my $len = scalar(@files);

my $total=0;

my $done = {};

map {
    my $current = $_;

    my $forbiddens = [qw(the of a to for and in an or from on with s as)]; # get from testing and sorting most cited increment
    if($current =~ /^(\w+)(.*)\((\d)\)\s*-\s*(.*)$/) { #for section have |\w|\d\w+
	my ($name, $misc, $section, $s) = ($1, $2, $3, $4);
	if(!exists($done->{$name})) {
#	    $done->{$name} = "done";
	    $i++;
	    my $words = mapf(sub { my ($s) = @_; "keyword:$s" }, filter (sub { my ($s) = @_; !(mems($s, $forbiddens))}, words($s)));
	    my $add = "";
	    if($misc =~ /\[(.*)\]/) { $add = $1; }
	    print "Testing:$i/$len: $current\n";
	    #system("man $section $name > /sfl/man/mine/.ext/" . join("/", @{$words}) . "/section:$section/$name$add.man\n");
	    #biais: system("man $section $name > /sfl/man/.relaxed/mine/" . join("/", @{$words}) . "/section:$section/$name$add.man\n");
	    #system("man $section $name > " . join("/", @{$words}) . "/$name$add");
            system("man $section $name > /tmp/page.man");
	    my $time = Time::HiRes::time();
            system("cp /tmp/page.man /sfl/man/.relaxed/mine/" . join("/", @{$words}) . "/section:$section/$name$add.man\n");
	    my $time2 = Time::HiRes::time();
	    print "res = " . ($time2 - $time) ."\n";
	    print CP ($time2 - $time);    print CP "\n";
	    $total+= ($time2 - $time);
#    exit if $i==300;

	}

    } else {
	#pr2 "not a valid entry: $_"
    }

#    exit;

} @files;

#exit;

print ("total = " . $total/60 . " minutes \n");

#LS: (view manprop^ et .relaxed)
# section:
# section:1/keyword:
# section:2/keyword:
# keyword:change

# note than less obj in section:1 than in section:3 but description are more complete => more prop => slower in 1 than in 3

# note that the counting x/y  is not really acute, cos at the end have only 11000/20000 cos
#  many man page are not computed (cos selection on regexp of man page)


`umount /sfl/man`;
`sleep 2; sudo pkill -9 LfsOcaml; sudo pkill -9 LfsOcaml; sudo pkill -9 LfsOcaml`;

# size/stat
system("cd /sfl-meta/man/; du -c children parents iprop_prop prop_iprop extfiles filesdb");
system("cat /sfl-meta/man/stat_context");
