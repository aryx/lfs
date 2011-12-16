#!/usr/bin/perl

#time sudo ./plot_email
use Time::HiRes;

#my @files =  reverse  @ARGV;
#print "@files\n";

#exit;

my $SRC = "/home/pad/work/lfs/code";

open (CP, "> /tmp/CP") or die $!;

system("mkfs.lfs /sfl-meta/email/");
system("mount.lfs /sfl-meta/email/ /sfl/email/");
system("cd /sfl/email/; sh -x $SRC/demos/lfs-email.sh");

use Common;
#my @files = @{cat_chomped("find /home/pad/work/lfs/data/lfs/email/ -type f |")};
my @files = @{cat_chomped("cd /home/pad/work/lfs/data/lfs/email2/; ls -t -r -1 | sed -e 's/^/\\/home\\/pad\\/work\\/lfs\\/data\\/lfs\\/email2\\//' |")};
#my @files = @{cat_chomped("cd /home/pad/work/lfs/data/lfs/email3/lkml; ls -1 | sed -e 's/^/\\/home\\/pad\\/work\\/lfs\\/data\\/lfs\\/email3\\/lkml\\//' |")};

#get them from private/mail (use cp -a to keep the good date), mv $i $i.mail,  rename them to avoid conflict (cos same numer can be allocated in different dir (in bogus/ spam/ ...)
# and put them in same dir

my $i=0;
my $len = scalar(@files);

my $total=0;

map {
    my $current = $_;
    $i++;
    print "Testing:$i/$len: $current\n";

    my $time = Time::HiRes::time();
    #`cp "$current" /sfl/email/mine/.ext/`; # have quadratic ls!!
    `cp "$current" /sfl/email/.relaxed/mine/`;
    my $time2 = Time::HiRes::time();
    print "res = " . ($time2 - $time) ."\n";
    print CP ($time2 - $time);    print CP "\n";
    $total+= ($time2 - $time);

#    exit if $i > 100;

} @files;


print ("total = " . $total/60 . "minutes \n");

#exit;

#LS (view myemail (et .relaxed))
# size:
# year:
# !SPAM/thema:LIS|thema:LISFS/.strict !!!  (3.4s only so fast) (1.0 en relaxed)
# mailing-list:Caml-list/thema:camlp4/thread: 1.7 (et 3.4 en relaxed)

# !SPAM/thema: => 11 a 44 sec,  mais bon vague aussi

`umount /sfl/email`;
`sleep 2; sudo pkill -9 LfsOcaml; sudo pkill -9 LfsOcaml; sudo pkill -9 LfsOcaml`;

system("cd /sfl-meta/email/; du -c children parents iprop_prop prop_iprop extfiles filesdb");
system("cat /sfl-meta/email/stat_context");

