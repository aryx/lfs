#!/usr/bin/perl
use Common;

#sudo ./bench_ab

#Andrew (x 10) (Modified Andrew Benchmark)

my $DIR ="/sfl/ab";

my $SRC = "/home/pad/work/lfs/code";



#if not test LFS, put in comment
`mkfs.lfs /sfl-meta/ab/`;
`mount.lfs /sfl-meta/ab/ /sfl/ab/`;
`cd /sfl/ab/; sh -x $SRC/demos/basicenv.sh`;

`cd $DIR; mkdir function:; mkdir abprop; cd abprop; mkdir include; mkdir sys; mkdir netinet; mkdir testfs1`; ## those prop are shared, biais mais bon.
#`cd $DIR; cp $SRC/p_transducer/c_transducer.pl transducer:c/`;
#`cd $DIR; cp $SRC/p_transducer/c_transducer2.pl transducer:c/`;
`cd $DIR; cp $SRC/p_transducer/c_transducer4 transducer:c/`;

#exit;

my @phase;

for my $i (1..10) { 
    print "test:$i\n"; 
    `cd $DIR; mkdir pad-$i; cd pad-$i;`;

    for my $j (1..5) { 
	my $time = Time::HiRes::time();
#	`cd $DIR/pad-$i; make -f /home/pad/work/lfs/bench/ab/Makefile2 phase$j`;  #test EXT3
	
#pire :(	`cd $DIR/pad-$i/.relaxed/abprop^/.ext/; make -f /home/pad/work/lfs/bench/ab/Makefile2 phase$j` if $j==2;
	`cd $DIR/pad-$i/.relaxed/; make -f /home/pad/work/lfs/bench/ab/Makefile2 phase$j` if $j==2;
	`cd $DIR/pad-$i/.relaxed/abprop^/; make -f /home/pad/work/lfs/bench/ab/Makefile2 phase$j` if $j !=2;
	my $time2 = Time::HiRes::time();
	$phase[$j] += ($time2-$time);
    }
}

pr "phase1=$phase[1]";
pr "phase2=$phase[2]";
pr "phase3=$phase[3]";
pr "phase4=$phase[4]";
pr "phase5=$phase[5]";
 
pr ("total: " . ($phase[1]+$phase[2]+$phase[3]+$phase[4]+$phase[5]));

`umount /sfl/ab`;
`sleep 2; sudo pkill -9 LfsOcaml; sudo pkill -9 LfsOcaml; sudo pkill -9 LfsOcaml`;
system("cat /sfl/ab/stat_context");
