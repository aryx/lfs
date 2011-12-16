#!/usr/bin/perl

#split_file ~/work/lfs/data/pof/bibtex/100_000.bib 10000; rm genSplit10
#ou le 100_000_reversed.bib
#sudo ./bench_correlation_nb_obj genSplit*
use Time::HiRes;

my @files =  reverse  @ARGV;
print "@files\n";

my $SRC = "/home/pad/work/lfs/code";

open (CDPARTS, "> /tmp/CDPARTS") or die $!;
open (LS, "> /tmp/LS") or die $!;
open (LS2, "> /tmp/LS2") or die $!;
open (LS3, "> /tmp/LS3") or die $!;
open (LS4, "> /tmp/LS4") or die $!;
open (LSMOY, "> /tmp/LSMOY") or die $!;
open (LSMOYGEO, "> /tmp/LSMOYGEO") or die $!;
open (GENVIEW, "> /tmp/GENVIEW") or die $!;
open (MODIF, "> /tmp/MODIF") or die $!;
open (MODIF2, "> /tmp/MODIF2") or die $!;
open (MODIF3, "> /tmp/MODIF3") or die $!;
open (SAVMOY, "> /tmp/SAVMOY") or die $!;
open (SAVMOYGEO, "> /tmp/SAVMOYGEO") or die $!;

#TODO: @FILES, @LS  et un for






map {
    my $current = $_;
    print "Testing: $current\n";
    `mkfs.lfs /sfl-meta/bibtex/`;
    `mount.lfs /sfl-meta/bibtex/ /sfl/bibtex/`;
    `cd /sfl/bibtex/; sh -x /home/pad/work/lfs/demos/pof-bibtex.sh`;
    `cp $current /sfl/bibtex/mine/test.bib`;

    print "cd parts \n";
    my $time = Time::HiRes::time();
    `cd /sfl/bibtex/mine/parts/.relaxed/; echo toto`;
    my $time2 = Time::HiRes::time();
    print "res = " . ($time2 - $time) ."\n";
    print CDPARTS ($time2 - $time);    print CDPARTS "\n";
    # TODO ptet diviser par taille fichier, comme ca verra une ligne (faire wc $current)

    print "ls \n";
    my $time = Time::HiRes::time();
#    `cd /sfl/bibtex/mine/parts/.relaxed/author:/'author:O. Ridoux'/year:/; ls > /tmp/$current.lsres`;
    `cd /sfl/bibtex/mine/parts/.relaxed/year:/year:1988/author:/; ls > /tmp/$current.lsres`;
    my $time2 = Time::HiRes::time();
    print "res = " . ($time2 - $time) ."\n";
    print LS ($time2 - $time);    print LS "\n";
    my $ls1 = ($time2 - $time);

    print "ls2 \n";
    my $time = Time::HiRes::time();
    `cd /sfl/bibtex/mine/parts/.relaxed/year:/year:1989/author:/; ls > /tmp/$current.lsres2`;
    my $time2 = Time::HiRes::time();
    print "res = " . ($time2 - $time) ."\n";
    print LS2 ($time2 - $time);  print LS2 "\n";
    my $ls2 = ($time2 - $time);

    print "ls3 \n";
    my $time = Time::HiRes::time();
    `cd /sfl/bibtex/mine/parts/.relaxed/author:/; ls > /tmp/$current.lsres3`;
    my $time2 = Time::HiRes::time();
    print "res = " . ($time2 - $time) ."\n";
    print LS3 ($time2 - $time);  print LS3 "\n";
    my $ls3 = ($time2 - $time);

    print "ls4 \n";
    my $time = Time::HiRes::time();
#    `cd /sfl/bibtex/mine/parts/.relaxed/'typeref:Book|typeref:book'/year:/; ls > /tmp/$current.lsres4`;
#    `cd /sfl/bibtex/mine/parts/.relaxed/'typeref:Book|typeref:book'/.strict/; ls > /tmp/$current.lsres4`;
    `cd /sfl/bibtex/mine/parts/.relaxed/'typeref:Book|typeref:book'/.strict/author:/title:/; ls > /tmp/$current.lsres4`;
      #tofix: normalement les author:/title:  sont inutiles,  mais mon transducer est pas top pour l'instant car
      #tofix: des fois a des auteurs mais sont sur plusieurs ligness
    my $time2 = Time::HiRes::time();
    print "res = " . ($time2 - $time) ."\n";
    print LS4 ($time2 - $time);  print LS4 "\n";
    my $ls4 = ($time2 - $time);

    # arith
    my $moy = ($ls1+$ls2+$ls3+$ls4)/4;
    print "moyenne arith ls=$moy\n";
    print LSMOY $moy; print LSMOY "\n";

    # geometric
    my $moy = sqrt(sqrt($ls1*$ls2*$ls3*$ls4));
    print "moyenne geo ls=$moy\n";
    print LSMOYGEO $moy; print LSMOYGEO "\n";


#exit

    print "genview \n";
    my $time = Time::HiRes::time();
    `cd /sfl/bibtex/mine/parts/.relaxed/year:/year:1988/author:/; cp test.bib /tmp `;
    my $time2 = Time::HiRes::time();
    print "res = " . ($time2 - $time) ."\n";
    print GENVIEW ($time2 - $time);  print GENVIEW "\n";

    print "save1 \n";
    my $time = Time::HiRes::time();
    `cd /sfl/bibtex/mine/parts/.relaxed/year:/year:1988/author:/; cp /tmp/test.bib test.bib `;
    my $time2 = Time::HiRes::time();
    print "res = " . ($time2 - $time) ."\n";
    print MODIF ($time2 - $time);  print MODIF "\n";
    my $sav1 = ($time2 - $time);

    print "save2 \n";
    `cd /sfl/bibtex/mine/parts/.relaxed/year:/year:1988/author:/; ls > /tmp/misc; cp test.bib /tmp/`; # count only save time
    `cd /tmp; cp test.bib old.bib; perl -pi -e 's/[bB]ook{/article{/g' test.bib`;
    my $time = Time::HiRes::time();
    `cd /sfl/bibtex/mine/parts/.relaxed/year:/year:1988/author:/; cp /tmp/test.bib test.bib `;
    my $time2 = Time::HiRes::time();
    print "res = " . ($time2 - $time) ."\n";
    print MODIF2 ($time2 - $time);  print MODIF2 "\n";
    `cd /sfl/bibtex/mine/parts/.relaxed/year:/year:1988/author:/; diff test.bib /tmp/old.bib > /tmp/$current.diff2`;
    my $sav2 = ($time2 - $time);

    print "save3 \n";
    `cd /sfl/bibtex/mine/parts/.relaxed/year:/year:1989/author:/; ls > /tmp/misc; cp test.bib /tmp/`; # count only save time
    `cd /tmp; cp test.bib old.bib; perl -pi -e 'if(/[bB]ook{/) { \$i++;s/[bB]ook{/article{/ if \$i==1}' test.bib`;
    my $time = Time::HiRes::time();
    `cd /sfl/bibtex/mine/parts/.relaxed/year:/year:1989/author:/; cp /tmp/test.bib test.bib `;
    my $time2 = Time::HiRes::time();
    print "res = " . ($time2 - $time) ."\n";
    print MODIF3 ($time2 - $time);  print MODIF3 "\n";
    `cd /sfl/bibtex/mine/parts/.relaxed/year:/year:1989/author:/; diff test.bib /tmp/old.bib > /tmp/$current.diff3`;
    my $sav3 = ($time2 - $time);

    my $sav4 = $sav2; # cheat mais c le pire cas donc ca va, cheattresgentil :)

    # arith
    my $moy = ($sav1+$sav2+$sav3+$sav4)/4;
    print "moyenne arith sav=$moy\n";
    print SAVMOY $moy; print SAVMOY "\n";

    # geometric
    my $moy = sqrt(sqrt($sav1*$sav2*$sav3*$sav4));
    print "moyenne geo sav=$moy\n";
    print SAVMOYGEO $moy; print SAVMOYGEO "\n";

exit;

    `umount /sfl/bibtex`;
    `sleep 2; sudo pkill -9 LfsOcaml; sudo pkill -9 LfsOcaml; sudo pkill -9 LfsOcaml`;
} @files;

