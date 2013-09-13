#!/usr/bin/perl

#sudo ./bench_pof_exp
# to then get the stat, cat /tmp/INFO  or look at the trace of the script
# to get better view on the stat (cos many info)  grep "data|xxx" to get more directly xxx info
use Time::HiRes;

#exit;

my $SRC  = "/home/pad/work/lfs/code";
my $DATA = "/home/pad/work/lfs/data/pof";

open (INFO, "> /tmp/INFO") or die $!;

map {
    my $h = $_;
    my $current = $h->{src};
    my $mount = $h->{mount};
    my $demo = $h->{demo};
    my $dest = $h->{dest};
    print "Testing: $current\n";
    system("mkfs.lfs /sfl-meta/$mount/");
    system("mount.lfs /sfl-meta/$mount/ /sfl/$mount/");
    `cd /sfl/$mount/; sh -x /home/pad/work/lfs/demos/$demo`;
    `cp $DATA/$mount/$current /sfl/$mount/mine/$dest` if !(exists($h->{special}));


    print "copy time \n";
    my $time = Time::HiRes::time();
    system("cd /sfl/$mount/;" . $h->{special}) if (exists($h->{special}));
    my $time2 = Time::HiRes::time();
    print "res = " . ($time2 - $time) ."\n";
    my $copy = ($time2 - $time);

#exit;
    system("umount /sfl/$mount/");
#DBM:    `sleep 2; sudo pkill -9 LfsOcaml; sudo pkill -9 LfsOcaml; sudo pkill -9 LfsOcaml`;

    print INFO "data = $current\n";
    print INFO "copy time = $copy\n";
    my @xs = `cat /sfl-meta/$mount/stat_context`;
    map { print INFO "$_"; } @xs;
    my @xs = `cd /sfl-meta/$mount/; du -c children parents extfiles iprop_prop prop_iprop filesdb `;     #not extfiles
    map { print INFO "$_"; } @xs;
    print INFO "\n";

    system("mount.lfs /sfl-meta/$mount/ /sfl/$mount/");

#exit;
    print "cd parts \n";
    my $time = Time::HiRes::time();
    `cd /sfl/$mount/mine/parts/.relaxed/; echo toto`;
    my $time2 = Time::HiRes::time();
    print "res = " . ($time2 - $time) ."\n";
    my $cdparts = ($time2 - $time);

#exit;
#    print "ls \n";
#    my $time = Time::HiRes::time();
#    `cd /sfl/$mount/mine/parts/.relaxed/$h->{LS1}; ls > /tmp/$current.lsres`;
#    my $time2 = Time::HiRes::time();
#    print "res = " . ($time2 - $time) ."\n";
#    my $ls1 = ($time2 - $time);
#
#    print "ls2 \n";
#    my $time = Time::HiRes::time();
#    `cd /sfl/$mount/mine/parts/.relaxed/$h->{LS2}; ls > /tmp/$current.lsres2`;
#    my $time2 = Time::HiRes::time();
#    print "res = " . ($time2 - $time) ."\n";
#    my $ls2 = ($time2 - $time);
#
#    print "ls3 \n";
#    my $time = Time::HiRes::time();
#    `cd /sfl/$mount/mine/parts/.relaxed/$h->{LS3}; ls > /tmp/$current.lsres3`;
#    my $time2 = Time::HiRes::time();
#    print "res = " . ($time2 - $time) ."\n";
#    my $ls3 = ($time2 - $time);
#
#    print "ls4 \n";
#    my $time = Time::HiRes::time();
#    `cd /sfl/$mount/mine/parts/.relaxed/$h->{LS4}; ls > /tmp/$current.lsres4`;
#    my $time2 = Time::HiRes::time();
#    print "res = " . ($time2 - $time) ."\n";
#    my $ls4 = ($time2 - $time);
#
#    my $moyenne_LS = ($ls1+$ls2+$ls3+$ls4)/4;
#    print "moyenne ls=$moyenne_LS\n";
#
#
#
#    print "genview \n";
#    my $time = Time::HiRes::time();
#    `cd /sfl/$mount/mine/parts/.relaxed/$h->{VIEW}; cp $dest /tmp `;
#    my $time2 = Time::HiRes::time();
#    print "res = " . ($time2 - $time) ."\n";
#
#    print "save1 \n";
#    my $time = Time::HiRes::time();
#    `cd /sfl/$mount/mine/parts/.relaxed/$h->{VIEW}; cp /tmp/$dest $dest `;
#    my $time2 = Time::HiRes::time();
#    print "res = " . ($time2 - $time) ."\n";
#    my $sav1 = ($time2 - $time);
#
#    print "save2 \n";
#    `cd /sfl/$mount/mine/parts/.relaxed/$h->{VIEW}; ls > /tmp/misc; cp $dest /tmp`; # count only save time
#    `cd /tmp; cp $dest old.$dest; perl -pi -e $h->{MODIF} $dest`;
#    my $time = Time::HiRes::time();
#    `cd /sfl/$mount/mine/parts/.relaxed/$h->{VIEW}; cp /tmp/$dest $dest `;
#    my $time2 = Time::HiRes::time();
#    `cd /sfl/$mount/mine/parts/.relaxed/$h->{VIEW}; diff $dest /tmp/old.$dest > /tmp/$dest.diff2`;
#    print "res = " . ($time2 - $time) ."\n";
#    my $sav2 = ($time2 - $time);
#
#    print "save3 \n";
#    `cd /sfl/$mount/mine/parts/.relaxed/$h->{VIEW2}; ls > /tmp/misc; cp $dest /tmp/`; # count only save time
#    `cd /tmp; cp $dest old.$dest; $h->{SAVE2COMMAND} $dest`;
#    my $time = Time::HiRes::time();
#    `cd /sfl/$mount/mine/parts/.relaxed/$h->{VIEW2}/; cp /tmp/$dest $dest `;
#    my $time2 = Time::HiRes::time();
#    print "res = " . ($time2 - $time) ."\n";
#    print MODIF3 ($time2 - $time);  print MODIF3 "\n";
#    `cd /sfl/$mount/mine/parts/.relaxed/$h->{VIEW2}; diff $dest /tmp/old.$dest > /tmp/$dest.diff3`;
#    my $sav3 = ($time2 - $time);
#
#    my $sav4 = $sav2; # cheat mais c le pire cas donc ca va, cheattresgentil :)
#
#    my $savmoy = ($sav1+$sav2+$sav3+$sav4)/4;
#    print "moyenne arith sav=$savmoy\n";
#
##exit;


#    `umount /sfl/$mount`;
#    `sleep 2; sudo pkill -9 LfsOcaml; sudo pkill -9 LfsOcaml; sudo pkill -9 LfsOcaml`;

#
#    print INFO "cdparts = $cdparts\n";
#    print INFO "ls = $moyenne_LS\n";
#    print INFO "save = $savmoy\n";
#    print INFO "size (Ko) = ";
#    my @xs = `du $DATA/$mount/$current`;
#      #not correct with multi
#    print INFO "$xs[0]";
#    print INFO "length (lines) = ";
#    my @xs = `wc -l $DATA/$mount/$current`;
#      #not correct with multi
#    print INFO "$xs[0]";
#    print INFO "metasize (Ko)\n";
#    my @xs = `cd /sfl-meta/$mount/; du -c children parents extparts* iprop_prop prop_iprop partsinfo* `;     #not extfiles
#    map { print INFO "$_"; } @xs;
#    my @xs = `cat /sfl-meta/$mount/stat_context`;
#    map { print INFO "$_"; } @xs;
#    print INFO "\n\n";


}
(
#  {src=> "impl.mlx", mount=>"code",  demo=>"pof-code.sh", dest=>"lfs.mlx",
  #  {src=> "pervasives.mli", mount=>"code",  demo=>"pof-code.sh", dest=>"pervasives.mli",

#  {src=> "impl.mlx", mount=>"code",  demo=>"pof-code.sh", dest=>"lfs.mlx",
#   special => " cp -a $DATA/code/*.mli mine/.relaxed/",

  {src=> "impl.mlx", mount=>"code",  demo=>"pof-code.sh", dest=>"impl.mlx",
   special => "cd /tmp; rm -rf all_code; cp -a /home/pad/work/lfs/code/ all_code; cd all_code; make clean; rm -rf RCS ocamlbdb txt todo; find -type l -exec rm {} \\; ; chown pad -R *;  cp -a * /sfl/code/mine/.relaxed",
   #old: special => "cd /home/pad/work/lfs/code; cp -a Common.pmx common.mlx    LfsOcamlOrig.mlx LfsPerl.pmx  spec.mlx impl.mlx     /sfl/code/mine/.relaxed/",

   LS1 => "function:/'section:Le shell'/",
   LS2 => "function:/'section:Plug-ins'/",
   LS3 => "functype:/", # contain:
   LS4 => "'function:mkdir|function:cd'/field:/",
   VIEW => "function:mkdir/",
   MODIF => "'s/Assert/Assevv/g'",
   VIEW2 => "function:cd/",
   SAVE2COMMAND => "perl -pi -e 's/Slash/toto/'",
  },

  {src=> "thesis.tex", mount=>"thesis",  demo=>"pof-latex.sh", dest=>"thesis.tex",
   LS1 => "chapter:Principes/title:/",
   LS2 => "chapter:Extensions/title:/",
   LS3 => "title:/", # contain:   note:
   LS4 => "chapter:Extensions/.strict/",
   VIEW => "'title:Les transducteurs avancés'/!comment:/",
   MODIF => "'s/normaux/normauxx/g'",
   VIEW2 => "title:Résumé/",
   SAVE2COMMAND => "perl -pi -e 's/organiser/toto/'",
  },

 {src=> "thesis.bib", mount=>"bibtex",  demo=>"pof-bibtex.sh", dest=>"thesis.bib",
   LS1 => "year:/year:1988/author:/",
   LS2 => "year:/year:1989/author:/",
   LS3 => "author:/",
   LS4 => "'typeref:Book|typeref:book'/.strict/author:/title:/",
   VIEW => "year:/year:1988/author:/",
   MODIF => "'s/[bB]ook{/article{/g'",
   VIEW2 => "year:/year:1989/author:/",
   SAVE2COMMAND => "perl -pi -e 'if(/[bB]ook{/) { \$i++;s/[bB]ook{/article{/ if \$i==1}'",
  },

 {src=> "foo.c", mount=> "tutorial_pof",  demo=>"pof-naivec.sh", dest=>"foo.c",
   LS1 => "function:f/",
   LS2 => "function:f2/", #contain:
   LS3 => "function:/", #contain:
   LS4 => "'function:f|function:f2'/",
   VIEW => "function:f",
   MODIF => "'s/y/z/g'",
   VIEW2 => "function:f/",
   SAVE2COMMAND => "perl -pi -e 's/y/z/'",
  },

);
(
 {src=> "andrew", mount=> "ab",  demo=>"pof-c.sh", dest=>"arbitrator.c",
#   special => "cp $SRC/p_transducer/c_transducer.pl transducer:c/; cp -a $DATA/ab/* mine/.relaxed/",
   special => "cp $SRC/p_transducer/c_transducer4 transducer:c/; cp -a $DATA/ab/* mine/.relaxed/",
 #  special => "cp $SRC/p_transducer/c_transducer.pl transducer:c/; cp -a $DATA/ab/fscript/*.c mine/.relaxed/",
 # special => "cp $SRC/p_transducer/c_transducer.pl transducer:c/; cp -a $DATA/total-ab.c mine/.relaxed/",
   LS1 => "function:Eshell/",
   LS2 => "function:Eshell/", #contain:
   LS3 => "function:/", #contain:
   LS4 => "'function:Eshell|function:Exit'/",
   VIEW => "function:Eshell",
   MODIF => "'s/close/toto/g'",
   VIEW2 => "function:NewConnection/",
   SAVE2COMMAND => "perl -pi -e 's/union/toto/'",
  },


 {src=> "foo.c", mount=> "tutorial_pof",  demo=>"pof-naivec.sh", dest=>"foo.c",
   LS1 => "function:f/",
   LS2 => "function:f2/", #contain:
   LS3 => "function:/", #contain:
   LS4 => "'function:f|function:f2'/",
   VIEW => "function:f",
   MODIF => "'s/y/z/g'",
   VIEW2 => "function:f/",
   SAVE2COMMAND => "perl -pi -e 's/y/z/'",
  },



 {src=> "linux", mount=> "linux",  demo=>"pof-c.sh", dest=>"fork.c", # process.c if contain:
  #special => "cp $SRC/p_transducer/c_transducer.pl transducer:c/; cp -a $DATA/linux/src-last/*.c mine/.relaxed/",
  special => "cp $SRC/p_transducer/c_transducer4 transducer:c/; cp -a $DATA/linux/src-last/*.c mine/.relaxed/",
  #special => "cp $SRC/p_transducer/c_transducer.pl transducer:c/; cp -a $DATA/linux/total-linux.c mine/.relaxed/",
  #full source ? but have diplicate file
   LS1 => "function:do_fork/",
   LS2 => "contain:do_fork/function:", #contain:
   LS3 => "function:/", #contain:
   LS4 => "'function:do_fork|function:do_fork'/",
   VIEW => "function:do_fork", #contain:do_fork
   MODIF => "'s/struct/toto/g'",
   VIEW2 => "function:mm_release/",
   SAVE2COMMAND => "perl -pi -e 's/struct/toto/'",
  },



  # bench were done with old version of pof => normal if different number
  {src=> "pof_article.tex", mount=>"tex",  demo=>"pof-latex.sh", dest=>"pof_article.tex",
   LS1 => "section:Principles/title:/",
   LS2 => "section:Extensions/title:/",
   LS3 => "title:/", # contain:
   LS4 => "'title:Updating|title:Indexing'/title:/.strict",
   VIEW => "title:Updating/",
   MODIF => "'s/Updating/Updatingg/g'",
   VIEW2 => "section:Conclusion/",
   SAVE2COMMAND => "perl -pi -e 's/contribution/toto/'",

  },




 {src=> "ridoux.bib", mount=>"bibtex",  demo=>"pof-bibtex.sh", dest=>"ridoux.bib",
   LS1 => "year:/year:1988/author:/",
   LS2 => "year:/year:1989/author:/",
   LS3 => "author:/",
   LS4 => "'typeref:Book|typeref:book'/.strict/author:/title:/",
   VIEW => "year:/year:1988/author:/",
   MODIF => "'s/[bB]ook{/article{/g'",
   VIEW2 => "year:/year:1989/author:/",
   SAVE2COMMAND => "perl -pi -e 'if(/[bB]ook{/) { \$i++;s/[bB]ook{/article{/ if \$i==1}'",
  },





);

(


  {src=> "100_000.bib", mount=>"bibtex1",  demo=>"pof-bibtex.sh", dest=>"100_000.bib",
   LS1 => "year:/year:1988/author:/",
   LS2 => "year:/year:1989/author:/",
   LS3 => "author:/",
   LS4 => "'typeref:Book|typeref:book'/.strict/author:/title:/",
   VIEW => "year:/year:1988/author:/",
   MODIF => "'s/[bB]ook{/article{/g'",
   VIEW2 => "year:/year:1989/author:/",
   SAVE2COMMAND => "perl -pi -e 'if(/[bB]ook{/) { \$i++;s/[bB]ook{/article{/ if \$i==1}'",
  },



);

(






);

(




  {src=> "Lfs.pmi", mount=> "code",  demo=> "pof-perl.sh", dest=> "Lfs.pmi",
   LS1 => "section:Contents/function:/",
   LS2 => "section:Create/function:/",
   LS3 => "function:/",
   LS4 => "'function:read|function:write'/",
   VIEW => "section:Contents/function:read/",
   MODIF => "'s/read/readx/g'",
  },





);



(









);



