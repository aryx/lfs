##############################################################################
# Prelude
##############################################################################

# This script can be run by the user who use the LFS real mode, in order
# to populate LFS with a first hierarchy of properties.
#
# update: this script is also now analyzed by build-db.ml to also
# generate properties (the ext: and type:) for the LFS semi-real mode.



root=`pwd`
src=/home/pad/c-lfs

if [ ! -r $src/p_logic ]
    then echo
"It seems that $src is not the directory where resides LFS source.
You must adjust in this file the src variable.
";
    exit
fi

if [ ! -r $root/.relaxed ]
    then echo
"It seems that $root is not a directory where LFS is mounted.
You must launch this script from a mounted LFS path.
";
    exit
fi

#if [ $# -ne 2 ]
#    then echo "Usage: $0 <lfs mounted directory> <Lfs source absolute path>"
#    exit
#fi
#src=$2


##############################################################################
# Plugins
##############################################################################
cd "$root";
mkdir "plugins";

cd "plugins";
mkdir "logic:";
mkdir "transducer:";
mkdir "adv_transducer:";
#todo?: rename in  part_transducer, file_transducer

##############################################################################
# Synchro
##############################################################################
cd "$root";
mkdir "synchro";
mkdir "multisynchro";

cd "multisynchro";
 mkdir "synchro1:";
 mkdir "synchro2:";
 mkdir "synchro3:";
 mkdir "synchro4:";
 mkdir "synchro5:";






##############################################################################
# System
##############################################################################
cd "$root";
mkdir "props-system";

cd "props-system";
mkdir "size:";
mkdir "name:";
mkdir "ext:";
mkdir "file:";
mkdir "date:";

cd "$root";
cd "logic:transducer:/logic:adv_transducer:/";
#bug cos have sometimes wrong extension (such as numbers)
cp $src/p_logic/prop_logic .
#cp $src/p_logic/prop_logic2 .

cd "$root"/'adv_transducer:BOTTOM';
#cp $src/p_adv_transducer/generic_adv_transducer.pl .


cd "$root"/logic:size:/;
cp $src/p_logic/size_logic .

cd "$root"/logic:name:/;
#cp $src/p_logic/string_logic2.pl .

#there is bug here
#cd "$root"/logic:ext:/;
#cp $src/p_logic/string_logic2.pl .


# mkdir -p system/mtime
# mkdir -p system/ctime
# mkdir -p system/atime
# mkdir -p system/inode
#
# mkdir -p system/uid
# mkdir -p system/gid
# mkdir -p system/nuid
#
# mkdir -p system/mode
#
# mkdir -p system/mode/have
# mkdir -p system/mode/have/user
# mkdir -p system/mode/have/group
# mkdir -p system/mode/have/other
#
# mkdir -p system/mode/style
# mkdir -p system/mode/style/execute
# mkdir -p system/mode/style/write
# mkdir -p system/mode/style/read
# mkdir -p system/mode/style/special
#
# mkdir -p system/mode/final
# mkdir -p system/mode/final/execute/user/xu
# mkdir -p system/mode/final/execute/group/xg
# mkdir -p system/mode/final/execute/other/xo
#
# mkdir -p system/mode/final/write/user/wu
# mkdir -p system/mode/final/write/group/wg
# mkdir -p system/mode/final/write/other/wo
#
# mkdir -p system/mode/final/read/user/ru
# mkdir -p system/mode/final/read/group/rg
# mkdir -p system/mode/final/read/other/ro

##############################################################################
# Contain
##############################################################################
cd "$root";
mkdir "contain:";

#related = google:, glimpse:, agrep:

#cd "$root"/'logic:contain:';
##cp $src/p_logic/string_logic2.pl .
##for i in a b c d e f g h i j k l m n o p q r s t u v w x y z; do ls 'contain:<'$i; done
##for i in A B C D E F G H I J K L M N O P Q R S T U V W X Y Z; do ls 'contain:<'$i; done


##############################################################################
# Type
##############################################################################
cd "$root";
mkdir "props-type";


# those props are used to make coarser category for other properties
# they are not intended to be assigned to files (cos type: does already this job automatically)
cd "props-type";
mkdir "Programs";
mkdir "Documents";
mkdir "Documentation";
mkdir "Emails";
mkdir "Music";
mkdir "Pictures";
mkdir "Video";



cd "$root";
cd "props-type";
mkdir "type:"

cd "type:Program"
cd "type:Document"
cd "type:Music"
cd "type:Picture"
cd "type:Video"
cd "type:Application"
cd "type:Compressed"
cd "type:Archive"
cd "$root";


# note that can check that have covered all the extension at least for
# your data by doing cd '!type:'/ext:^  and then adjust with mvdir

#-----------------------------
# Programs
#-----------------------------


mv "ext:c"  "type:Program/noprop_ext:c"
mv "ext:h"  "type:Program/noprop_ext:h"
mv "ext:cpp"  "type:Program/noprop_ext:cpp"
mv "ext:cc" "type:Program/noprop_ext:cc"
mv "ext:C" "type:Program/noprop_ext:C"

mv "ext:ml" "type:Program/noprop_ext:ml"
mv "ext:mli" "type:Program/noprop_ext:mli"
mv "ext:mll" "type:Program/noprop_ext:mll"
mv "ext:mly" "type:Program/noprop_ext:mly"

mv "ext:y" "type:Program/noprop_ext:y"
mv "ext:l" "type:Program/noprop_ext:l"

mv "ext:el" "type:Program/noprop_ext:el"

mv "ext:mlx" "type:Program/noprop_ext:mlx"
mv "ext:pmx" "type:Program/noprop_ext:pmx"
mv "ext:pmi" "type:Program/noprop_ext:pmi"
mv "ext:pli" "type:Program/noprop_ext:pli"

mv "ext:pl" "type:Program/noprop_ext:pl"
mv "ext:pas" "type:Program/noprop_ext:pas"
mv "ext:bet" "type:Program/noprop_ext:bet"
mv "ext:hs" "type:Program/noprop_ext:hs"
mv "ext:lhs" "type:Program/noprop_ext:lhs"
mv "ext:pm" "type:Program/noprop_ext:pm"
mv "ext:sh" "type:Program/noprop_ext:sh"
mv "ext:csh" "type:Program/noprop_ext:csh"
mv "ext:java" "type:Program/noprop_ext:java"
mv "ext:scm" "type:Program/noprop_ext:scm"
mv "ext:asm" "type:Program/noprop_ext:asm"
mv "ext:s" "type:Program/noprop_ext:s"
mv "ext:ada" "type:Program/noprop_ext:ada"
mv "ext:dylan" "type:Program/noprop_ext:dylan"
mv "ext:tcl" "type:Program/noprop_ext:tcl"
mv "ext:p" "type:Program/noprop_ext:p"
mv "ext:acl2" "type:Program/noprop_ext:acl2"
mv "ext:lisp" "type:Program/noprop_ext:lisp"
mv "ext:m4" "type:Program/noprop_ext:m4"
mv "ext:idl" "type:Program/noprop_ext:idl"
mv "ext:m" "type:Program/noprop_ext:m"
mv "ext:sig" "type:Program/noprop_ext:sig"
mv "ext:chr" "type:Program/noprop_ext:chr"
mv "ext:sql" "type:Program/noprop_ext:sql"
mv "ext:cgi" "type:Program/noprop_ext:cgi"

mv "ext:cocci" "type:Program/noprop_ext:cocci"

mv "ext:hdl"  "type:Program/noprop_ext:hdl"
mv "ext:jack" "type:Program/noprop_ext:jack"
mv "ext:hack" "type:Program/noprop_ext:hack"
mv "ext:php"  "type:Program/noprop_ext:php"


mv "name:Makefile" "type:Program/noprop_name:Makefile"



#-----------------------------
# Document
#-----------------------------

mv "ext:txt"  "type:Document/noprop_ext:txt"
mv "ext:doc"  "type:Document/noprop_ext:doc"
mv "ext:TXT"  "type:Document/noprop_ext:TXT"
mv "ext:org"  "type:Document/noprop_ext:org"


mv "ext:tex"  "type:Document/noprop_ext:tex"
mv "ext:bib"  "type:Document/noprop_ext:bib"

mv "ext:dot" "type:Document/noprop_ext:dot"
mv "ext:fig"  "type:Document/noprop_ext:fig"
mv "ext:dia"  "type:Document/noprop_ext:dia"
mv "ext:jgr" "type:Document/noprop_ext:jgr"



mv "ext:ppt"  "type:Document/noprop_ext:ppt"
mv "ext:rtf"  "type:Document/noprop_ext:rtf"
mv "ext:xls"  "type:Document/noprop_ext:xls"
mv "ext:odt"  "type:Document/noprop_ext:odt"
mv "ext:mgp"  "type:Document/noprop_ext:mgp"

mv "ext:troff"  "type:Document/noprop_ext:troff"
mv "ext:nroff"  "type:Document/noprop_ext:nroff"

mv "ext:pdf"  "type:Document/noprop_ext:pdf"
mv "ext:ps"  "type:Document/noprop_ext:ps"
mv "ext:dvi"  "type:Document/noprop_ext:dvi"
mv "ext:PDF"  "type:Document/noprop_ext:PDF"

mv "ext:htm"  "type:Document/noprop_ext:htm"
mv "ext:html"  "type:Document/noprop_ext:html"
mv "ext:sgml"  "type:Document/noprop_ext:sgml"
mv "ext:shtml"  "type:Document/noprop_ext:shtml"
mv "ext:HTM"  "type:Document/noprop_ext:HTM"

mv "ext:css" "type:Document/noprop_ext:css"

mv "ext:lyx"  "type:Document/noprop_ext:lyx"
mv "ext:man"  "type:Document/noprop_ext:man"
mv "ext:texi" "type:Document/noprop_ext:texi"
mv "ext:db" "type:Document/noprop_ext:db"


mv "ext:eps"  "type:Document/noprop_ext:eps"
mv "ext:epsi"  "type:Document/noprop_ext:epsi"


#-----------------------------
# Music
#-----------------------------


mv "ext:mp3/" "type:Music/noprop_ext:mp3/"
mv "ext:ogg/" "type:Music/noprop_ext:ogg/"
mv "ext:xm/" "type:Music/noprop_ext:xm/"
mv "ext:s3m/" "type:Music/noprop_ext:s3m/"
mv "ext:wma/" "type:Music/noprop_ext:wma/"
mv "ext:mid/" "type:Music/noprop_ext:mid/"
mv "ext:it/" "type:Music/noprop_ext:it/"
mv "ext:mod/" "type:Music/noprop_ext:mod/"
mv "ext:wav/" "type:Music/noprop_ext:wav"


#-----------------------------
# Picture
#-----------------------------

mv "ext:jpg/" "type:Picture/noprop_ext:jpg/"
mv "ext:bmp/" "type:Picture/noprop_ext:bmp/"
mv "ext:gif/" "type:Picture/noprop_ext:gif/"
mv "ext:JPG/" "type:Picture/noprop_ext:JPG/"
mv "ext:jpeg/" "type:Picture/noprop_ext:jpeg/"
mv "ext:png/" "type:Picture/noprop_ext:png/"
mv "ext:tif/" "type:Picture/noprop_ext:tif/"
mv "ext:xpm/" "type:Picture/noprop_ext:xpm/"
mv "ext:GIF/" "type:Picture/noprop_ext:GIF/"

#-----------------------------
# Video
#-----------------------------

mv "ext:avi/" "type:Video/noprop_ext:avi/"
mv "ext:mpg/" "type:Video/noprop_ext:mpg/"
mv "ext:mpeg/" "type:Video/noprop_ext:mpeg/"
mv "ext:mov/" "type:Video/noprop_ext:mov/"
mv "ext:rm/" "type:Video/noprop_ext:rm/"
mv "ext:ram/" "type:Video/noprop_ext:ram/"
mv "ext:wmv/" "type:Video/noprop_ext:wmv/"
mv "ext:asf/" "type:Video/noprop_ext:asf/"



#-----------------------------
# Compressed
#-----------------------------

mv "ext:gz/"  "type:Compressed/noprop_ext:gz/"
mv "ext:bz2/" "type:Compressed/noprop_ext:bz2/"
mv "ext:tgz/" "type:Compressed/noprop_ext:tgz/"
mv "ext:zip/" "type:Compressed/noprop_ext:zip/"
mv "ext:shar/" "type:Compressed/noprop_ext:shar/"
mv "ext:hqx/" "type:Compressed/noprop_ext:hqx/"
mv "ext:Z/" "type:Compressed/noprop_ext:Z/"
mv "ext:rpm" "type:Compressed/noprop_ext:rpm"
mv "ext:tar" "type:Compressed/noprop_ext:tar"
mv "ext:rar/"  "type:Compressed/noprop_ext:rar/"
mv "ext:7z/"  "type:Compressed/noprop_ext:7z/"

# tar,  type:Archive
# rpm deb ipk

#-----------------------------
# Object
#-----------------------------


mv "ext:o/"   "type:Object/noprop_ext:o/"

mv "ext:cmi/" "type:Object/noprop_ext:cmi/"
mv "ext:cmo/" "type:Object/noprop_ext:cmo/"
mv "ext:cma/" "type:Object/noprop_ext:cma/"
mv "ext:cmx/" "type:Object/noprop_ext:cmx/"
mv "ext:cmxa/" "type:Object/noprop_ext:cmxa/"

mv "ext:a/" "type:Object/noprop_ext:a/"
mv "ext:so/" "type:Object/noprop_ext:so/"

# take care, I think mercurial also sometimes use this extension
mv "ext:d/" "type:Object/noprop_ext:d/"
mv "ext:di/" "type:Object/noprop_ext:di/"


mv "ext:elc" "type:Object/noprop_ext:elc"

mv "ext:zi" "type:Object/noprop_ext:zi"
mv "ext:zo" "type:Object/noprop_ext:zo"

mv "ext:class" "type:Object/noprop_ext:class"

mv "ext:hi" "type:Object/noprop_ext:hi"
mv "ext:gph" "type:Object/noprop_ext:gph"


mv "ext:bak/" "type:Object/noprop_ext:bak/"
mv "ext:log/" "type:Object/noprop_ext:log/"
mv "ext:old/" "type:Object/noprop_ext:old/"

mv "ext:dvi/" "type:Object/noprop_ext:dvi/"
mv "ext:aux/" "type:Object/noprop_ext:aux/"
mv "ext:bbl" "type:Object/noprop_ext:bbl"
mv "ext:blg" "type:Object/noprop_ext:blg"
mv "ext:idx" "type:Object/noprop_ext:idx"
mv "ext:toc" "type:Object/noprop_ext:toc"


#-----------------------------

# .out, .data

#TODO cf dircolors.el


#less: a bigtype:,  with coarser category,  Multimedia, Document, ...

##############################################################################
# Time
##############################################################################

cd "$root";

mkdir "props-time";
cd "props-time";
mkdir "year:";
mkdir "month:";
# can also be done via date solver


cd "$root"/'logic:year:'/;
cp $src/p_logic/int_logic .

cd "$root"/'logic:month:'/;
cp $src/p_logic/string_logic2.pl .

cd "$root"/'logic:date:'/;
cp $src/p_logic/date_logic_via_logfun .
#TODO secu:  and secu solver


##############################################################################
# Owner
##############################################################################

#cd "$root";
#mkdir "root";
#mkdir "home";
#mkdir "home/pad";
#chown pad home/pad
#chgrp pad home/pad


##############################################################################
# Generic
##############################################################################
cd "$root"

mkdir "props-generic";
cd "props-generic";
mkdir "title:";
mkdir "comment:";
mkdir "note:";
mkdir "aspect:";
# kind of inheritance, an aspect is a note which is a comment

cd "$root"

cd "$root"/'logic:title:';
#cp $src/p_logic/string_logic2.pl .

cd "$root"/'logic:note:';
cp $src/p_logic/string_logic2.pl .


##############################################################################
# Quality
##############################################################################
cd "$root";

mkdir "props-quality";
cd "props-quality";
mkdir "Great";
mkdir "Favourite";
mkdir "Excellent";
mkdir "Bof";
mkdir "Fun";

##############################################################################
# Project
##############################################################################
cd "$root";

mkdir "project:";


##############################################################################
# Classic
##############################################################################

cd "$root";
mkdir "props-classic";
cd "props-classic";

mkdir "bin";
mkdir "admin"; # =~ etc  #mkdir machine: diskette, otheros
mkdir "docs";  #mkdir comparaison #mkdir synthese #mkdir lecture-notes #mkdir citations # report, thesis, ...
mkdir "code";
mkdir "txt";
mkdir "src";
mkdir "todo";
mkdir "tmp";


mkdir "backup";
mkdir "obsolete";
mkdir "archive";

mkdir "less";
mkdir "old";
mkdir "oldold";
mkdir "done";

mkdir "misc";
mkdir "vrac";
mkdir "divers";

mkdir "work";
mkdir "progs";
mkdir "research";
mkdir "teaching";


#donebefore:mkdir "bin";
mkdir "doc";
mkdir "etc";
mkdir "include";
mkdir "lib";
mkdir "share";
mkdir "var";
#donebefore:mkdir "src";

mkdir "test";
mkdir "examples";
mkdir "source";
mkdir "headers";

mkdir "usr";
mkdir "local";
mkdir "opt";

#a bit specific to make lfs source easily tarrable/browsable
cd "todo";
mkdir "todo:";



##ridoux taxo
#mkdir "reviews";
#mkdir "rapport";
#mkdir "texte";
#mkdir "soutenance";
## orga (IFSCI, INSA)  cours (DEUG ...) student (YOANN FERRE *SAVED)



##############################################################################
##############################################################################
cd "$root";

#pad personal
mkdir "props-misc";
cd "props-misc";
mkdir "RCS";       #TODO type:Backup ??
mkdir "CVS";
mkdir "SCCS";
mkdir "formatted"; #TODO type:CompiledFrom
mkdir "target:";
#period: (dea), cours: (tcpr)

mkdir "Mine";
mkdir "Me";
# friends-jobs, friends-personal, possession





##############################################################################
# Music
##############################################################################
cd "$root/transducer:mp3";
cp $src/p_transducer/mp3_transducer.pl .

cd "$root/transducer:ogg";
cp $src/p_transducer/ogg_transducer.pl .

cd "$root";


#done: mkdir "music";
cd "music";
mkdir "artist:";
mkdir "album:";
mkdir "genre:";
#done: mkdir "year:" "title:" "comment:"

mkdir "time:";

#mkdir "bitrate:";
#mkdir "frequency:";

mkdir "playlist:";


cd "$root"/logic:artist:/logic:album:/;
cp $src/p_logic/string_logic2.pl .

cd "$root"/logic:genre:/;
cp $src/p_logic/string_logic2.pl .

cd "$root"/logic:time:/;
cp $src/p_logic/duree_logic .

#cd 'genre:.*Rock.*'
#cd 'genre:(Disco|Dance|Funk)'
#cd 'genre:(Darkwave|New Wave)'
#cd 'genre:(Retro|Cult|Oldies)'
#cd 'genre:(Electronic|Techno|Trance)'
#cd 'genre:(Hip-Hop|Rap)'
#cd 'genre:(Comedy|Humour)'

##############################################################################
# Pictures
##############################################################################
cd "$root/transducer:jpg";
cp $src/p_transducer/jpg_transducer.pl .

cd "$root/transducer:gif";
cp $src/p_transducer/gif_transducer.pl .

cd "$root";


cd "pictures";
mkdir "resolution:";

#inspired by iDive
mkdir "people:";
mkdir "place:";
mkdir "event:";
mkdir "webalbum:";
#done:  mkdir "year:" "month:"

#cd "$root"/logic:resolution:/;
#cp $src/p_logic/resolution_logic .


##############################################################################
# Documentation
##############################################################################
cd "$root";

cd "documentation";
mkdir "keyword:";
mkdir "mansection:";

##############################################################################
# Domains
##############################################################################

cd "$root";
mkdir "Domain";

cd "Domain";
#mkdir "logic";
mkdir "Computer-science";
mkdir "Math";
mkdir "History";
mkdir "Economics";
mkdir "Psychology";
mkdir "Philosophy";
mkdir "Politics";
mkdir "Games";
mkdir "Noninfo"; #BOF
#TODO science, ... (inspire from yahoo or google directory)

cd "Computer-science";


##############################################################################
# Emails
##############################################################################
cd "$root/transducer:mail/transducer:emlx/";
cp $src/p_transducer/email_transducer.pl .

cd "$root";
cd "Emails";
mkdir "subject:";
 #TODO can be done via string solver
 mkdir "thread:";
 mkdir "thema:";
mkdir "mailing_list:";
mkdir "from:";
 #TODO can be done via string solver
 mkdir "from_domain:";
 mkdir "from_surname:";
 mkdir "from_lastname:";
mkdir "to:";
mkdir "date_send:";

#done: year: month:

mkdir "SPAM";
#TODO  To: Cc:

mkdir "headmessage";

#cd "$root"/logic:from:/logic:to:/;
#cp $src/p_logic/string_logic2.pl .
 # cos contain <> so string_logic will have formula

#cd "$root"/logic:mailing_list:/;
#cp $src/p_logic/string_logic .
 # cos i thing may contain []

cd "$root"/logic:thema:/;
cp $src/p_logic/string_logic2.pl .


##############################################################################
# Bibtex
##############################################################################
cd "$root";

#done: year: title:

cd "Documentation";

mkdir "author:";
mkdir "institution:";
mkdir "typeref:";
mkdir "ref:";
mkdir "domain:";


cd "$root"/'adv_transducer:bib';
cp $src/p_adv_transducer/bibtex_adv_transducer .

cd "$root"/'logic:author:';
#cp $src/p_logic/string_logic2.pl .

cd "$root"/'logic:domain:';
#cp $src/p_logic/string_logic2.pl .

##############################################################################
# Documents
##############################################################################

cd "$root";

cd "Documents";


mkdir "part:";
mkdir "chapter:";
mkdir "section:";
mkdir "subsection:";
mkdir "subsubsection:";

#done: title: comment: note: aspect:

mkdir "depth_title:";

#done: mkdir "latex"; done in domain
mkdir "environment:";

mkdir "index:";

mkdir "paracontain:";

cd "$root"/'adv_transducer:tex';
cp $src/p_adv_transducer/latex_adv_transducer.pl .

cd "$root"/'adv_transducer:txt';
cp $src/p_adv_transducer/txtpad_adv_transducer.pl .

cd "$root"/'logic:index:';
cp $src/p_logic/string_logic2.pl .

#cd "$root"/'index:(fixed.*|less.*|done.*)';
#cd "$root"/'index:lfs.*';
#cd "$root"/'index:project.*';


cd "$root"/'logic:part:/logic:chapter:/logic:section:/logic:subsection:/logic:subsubsection:';
cp $src/p_logic/string_logic2.pl .

cd "$root"/'logic:depth_title:';
cp $src/p_logic/int_logic .

##############################################################################
# Programs
##############################################################################

cd "$root";

cd "Programs";

mkdir "function:";
mkdir "deftype:";
mkdir "defvar:"

mkdir "class:";
mkdir "method:";
mkdir "visibility:";
mkdir "interface";
mkdir "implementation";
mkdir "call:";

mkdir "var:";
mkdir "debugging";

mkdir "functype:"

mkdir "indent:";

mkdir "make:";

#done: "aspect:";
 #error, fault-tolerance, security, specification,
 #debugging, logging, profiling
 #optimisation, version
#done: note:
 # robust:, ....

#done: comment: note:


mkdir "Header";
mkdir "Error";
 cd "Error";
 mkdir "Raise:";
 cd "..";
mkdir "Algo";

mkdir "Spec";
 cd "Spec";
 #mkdir "header";
 #mkdir "functype:"
 mkdir "Example";
 mkdir "Assert";
 mkdir "Contract";
 cd "Contract";
  mkdir "Pre";
  mkdir "Post";
  mkdir "Invariant";

cd "$root"/'logic:functype:';
cp $src/p_logic/type_logic .

cd "$root"/'logic:function:';
cp $src/p_logic/string_logic2.pl .

cd "$root"/'logic:indent:';
cp $src/p_logic/int_logic .




#------------------------------------------------------------------------------
# ml
#------------------------------------------------------------------------------
cd "$root";

cd "Programs";

mkdir "field:";
mkdir "match:";
mkdir "effect:";

cd "$root"/'adv_transducer:mlx/adv_transducer:ml/adv_transducer:mli';
cp $src/p_adv_transducer/ml_adv_transducer.pl .

#------------------------------------------------------------------------------
# perl
#------------------------------------------------------------------------------

cd "$root"/'adv_transducer:pmx';
cp $src/p_adv_transducer/fixperl_adv_transducer.pl .

#------------------------------------------------------------------------------
# C
#------------------------------------------------------------------------------
cd "$root"/'adv_transducer:cnaive';
cp $src/p_adv_transducer/naivec_adv_transducer .

cd "$root"/'adv_transducer:c';
cp $src/p_adv_transducer/c_adv_transducer.pl .


#------------------------------------------------------------------------------

cd "$root"/'adv_transducer:cpp';
cp $src/p_adv_transducer/cpp_adv_transducer.pl .

cd "$root"/'adv_transducer:java';
cp $src/p_adv_transducer/java_adv_transducer.pl .

#------------------------------------------------------------------------------
cd "$root"/'adv_transducer:make';
cp $src/p_adv_transducer/makefile_adv_transducer.pl .

##############################################################################
##############################################################################

cd "$root"
mkdir "props-patch";
cd "props-patch";


mkdir "pdirectory:"
mkdir "pfile:"
mkdir "pext:"
mkdir "ppath:"
mkdir "pdirfull:"


mkdir "pdiff:"
mkdir "pdiffkind:"

mkdir "pcomment:"
mkdir "pfunction:"
mkdir "pfunctionpointer:"
mkdir "pdefine:"



## with emails
mkdir "porganization:"
mkdir "phavepatch"

mkdir "pkernel:"

mkdir "pthemafunction:"
mkdir "pthemaclassfunction:"


cd "$root";

cd "Programs";


mkdir "kspecial:"
mkdir "kqualifier:"
mkdir "cbody"
mkdir "cinclude:"
mkdir "cdefine:"
mkdir "cfunctionpointer:"
mkdir "cfieldop:"
mkdir "csetstruct:"
mkdir "clabel:"

mkdir "functionthema:"

##############################################################################
# Symlinks/Views
##############################################################################

cd "$root";

mkdir "Original";

mkdir "Symlinks";
cd "Symlinks";
ln -s "../.best/!props-system^" nosys
ln -s "../.compat/props-classic^|props-misc^/.strict" classicview

##############################################################################

