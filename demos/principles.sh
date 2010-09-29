##############################################################################
# Prelude
##############################################################################
root=`pwd`
src=/home/pad/c-lfs/

if [ ! -r $src/p_logic ]
    then echo "It seems that $src is not the directory where resides LFS source. 
You must adjust in this file the src variable.
";
    exit
fi

if [ ! -r $root/.relaxed ]
    then echo "It seems that $root is not a directory where LFS is mounted. 
You must launch this script from a mounted LFS path.
";
    exit
fi

#if [ $# -ne 1 ]
#    then echo "Usage: $0 <Lfs source absolute path>"
#    exit
#fi
#src=$1


#################################################################
mkdir "art";
mkdir "music";
mkdir "movie";
mkdir "port";
mkdir "seaside";
mkdir "USA";
mkdir "capital";

cd "$root/art/movie/port/seaside/USA"; touch "los-angeles.jpg";
cd "$root/USA/capital";                touch "washington.jpg";
cd "$root/port/seaside/USA";           touch "miami.jpg";
cd "$root/art/music/port/seaside/USA"; touch "boston.jpg";
cd "$root/port";                       touch "hamburg.jpg";
cd "$root/port/seaside/USA";           touch "san-diego.jpg";
cd "$root/art/music/port/seaside/USA"; touch "new-york.jpg";

#################################################################
cd "$root/port/USA";
ls;
# => [ "music/"; "movie/"; "art/";  "miami.jpg"; "san-diego.jpg"]));

cd "$root"'/capital|movie/!seaside'; 
ls;
# => ["washington.jpg"]));

#################################################################
rm "washington.jpg";

cd "$root"
mkdir hot
cd "port/USA";
ls;
# => [ "music/"; "movie/"; "art/"; "miami.jpg"; "san-diego.jpg"]));
mv "miami.jpg" "../hot";

#################################################################
cd "$root";
rmdir "movie";
rmdir "music";
rmdir "art";

cd "$root";
mkdir "art";
cd "art";
mkdir "music";
mkdir "movie";

cd "$root"'/port&USA';
ls;
# => ["new-york.jpg"; "san-diego.jpg"; "boston.jpg"; "los-angeles.jpg"]);
mv "boston.jpg"      "music/";
mv "new-york.jpg"    "music/";
mv "los-angeles.jpg" "movie/";
ls;
# => ["san-diego.jpg"; "art/"]);
cd "art";
ls;
# => ["movie/"; "music/"]);    

cd "$root"/'USA&music';
touch "chicago.jpg";
cd "$root";
ls;
# => ["art/"; "port/"; "USA/"; "seaside/"; "hot/"]);
cd "art";
ls;
# => ["music/"; "movie/"; "port/"; "seaside/"]);

cd "$root";
mkdir "geography";
mv "seaside" "geography/noprop_seaside";
mv "port"    "geography/noprop_port";
mv "USA"     "geography/noprop_USA";
mv "capital" "geography/noprop_capital";

#################################################################

cd "$root";
mkdir "type:";
cd "$root/type:picture"; touch "misc.jpg" ;
cd "$root/type:program"; touch "foo.c" ;
cd "$root";
ls;
#    assert(ls_() $=$ set ["type:/"; "art/"; "geography/"; "hot/"]);
cd "type:";
ls;
#    assert(ls_() $=$ set ["type:program/"; "type:picture/"]);

cd "$root";
mkdir "plugins";
cd "plugins";
mkdir "logic:";
cd "$root";
mkdir "year:";
cd 'logic:year:';

cp $src/p_logic/int_logic .


cd "$root/year:2000"; touch "vacation-corsica.jpg" ;
cd "$root/year:2001"; touch "vacation-england.jpg" ;
cd "$root/year:2002"; touch "vacation-france.jpg" ;
cd "$root"/'year:>2000';
ls;
# => ["year:2001/"; "year:2002/"]);
cd "$root/year:";
ls;
# => ["year:2000/"; "year:>2000/"]);    
	     
#    assert(interval_logic (Prop "2001")  (Prop ">2000") = true);
#    assert(interval_logic (Prop ">2000") (Prop "2001")  = false);

#################################################################

cd "$root";
mkdir "size:";
mkdir "name:";
mkdir "ext:";
mkdir "mypictures";
cd "mypictures";
echo "xxxxxxxxxxxxxxxxx" > "big.jpg";
echo "xx" > "small.jpg";
cd "ext:jpg";
# => [ "size:2/"; "size:17/"; "name:small/"; "name:big/"]);

cd "$root/plugins";
mkdir "transducer:";
cd "$root/transducer:mp3";

cp $src/p_transducer/fakemp3_transducer .

cd "$root/";
mkdir "mymusic";
cd "mymusic";
mkdir "artist:";
mkdir "genre:";
printf "tag_genre=Disco\ntag_artist=BeeGees\ndataaaa" > "staying_alive.mp3"
printf "tag_genre=Pop\ntag_artist=JoeDassin\ndata" > "ete_indien.mp3"    
ls
# => [ "genre:Disco/"; "genre:Pop/"; "artist:BeeGees/"; "artist:JoeDassin/"; "name:ete_indien/";  "name:staying_alive/"; "size:39/"; "size:42/" ] );

#    cd_ "genre:Disco";
#    assert(mp3_transducer (read "staying_alive.mp3") $=$ 
#	   set [Prop "artist:BeeGees"; Prop "genre:Disco"]);


#################################################################
cd "$root"
mkdir "myprograms"
cd "myprograms"

printf 	"int f(int x) {\nint y;\nassert(x > 1);\ny = x;\nfprintf(stderr, \"x = %d\", x);\nreturn y * 2\n}\nint f2(int z) {\nreturn z * 4\n}" > foo.c
#cat foo.c
# => 
#	     [ "int f(int x) {";
#	       "int y;";
#	       "assert(x > 1);";
#	       "y = x;";
#	       "fprintf(stderr, \"x = %d\", x);";
#	       "return y * 2";
#	       "}";
#	       "int f2(int z) {";
#	       "return z * 4";
#	       "}";

#   assert(c_adv_transducer (read "foo.c" +> lines +> take 2)
#	     = [ [Prop "var:x"; Prop "synchro"; Prop "function:f"];  (* ADDON *)
#		 [Prop "var:y"; Prop "function:f"]]);

#mkdir "part:"; #TEST
mkdir "function:";
mkdir "var:";
mkdir "debugging";
mkdir "error";
mkdir "synchro"; #(* ADDON *)
cd "$root/plugins";
mkdir "adv_transducer:";
cd "$root"/'adv_transducer:c';

cp $src/p_adv_transducer/naivec_adv_transducer .

cd "$root/myprograms";
ls
# => ["foo.c"]);

cd "parts";

ls
# => [ "debugging/"; "error/";  "function:f/"; "function:f2/"; "var:/"; "synchro/"; (* ADDON *)"foo.c"  ] );
wc foo.c
# => 10;

cd "function:f/var:/";
ls
# => [ "var:x/";  "var:y/";  "debugging/"; "error/"; "synchro/"; (* ADDON *)"foo.c"]);
wc foo.c
# => 7

cd '!(debugging|error)';
ls
# => ["var:x/"; "var:y/"; "synchro/"; (* ADDON *) "foo.c"]);
cat foo.c
# =>
#	     [ "int f(int x) {";
#	       "int y;";
#	       ".........:1";
#	       "y = x;";
#	       ".........:2";
#	       "return y * 2";
#	       ".........:3";

cat foo.c | sed -e s/y/z/ > /tmp/foo.c
cat /tmp/foo.c > foo.c
cd "$root/myprograms/parts"
cat foo.c
# => 
#	     [ "int f(int x) {";
#	       "int z;";
#	       "assert(x > 1);";
#	       "z = x;";
#	       "fprintf(stderr, \"x = %d\", x);";
#	       "return z * 2";
#	       "}";
#	       "int f2(int z) {";
#	       "return z * 4";
#	       "}";



exit 0
