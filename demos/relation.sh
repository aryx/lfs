root=`pwd`
src="/home/pad/lfs_code"

sh -x $src/demos/basicenv.sh

# alain + mado
#  => pad
#  => dedef
#  => severine    +  wilfied 
#        => noemi
#        => maxence

mkdir "demo";
root="$root/demo";
cd "$root";

mkdir "person";

mkdir "male"; mkdir "female";
mkdir "tall";  mkdir "small";

mkdir "father:";
mkdir "mother:";

cd "$root/person/male/";
touch "pad";
touch "alain";
touch "wilfried";
touch "maxence";


cd "$root/person/female/";
touch "mado" ;
touch "dedef" ;
touch "severine" ;
touch "noemie" ;


cd "$root/.ext";
ln "alain" "<father:of>pad";
ln "alain" "<father:of>dedef";
ln "alain" "<father:of>severine";

ln "mado" "<mother:of>pad";
ln "mado" "<mother:of>dedef";
ln "mado" "<mother:of>severine";

ln "wilfried" "<father:of>maxence";
ln "wilfried" "<father:of>noemie";

ln "severine" "<mother:of>maxence";
ln "severine" "<mother:of>noemie";

mv "alain"    "small/";
mv "wilfried" "tall/";

cd "$root";
ls;
#father:
#female
#male
#mother:
#name:alain
#name:dedef
#name:mado
#name:maxence
#name:noemie
#name:pad
#name:severine
#name:wilfried
#small
#tall


cd "$root/father:";
ls;
#father:is==alain
#father:is==wilfried
#father:of==dedef
#father:of==maxence
#father:of==noemie
#father:of==pad
#father:of==severine
#female
#male
#mother:
#name:alain
#name:dedef
#name:maxence
#name:noemie
#name:pad
#name:severine
#name:wilfried
#small
#tall


cd "$root/father:is==wilfried";
ls;
#female
#male
#name:maxence
#name:noemie

cd "$root/father:is===(tall&tall&name:wilfried)";
ls;
#female
#male
#name:maxence
#name:noemie

cd "$root/father:/father:is=>=";
ls;
#father:of==dedef
#father:of==maxence
#father:of==noemie
#father:of==pad
#father:of==severine
#name:alain
#name:wilfried
#small
#tall
