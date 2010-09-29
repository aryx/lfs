root=`pwd`
src="/home/pad/work/lfs/code"

sh -x $src/demos/basicenv.sh

cd "$root"
mkdir "prop:";

cd "$root"/'logic:prop:';
cp $src/p_logic/prop_logic2 .

cd "$root"
mkdir "mine"
cd "mine"

touch prop:a/fa
touch prop:b/fb
touch 'prop:a AND b'/fab
touch prop:a/prop:b/fab2
cd 'prop:a OR b'
ls


