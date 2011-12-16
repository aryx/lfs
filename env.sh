# I put both stuff useful for the user and developer in this file. Could
# separate have a env-user.sh, env-compile.sh, env-developer.sh but it's
# not worth it.

#!!!!You need to source me with "source env.sh" from the _RIGHT_ directory!!!!
if [ ! -r data/basicenv.sh ]
    then echo "There is no basicenv.sh in data/.
Are you sure you run this script from the LFS source directory ?
";
fi

##############################################################################
# Compile
##############################################################################

#For hg, and to compile the source (ocamlc, menhir, camlidl for lfs, ...)
#echo setting PATH
#export PATH=~pad/packages/bin:~pad/packages/sbin:$PATH
#echo setting LD_LIBRARY_PATH
#export LD_LIBRARY_PATH=~pad/packages/lib:$LD_LIBRARY_PATH

# To debug
#export OCAMLRUNPARAM="b"

##############################################################################
# Run
##############################################################################

# To run. To find the plugins files. cf also globals/config.ml. This is not
# needed if you have done a configure --prefix=xxx and make install.
# The following is needed if you want to test lfs from its source
# directory.
echo setting LFS_HOME
export LFS_HOME=`pwd`
