/*
  This file is part of the "OCamlFuse" library.

  OCamlFuse is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation (version 2 of the License).

  OCamlFuse is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with OCamlFuse.  See the file LICENSE.  If you haven't received
  a copy of the GNU General Public License, write to:

  Free Software Foundation, Inc.,
  59 Temple Place, Suite 330, Boston, MA
  02111-1307  USA

  Vincenzo Ciancia

  applejack@users.sf.net
  vincenzo_ml@yahoo.it
*/


#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/bigarray.h>
#include <caml/camlidlruntime.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <dirent.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <math.h>
#include <fuse.h>
#include <sys/types.h>
#include <sys/xattr.h>

#include "Fuse_bindings.h"

#define min(a,b) (a<b?a:b)

struct cstring
{
  int length;
  char * data;
};

value c2ml_cstring(int length,char * data,struct cstring * res)
{
  res->length = length;
  res->data = data;
  value vres = alloc_small(1,Abstract_tag);
  Field(vres,0) = (value) res;
  return vres;
}

value cstring_length(value * vstr)
{
  struct cstring * str = (struct cstring *) Field(vstr,0);
  return (Val_int(str->length));
}

value c2ml_copy_string(value vsource,value vdest)
{
  CAMLparam2(vsource,vdest);

  struct cstring * str = (struct cstring *) Field(vsource,0);
  memcpy(String_val(vdest),str->data,min(str->length,string_length(vdest)));

  CAMLreturn(Val_unit);
}

value ml2c_copy_string(value vsource,value vdest)
{
  CAMLparam2(vsource,vdest);

  struct cstring * str = (struct cstring *) Field(vdest,0);
  memcpy(str->data,String_val(vsource),min(str->length,string_length(vsource)));

  CAMLreturn(Val_unit);
}

value * ocaml_string_create;

value c2ml_setxattr_flags(int flags)
{
  CAMLparam0 ();
  CAMLlocal1(res);

  if (flags==XATTR_CREATE)
    {
      res=Val_int(1);
    }
  else if (flags==XATTR_REPLACE)
    {
      res=Val_int(2);
    }
  else res=Val_int(0);

  CAMLreturn(res);
}

/* This part shamelessly copied from mlfuse */
#define ADDFLAGT(T, X) \
  num_ml_constr--;\
  if (T) {\
    tmp = alloc_small (2, 0);\
    Field (tmp, 0) = Val_int (num_ml_constr);\
    Field (tmp, 1) = res;\
    res = tmp;\
  }

#define ADDFLAG(X) ADDFLAGT(flags & X, X)
#define ADDBASEFLAG(X) ADDFLAGT((flags & 3) == X, X)

value c_flags_to_open_flag_list (int flags) {
  CAMLparam0 ();
  CAMLlocal2 (res, tmp);
  int num_ml_constr = 8;

  res = Val_int (0);

  ADDFLAG (O_EXCL);
  ADDFLAG (O_TRUNC);
  ADDFLAG (O_CREAT);
  ADDFLAG (O_APPEND);
  ADDFLAG (O_NONBLOCK);

  ADDBASEFLAG (O_RDWR);
  ADDBASEFLAG (O_WRONLY);
  ADDBASEFLAG (O_RDONLY);

  CAMLreturn (res);
}
/* End of shame */

static int ml2c_unix_error[] =
{
  E2BIG,
  EACCES,
  EAGAIN,
  EBADF,
  EBUSY,
  ECHILD,
  EDEADLK,
  EDOM ,
  EEXIST,
  EFAULT,
  EFBIG,
  EINTR,
  EINVAL,
  EIO,
  EISDIR,
  EMFILE,
  EMLINK,
  ENAMETOOLONG,
  ENFILE,
  ENODEV,
  ENOENT,
  ENOEXEC,
  ENOLCK,
  ENOMEM,
  ENOSPC,
  ENOSYS,
  ENOTDIR,
  ENOTEMPTY,
  ENOTTY,
  ENXIO,
  EPERM,
  EPIPE,
  ERANGE,
  EROFS,
  ESPIPE,
  ESRCH,
  EXDEV,
  EWOULDBLOCK,
  EINPROGRESS,
  EALREADY,
  ENOTSOCK,
  EDESTADDRREQ,
  EMSGSIZE,
  EPROTOTYPE,
  ENOPROTOOPT,
  EPROTONOSUPPORT ,
  ESOCKTNOSUPPORT ,
  EOPNOTSUPP,
  EPFNOSUPPORT,
  EAFNOSUPPORT,
  EADDRINUSE,
  EADDRNOTAVAIL,
  ENETDOWN,
  ENETUNREACH,
  ENETRESET,
  ECONNABORTED,
  ECONNRESET,
  ENOBUFS,
  EISCONN,
  ENOTCONN,
  ESHUTDOWN,
  ETOOMANYREFS,
  ETIMEDOUT,
  ECONNREFUSED,
  EHOSTDOWN,
  EHOSTUNREACH,
  ELOOP,
  EOVERFLOW
};

int ml2c_unix_file_kind[] =
{
  S_IFREG,
  S_IFDIR,
  S_IFCHR,
  S_IFBLK,
  S_IFLNK,
  S_IFIFO,
  S_IFSOCK
};

void ml2c_Unix_stats_struct_stat(value v,struct stat * s)
{
  s->st_dev=Int_val(Field(v,0));
  s->st_ino=Int_val(Field(v,1));
  s->st_mode=0 | Int_val(Field(v,3)) | ml2c_unix_file_kind[Int_val(Field(v,2))];
  s->st_nlink=Int_val(Field(v,4));
  s->st_uid=Int_val(Field(v,5));
  s->st_gid=Int_val(Field(v,6));
  s->st_rdev=Int_val(Field(v,7));
  s->st_size=Int64_val(Field(v,8));
  s->st_blksize=4096; /* STUB */
  s->st_blocks=ceil((double)s->st_size/(double)s->st_blksize); /* STUB! */
  s->st_atime=Double_val(Field(v,9));
  s->st_mtime=Double_val(Field(v,10));
  s->st_mtim.tv_nsec = 0; // PAD, otherwise have fuzzy value in this field, and get wrong date (and make sux)
  s->st_ctime=Double_val(Field(v,11));
}

void ml2c_Unix_struct_statfs(value v,struct statfs * st)
{
  st->f_bsize = Long_val(Field(v,0));
  st->f_blocks = Long_val(Field(v,1));
  st->f_bfree = Long_val(Field(v,2));
  st->f_bavail = Long_val(Field(v,3));
  st->f_files = Long_val(Field(v,4));
  st->f_ffree = Long_val(Field(v,5));
  st->f_namelen = Long_val(Field(v,6));
}

#define FOR_ALL_OPS(MACRO) \
    MACRO(getattr) \
    MACRO(readlink) \
    MACRO(getdir) \
    MACRO(mknod) \
    MACRO(mkdir) \
    MACRO(symlink) \
    MACRO(unlink) \
    MACRO(rmdir) \
    MACRO(rename) \
    MACRO(link) \
    MACRO(chmod) \
    MACRO(chown) \
    MACRO(truncate) \
    MACRO(utime) \
    MACRO(open) \
    MACRO(read) \
    MACRO(write) \
    MACRO(statfs) \
    MACRO(release) \
    MACRO(flush) \
    MACRO(fsync) \
    MACRO(setxattr) \
    MACRO(getxattr) \
    MACRO(listxattr) \
    MACRO(removexattr)

#define SET_NULL_OP(OPNAME) .OPNAME = NULL,

static struct fuse_operations ops = {
  FOR_ALL_OPS(SET_NULL_OP)
};

static value * ocaml_list_length=NULL;

#define DECLARE_OP_CLOSURE(OPNAME) static value * OPNAME##_closure=NULL;
FOR_ALL_OPS(DECLARE_OP_CLOSURE)

#define getattr_ARGS (const char* path, struct stat * buf)
#define getattr_CB vres=callback(*getattr_closure,vpath);
#define getattr_RES ml2c_Unix_stats_struct_stat(Field(vres,0),buf);

#define getdir_ARGS (const char *path, fuse_dirh_t h, fuse_dirfil_t filler)
#define getdir_CB vres=callback(*getdir_closure,vpath);
#define getdir_RES \
     vtmp=Field(vres,0);\
     while (Is_block(vtmp)) \
     { \
	res=filler(h,String_val(Field(vtmp,0)),DT_UNKNOWN); \
        if (res != 0) break; \
        vtmp=Field(vtmp,1);\
     }

#define mknod_ARGS (const char *path, mode_t mode, dev_t rdev)
#define mknod_CB vres=callback2(*mknod_closure,vpath,Val_int(mode)); /* STUB: special nodes can't be made */
#define mknod_RES

#define mkdir_ARGS (const char *path, mode_t mode)
#define mkdir_CB vres=callback2(*mkdir_closure,vpath,Val_int(mode));
#define mkdir_RES

#define unlink_ARGS (const char *path)
#define unlink_CB vres=callback(*unlink_closure,vpath);
#define unlink_RES

#define rmdir_ARGS (const char *path)
#define rmdir_CB vres=callback(*rmdir_closure,vpath);
#define rmdir_RES

#define readlink_ARGS (const char *path, char *buf, size_t size)
#define readlink_CB vres=callback(*readlink_closure,vpath);
#define readlink_RES strncpy(buf,String_val(Field(vres,0)),size-1);

#define symlink_ARGS (const char *path, const char *dest)
#define symlink_CB \
     vtmp = copy_string(dest); \
     vres=callback2(*symlink_closure,vpath,vtmp);
#define symlink_RES

#define rename_ARGS (const char *path, const char *dest)
#define rename_CB \
     vtmp = copy_string(dest); \
     vres=callback2(*rename_closure,vpath,vtmp);
#define rename_RES

#define link_ARGS (const char *path, const char *dest)
#define link_CB  \
     vtmp = copy_string(dest); \
     vres=callback2(*link_closure,vpath,vtmp);
#define link_RES

#define chmod_ARGS (const char *path, mode_t mode)
#define chmod_CB vres=callback2(*chmod_closure,vpath,Val_int(mode));
#define chmod_RES

#define chown_ARGS (const char *path, uid_t uid, gid_t gid)
#define chown_CB vres=callback3(*chown_closure,vpath,Val_int(uid),Val_int(gid));
#define chown_RES

#define truncate_ARGS (const char *path, off_t size)
#define truncate_CB vres=callback2(*truncate_closure,vpath,copy_int64(size));
#define truncate_RES

#define utime_ARGS (const char *path, struct utimbuf *buf)
#define utime_CB vres=callback3(*utime_closure,vpath,copy_double(buf->actime),copy_double(buf->modtime));
#define utime_RES

#define open_ARGS (const char *path, int flags)
#define open_CB vres=callback2(*open_closure,vpath,c_flags_to_open_flag_list(flags));
#define open_RES

#define read_ARGS (const char *path, char *buf, size_t size, off_t offset)
#define read_CB \
     vres=callback3(*read_closure,vpath,c2ml_cstring(size,buf,&cstr),copy_int64(offset));
#define read_RES res=Int_val(Field(vres,0));

#define write_ARGS (const char *path, const char *buf, size_t size,off_t offset)
#define write_CB \
     vres=callback3(*write_closure,vpath,c2ml_cstring(size,buf,&cstr),copy_int64(offset));
#define write_RES res=Int_val(Field(vres,0));

#define release_ARGS (const char *path, int flags)
#define release_CB vres=callback2(*release_closure,vpath,c_flags_to_open_flag_list(flags));
#define release_RES res=0;

#define flush_ARGS (const char *path)
#define flush_CB vres=callback(*flush_closure,vpath);
#define flush_RES res=0;

#define statfs_ARGS (const char *path, struct statfs *stbuf)
#define statfs_CB vres=callback(*statfs_closure,vpath);
#define statfs_RES ml2c_Unix_struct_statfs(Field(vres,0),stbuf);

#define fsync_ARGS (const char *path, int isdatasync)
#define fsync_CB vres=callback2(*fsync_closure,vpath,Val_bool(isdatasync));
#define fsync_RES

#define setxattr_ARGS (const char *path, const char *name, const char *val,size_t size,int flags)
#define setxattr_CB \
    value args[]={ vpath,copy_string(name),c2ml_cstring(size,val,&cstr),c2ml_setxattr_flags(flags) }; \
    vres=callbackN(*setxattr_closure,4,args);
#define setxattr_RES

#define getxattr_ARGS (const char *path, const char *name, char *val,size_t size)
#define getxattr_CB vres=callback2(*getxattr_closure,vpath,copy_string(name));
#define getxattr_RES \
     res=string_length(Field(vres,0)); \
     if (size > 0) \
        if (string_length(Field(vres,0))>=size) \
        { \
           res = -ERANGE; \
        } \
        else \
        { \
	  memcpy(val,String_val(Field(vres,0)),string_length(Field(vres,0))); \
        }

#define listxattr_ARGS (const char *path, char *list, size_t size)
#define listxattr_CB vres=callback(*listxattr_closure,vpath);
#define listxattr_RES \
     vtmp=Field(Field(vres,0),0);\
     int len; \
     char * dest=list; \
     int rem=size; \
     if (size==0) \
     { \
        res = Int_val(Field(Field(vres,0),1)); \
     } \
     else \
     { \
        while (Is_block(vtmp)) \
        { \
           len = string_length(Field(vtmp,0))+1; \
           if (rem>=len) \
           { \
              memcpy(dest,String_val(Field(vtmp,0)),len); \
              vtmp=Field(vtmp,1);\
              dest+=len; \
              rem=rem-len; \
           } \
           else \
           { \
             res = -ERANGE; \
             break; \
           } \
        } \
        res=size-rem; \
     }

#define removexattr_ARGS (const char *path, const char *name)
#define removexattr_CB vres=callback2(*removexattr_closure,vpath,copy_string(name));
#define removexattr_RES

#define CALLBACK(OPNAME) \
static int ops_##OPNAME OPNAME##_ARGS \
{ \
  leave_blocking_section(); \
  struct cstring cstr; \
  value vpath; \
  value vres; \
  value vtmp; \
  int res=-1; \
  vpath = copy_string(path); \
  OPNAME##_CB \
  if (Tag_val(vres)==1) /* Result is not Bad */ \
     { \
       res=0; \
       OPNAME##_RES /* res can be changed here */ \
     } \
  else \
  { \
     if (Is_block(Field(vres,0))) \
         res=-127; /* STUB: deal with unknown errors */ \
     else res=-ml2c_unix_error[Int_val(Field(vres,0))]; \
  } \
  enter_blocking_section(); \
  return res; \
}

FOR_ALL_OPS(CALLBACK)

#define SET_OPERATION(OPNAME) \
  if (op->OPNAME==NULL) ops.OPNAME=NULL; \
  else \
    { \
      OPNAME##_closure=caml_named_value(op->OPNAME); \
      ops.OPNAME=ops_##OPNAME; \
    }

void set_fuse_operations(struct fuse_operation_names const *op)
{
  FOR_ALL_OPS(SET_OPERATION)
}

struct fuse_operations * get_fuse_operations()
{
  return &ops;
}

value * ocaml_fuse_loop_closure;

int mainloop(struct fuse * f,int multithreaded)
{
  if (f == NULL)
    return(-1);

  value _fuse=alloc_small(1, Abstract_tag);
  Field(_fuse, 0) = (value) f;

  return callback2(*ocaml_fuse_loop_closure,_fuse,Val_bool(multithreaded));
}

void ml_fuse_main(int argc,str * argv,struct fuse_operations const * op)
{
  ocaml_fuse_loop_closure = caml_named_value("ocaml_fuse_loop");
  ocaml_string_create = caml_named_value("ocaml_string_create");
  ocaml_list_length = caml_named_value("ocaml_list_length");

  char * mountpoint;
  int multithreaded;
  int fd;

  struct fuse * fuse = __fuse_setup(argc,argv,op,&mountpoint,&multithreaded,&fd);

  if (fuse!=NULL)
    {
      mainloop(fuse,multithreaded);
      __fuse_teardown(fuse,fd,mountpoint);
    }
}

value ocaml_fuse_is_null(value v) /* For Com.opaque values */
{
  return Val_bool(0==Field(v,0)); // Is this the right way to check for null?
}
