; Eclipse spirit. To be used with C-c C-l and C-c C-g.


(defun pad-ocaml-project-LFS ()
  (interactive)

  (setq 
   pad-ocaml-project-path "/home/pad/c-lfs"
   pad-ocaml-project-subdirs 
   (split-string 
    "commons globals  
     lfs_core lfs_path lfs_real 
     gui 
     ocamlbdb ocamlfuse ocamlagrep
     ")
   pad-ocaml-project-toplevel "mount.top"
   )





  ; --------------------------------------------------------------------------
  (setq
   pad-ocaml-project-prog "mount-byte.lfs"
   pad-ocaml-project-args 
   (join-string 
    (list 
     "-debugger"
     (case 1
       (1 "-verbose_level 3 -nofsck -nofuse -command_lfs /home/pad/meta-lfs/src/ /home/pad/lfs/")
       (2 "-verbose_level 3 /tmp/comments_lfs /home/pad/lfs/")
       )
     )))

  ; --------------------------------------------------------------------------
  (setq
   pad-ocaml-project-prog "build_db"
   pad-ocaml-project-args 
   (join-string 
    (list 
     "-debugger"
     (case 4
       (1 " -build_db_pad_home /home/pad/meta-lfs")
       (2 "-verbose_level 3 /tmp/comments_lfs /home/pad/lfs/")
       (3 "-profile -no_logic -verbose_level 2 -lfs_metapath /home/pad/meta-lfs/test/ /home/pad/Desktop/music6/")
       (4 "-use_c_transducer -lfs_metapath /home/pad/meta-lfs/linux /home/pad/linux/init")
       )
     )))



  ; --------------------------------------------------------------------------
  ; for the help system, for C-c C-h to find where to look for
  (mapcar (lambda (p) 
            (ocaml-add-path (concat pad-ocaml-project-path "/" p)))
          pad-ocaml-project-subdirs
          )


  )

(setq ocaml-lib-path nil)

(setq ocaml-module-alist)