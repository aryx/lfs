------------------------------------------------------------------------------
A Logic File System:

Information systems provide a means for organising, retrieving and
manipulating information. They are becoming more and more important
with the advent of the numerical age, with the increasing variety and
numbers of digital documents (e.g., music files, pictures, videos,
emails, programs source, xml documents). To search for those documents,
traditional information systems like file systems, and the web with
its search engines provide both navigation and query tools but do not allow
to combine them. On the one hand, navigation is intuitive and
progressive but it implies a rigid and single classification of data.
On the other hand, query brings flexibility and expressivity but lacks
the advantages of navigation. To make it easy to manipulate the
contents of those documents, those systems come with separate tools such
as advanced text editors or integrated development environments (IDE) which
suffer too from the same limitation.

We propose a new paradigm for information system, the "Logic File
System" (LFS), that offers expressive organisation, retrieval
combining both query and navigation, and ease of manipulation of both
files and contents of files in an integrated way at the file system
level.

To achieve this integration, this paradigm associates logical
properties to files and parts of file, and logical deduction serves as
a basis for navigation and querying. "Paths are formulas", directories
represent queries and determine set of files and parts of file whose
description satisfies the formula. The root directory represents the
formula "true", and sub-directories of a directory are determined by
the most general properties refining the query, hence combining
navigation and query. File contents are determined by those parts of
the original file that satisfy the formula. This permits simultaneous
read and write accesses to different "views" on a file, in order to
help in separating a user's concerns. Properties can be attached to
information manually by the user and automatically via programs called
"transducers", and can be ordered manually by the user to form
taxonomies or automatically via logic deduction engines. Users can
dynamically extend the system by providing their own logic deduction
engines and transducers.


See the docs/ directory (and the code :) ) for more information.

See the demos/ directory for example of use.

See the install.txt file for the installation procedure.

You can find examples of (useful) logic engines and transducers in
 the p_logic/, p_transducer/ and p_adv_transducer/ directories.


For bugs or problems send an e-mail to yoann.padioleau@gmail.com with LFS
 in the subject of your mail.

Authors:
  principles: Olivier ridoux (ridoux@irisa.fr)
  theory: Sebastien Ferre (ferre@irisa.fr)
  coding: Yoann Padioleau (aka pad) (yoann.padioleau@gmail.com)


------------------------------------------------------------------------------
FAQ:
Q. I enjoy so much LFS, is there any LFS t-shirt ?
A. not yet, stay tuned...
