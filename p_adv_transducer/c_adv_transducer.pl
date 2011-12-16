#!/usr/bin/perl

use diagnostics;
use strict;
use warnings;

#c  (ab/linux) more complex c_transducer (sux to say that takes one line :) and extract one prop)
# d'ailleurs dommage que a pas les drivers,
#  defstruct (to know what are the field of a struct inode)
#
#  cpp/include:font, cpp/define:XXX (need state, can take many lines)
#   (and duplicate with with def:... uniform search for all kind of name func/var/macro)
#  size of func(=> need forward reading),
#  function:xxx, take:x (take:void), type?:v type!:v
#   (\w+(  when not in code { } )
#   cool if have filter (cd sys_*)
#  header/body
#  declaration (var:xxx) (type:xxx too)
#  global (need state know that not in func) (var:xxx & global)
#   static, extern
#  comment (need state), annotation (share line with some code)
#  structdef/typedef
#   hasfield:xxx
#  string:xxx (cd string:<error>)
#  authors, date de modif
#  emacs:
#   ifdef(how with synchro? need state, but will compartiment it, does not interfere with func def),
#     cpp/ifdef/ifdeftrue:xxx ifdeffalse:xxx (factorise ifnedef, ifdef, #else)
#   indent
#   function (tags)
#   syscall=sys_... + asmlinkage,
#   lock_kernel/down()/up() (see critical) how if conditionnal ? goto out: ?
#  inferer des props interessantes du noyaux
#  mixed with javadoc (take comment)
# how? naive ? my C compiler ? use emacs ? use ctags ?

sub maybe { my ($Uv) = @_;    (defined($Uv))  ?  ($Uv)  :  () ;}

my $state = {};

while(<STDIN>) {
    my $s = $_;
    my @aspects = ();

    if($state->{isincomment}) {
	push @aspects, "comment:";
	if($s =~ /\*\//) { $state->{isincomment} = 0 }
    }
    elsif($state->{isinbody}) {
	if($s =~ /^}/) { $state->{isinbody} = 0 }
    }
    else {
	if($s =~ /^\/\*/) {
	    $state->{isincomment} = 1;
	    push @aspects, "comment:";
	}
	elsif($s =~ /(\w+)\s*\(/) {
	    $state->{isinbody} = 1;
	    $state->{function} = "function:$1";
	    push @aspects, "synchro"; #TODO should erase state
	    push @aspects, "header";
	}
    }

    my $res =
      [
       @aspects,
       maybe($state->{function}),
      ];
    print (join "/", @{$res}); print "\n";

    $state->{function} = undef if ($state->{isinbody}||0) == 0;

}
