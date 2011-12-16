#!/usr/bin/perl

use diagnostics;
use strict;
use warnings;

sub maybe { my ($Uv) = @_;    (defined($Uv))  ?  ($Uv)  :  () ;}
sub filter {
    my ($f, $xs) = @_;
    my @res = ();
    foreach $a (@{$xs}) {  push @res, $a if (&$f($a)) }
    [@res];
}

my $state = {};


#old: while(<STDIN>) {
my @data = ();
while(<STDIN>) {
    my $s = $_;
    push @data, $s;
}
my $i = 0;
map {


    my $s = $_;
    my @aspects = ();

    ####################################################
    # the structure/hierarchy of a doc (and synchro points)
    ####################################################
    #my format
     #note: genant le fait que c pas la ligne qui est en fait la synchro ?
     #note: si only one level synchro, c pas grave.
    if(($s =~ /\(\*\*\*/) && ($data[$i+1] =~ /\(\* \*(.*)\* \*\)/)) {
	push @aspects, "synchro"; $state = {};
	push @aspects, "header";
	$state->{section} = "section:$1";
    }
    if(($s =~ /\(\+\+\+/) && ($data[$i+1] =~ /\(\* \+(.*)\+ \*\)/)) {
	push @aspects, "header";
	$state->{subsection} = "subsection:$1";
    }


    #ocaml standard format (often in .ml only), ex: (* Comparaisons *)
    if($s =~ /^\(\*\s+([A-Z]\w+)\s+\*\)/) {
        push @aspects, "synchro"; $state = {};
        push @aspects, "header";
        $state->{section} = "section:$1";
    }

    #literate ocaml format, ex:   (** {6 Comparaisons}
    if($s =~ /^\(\*\*\s+{\d\s+(.*)}\s+/) {
        push @aspects, "synchro"; $state = {};
        push @aspects, "header";
        $state->{section} = "section:$1";
    }

    ####################################################


    if($s =~ /^\s*##/) { push @aspects, "comment:" }
    if($s =~ /^\(\*(.*)\*\)\s*$/) { push @aspects, "comment:" }
    if($s =~ /((##)|(\(\*))(\w+):/) { push @aspects, "note:$4" }

    if($s =~ /[aA]ssert/) { push @aspects, "error", "aspect:error" };
    if($s =~ /[eE]xample/) { push @aspects, "example", "aspect:specification" };
    #pre post inv,
    #algo?


    if($s =~ /\bprintf\b|\bfprintf\b/) { push @aspects, "aspect:debugging" };
    if($s =~ /\blog\b/) { push @aspects, "aspect:logging" };
    if($s =~ /Timing/) { push @aspects, "aspect:profiling" };


    if($s =~ /raise (\w+)/) { push @aspects, "raise:$1", "aspect:error" };
    if($s =~ /[fF]ailwith "(.*?)"/) { push @aspects, "raise:failwith_$1", "aspect:error" };



    ####################################################
    # def func/var/type
    ####################################################
    #todo6:  multilvevel synchro with func, type, ..., but less needed

    #note: so many format for fun specification in ocaml :(
    if($s =~ /^let\s*(rec\s*)?(\w+)\s*=\s*(fun|function)\s+/) {
	push @aspects, "header";
	$state->{decl} = "function:$2"
    }
    elsif($s =~ /^let\s*(rec\s*)?(\w+)\s+(\w+)/) {
	push @aspects, "header";
	$state->{decl} = "function:$2"
    }
    elsif($s =~ /^let\s*(rec\s*)?\((\w+):\s*(.*)\)\s+=\s*fun/) {
	push @aspects, "header";
	$state->{decl} = "function:$2";
	$state->{type} = "functype:$3";
    } #todo6: et les and


    elsif($s =~/^let\s*((\w+)|\((\w+):(.*)\))\s*=/) {
	my $id = $2 || $3;
	if(!($id eq "_")) {
	push @aspects, "header";
	$state->{decl} = "defvar:$id"; #todo6: vartype:
        }
    }
    elsif($s =~ /^type\s*(\w+)/) {
	push @aspects, "header";
	$state->{decl} = "deftype:$1" } #todo6: et les type and

    else { }

    # for mli
    if(($s =~ /^external\s+(\w+)\s*:\s*(.*?)\s*=/) || ($s =~ /^external\s+\(\s*(.*?)\s*\)\s*:\s*(.*?)\s*=/)) {
	push @aspects, "header";
	$state->{decl} = "function:$1";
	$state->{type} = "functype:$2";
    }

    if($s =~ /^val\s+(\w+)\s*:\s*([^;\n]+)/) { # normally .* but sometimes lines get finished by ;;
##        if($s =~ /^val\s+(\w+)\s*:\s*(.+)/) { # normally .* but sometimes lines get finished by ;;
	push @aspects, "header";
	$state->{decl} = "function:$1";
	$state->{type} = "functype:$2";
    }

    # fucking multi line
    if($s =~ /^val\s+(\w+)\s*:\s*$/) {
        my $func = $1;
        if ($data[$i+1] =~ /\s*([^;\n]+)/) {
##        if ($data[$i+1] =~ /\s*(.*)/) {
            push @aspects, "header";
            $state->{decl} = "function:$func";
            $state->{type} = "functype:$1";
        }
    }

    #todo: class  and so have different condition for function, or have method (mais bon aimerait les avoir toute)

    #todo:  cd take:acc, cd take:f (idioms/norm of ocaml),


    #todo: when have multi-obj for pof,  can have   prop have_field:  pour les entités types
    #todo: can search for type,  so the prop are also the component (is_pair), the value of enum (value:And),...
    #todo: will have better negation,  cos if want see the func that have no side effect on w ? actually the negation
    #todo:  just select lines withouth, and so when ls function:,  get also func that do :(

    ####################################################
    #indentation based parsing.

    # if use tab, then should change length() to compute_tab_space(), but complex cos depending on column.
    # indeed tabbing does not take same space,
    # there is coupling with the editor => dont use tabs. in emacs put (setq-default indent-tabs-mode nil) et C-x h M-x untabify for converting
    my $current_indent = 0;
    if($s =~/^\s*$/) { $current_indent ||= $state->{oldindent} } #empty line does not count
    if($s =~ /^(\s+)[^\s]/) { $current_indent = length($1) }
    $state->{oldindent} = $current_indent;
    $state->{indent} ||= [];
    $state->{indent} = filter( sub { my ($e) = @_; ($e->{level} < $current_indent)} , $state->{indent});



    #just indent level
    push @aspects, "indent:$current_indent";

    #type enumere
    #if($s =~ /^\s*\|\s*([A-Z]\w*)(.*?)\s*->/) {
    if($s =~ /^\s*\|\s*(.*?)\s*->/) {
        push @{$state->{indent}}, { prop => "match:$1", level => $current_indent};
    }

    #todo: if then else (must not erase level)


    # field assignation,  w with ...
    if($s =~ /^\s*(\w+)\s*=/) {
        push @{$state->{indent}}, { prop => "field:$1", level => $current_indent};
    }
    if($s =~ /^(\s*{).*\bwith\s*(\w+)\s*=/) {
        push @{$state->{indent}}, { prop => "field:$2", level => lentgh($1)};
    }
    # field acess
    while($s =~ /(\w+)\.(\w+)/g) {
	my $name=$2;
	if ($1 =~ /^[a-z].*/) { #  dont want List.nth
	    push @aspects, "field:$name"
	}
    }


    # effect assign
    if($s =~ /^\s*(\w+)\s*:=/) {
        push @{$state->{indent}}, { prop => "effect:$1", level => $current_indent};
    }
    # effect access
    while($s =~ /!(\w+)/g) {
        push @aspects, "effect:$1"
    }



    ##########################################
    my $res =
      [
       @aspects,
       maybe($state->{section}),
       maybe($state->{subsection}),
       maybe($state->{decl}),
       maybe($state->{type}),
       (map { $_->{prop} } @{$state->{indent}}),
      ];
    print (join "/", @{$res}); print "\n";
    $i++;
} @data;



