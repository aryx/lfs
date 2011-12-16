#!/usr/bin/perl
use diagnostics;
use strict;
use warnings;


sub maybe { my ($Uv) = @_;    (defined($Uv))  ?  ($Uv)  :  () ;}
sub maybe2 { my ($Uv, $s) = @_;    (defined($Uv))  ?  ("$s$Uv")  :  () ;}
sub take_until { my ($f, $xs) = @_;
    my @res = ();
    foreach $a (@{$xs}) {
	if (&$f($a)) { return \@res }
	else { push @res, $a }
    }
    return \@res;
}



my $state = {};

my @data = ();
while(<STDIN>) {
    my $s = $_;
    push @data, $s;
}
my $i = 0;
map {

#while(<STDIN>) {
    my $s = $_;
    my @aspects = ();

    ####################################################
    # for the toc
    ####################################################
    if($s =~ /^\s*\\(part|chapter|section|subsection|subsubsection|paragraph)/)
      { push @aspects, "depth_title:" . ({part => 1, chapter => 2, section => 3, subsection => 4, subsubsection => 5, paragraph => 6}->{$1}) }
    if($s =~ /%(overview|intro|conclu|plan|transition):/)        { push @aspects, "depth_title:4"}

    ####################################################
    # synchro points,  the structure/hierarchy of a doc
    ####################################################
    # title:xxx, apres tout se rappele plus si c une section, subsection, ...
    #  could via *:.*xxx.*  mais supporte pas encore ca
    # note that need to keep it in state, and so synchrox:...

    if($s =~ /^\s*\\part\*?{(.*?)}/) {
	push @aspects, "synchro"; $state = {}; # so delete all state->{sub*section}
	$state->{part}  = "$1";
	push @aspects, "synchro1:part:$1::title:$1";
    }
    do { push @aspects, "synchro"; $state = {}; } if ($s =~ /^\s*\\end{document}/);
    do { push @aspects, "synchro"; $state = {}; } if ($s =~ /^%##############################################/);

    if($s =~ /^\s*\\chapter\*?{(.*?)}/) {
	$state->{chapter}  = "$1";
	delete $state->{section};
	delete $state->{subsection};
	delete $state->{subsubsection};
	push @aspects, "synchro2:chapter:$1::title:$1";
    }
    if($s =~ /^\s*\\section\*?{(.*?)}/) {
	$state->{section}  = "$1";
	delete $state->{subsection};
	delete $state->{subsubsection};
	push @aspects, "synchro3:section:$1::title:$1";
    }
    if ($s =~ /^\s*\\subsection\*?{(.*?)}/) {
	$state->{subsection}  = "$1";
	delete $state->{subsubsection};
	push @aspects, "synchro4:";
    }
    $state->{subsubsection}  = "$1"  if $s =~ /^\s*\\subsubsection\*?{(.*?)}/;


    ##########################################
    # paracontain (premier jet)
    ##########################################
    #TODO
    # use tag, => ls => overview:xxxxx,  cd overview:xxx et verra le paragraph
    # for some tags, if line empty then mean want propage prop to all para until next
    #  could use indentation too ?

    if($s =~ /^\s*$/) { delete $state->{index} }
    elsif(!defined($state->{index})) {
	my $para = take_until(sub { my ($e) = @_; (!defined($e)) ? 1 : ($e =~ /^\s*$/) }, [ @data[$i..$i+300] ]);
	my @aux = ();
	map { if($_ =~ /\\index{(.*?)}/) { push @aux, "index:$1" } } @{$para};
	map { if($_ =~ /\\idx{(.*?)}/) { push @aux, "index:$1" } } @{$para};
	$state->{index} = \@aux;
    }

    ##########################################
    while($s =~ /%(\w+):/g) { push @aspects, "note:$1" }
    if($s =~ /^\s*%%/)      { push @aspects, "note:toomuch" }
    if($s =~ /^\s*%/) {
	if($s =~ /%((##########)|(\+\+\+\+\+\+\+\+)|(----------)|(\s*$))/) { push @aspects, "comment:estethic" }
	else { push @aspects, "comment:base" }
    }


    if($s =~ /%(overview|((\w*)axis)|intro|conclu|plan|transition|idea|choice|def|notions):/)      # spirit ? notestructure
      { push @aspects, "aspect:structure"}
    if($s =~ /%(naive|interet|why|cool|example|ex):/)        { push @aspects, "aspect:structure"}
    if($s =~ /%(((\w*)later)|seenbefore|idea):/)  { push @aspects, "aspect:crossref"}
    if($s =~ /%todo\w*:/)        { push @aspects, "aspect:todo"}


    ##########################################
    if($s =~ /^\s*\\(setlength|documentclass|usepackage|newcommand|date|maketitle|addcontent|index)/)
      { push @aspects, "latex"}
    if($s =~ /^\s*\\(begin|end){document}/) { push @aspects, "latex"}
    if($s =~ /^\s*\\(label|ref|cite)/)
      { push @aspects, "aspect:crossref"}

    $state->{environment}{$1} = 1 if $s =~ /\\begin{(.*?)}/;

    #while($s =~ /\\index{(.*?)}/g) { push @aspects, "index:$1" }

    ##########################################
    my $res =
      [
       @aspects,
       maybe2($state->{part}, "part:"),
       maybe2($state->{chapter}, "chapter:"),
       maybe2($state->{section}, "section:"),
       maybe2($state->{subsection},"subsection:"),
       maybe2($state->{subsubsection},"subsubsection:"),
       maybe2($state->{part}, "title:"),
       maybe2($state->{chapter}, "title:"),
       maybe2($state->{section}, "title:"),
       maybe2($state->{subsection},"title:"),
       maybe2($state->{subsubsection},"title:"),

       (defined($state->{index}) ? @{$state->{index}} : ()),

       (map { "environment:$_" } keys(%{$state->{environment}})),
      ];
    delete $state->{environment}{$1} if $s =~ /\\end{(.*?)}/;

    print (join "/", @{$res}); print "\n";
  $i++;
}

@data;
