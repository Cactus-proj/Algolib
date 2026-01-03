#!/usr/bin/env perl5.10.0

# - je ne comprends pas indexpage
# - je crois que ce script déconne si les alias pour une page sont vides
#   (il reprend ceux de la page suivante)

my $lib = "lib/gfun.hdb";

while(my $filename=shift) {
  open(my $fh, "<", $filename);
  my $file = do { local $/; <$fh> };
  my @entry;
  while($file =~ /\\([A-Z]*)\{((?:[^{}]++|(?1))*)\}/gs) {
    my $value = $2;
    $value =~ s/\n//;
    $f{$1} = $value;
  }
  my $topic = $f{"TOPIC"};
  $topic =~ s/`//g;
  # utiliser plutôt pkgfunction, pkgfunctionitem ?
  my $browser = $f{"TOPIC"};
  $browser =~ s/gfun//g;
  $browser =~ s/[\[\]\/]//g;
  my $aliases = $f{"ALIAS"};
  $aliases =~ s/,/","/g;
  $aliases = "\"$aliases\"";
  my $mws = "help/$filename";
  $mws =~ s/i$/mws/;
  print "makehelp(\n  `$topic`,\n  `$mws`,\n  `$lib`,\n";
  print "  'aliases'=[$aliases],\n  'browser'=[cat(basedir, \"$browser\")]);\n";
}
