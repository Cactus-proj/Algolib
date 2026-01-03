#!/usr/bin/perl

use Getopt::Long;
use XML::LibXML;


## Parameters

my $test_files_ext = "test";




GetOptions(
    "help" => \$print_help,
    "test_name=s" => \$test_name,
    "file_name=s" => \$custom_filename,
    "profile!" => \$profile,
    "d_min=i" => \$d_min,
    "d_max=i" => \$d_max,
    "assert_level=i" => \$assert_level,
    "verbose:i" => \$verbose_level,
    "uncoupling_method=s" => \$uncoupling_method,
    "bsearch" => \$bsearch,
    "min_order_solution" => \$min_order_solution,
    "silent" => \$silent,
    "tools_path=s" => \$tools_path,
    );

sub print_help {
    print "$0 --help\n";
    print "$0 testfile.xml [testfile2.xml [testfile3.xml ...]]\n";
    print "\nOPTIONS:\n";
    print "--test_name=string\n";
    exit();
}

print_help() if $print_help;

$test_name = ".*" if !$test_name;
$tools_path = ".." if !$tools_path;

my $nb_tests = 0;

foreach $xml_test_file(@ARGV) {
    print "Processing xml test file: $xml_test_file\n" unless $silent;
    read_xml_file($xml_test_file);
    print "\n" unless $silent;
}


sub read_xml_file {
    my $xml_parser = new XML::LibXML;
    my $xml_test_file = @_[0];
    my $xml_doc = $xml_parser->parse_file($xml_test_file);
    my $xml_root = $xml_doc->getDocumentElement();
    my $xml_root_name = $xml_root->getName();
    unless($xml_root_name eq "tests") {
        die "xml root should be \"tests\", but $xml_test_file has root \"" .
            "$xml_root_name\"";
    }

    $xml_test_file =~ /([^\/]*)\.xml/;
    $short_file_name = $1;

    foreach $xml_element_test($xml_root->childNodes()) {
        if($xml_element_test->nodeType == XML_ELEMENT_NODE) {
            my $xml_element_name = $xml_element_test->getName();
            unless($xml_element_name eq "test") {
                die "xml element should be \"test\", but received \"" .
                    "$xml_element_name\"";
            }
            @attributes = $xml_element_test->getAttributes();
            my %test_args;
            foreach $at(@attributes) {
                my $attribute_name = $at->getName();
                $test_args{$attribute_name} = $at->getValue();
                
                unless ($at->getName() eq "name") {
                    warn ("Unknown attribute $attribute_name in element test i".
                          "n file $xml_test_file");
                }
            }
            
            unless (defined $test_args{"name"}) {
                die "Attribute name required in element test in file ".
                    "$xml_test_file";
            }

            if(@test_args{"name"} =~ m/$test_name/) {
                create_test($xml_element_test, @test_args{"name"},
                            $short_file_name);
            }
        }
    }
}


sub gener_args_test {
    return
        ($d_min ? ", d_min = $d_min" : "") .
        ($d_max ? ", d_max = $d_max" : "") .
        ($uncoupling_method ? ", uncoupling_method = $uncoupling_method" : "") .
        ($bsearch ? ", bsearch" : "") .
        ($min_order_solution ? ", min_order_solution" : "");
}


sub read_param_test {
    my $xml_element_test = @_[0];
    my $function_expr = @_[1];
    my $var_int = @_[2];
    my $vars = @_[3];
    my $comm_vars = @_[4];
    my $set_vals = @_[5];
    my $options = @_[6];

    foreach $child($xml_element_test->childNodes())
    {
        my $element_name = $child->getName();
        my @attributes = $child->getAttributes();

        if(!($element_name eq "#text" || $element_name eq "#comment" || $element_name eq "text" || $element_name eq "comment")) {

            my $name, $type;
            my %args;
            
            foreach $at (@attributes) {
                $args{$at->getName()} = $at->getValue();
            }
            
            if($element_name eq "fun") {
                foreach $f($child->childNodes()) {
                    if($f->getName() eq "#text" || $f->getName() eq "text") {
                        $$function_expr = $f->getValue();
                    }
                }
            }

            elsif($element_name eq "var_int") {
                $$var_int = $args{"name"};
            }

            elsif($element_name eq "set_option") {
                push @$options, $args{"value"};
            }

            elsif($element_name eq "var") {
                @$vars{$args{"name"}} = $args{"type"};
            }
            elsif($element_name eq "set_val") {
                push @$set_vals, [$args{"name"}, $args{"type"}, $args{"inter"}];
            }
            elsif(!($element_name eq "tex")) {
                warn "Unknown element name $element_name.";
            }
        }
    }
}


sub create_test {
    my $file_name_full;

    if($custom_filename) {
        $file_name_full = "$custom_filename" . ($nb_tests ? "_$nb_tests" : "");
        $nb_tests++;        
    }

    else {
        $file_name_full = "@_[2]_@_[1]" .
            ($uncoupling_method ? "_$uncoupling_method" : "") .
            ($bsearch ? "_bsearch" : "") .
            ($min_order_solution ? "_minorder" : "") .
            ($profile ? "_profile" : "");
    }

    $file_name_full = "$file_name_full.$test_files_ext";
    print "  Generating test file: $file_name_full\n" unless $silent;

    my $args_test = gener_args_test();
    my $function_expr;
    my $var_int;
    my %vars;
    my @comm_vars;
    my @set_vals;
    my @options;
    read_param_test(@_[0], \$function_expr, \$var_int, \%vars, \@comm_vars,
                    \@set_vals, \@options);


    my @var_int = split ",", $var_int;
    
    my $typed_var_int = "[";
    foreach $vi(@var_int) {
	$typed_var_int = "${typed_var_int}," if(!($typed_var_int eq "["));
	$typed_var_int = "${typed_var_int}${vi}::$vars{$vi}";
    }
    $typed_var_int = "${typed_var_int}]";

    my $vars_list = "[";
    while(($v, $t) = each %vars) {
	unless(grep($_ eq $v, @var_int)) {
            $vars_list = $vars_list.($vars_list eq "[" ? "" : ", ")."${v}::$t";
        }
    }
    $vars_list = "$vars_list]";

    print "    Function = $function_expr\n" unless $silent;

    open MAPLE_INPUT, ">$file_name_full";

    if($profile) {
        print MAPLE_INPUT "writeto(\"/tmp/profile\"):\n";
        print MAPLE_INPUT "kernelopts(profile = true):\n";
    } else {
        print MAPLE_INPUT "read(\"$tools_path/creative_telescoping_tools.mpl" .
            "\"):\n";
    }

    print MAPLE_INPUT  "infolevel[CreativeTelescoping] := $verbose_level:\n" if
        $verbose_level;
    print MAPLE_INPUT  "f := $function_expr:\n";

    print MAPLE_INPUT "TIME := time():\n" if !$profile;
    print MAPLE_INPUT "res := Mgfun:-creative_telescoping(f, $vars_list"
        . ", $typed_var_int";

    if($#options >= 0) {
        print MAPLE_INPUT ", [";
        foreach $i (0 .. $#options) {
            print MAPLE_INPUT ($i == 0 ? "" : ", ");
            print MAPLE_INPUT $options[$i];
        }
        print MAPLE_INPUT "]";
    }

    print MAPLE_INPUT "):\n";
    
    if(!$profile) {
        print MAPLE_INPUT "printf(\"# TIME=%.3f\\n\", time()-TIME):\n";
        print MAPLE_INPUT "check_ct(f, $vars_list, $typed_var_int, res";
        
        if($#set_vals >= 0) {
            print MAPLE_INPUT ", [";
            
            foreach $i (0 .. $#set_vals) {
                print MAPLE_INPUT ($i == 0 ? "" : ", ");
                print MAPLE_INPUT "[$set_vals[$i][0]::$set_vals[$i][1]";
                if(defined($set_vals[$i][2])) {
                    print MAPLE_INPUT ", $set_vals[$i][2]";
                }            
                print MAPLE_INPUT "]";
            }   
            print MAPLE_INPUT "]";
        }
        print MAPLE_INPUT ");\n";
    }
    
    close MAPLE_INPUT;
}
