#!/usr/bin/perl


## Parameters

my $default_time_limit_seconds = 200;
my $default_memory_limit_mb = 3000;
my $working_dir = "__tmp_working_dir";
my $maple_command = "maple -b ~/mgfun/Mgfun.mla -B";
my $html_filename = "test_zeilberger.html";
my $css_filename = "style.css";
my $jsmath_filename = "load.js";
my $test_files_ext = "test";

my @arguments = (
    ["uncoupling_method=lexdeg"],
#    ["uncoupling_method=lexdeg", "bsearch"],
#    ["uncoupling_method=lexdeg", "bsearch", "min_order_solution"],
#    ["uncoupling_method=AZ"],
#    ["uncoupling_method=AZ", "bsearch"],
#    ["uncoupling_method=AZ", "bsearch", "min_order_solution"],
    );



## Libraries

use XML::LibXML;
use Getopt::Long;



## Global variables

my $html_doc = XML::LibXML->createDocument;
my $html_body = $html_doc->createElement("body");
my $html_table = $html_doc->createElement("table");

my $test_name = ".*";
my $time_limit_seconds = $default_time_limit_seconds;
my $memory_limit_mb = $default_memory_limit_mb;



## Parse arguments

GetOptions(
    "help" => \$print_help,
    "test_name=s" => \$test_name,
    "time_limit=i" => \$time_limit_seconds,
    "memory_limit=i" => \$memory_limit_mb,
    "silent" => \$silent,
    "profile" => \$profile,
    "html_filename=s" => \$html_filename,
    "latex" => \$latex,
    "log_file=s" => \$log_file
    );

my $pattern = $#ARGV >= 0 ? @ARGV[0] : ".*";
if($latex) { $silent=1; }


### Help

sub print_help {
    print "$0 --help\n";
    print "$0 [OPTIONS] [regexp]\n\n";
    print "\nOPTIONS:\n";
    print "  --test_name=regexp      run only tests whose name match regexp\n";
    print "  --time_limit=int        time limit in seconds\n";
    print "  --memory_limit=int      memory limit in Megabytes\n";
    print "  --profile               enable profiling\n";
    print "  --html_filename=string  html filename\n";
    print "  --silent                quiet mode\n";
    print "  --latex                 latex array output\n";
    print "  --log_file=string       log file\n\n";
    exit();
}



## Main

print_help() if $print_help;

unless(mkdir $working_dir) {
    warn "Directory $working_dir already exists.\n\n" unless $silent;
}

unless(chdir $working_dir) {
    die "Error while entering directory: $working_dir.\n";
}

if($log_file) {
    open LOG_FILE, ">../$log_file";
    print("Print logs in $log_file\n") unless $silent;
}
print("Time limit: ${time_limit_seconds}s\n") unless $silent;
print("Memory limit: ${memory_limit_mb}Mb\n\n") unless $silent;

create_html();

@files = glob "../tests/*.xml";
foreach $file(@files) {
    if($file =~ m/$pattern/) {
        print "Processing xml test file: $file\n" unless $silent;
        read_xml_file($file);
        print_line(80) unless $silent;
    }
}

gener_bottom_file();
print_html();

close LOG_FILE;



## 

sub print_line {
    for $i(1..@_[0]) {
        print "-";
    }
    print "\n";
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


sub create_test {
    my $xml_filename = "@_[2]" . ".xml";
    my $test_name = @_[1];
    my $dimension = "";

    print "  Running test $test_name\n" unless $silent;
    print LOG_FILE "@_[2], $test_name  " if($log_file);

    my $html_tr = $html_doc->createElement("tr");
    $html_table->addChild($html_tr);
    my ($function_expr, $tex, $var_int, %vars)
        = parse_xml(@_[0]);

    my @var_int = split ",", $var_int;
    my $td =  $html_doc->createElement("td");
    $html_tr->addChild($td);

    if(!$tex) {
	my $str = "$function_expr";
	for $i(0..$#var_int) {
	    my $v = $var_int[$i];
	    my $var_int_type = $vars{$v};
	    my $symbol_int = $var_int_type eq "shift" ? "sum" : "Int";
	    $str = "$symbol_int($str, $v)";
	}
	$tex = `echo \"latex($str);\"| maple -q`;
    }

    my $div = $html_doc->createElement("div");
    $div->setAttribute("class", "math");
    $div->appendText("$tex");
    $td->addChild($div);

    if($latex) {
	my $chomped_tex;
	chomp($chomped_tex = $tex);
	print("\$$chomped_tex\$ & ");
    }
    
    my $html_td_vars = $html_doc->createElement("td");
    $html_tr->addChild($html_td_vars);

    my $op_str = "";
    while(($v, $t) = each %vars) {
	unless(grep($_ eq $v, @var_int)) {
	    my $tex_var = length $v > 1 ? "\\$v" : "$v";
	    $op_str = $op_str ? "$op_str,$tex_var" : "$tex_var";
        }
    }
    $op_str = "${op_str}";
    my $div = $html_doc->createElement("div");
    $div->setAttribute("class", "math");
    $div->appendText("P($op_str)=");
    $html_td_vars->addChild($div);

    for $i(0..$#var_int) {
	my $vi = $var_int[$i];
	my $div = $html_doc->createElement("div");
	$div->setAttribute("class", "math");
	my $tex_var = length $vi > 1 ? "\\$vi" : "$vi";
	$op_str = "$op_str,$tex_var";
	my $Q = ($vars{$vi} eq "diff" ? "d" : "\\Delta") .
	    "_${tex_var}Q_{$i}($op_str)";
	$div->appendText($Q . ($i < $#var_int ? "+" : ""));
	$html_td_vars->addChild($div);
    }


    my $html_td_dim = $html_doc->createElement("td");
    $html_tr->addChild($html_td_dim);

    my %successfull_tests;
    my $order_sol = 3;

    foreach $arg(@arguments) {

        print "    @$arg\n" unless $silent;

        my (undef, $method) = split "=", @$arg[0];

        if($#{$arg} > 0 && (!$successfull_tests{$method} || $order_sol < 3)) {

            print "      skipped...\n\n" unless $silent;
            my $td_time =  $html_doc->createElement("td");
            $td_time->setAttribute("class", "not_run");
            $html_tr->addChild($td_time);
            my $td =  $html_doc->createElement("td");
            $td->setAttribute("class",  "not_run");
            $html_tr->addChild($td);

	    if($profile) {
		my $td_profile = $html_doc->createElement("td");
		$td_profile->setAttribute("class", "not_run");            
		$html_tr->addChild($td_profile);
	    }
        } else {
            
            my $args_gener_mpl = "";
            my $file_name_full = "@_[2]_@_[1]";
            
            for $a(@$arg) {
                $args_gener_mpl = "$args_gener_mpl --$a";
                my $aa = $a;
                $aa =~ s/uncoupling_method=//;
                $aa =~ s/min_order_solution/minorder/;
                $file_name_full = "${file_name_full}_$aa";
            }

            system("../gener_test.pl ../tests/$xml_filename --test_name=" .
                   "$test_name --silent --verbose=4 $args_gener_mpl");
            
            my $memory_limit_kb = 1000 * $memory_limit_mb;
            system("$maple_command -c 'infolevel[html_generation] := 1:' -t -T".
		   "$time_limit_seconds,$memory_limit_ko ".
                   "$file_name_full.$test_files_ext > $file_name_full.out 2> " .
                   "$file_name_full.time");
            
            my ($failure, $error_msg, $time, $memory, $last_d, $__dimension) = 
                analyse_result($file_name_full);
            
            $dimension = $__dimension if $__dimension;
            
            my $td_time =  $html_doc->createElement("td");
            $td_time->setAttribute("class", $failure ? "fail" : "time_success");
            $html_tr->addChild($td_time);
            

	    if($profile) {
		my $td_profile =  $html_doc->createElement("td");
		$td_profile->setAttribute("class", 
					  $failure ? "fail" : "pprofile");
		$html_tr->addChild($td_profile);
	    

		my $td_profile_link = $html_doc->createElement("td");
		$td_profile_link->setAttribute("class", $failure ? "fail" :
					       "profile_success");
		$html_tr->addChild($td_profile_link);
	    }

            if($failure) {
                print "      FAILED!!!!   $error_msg\n" unless $silent;
		print LOG_FILE "FAILED\n" if($log_file);
		$td_profile->appendText($error_msg) if($profile);
            }
            else {
		if(!$silent) {
		    print "      SUCCESSFULL!!!\n" unless $silent;
		    print "      Time: ${time}s\n" unless $silent;
		    print "      Memory: $memory\n" unless $silent;
		    print "      Order solution: $last_d\n" unless $silent;
		    print LOG_FILE "${time}s  $memory\n" if($log_file);
		}

		if($latex) {
		    print "$dimension & ";
		    print "$last_d & ";
		    print "${time}s / ${memory} \\\\ ";
		    print "\\hline\n";
		}

                $td_time->appendText("${time}s");
                for $b(1..2) {
                    my $br = $html_doc->createElement("br");
                    $td_time->addChild($br);
                }
                $td_time->appendText("$memory");
                for $b(1..2) {
                    my $br = $html_doc->createElement("br");
                    $td_time->addChild($br);
                }
                $td_time->appendText("$last_d");
                $successfull_tests{$method} = 1;
                $order_sol = $last_d;
                

                if($profile) {
                    my $a = $html_doc->createElement("a");
                    $a->appendText("Profile");
                    $a->setAttribute("href",
                                     "$working_dir/${file_name_full}_profi".
                                     "le.profile");
                    $td_profile_link->addChild($a);
                    
                    run_profile($file_name_full, $td_profile, $args_gener_mpl, 
                                $xml_filename, $test_name);
                }
            }
        }
        print_html();
    }

    print "\n" unless $silent;
    $html_td_dim->appendText("$dimension") if $dimension;
    print_html();
}


sub run_profile {
    my $file_name_full = @_[0];
    my $td_profile = @_[1];
    my $args_gener_mpl = @_[2];
    my $xml_filename = @_[3];
    my $test_name = @_[4];

    system("../gener_test.pl ../tests/$xml_filename --test_name=$test_name".
           " --silent --prof $args_gener_mpl");

    print "      Running profile...\n" unless $silent;
    system("$maple_command -q ${file_name_full}_profile.$test_files_ext");

    print "      Analysing results...\n\n" unless $silent;
    system("nprofile -callgraph < /tmp/profile > ${file_name_full}_profile.".
	   "profile");
    
    my $table_profile =  $html_doc->createElement("table");
    $td_profile->addChild($table_profile);
    $table_profile->setAttribute("class", "profile");
    $table_profile->setAttribute("width", "100%");
    $table_profile->setAttribute("height", "100%");
    
    open OUTPUT, "<${file_name_full}_profile.profile";
                    
    while(<OUTPUT>) {
        my $txt = $_;
        @prof_func = split " ", $txt;
        for ($j = 0; $j <= $#prof_func; $j++) {
            @prof_func[$j] =~ s/,//;
        }
        
        if(@prof_func[5] =~ "creative_telescoping") {
            $time_profile = @prof_func[1];
        }
        elsif(@prof_func[5] =~ "Zeilberger" ||
              @prof_func[5] =~ "Mgfun:-MG_Internals:-dfinite_expr_to_sys" ||
              @prof_func[5] =~ "LOE_sys_rat_" ||
              @prof_func[5] =~ "LOE_rat" ||
              @prof_func[5] =~ "LOE_poly" ||
              @prof_func[5] =~ "degree_upper_bound" ||
              (@prof_func[3] =~ "skew_poly_creative_telescoping" &&
               @prof_func[5] eq "Groebner:-Basis") ||
              @prof_func[5] =~ "denom_solution" ||
              @prof_func[5] =~ "uncouple_system_" ||
              @prof_func[5] =~ "test_order"
            ) {
            my $tr = $html_doc->createElement("tr");
            
            my $time_sec = $time_profile == 0 ? "42" :
                sprintf("%.2f%%", 100. * @prof_func[1] / $time_profile);
            
            $caller = @prof_func[3];
            $caller =~ s/Mgfun:-//;
            $called = @prof_func[5];
            $called =~ s/Mgfun:-//;
            $called =~ s/MG_Internals:-//;
            
            foreach $i("$caller -> $called", @prof_func[0], $time_sec) {
                my $td =  $html_doc->createElement("td");
                $td->appendText($i);
                $td->setAttribute("class", "profile");
                $tr->addChild($td);
            }
            
            $table_profile->addChild($tr);
        }
    }
}

sub analyse_result {
    my $file_name_full = @_[0];

    open OUTPUT, "<$file_name_full.out";

    my $failure = 1;
    my $error_msg = "Wrong answer.";
    my $time;
    my $memory;
    my $dimension = "";
    my $last_d;

    while(<OUTPUT>)
    {
        if(/PROFILE/) {
            $txt = $_;
            $txt =~ s/[^:]*:   \"PROFILE - //;
            $txt =~ s/\"  //;
            @args = split " ", $txt;
            $param = @args[0];
            $val = @args[1];

            if($param eq "LAST_D") {
                $last_d = $val;
            }
            elsif($param eq "DIMENSION") {
                $dimension = ($dimension ? "$dimension, " : "") . "$val";
            }
            else {
                die "Unknown PROFILE field in maple output: $param";
            }
        }
        if(/okay/) {
            $failure = 0;
        }                    
        if(/# TIME\=[^%]/) {
            split "=";
            $time = @_[1];
            $time =~ s/\n//;
        }
        if(/Cannot allocate memory/) {
            $error_msg =  "Memory limit exceded.";
        }
    }
    
    close OUTPUT;

    open TIME, "<$file_name_full.time";
    
    while(<TIME>)
    {
        if(/Execution stopped: CPU time limit reached./) {
            $error_msg = "Time limit exceded.";
        }
        elsif($_ =~ /out of memory/ || $_ =~ /Memory allocation failed./) {
            $error_msg =  "Memory limit exceded.";
        }
        else {
            $txt = $_;
            $txt =~ s/=/ /g;
            $txt =~ s/,/ /g;
            @tab = split " ", $txt;
            $mem = @tab[4];
            $memory =  sprintf("%.02lfMb", $mem / 1e6);
        }
    }

    return ($failure, $error_msg, $time, $memory, $last_d, $dimension);
}


sub parse_xml {
    my $function_expr = "", $tex = "", $var_int;
    my %vars;
    foreach $child(@_[0]->childNodes())
    {        
        $element_name = $child->getName();
        my @attributes = $child->getAttributes();
        my %args;

        foreach $at(@attributes) {
            $args{$at->getName()} = $at->getValue();
        }

        if($element_name eq "fun") {
            foreach $f($child->childNodes()) {
                if($f->getName() eq "#text" || $f->getName() eq "text") {
                    $function_expr = $f->getValue();
                }
            }
        }

        elsif($element_name eq "tex") {
            foreach $f($child->childNodes()) {
                if($f->getName() eq "#text" || $f->getName() eq "text") {
                    $tex = $f->getValue();
                }
            }
        }

        elsif($element_name eq "var") {
            $vars{$args{"name"}} = $args{"type"};
        }

        elsif($element_name eq "var_int") {
            $var_int = $args{"name"};
        }

    }

    return ($function_expr, $tex, $var_int, %vars);
}


## Processing html file

sub create_html {
    my $html_root = $html_doc->createElement("html");
    $html_doc->setDocumentElement($html_root);

    my $html_head = $html_doc->createElement("head");
    $html_root->addChild($html_head);

    $html_root->addChild($html_body);


    ## Header

    my $html_title = $html_doc->createElement("title");
    $html_title->appendText("Zeilberger");
    $html_head->addChild($html_title);

    my $css = $html_doc->createElement("link");
    $css->setAttribute("rel","stylesheet");
    $css->setAttribute("type","text/css");
    $css->setAttribute("href", "$css_filename");
    $html_head->addChild($css);

    my $jsmath = $html_doc->createElement("script");
    $jsmath->setAttribute("src", "$jsmath_filename");
    $html_head->addChild($jsmath);


    ## Table

    $html_body->addChild($html_table);
    $html_table->setAttribute("class", "main");
    
    my $html_title_row = $html_doc->createElement("tr");
    $html_table->addChild($html_title_row);
    foreach $t("Integral \\ sum", "Operators", "Dim.") {
        my $html_th = $html_doc->createElement("th");
        $html_th->appendText($t);
        $html_title_row->addChild($html_th);
    }

    for $i (0 .. $#arguments)
    {
	my $html_th = $html_doc->createElement("th");
	$html_title_row->addChild($html_th);
	foreach $title("Time", "Memory", "Order") {
	    $html_th->appendText($title);
	    my $html_br = $html_doc->createElement("br");
	    $html_th->addChild($html_br);
	}
	
	if($profile) {
	    my $html_th = $html_doc->createElement("th");
	    $html_title_row->addChild($html_th);
	    for $j (0..$#{$arguments[$i]}) {
		$html_th->appendText($arguments[$i][$j]);
		my $html_br = $html_doc->createElement("br");
		$html_th->addChild($html_br);
	    }
	    my $html_th = $html_doc->createElement("th");
	    $html_title_row->addChild($html_th);
	    $html_th->appendText("Profile");
	}
    }
}


sub gener_bottom_file {
    ($sec, $min, $hour, $mday, $month, $year) = localtime;
    my $date = sprintf("%d-%02d-%02d %02d:%02d:%02d\n", $year+1900, $month+1,
                       $mday, $hour, $min, $sec);
    my $hostname = `hostname`;
    my $time_limit_str = "Time limit: ${time_limit_seconds}s";
    my $mem_limit_str = "Memory limit: ${memory_limit_mb}Mb";

    if($log_file) {
	print LOG_FILE "\n$date";
	print LOG_FILE "$hostname";
	print LOG_FILE "$time_limit_str\n";
	print LOG_FILE "$mem_limit_str\n";
    }

    my $div = $html_doc->createElement("div");    
    add_br($div, 2);
    $div->appendText($date);
    add_br($div);
    $div->appendText($hostname);
    add_br($div, 2);
    $div->appendText($time_limit_str);
    add_br($div);
    $div->appendText($mem_limit_str);
    $html_body->addChild($div);
}


sub add_br {
    my $nb = @_[1] ? @_[1] : 1;
    for $i(1..$nb) {
        my $br = $html_doc->createElement("br");
        @_[0]->addChild($br);
    }
}


sub print_html {
    open HTML_FILE, ">../$html_filename";
    print HTML_FILE $html_doc->toStringHTML();
    close HTML_FILE;
}
