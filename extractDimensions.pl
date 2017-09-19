$file_path = $ARGV[0]; 
$out_file1  = 'refined_text_data1.txt';
$out_file2  = 'refined_text_data2.txt';
$out_file3  = 'refined_text_data3.txt';
$out_final  = 'final_text_data.txt';

$cm_to_inch = 0.393701;


open( FH, $file_path ) or die "Cannot open file";
open( OUT_FH1, "> $out_file1" ) or die "Cannot open out file1";
open( OUT_FH2, "> $out_file2" ) or die "Cannot open out file2";
open( OUT_FH3, "> $out_file3" ) or die "Cannot open out file3";
open( OUT_FINAL, "> $out_final" ) or die "Cannot open out final";

# Add all regular expressions to an array.
push @regex_array, "([0-9]) *cm *x *([0-9]) *cm";


#*******************************************************
# STEP1 : Cleanup
#*******************************************************
while( $line = <FH> ) {
  chomp( $line );
  
  # First, make sure that we get MAX only 1 "mycusteol" per line.
  $line =~ s/~//g;
  $line =~ s/mycusteol/ mycusteol\n/g;

  print OUT_FH1 "$line\n";
}

close( OUT_FH1 );

#*******************************************************
# STEP2 : More Cleanup
#*******************************************************
# Open OUT_FH1 for reading
open( OUT_FH1, $out_file1 ) or die "Cannot open out file1 for reading";
# Start from the second row.
while( $line = <OUT_FH1> ) {
  chomp( $line );

  # Replace newline by "space"
  $line =~ s/\n/ /g;
  $line =~ s/\t/ /g;

  $line =~ s/mycustdelim/ mycustdelim \n/g;

  print OUT_FH2 "$line\n";
}

close( OUT_FH1 );
close( OUT_FH2 );


#*******************************************************
# STEP3: Some more cleanup so that we collect all data 
#        associated with a given listing ID on 1 row.
#        This makes it easier for later stages.
#*******************************************************
open( OUT_FH2, $out_file2 ) or die "Cannot open out file2 for reading";

while( $line = <OUT_FH2> ) {
  if( $line =~ / mycusteol/ ) {
    $line =~ s/mycusteol//g;
    print OUT_FH3 $line;
  } else {
    $line =~ s/\n/ /g;
    print OUT_FH3 $line;
  }
}

close( OUT_FH3 );

#*******************************************************
# STEP4: 
#       
#      
#*******************************************************

$line_num = 1;
$matches = 0;
open( OUT_FH3, $out_file3 ) or die "Cannot open out file3 for reading";

# ignore first line
$line = <OUT_FH3>;

while( $line = <OUT_FH3> ){
  chomp $line;
  @columns = split( /mycustdelim/, $line );
  $found = 0;
  shift @columns;
  
  # Column0 : Listing ID
  # Column1 : Title
  # Column2 : Description
  # Column3 : Length
  # Column4 : Height
  # Column5 : Width

  $array_size = @columns;
  if( $array_size != 6) {
    die "Array size is not 6. Error detected for lineNumber=$line_num";
  }

  if( $line !~ /^"[0-9]+/ ) {
    $line_num++;
    next;
  }
  
  if( ( $columns[3] =~ /NA/ ) 
    ||( $columns[4] =~ /NA/ )
    )
  {


    if( ( !$found ) && ( $columns[2] =~ /([0-9,\.]+) *inch *\* *([0-9,\.]+) *inch/i ) ) { # 25.1inch * 35.0 inch
      if( $1 > $2 ) {
        $len = $1;
        $bre = $2;
      } else {
        $len = $2;
        $bre = $1;
      }
      print "LineNum: $line_num, Listing ID: $columns[0], Length=$len, Breadth=$bre inch\n";
      $columns[3] = $len;
      $columns[4] = $bre;
      $matches++;
      $found = 1;
    } 

    if( ( !$found ) && ( $columns[2] =~ /([0-9,\.]+) *inch *x *([0-9,\.]+) *inch/i ) ) { # 25.1inch x 35.0 inch
      if( $1 > $2 ) {
        $len = $1;
        $bre = $2;
      } else {
        $len = $2;
        $bre = $1;
      }
      print "LineNum: $line_num, Listing ID: $columns[0], Length=$len, Breadth=$bre inch\n";
      $columns[3] = $len;
      $columns[4] = $bre;
      $matches++;
      $found = 1;
    } 

    if( ( !$found ) && ( $columns[2] =~ /([0-9,\.]+) *\* *([0-9,\.]+) *inch/i ) ) { # 25.1 * 35.0 inch
      if( $1 > $2 ) {
        $len = $1;
        $bre = $2;
      } else {
        $len = $2;
        $bre = $1;
      }
      print "LineNum: $line_num, Listing ID: $columns[0], Length=$len, Breadth=$bre inch\n";
      $columns[3] = $len;
      $columns[4] = $bre;
      $matches++;
      $found = 1;
    } 

    if( ( !$found ) && ( $columns[2] =~ /([0-9,\.]+) *x *([0-9,\.]+) *inch/i ) ) { # 25.1 x 35.0 inch
      if( $1 > $2 ) {
        $len = $1;
        $bre = $2;
      } else {
        $len = $2;
        $bre = $1;
      }
      print "LineNum: $line_num, Listing ID: $columns[0], Length=$len, Breadth=$bre inch\n";
      $columns[3] = $len;
      $columns[4] = $bre;
      $matches++;
      $found = 1;
    } 

    if( ( !$found ) && ( $columns[2] =~ /([0-9,\.]+) *\\ *([0-9,\.]+) *inch/i ) ) { # 25.1 \ 35.0 inch
      if( $1 > $2 ) {
        $len = $1;
        $bre = $2;
      } else {
        $len = $2;
        $bre = $1;
      }
      print "LineNum: $line_num, Listing ID: $columns[0], Length=$len, Breadth=$bre inch\n";
      $columns[3] = $len;
      $columns[4] = $bre;
      $matches++;
      $found = 1;
    } 

    if( ( !$found ) && ( $columns[2] =~ /([0-9,\.]+) *&quot; *x *([0-9,\.]+) *&quot;/i ) ) { # 25.1&quot; X 35.0&quot; 
      if( $1 > $2 ) {
        $len = $1;
        $bre = $2;
      } else {
        $len = $2;
        $bre = $1;
      }
      print "LineNum: $line_num, Listing ID: $columns[0], Length=$len, Breadth=$bre quot; inch\n";
      $columns[3] = $len;
      $columns[4] = $bre;
      $matches++;
      $found = 1;
    } 

    if( ( !$found ) && ( $columns[2] =~ /([0-9,\.]+) *cm *x *([0-9,\.]+) *cm *x *([0-9\.]+) *cm/i ) ) { # 25.1 cm x 35.0cm x 100.8cm
      if( $1 > $2 ) {
        $len = $1 * $cm_to_inch;
        $bre = $2 * $cm_to_inch;
      } else {
        $len = $2 * $cm_to_inch;
        $bre = $1 * $cm_to_inch;
      }
      print "LineNum: $line_num, Listing ID: $columns[0], Length=$len, Breadth=$bre  Height=$3 inch\n";
      $columns[3] = $len;
      $columns[4] = $bre;
      $matches++;
      $found = 1;
    } 

    if( ( !$found ) && ( $columns[2] =~ /([0-9,\.]+) *x *([0-9,\.]+) *cm/i ) ) { #25.1 x 35.0 cm
      if( $1 > $2 ) {
        $len = $1 * $cm_to_inch;
        $bre = $2 * $cm_to_inch;
      } else {
        $len = $2 * $cm_to_inch;
        $bre = $1 * $cm_to_inch;
      }
      print "LineNum: $line_num, Listing ID: $columns[0], Length=$len, Breadth=$bre inch\n";
      $columns[3] = $len;
      $columns[4] = $bre;
      $matches++;
      $found = 1;
    } 
  
    if( ( !$found ) && ( $columns[2] =~ /([0-9,\.]+) *cm *x *([0-9,\.]+) *cm/i ) ) { # 25.1cm x 35.0 cm
      if( $1 > $2 ) {
        $len = $1 * $cm_to_inch;
        $bre = $2 * $cm_to_inch;
      } else {
        $len = $2 * $cm_to_inch;
        $bre = $1 * $cm_to_inch;
      }
      print "LineNum: $line_num, Listing ID: $columns[0], Length=$len, Breadth=$bre inch\n";
      $columns[3] = $len;
      $columns[4] = $bre;
      $matches++;
      $found = 1;
    } 
  
    if( ( !$found ) && ( $columns[2] =~ /([0-9,\.]+) *\* *([0-9,\.]+) *cm/i ) ) { #25.1 * 35.0 cm
      if( $1 > $2 ) {
        $len = $1 * $cm_to_inch;
        $bre = $2 * $cm_to_inch;
      } else {
        $len = $2 * $cm_to_inch;
        $bre = $1 * $cm_to_inch;
      }
      print "LineNum: $line_num, Listing ID: $columns[0], Length=$len, Breadth=$bre inch\n";
      $columns[3] = $len;
      $columns[4] = $bre;
      $matches++;
      $found = 1;
    } 
  
    if( ( !$found ) && ( $columns[2] =~ /([0-9,\.]+) *cm *\* *([0-9,\.]+) *cm/i ) ) { # 25.1cm * 35.0 cm
      if( $1 > $2 ) {
        $len = $1 * $cm_to_inch;
        $bre = $2 * $cm_to_inch;
      } else {
        $len = $2 * $cm_to_inch;
        $bre = $1 * $cm_to_inch;
      }
      print "LineNum: $line_num, Listing ID: $columns[0], Length=$len, Breadth=$bre inch\n";
      $columns[3] = $len;
      $columns[4] = $bre;
      $matches++;
      $found = 1;
    } 

    if( ( !$found ) && ( $columns[2] =~ /([0-9,\.]+) *\\ *([0-9,\.]+) *cm/i ) ) { # 25.1 \ 35.0 cm
      if( $1 > $2 ) {
        $len = $1 * $cm_to_inch;
        $bre = $2 * $cm_to_inch;
      } else {
        $len = $2 * $cm_to_inch;
        $bre = $1 * $cm_to_inch;
      }
      print "LineNum: $line_num, Listing ID: $columns[0], Length=$len, Breadth=$bre inch\n";
      $columns[3] = $len;
      $columns[4] = $bre;
      $matches++;
      $found = 1;
    } 
  } else {
    $matches++;
  }
  
  if( $columns[3] !~ /\"/ ) {
    $columns[3] =~ s/ //g;
    $columns[3] =~ s/\t//g;
    #$columns[3] = "\"".$columns[3]."\"";
  }
  
  if( $columns[4] !~ /\"/ ) {
    $columns[4] =~ s/ //g;
    $columns[4] =~ s/\t//g;
    #$columns[4] = "\"".$columns[4]."\"";
  }
  if( $columns[5] !~ /\"/ ) {
    $columns[5] =~ s/ //g;
    $columns[5] =~ s/\t//g;
    #$columns[5] = "\"".$columns[5]."\"";
  }
  
  print OUT_FINAL "$columns[0] , $columns[3] , $columns[4] , $columns[5]\n"; 
  
  print OUT_FINAL "\n";


  $line_num = $line_num + 1;
}

print "Matches: $matches\n";

 
