# PERL script to get ART TYPE

$file_path = $ARGV[0];
$out_final  = 'art_style_final_text_data.txt';
$out_file1  = 'art_style_refined_text_data1.txt';
$out_file2  = 'art_style_refined_text_data2.txt';
$out_file3  = 'art_style_refined_text_data3.txt';
$out_file4  = 'art_style_refined_text_data4.txt';
$out_file5  = 'art_style_refined_text_data5.txt';
$out_file6  = 'art_style_refined_text_data6.txt';
$out_file7  = 'art_style_refined_text_data7.txt';
$out_file8  = 'art_style_refined_text_data8.txt';

open( FH, $file_path ) or die "Cannot open file";
open( OUT_FINAL, "> $out_final" ) or die "Cannot open out final";
open( OUT_FH1, "> $out_file1" ) or die "Cannot open out file1";
open( OUT_FH2, "> $out_file2" ) or die "Cannot open out file2";
open( OUT_FH3, "> $out_file3" ) or die "Cannot open out file3";
open( OUT_FH4, "> $out_file4" ) or die "Cannot open out file4";
open( OUT_FH5, "> $out_file5" ) or die "Cannot open out file5";
open( OUT_FH6, "> $out_file6" ) or die "Cannot open out file6";
open( OUT_FH7, "> $out_file7" ) or die "Cannot open out file7";
open( OUT_FH8, "> $out_file8" ) or die "Cannot open out file8";

#*******************************************************
# STEP1 : Cleanup
#*******************************************************
while( $line = <FH> ) {
  chomp( $line );
  
  # Replace fancy characters with spaces
  $line =~ s/#/ /g;
  $line =~ s/~/ /g;

  # First, make sure that we get MAX only 1 "mycusteol" per line.
  $line =~ s/mycusteol/ ######## mycusteol\n/g;

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

  # Replace "mycustdelim" by "########" -> it is easier to identify using my eyes.
  $line =~ s/mycustdelim/ ######## \n/g;

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
  if( $line =~ / ######## mycusteol/ ) {
    print OUT_FH3 $line;
  } else {
    $line =~ s/\n/ /g;
    print OUT_FH3 $line;
  }
}

close( OUT_FH3 );

#*******************************************************
# STEP4: Some more cleanup so that we collect all data 
#        associated with a given listing ID on 1 row.
#        This makes it easier for later stages.
#*******************************************************
open( OUT_FH3, $out_file3 ) or die "Cannot open out file3 for reading";

# Ignore first line
$line = <OUT_FH3>;
while( $line = <OUT_FH3> ) {
		@columns = split( /########/, $line );

		# Remove the serial number column
		shift @columns;

		# Print only required columns to new file
		# Column 0 : "listing_id" ######## 
		# Column 1 : "user_id" ######## 
		# Column 2 : "title" ######## 
		# Column 3 : "description" ######## 
		# Column 4 : "creation_tsz" ######## 
		# Column 5 : "ending_tsz" ######## 
		# Column 6 : "original_creation_tsz" ######## 
		# Column 7 : "last_modified_tsz" ######## 
		# Column 8 : "price" ######## 
		# Column 9 : "quantity" ######## 
		# Column 10 : "shop_section_id" ######## 
		# Column 11 : "featured_rank" ######## 
		# Column 12 : "state_tsz" ######## 
		# Column 13 : "url" ######## 
		# Column 14 : "views" ######## 
		# Column 15 : "num_favorers" ######## 
		# Column 16 : "shipping_template_id" ######## 
		# Column 17 : "processing_min" ######## 
		# Column 18 : "processing_max" ######## 
		# Column 19 : "who_made" ######## 
		# Column 20 : "is_supply" ######## 
		# Column 21 : "when_made" ######## 
		# Column 22 : "item_weight" ######## 
		# Column 23 : "item_weight_units" ######## 
		# Column 24 : "item_length" ######## 
		# Column 25 : "item_width" ######## 
		# Column 26 : "item_height" ######## 
		# Column 27 : "item_dimensions_unit" ######## 
		# Column 28 : "is_private" ######## 
		# Column 29 : "recipient" ######## 
		# Column 30 : "occasion" ######## 
		# Column 31 : "non_taxable" ######## 
		# Column 32 : "is_customizable" ######## 
		# Column 33 : "is_digital" ######## 
		# Column 34 : "file_data" ######## 
		# Column 35 : "has_variations" ######## 
		# Column 36 : "taxonomy_id" ######## 
		# Column 37 : "used_manufacturer" ######## 
		# Column 38 : "suggested_taxonomy_id" ######## 
		# Column 39 : "tag_col" ######## 
		# Column 40 : "mat_col" ######## 
		# Column 41 : "sty" ######## 
		# Column 42 : "tax_col" ######## 
		print OUT_FH4 "$columns[0] ######## $columns[2] ######## $columns[3] ######## $columns[39] ######## $columns[40] ######## $columns[41] ######## $columns[42]\n";
}

close( OUT_FH4 );

#*******************************************************
# STEP5: ABSTRACTS
#*******************************************************
open( OUT_FH4, $out_file4 ) or die "Cannot open out file4 for reading";

$abstract_match = 0;
$landscape_match = 0;
$portrait_match = 0;
$fantasy_match = 0;
$realism_match = 0;
$animal_match = 0;
$nautical_match = 0;
$traditional_match = 0;
$folk_match = 0;
$unclassified_match = 0;

while( $line = <OUT_FH4> ) {
	$found = 0;
	@columns = split( /########/, $line );

  # Column0 : Listing ID
  # Column1 : Title
  # Column2 : Description
  # Column3 : tag
  # Column4 : mat
  # Column5 : sty
	# Column6 : taxonomy
  if( ( $columns[5] =~ /abstra/i )
 	  ||(	$columns[5] =~ /modern/i )
 	  ||(	$columns[5] =~ /impressioni/i )
		)
	{
		$columns[5] = "abstract";
		$abstract_match++;
		$found = 1;
  } 


  if( ( ! $found )
		&&($columns[5] =~ /landsca/i ) 
		)
	{
		$columns[5] = "landscape";
		$landscape_match++;
		$found = 1;
  } 

  if( ( ! $found )
		&&( $columns[5] =~ /portrait/i ) 
		)
	{
		$columns[5] = "portrait";
		$portrait_match++;
		$found = 1;
  } 

  if( ( ! $found )
		&&( ( $columns[5] =~ /fantasy/i ) 
			||( $columns[5] =~ /boho/i )
			)
		)
	{
		$columns[5] = "fantasy";
		$fantasy_match++;
		$found = 1;
  } 

  if( ( ! $found )
		&&( $columns[5] =~ /realis/i ) 
		)
	{
		$columns[5] = "realism";
		$realism_match++;
		$found = 1;
  } 

  if( ( ! $found )
		&&( ( $columns[5] =~ /animal/i ) 
			||( $columns[5] =~ /dog/i )
			||( $columns[5] =~ /cat/i )
			||( $columns[5] =~ /horse/i )
			)
		)
	{
		$columns[5] = "animal";
		$animal_match++;
		$found = 1;
  } 

  if( ( ! $found )
		&&( ( $columns[5] =~ /nautical/i ) 
			||( $columns[5] =~ /ocean/i )
			||( $columns[5] =~ /beach/i )
			)
		)
	{
		$columns[5] = "animal";
		$nautical_match++;
		$found = 1;
  } 

  if( ( ! $found )
		&&( ( $columns[5] =~ /folk/i ) 
			||( $columns[5] =~ /african/i ) 
			||( $columns[5] =~ /asian/i ) 
			)
		)
	{
		$columns[5] = "folk";
		$folk_match++;
		$found = 1;
  } 

  if( ( ! $found )
		&&( $columns[5] =~ /traditional/i ) 
		)
	{
		$columns[5] = "traditional";
		$traditional_match++;
		$found = 1;
  } 

  #=========================================================================================
	# Search Title and description if we did not find anything in the "STY" column
	#=========================================================================================
	# If we did not find anything in the "STY" column, parse the title and description columns to see if it is an abstract art work.
	if( ( ! $found )
		&&( $columns[2] =~ /abstra[ck]t/i )
	  )
	{
		$columns[5] = "t:abstract";
		$abstract_match++;
		$found = 1;
	}

	if( ( ! $found )
		&&( $columns[3] =~ /abstra[ck]t/i )
	  )
	{
		$columns[5] = "d:abstract";
		$abstract_match++;
		$found = 1;
	}

	# Search for landscapes in title and description
	if( ( ! $found )
		&&( $columns[2] =~ /landscap/i )
	  )
	{
		$columns[5] = "t:landscape";
		$landscape_match++;
		$found = 1;
	}

	if( ( ! $found )
		&&( $columns[3] =~ /landscap/i )
	  )
	{
		$columns[5] = "d:landscape";
		$landscape_match++;
		$found = 1;
	}


	# Search for portraits in title and description
	if( ( ! $found )
		&&( $columns[2] =~ /portrait/i )
	  )
	{
		$columns[5] = "t:portrait";
		$portrait_match++;
		$found = 1;
	}

	if( ( ! $found )
		&&( $columns[3] =~ /portrait/i )
	  )
	{
		$columns[5] = "d:portrait";
		$portrait_match++;
		$found = 1;
	}


	# Search for fantasy
	if( ( ! $found )
		&&( $columns[2] =~ /fantasy/i )
	  )
	{
		$columns[5] = "t:fantasy";
		$fantasy_match++;
		$found = 1;
	}

	if( ( ! $found )
		&&( $columns[3] =~ /fantasy/i )
	  )
	{
		$columns[5] = "d:fantasy";
		$fantasy_match++;
		$found = 1;
	}

	# Search for realism
	if( ( ! $found )
		&&( $columns[2] =~ /realis/i )
	  )
	{
		$columns[5] = "t:realism";
		$realism_match++;
		$found = 1;
	}

	if( ( ! $found )
		&&( $columns[3] =~ /realis/i )
	  )
	{
		$columns[5] = "d:realism";
		$realism_match++;
		$found = 1;
	}


	# Search for Animals
	if( ( ! $found )
		&&( ( $columns[2] =~ /animal/i )
			||( $columns[2] =~ /dog/i )
			||( $columns[2] =~ /cat/i )
			||( $columns[2] =~ /pet/i )
			)
	  )
	{
		$columns[5] = "t:animal";
		$animal_match++;
		$found = 1;
	}

	if( ( ! $found )
		&&( ( $columns[3] =~ /realis/i )
			||( $columns[3] =~ /dog/i )
			||( $columns[3] =~ /cat/i )
			||( $columns[3] =~ /pet/i )
	  	)
		)
	{
		$columns[5] = "d:animal";
		$animal_match++;
		$found = 1;
	}



	# Search for Beach/Nautical
	if( ( ! $found )
		&&( ( $columns[2] =~ /nautical/i )
			||( $columns[2] =~ /beach/i )
			||( $columns[2] =~ /ocean/i )
			||( $columns[2] =~ /waves/i )
			)
	  )
	{
		$columns[5] = "t:nautical";
		$nautical_match++;
		$found = 1;
	}

	if( ( ! $found )
		&&( ( $columns[3] =~ /nautical/i )
			||( $columns[3] =~ /beach/i )
			||( $columns[3] =~ /ocean/i )
			||( $columns[3] =~ /waves/i )
	  	)
		)
	{
		$columns[5] = "d:nautical";
		$nautical_match++;
		$found = 1;
	}



	if( ! $found ) {
		$columns[5] = "unclassified";
		$unclassified_match++;
	}
	print OUT_FH5 "$columns[0] , $columns[5]\n";
}

print "Number of abstracts   	found = $abstract_match\n";
print "Number of landscapes  	found = $landscape_match\n";
print "Number of portraits   	found = $portrait_match\n";
print "Number of fantasy     	found = $fantasy_match\n";
print "Number of realism     	found = $realism_match\n";
print "Number of animal      	found = $animal_match\n";
print "Number of nautical    	found = $nautical_match\n";
print "Number of traditional 	found = $traditional_match\n";
print "Number of folk       	found = $folk_match\n";
print "Number of unclassified found = $unclassified_match\n";

close( OUT_FH4 );
close( OUT_FH5 );



# Go through file one last time to clean up unwanted lines
open( OUT_FH5, $out_file5 ) or die "Cannot open out file5 for reading";
while( $line = <OUT_FH5> ) {
  if( $line =~ /^ *[0-9]+/ ) {
    print OUT_FINAL $line;
  }
}
close( OUT_FH5 );
close( OUT_FINAL );
