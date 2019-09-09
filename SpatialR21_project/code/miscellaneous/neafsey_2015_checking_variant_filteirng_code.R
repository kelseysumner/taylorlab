# ----------------------------------------- #
#  Neafsey 2015 Haplotype Cleaning Check    #
#            September 9, 2019              #
#                K. Sumner                  #
# ----------------------------------------- #

# checking that the haplotype cleaning is working correctly

# look at haplotypes to remove from final and see if two variants occur in 1
haplotypes_to_remove_from_final
length(haplotypes_to_remove_from_final)
haplotypes_to_remove_from_final = c(haplotypes_to_remove_from_final)

# check some of the contig positions
# check seq 25
hap_chars = stringr::str_split(new_haplotype_sequences$reverse_complement_sequence[which(new_haplotype_sequences$sequence_names==">Seq25")],"")
  for (k in 1:nrow(variant_table)){
    contig_position = filtered_variant_table$`Contig Pos`[k]
    called_base = filtered_variant_table$`Called Base`[k]
    if (hap_chars[[1]][as.numeric(contig_position)] == as.character(called_base) & !(is.na(hap_chars[[1]][as.numeric(contig_position)])) & !(is.na(as.character(called_base)))){
      print(called_base)
      print(contig_position)
  }
  }
# has contig position 6 with called base A
hap_chars[[1]][6] # does have this A
# check another random contig position
hap_chars[[1]][3]  # has G here
hap_chars[[1]][7] # has A here

# now look for the haplotype that has two singleton variants to make sure code working correctly
# first make a function
checking_hap_chars_function = function(input_haplotype_sequence){
  for (k in 1:nrow(variant_table)){
    contig_position = filtered_variant_table$`Contig Pos`[k]
    called_base = filtered_variant_table$`Called Base`[k]
    if (input_haplotype_sequence[[1]][as.numeric(contig_position)] == as.character(called_base) & !(is.na(input_haplotype_sequence[[1]][as.numeric(contig_position)])) & !(is.na(as.character(called_base)))){
      print(called_base)
      print(contig_position)
    }
  }
}

# check each hapotype sequence pulled out to make sure code working correctly
checking_hap_chars_function(stringr::str_split(new_haplotype_sequences$reverse_complement_sequence[which(new_haplotype_sequences$sequence_names==">Seq44")],""))
checking_hap_chars_function(stringr::str_split(new_haplotype_sequences$reverse_complement_sequence[which(new_haplotype_sequences$sequence_names==">Seq69")],""))
checking_hap_chars_function(stringr::str_split(new_haplotype_sequences$reverse_complement_sequence[which(new_haplotype_sequences$sequence_names==">Seq72")],""))
checking_hap_chars_function(stringr::str_split(new_haplotype_sequences$reverse_complement_sequence[which(new_haplotype_sequences$sequence_names==">Seq91")],""))
checking_hap_chars_function(stringr::str_split(new_haplotype_sequences$reverse_complement_sequence[which(new_haplotype_sequences$sequence_names==">Seq98")],""))
checking_hap_chars_function(stringr::str_split(new_haplotype_sequences$reverse_complement_sequence[which(new_haplotype_sequences$sequence_names==">Seq127")],""))
checking_hap_chars_function(stringr::str_split(new_haplotype_sequences$reverse_complement_sequence[which(new_haplotype_sequences$sequence_names==">Seq129")],""))
checking_hap_chars_function(stringr::str_split(new_haplotype_sequences$reverse_complement_sequence[which(new_haplotype_sequences$sequence_names==">Seq138")],""))
checking_hap_chars_function(stringr::str_split(new_haplotype_sequences$reverse_complement_sequence[which(new_haplotype_sequences$sequence_names==">Seq142")],""))
checking_hap_chars_function(stringr::str_split(new_haplotype_sequences$reverse_complement_sequence[which(new_haplotype_sequences$sequence_names==">Seq152")],""))
checking_hap_chars_function(stringr::str_split(new_haplotype_sequences$reverse_complement_sequence[which(new_haplotype_sequences$sequence_names==">Seq154")],""))
checking_hap_chars_function(stringr::str_split(new_haplotype_sequences$reverse_complement_sequence[which(new_haplotype_sequences$sequence_names==">Seq164")],""))
checking_hap_chars_function(stringr::str_split(new_haplotype_sequences$reverse_complement_sequence[which(new_haplotype_sequences$sequence_names==">Seq165")],""))
checking_hap_chars_function(stringr::str_split(new_haplotype_sequences$reverse_complement_sequence[which(new_haplotype_sequences$sequence_names==">Seq173")],""))
checking_hap_chars_function(stringr::str_split(new_haplotype_sequences$reverse_complement_sequence[which(new_haplotype_sequences$sequence_names==">Seq174")],""))
checking_hap_chars_function(stringr::str_split(new_haplotype_sequences$reverse_complement_sequence[which(new_haplotype_sequences$sequence_names==">Seq176")],""))
checking_hap_chars_function(stringr::str_split(new_haplotype_sequences$reverse_complement_sequence[which(new_haplotype_sequences$sequence_names==">Seq181")],""))
checking_hap_chars_function(stringr::str_split(new_haplotype_sequences$reverse_complement_sequence[which(new_haplotype_sequences$sequence_names==">Seq183")],""))
checking_hap_chars_function(stringr::str_split(new_haplotype_sequences$reverse_complement_sequence[which(new_haplotype_sequences$sequence_names==">Seq184")],""))
checking_hap_chars_function(stringr::str_split(new_haplotype_sequences$reverse_complement_sequence[which(new_haplotype_sequences$sequence_names==">Seq185")],""))
checking_hap_chars_function(stringr::str_split(new_haplotype_sequences$reverse_complement_sequence[which(new_haplotype_sequences$sequence_names==">Seq192")],""))
checking_hap_chars_function(stringr::str_split(new_haplotype_sequences$reverse_complement_sequence[which(new_haplotype_sequences$sequence_names==">Seq196")],""))
checking_hap_chars_function(stringr::str_split(new_haplotype_sequences$reverse_complement_sequence[which(new_haplotype_sequences$sequence_names==">Seq201")],""))
checking_hap_chars_function(stringr::str_split(new_haplotype_sequences$reverse_complement_sequence[which(new_haplotype_sequences$sequence_names==">Seq202")],""))
# looks like it all worked correctly











