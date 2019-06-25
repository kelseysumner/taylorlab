#!/usr/bin/perl
#
use warnings;
use strict;

if ($#ARGV != 6) {
    print "usage: Enter an integer for the number of reference targets, a comma separated list of the reference fasta files (without spaces), output directory, directory with R1 reads, directory with R2 reads, the fasta file containing the forward primers to be trimmed, and the fasta file containing the reverse primers to be trimmed.\ne.g. splitReadsMultiRef.pl.pl 2 /home/refs/ref1/ref1.fasta,/home/refs/ref2/ref2.fasta /home/out /home/reads/1 /home/reads/2 /home/primers/forward.fasta /home/primers/reverse.fasta\n";
    exit;
}
my $numRef = $ARGV[0];
my $refs = $ARGV[1];
my $out = $ARGV[2];
my $pair1 = $ARGV[3];
my $pair2 = $ARGV[4];
my $forward = $ARGV[5];
my $reverse = $ARGV[6];

chomp $numRef;
unless ($numRef > 0)
{die "\n\tPlease enter a number greater than 0 for number of genomes\n";}
my ($refSeqs, $refNames) = getReference($numRef, $refs);
my @refSeqs = @{$refSeqs};
my @refNames = @{$refNames};

chomp $out;
makeOutDirs("$out", \@refNames);

my @PairedReadfiles1 = ();
my @PairedReadfiles2 = ();
my $baseQual;
my $trim1;
my $trim2;
my @trimReads1 = ();
my @trimReads2 = ();

@PairedReadfiles1 = getReads("$pair1");
@PairedReadfiles2 = getReads("$pair2");

QCreads("$pair1", "$pair2", "$out/fastqc_in", \@PairedReadfiles1, \@PairedReadfiles2, "fastq");

($trim1, $trim2) = trimReads("$pair1", "$pair2", "$out", \@PairedReadfiles1, \@PairedReadfiles2, "$forward", "$reverse");

@trimReads1 = getReads("$trim1");
@trimReads2 = getReads("$trim2");
    
splitReads(\@refSeqs, \@refNames, "$out", \@trimReads1, \@trimReads2, "$trim1", "$trim2");
############################################################
sub getReference
{
    my $numRef = shift;
    my $refPaths = shift;
    my $i;
    my @referenceFastas;
    my @refNames;
    print "\n$numRef\t$refPaths\n";

    my @refs = split(',', $refPaths);
    my $ref;
    $i = 0;
    foreach $ref (@refs)
    {
	$referenceFastas[$i] = $ref;
	$referenceFastas[$i] =~ s/^\s+|\s+$//g;
	unless (-e $referenceFastas[$i])
	{die "\n**Reference file does not exist**\n";}
	my @refSplit = split( '/', $referenceFastas[$i]);
	my $x = @refSplit;
	my $nameIndex = ($x - 1);
	my @refFasta = split('\.', $refSplit[$nameIndex]);
	$refNames[$i] = $refFasta[0];
	$i++;
    }
    return (\@referenceFastas, \@refNames);
}
############################################################
sub makeOutDirs
{
    my $topOut = shift;
    my $refs = shift;
    my @refNames = @{$refs};
    my $elem;
    my $i = 0;
    unless (-d $topOut)
    {system "mkdir $topOut";}
    if ( -d $topOut)
    {
        unless (-d "$topOut/fastq")
        {
            system "mkdir $topOut/fastq";
            foreach $elem (@refNames)
            {
                system "mkdir $topOut/fastq/$refNames[$i]";
                $i++;
            }
            print "\tSynchronized paired end fastq files stored in $topOut/fastq\n";
        }
			
        unless (-d "$topOut/Results")
        {system "mkdir $topOut/Results";}
        print "\n\tLog files stored in $topOut/Results\n";
            
        unless (-d "$topOut/fastqc_in")
        {system "mkdir $topOut/fastqc_in";}
        print "\n\tFastqc .html files of input reads stored in $topOut/fastqc_in\n";
            
        unless (-d "$topOut/cut")
        {system "mkdir $topOut/cut";
        system "mkdir $topOut/cut/1";
        system "mkdir $topOut/cut/2";}
        print "\tAdapter cut reads temporarily stored in $topOut/cut\n";

        unless (-d "$topOut/trim")
        {system "mkdir $topOut/trim";
        system "mkdir $topOut/trim/1";
        system "mkdir $topOut/trim/2";
        system "mkdir $topOut/trim/singleton";
        system "mkdir $topOut/trim/Log";
        system "mkdir $topOut/trim/Summary";
        system "mkdir $topOut/trim/fastqcTrim";}
        print "\tTrimmed reads stored in $topOut/trim\n";

        unless (-d "$topOut/fastqc_trim_split")
        {system "mkdir $topOut/fastqc_trim_split";}
        print "\n\tFastqc .html files of trimmed reads stored in $topOut/fastqc_trim_split\n\n";
	}
}
############################################################
sub getReads
{
    my $readsDir = shift();
    my @Reads = ();
    my $elem;
    if ( -d $readsDir)
    {
		opendir (READS, "$readsDir");
		@Reads = readdir READS;
		closedir READS;
		splice (@Reads, 0, 2);
    }
	else { die "\n\t**Reads not found**\n"; }

	my @sortReads = sort(@Reads);
	return @sortReads;
}
############################################################
sub QCreads
{
    my $read1Dir = shift;
    my $read2Dir = shift;
    my $outDir = shift;
    my $pair1Reads = shift;
    my $pair2Reads = shift;
    my $format = shift;
    my @Reads1 = @{$pair1Reads};
    my @Reads2 = @{$pair2Reads};
    my $i;
    my $size = @Reads1;
    my $size2 = @Reads2;
    if ($size != $size2) {die "Paired end read files unequal";}
    
    for ($i = 0; $i < $size; $i++)
    {
        system "fastqc -o $outDir -f $format $read1Dir/$Reads1[$i] $read2Dir/$Reads2[$i]";
    } 
}
############################################################
sub trimReads
{
    my $read1Dir = shift;
    my $read2Dir = shift;
    my $outDir = shift;
    my $pair1Reads = shift;
    my $pair2Reads = shift;
    my $forward = shift;
    my $reverse = shift;
    my @Reads1 = @{$pair1Reads};
    my @Reads2 = @{$pair2Reads};
    my $i;
    my $size = @Reads1;
    my $size2 = @Reads2;
    my @PRE;
    my $prefix;
    if ($size != $size2) {die "\n\t***Paired-end read file names unequal***\n";}

    for ($i = 0; $i < $size; $i++)
    {
        @PRE = split('_', $Reads1[$i]);
        $prefix = $PRE[0];
        print "\n\tTrimming $prefix reads\n";
        my @PRE2 = split('_', $Reads2[$i]);
        my $pre2 = $PRE2[0];
        if($prefix eq $pre2)
        {
	    system "cutadapt -g file:$forward -G file:$reverse -o $outDir/cut/1/$prefix.1.fastq.gz -p $outDir/cut/2/$prefix.2.fastq.gz $read1Dir/$Reads1[$i] $read2Dir/$Reads2[$i]";

        system "java -jar /gpfs/fs1/data/taylorlab/software/Trimmomatic-0.38/trimmomatic-0.38.jar PE -phred33 -summary $out/trim/Summary/$prefix.summary $outDir/cut/1/$prefix.1.fastq.gz $outDir/cut/2/$prefix.2.fastq.gz $outDir/trim/1/$prefix.1.fastq.gz $outDir/trim/singleton/$prefix.1_unpaired.fq.gz $outDir/trim/2/$prefix.2.fastq.gz $outDir/trim/singleton/$prefix.2_unpaired.fq.gz LEADING:10 TRAILING:10 SLIDINGWINDOW:4:15 MINLEN:80";

	    system "rm $outDir/cut/1/$prefix.1.fastq.gz $outDir/cut/2/$prefix.2.fastq.gz";
        }
    }

    my $trimReads1 = "$outDir/trim/1";
    my $trimReads2 = "$outDir/trim/2";
    
    return ($trimReads1, $trimReads2);
}
############################################################
sub splitReads
{
	my $fastaRefs = shift;
	my $refs = shift;
	my $out = shift;
	my $readsFile1 = shift;
	my $readsFile2 = shift;
	my $read1Dir = shift;
	my $read2Dir = shift;
	my @refSeqs = @{$fastaRefs};
	my @refNames = @{$refs};
	my @read1 = @{$readsFile1};
	my @read2 = @{$readsFile2};
	my $i;
	my $size = @read1;
	my @sampleName;
	my $elem;

	my $Refs = join( ',', @refSeqs );

	for($i = 0; $i < $size; $i++)
	{
		my @fileSplit = split(/\./, $read1[$i]);
		$sampleName[$i] = $fileSplit[0];
		print "\n\tAligning $sampleName[$i] to $Refs\n";
		my @file2Split = split(/\./, $read2[$i]);
		my $sampleName2 = $file2Split[0];

		if ($sampleName[$i] eq $sampleName2)
		{
			system "bbsplit.sh -Xmx800m in=$read1Dir/$read1[$i] in2=$read2Dir/$read2[$i] ref=$Refs basename=$sampleName[$i]_%_#.fastq >& $out/Results/$sampleName[$i].txt";
			
			foreach $elem (@refNames)
			{
				system "mv *$elem* $out/fastq/$elem";
				system "gzip $out/fastq/$elem/*.fastq";
				unless (-d "$out/fastq/$elem/1")
				{system "mkdir $out/fastq/$elem/1";}
				unless (-d "$out/fastq/$elem/2")
				{system "mkdir $out/fastq/$elem/2";}
				system "mv $out/fastq/$elem/*_1.fastq.gz $out/fastq/$elem/1";
				system "mv $out/fastq/$elem/*_2.fastq.gz $out/fastq/$elem/2";

			}
		}
	}
    
    print "\nRunning fastqc on trimmed and split reads...\n\n";
    foreach $elem (@refNames)
    {
        my @qcReads1 = getReads("$out/fastq/$elem/1");
        my @qcReads2 = getReads("$out/fastq/$elem/2");
        QCreads("$out/fastq/$elem/1", "$out/fastq/$elem/2", "$out/fastqc_trim_split", \@qcReads1, \@qcReads2, "fastq");
        
    }   
}
