#!/usr/bin/perl
use POSIX qw(ceil);
my $i = 1984;
#for my $file (<$ARGV[0]||"v*wav">) {
foreach my $file (@ARGV) {
	my $s =	-s $file;
	my $v = log($s / 2)/log(2);
	my $s = 2**ceil($v);
	print "f ".$i++." 0 $s  1 \"$file\" 0 4 1$/";
}
