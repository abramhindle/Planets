#!/usr/bin/perl
use strict;
use Tk;
use Tk::Adjuster;
use threads;
$|=1;

# Main Window
my $mw = new MainWindow;
#Text Area
my $txt = $mw -> Text();#-width=>40, -height=>10);
my $srl_y = $mw -> Scrollbar(-orient=>'v',-command=>[yview => $txt]);
my $srl_x = $mw -> Scrollbar(-orient=>'h',-command=>[xview => $txt]);
$txt -> configure(-yscrollcommand=>['set', $srl_y], 
		-xscrollcommand=>['set',$srl_x]);

$txt->pack(-expand=>1);

sub mloop {
	MainLoop;
}

sub ioloop {
	while(<>) {
		print "Got line: $_";
	}
}


my $ioloopthread = threads->create( 'ioloop' );
mloop;
