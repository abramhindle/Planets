#!/usr/bin/perl
use strict;
use lib qw(../fastGUI);
use FastGUI;
use Time::HiRes qw(time);
use constant PI => 3.14159;


my $global_pitch = 0;
my $tempo = 60;
my $ts = $tempo / 60;
$| = 1;
my $start_time = time();
my $nth = 3;
my $ticks = 0;
my $readstate = 0;
my @out = ();
open(FILE,"head.sco");
my @head = <FILE>;
close(FILE);
open(FILE,">","output.sco");
print FILE @head;
print FILE $/;
print "f0 3600$/";
print FILE "f0 0$/";
my $drum_alive = 0;
my %bodies = ();

use threads;
use threads::shared;
my $MinValue :shared;
my $MaxValue :shared;
$MinValue = 0;
$MaxValue = 1.0;

my $revolvethread = threads->create('revolvemain');

startFastGUI();

sub startFastGUI {
	my $fg = FastGUI->new(width=>200,height=>200,fullscreen=>0);
	
	my $adapter;
	#do $filetoload;
	
	
	my $slider1 = new EventSlider(value=>$MinValue, subdue=>1,height=>100,width=>200,listener => sub { my ($self, $v) = @_; $MinValue = $v; });
	my $slider2 = new EventSlider(value=>$MaxValue, subdue=>1,height=>100,width=>200,listener => sub { my ($self, $v) = @_; $MaxValue = $v; });
	my $group = new WidgetGroup(widgets=>[$slider1,$slider2]);
	$group->arrange;
	
	$adapter = new WidgetAdapter(widgets=>[$group],current=>$group);
	
	$fg->run($adapter);
}

sub revolvemain {
	while(my $line = <>) {
		#warn "$ticks $readstate";
		chomp $line;
		if ($line =~ /^Tick/) {
			if ($readstate) {
				process(@out);
				@out = ();
			}
			$ticks++;
			# read every $nth
			$readstate = (($ticks % $nth) == 0);
		} elsif ($readstate) {
			push @out,[ split(/\s+/,$line) ];
		}
	}
}
sub min { ($_[0] > $_[1])?$_[1]:$_[0] }
sub process {
    my @elms = @_;
    foreach my $elm (@elms) {
        my ($id,$mass,$radius,$x,$y,$xv,$yv) = @$elm;
        my $theta = atan2($xv,$yv);
        my $timeindex = (PI + -1*$theta)/(2*PI);
        #csprint("1902",0.10,0.1,1000,$mass,$radius,$x,$y,$xv,$yv);

        if (!$bodies{$id}) {
            $bodies{$id}->{instrument} = 1981;#choose(qw(1902 1902 1902 1903 1903 1903  1904));
            my $minr = 1984;
            my $maxr = 2320;
            my $range = $maxr - $minr;
            my $minrr = int($MinValue * $range + $minr);
            my $maxrr = int($MaxValue * $range + $minr);
            $bodies{$id}->{inf} = choose($minrr..$maxrr);
            warn "Choose: ".$bodies{$id}->{inf};
        }
        warn $id." ".$bodies{$id}->{instrument};
        my $instrument = $bodies{$id}->{instrument};
        $bodies{$id}->{alive} = $ticks;
        $bodies{$id}->{elm} = $elm;
        my $mag = sqrt(($xv * $xv) + ($yv * $yv));
        my $dur = 0.2 + 0.2 * abs($xv / (($mag==0)?0.01:$mag));
        my $amp = 200 + 3000*abs(cos($mass * $radius * $yv * $yv));
        csprint( $bodies{$id}->{instrument}, 0.01, $dur, $amp, $bodies{$id}->{inf}, $timeindex);
    }
}
sub choose { return @_[rand(@_)]; }
sub csprint {
	my ($instr,$time,$dur,@o) = @_;
	my ($rt,$out) = map {
		my $time = $_;
		my $str = join(" ",("i$instr",(map { sprintf('%0.5f',$_) } ($time,$dur,@o)))).$/;
		$str;
	} ($time, $time + $ts * ((time() - $start_time)));
	warn $rt;
	print $rt;
	print FILE $out;
}
