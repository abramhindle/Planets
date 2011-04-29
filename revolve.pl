#!/usr/bin/perl
use strict;
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
            $bodies{$id}->{inf} = choose(1984..2098);#2264..2320);#1984..2320);
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

__DATA__
960533502 76265.4836851 42.407500543 2468.41794823 1508.15009793 11.6450294421 -12.1781795551
841960017 252755.382437 63.2266450197 2948.74751434 1961.03126749 -1.76882751418 7.68902508475
960533502 76265.4836851 42.407500543 2479.06703791 1497.35210991 12.017018476 -11.8160022732
841960017 252755.382437 63.2266450197 2947.10470794 1967.90239606 -1.88107012332 7.57974303774
960533502 76265.4836851 42.407500543 2490.0439062 1486.88360382 12.373424597 -11.4461243729
841960017 252755.382437 63.2266450197 2945.36299882 1974.67410795 -1.98861080136 7.46813743434
960533502 76265.4836851 42.407500543 2501.33452974 1476.75117093 12.7142572008 -11.0692961176
841960017 252755.382437 63.2266450197 2943.52661834 1981.34441436 -2.09145238335 7.35443465628
960533502 76265.4836851 42.407500543 2512.92491092 1466.96073756 13.0395635253 -10.6862445638
841960017 252755.382437 63.2266450197 2941.59979009 1987.91152707 -2.18960912264 7.2388540829
960533502 76265.4836851 42.407500543 2524.8011105 1457.51758674 13.3494256575 -10.2976720876
841960017 252755.382437 63.2266450197 2939.58672004 1994.37385202 -2.28310578793 7.12160764662
960533502 76265.4836851 42.407500543 2536.94927774 1448.42638087 13.6439575751 -9.90425521079
841960017 252755.382437 63.2266450197 2937.49158747 2000.72998239 -2.37197677036 7.00289947861
960533502 76265.4836851 42.407500543 2549.35567769 1439.69118548 13.9233022445 -9.50664370512
841960017 252755.382437 63.2266450197 2935.3185367 2006.97869146 -2.45626520814 6.88292563866
960533502 76265.4836851 42.407500543 2562.0067161 1431.3154936 14.1876287992 -9.10545995454
841960017 252755.382437 63.2266450197 2933.07166962 2013.11892526 -2.53602213501 6.76187392261
960533502 76265.4836851 42.407500543 2574.88896166 1423.30225058 14.4371298138 -8.70129855206
841960017 252755.382437 63.2266450197 2930.75503894 2019.14979507 -2.61130565763 6.63992374078
960533502 76265.4836851 42.407500543 2587.98916593 1415.65387924 14.6720186875 -8.29472610987
841960017 252755.382437 63.2266450197 2928.3726422 2025.07056983 -2.68218016641 6.51724606066
960533502 76265.4836851 42.407500543 2601.294281 1408.37230504 14.8925271505 -7.88628126042
841960017 252755.382437 63.2266450197 2925.92841642 2030.88066856 -2.74871558267 6.39400340722
960533502 76265.4836851 42.407500543 2614.79147485 1401.45898118 15.0989028975 -7.47647482716
841960017 252755.382437 63.2266450197 2923.4262335 2036.57965279 -2.81098664497 6.27034991447
960533502 76265.4836851 42.407500543 2628.46814471 1394.91491356 15.2914073577 -7.06579014429
841960017 252755.382437 63.2266450197 2920.86989614 2042.16721899 -2.86907223598 6.1464314219
960533502 76265.4836851 42.407500543 2642.31192852 1388.74068525 15.4703136018 -6.65468350583
841960017 252755.382437 63.2266450197 2918.26313443 2047.64319123 -2.92305475104 6.02238560999
960533502 76265.4836851 42.407500543 2656.31071447 1382.93648063 15.6359043885 -6.24358472559
841960017 252755.382437 63.2266450197 2915.60960294 2053.00751388 -2.97301950874 5.89834216919
960533502 76265.4836851 42.407500543 2670.45264891 1377.50210894 15.7884703503 -5.83289779065
841960017 252755.382437 63.2266450197 2912.91287835 2058.26024449 -3.01905420348 5.77442299709
960533502 76265.4836851 42.407500543 2684.72614265 1372.43702715 15.9283083163 -5.42300159241
841960017 252755.382437 63.2266450197 2910.17645754 2063.40154692 -3.06124839934 5.65074241898
960533502 76265.4836851 42.407500543 2699.11987589 1367.74036224 16.0557197686 -5.01425072041
841960017 252755.382437 63.2266450197 2907.4037561 2068.43168459 -3.09969306446 5.52740742742
960533502 76265.4836851 42.407500543 2713.62280168 1363.41093269 16.1710094297 -4.60697630578
841960017 252755.382437 63.2266450197 2904.59810731 2073.35101401 -3.13448014469 5.4045179367
960533502 76265.4836851 42.407500543 2728.22414831 1359.44726918 16.2744839746 -4.20148690187
841960017 252755.382437 63.2266450197 2901.76276136 2078.15997852 -3.16570217504 5.28216704858
960533502 76265.4836851 42.407500543 2742.91342053 1355.8476345 16.3664508634 -3.79806939163
841960017 252755.382437 63.2266450197 2898.90088507 2082.85910231 -3.19345192733 5.16044132607
960533502 76265.4836851 42.407500543 2757.68039974 1352.61004267 16.4472172869 -3.39698991178
841960017 252755.382437 63.2266450197 2896.01556177 2087.44898461 -3.21782209224 5.03942107229
960533502 76265.4836851 42.407500543 2772.51514332 1349.73227716 16.5170892218 -2.9984947855
841960017 252755.382437 63.2266450197 2893.10979148 2091.93029423 -3.23890499406 4.9191806119
960533502 76265.4836851 42.407500543 2787.40798319 1347.21190834 16.5763705863 -2.60281145619
841960017 252755.382437 63.2266450197 2890.18649144 2096.30376429 -3.25679233595 4.79978857282
960533502 76265.4836851 42.407500543 2802.34952357 1345.04630999 16.6253624918 -2.21014941578
841960017 252755.382437 63.2266450197 2887.24849668 2100.5701872 -3.27157497416 4.6813081664
960533502 76265.4836851 42.407500543 2817.33063817 1343.23267511 16.664362583 -1.82070112229
841960017 252755.382437 63.2266450197 2884.29856094 2104.73040995 -3.28334271887 4.5637974642
960533502 76265.4836851 42.407500543 2832.34246679 1341.7680308 16.6936644608 -1.43464290175
841960017 252755.382437 63.2266450197 2881.33935766 2108.78532956 -3.2921841602 4.44730967021
960533502 76265.4836851 42.407500543 2847.37641149 1340.6492524 16.713557182 -1.05213583088
841960017 252755.382437 63.2266450197 2878.37348116 2112.73588885 -3.29818651706 4.33189338712
960533502 76265.4836851 42.407500543 2862.42413225 1339.87307682 16.7243248292 -0.673326597099
841960017 252755.382437 63.2266450197 2875.40344793 2116.58307243 -3.3014355075 4.21759287585
960533502 76265.4836851 42.407500543 2877.47754234 1339.4361151 16.7262461454 -0.298348333433
841960017 252755.382437 63.2266450197 2872.43169801 2120.32790285 -3.30201523843 4.10444830748
960533502 76265.4836851 42.407500543 2892.5288034 1339.33486429 16.7195942285 0.0726785737167
841960017 252755.382437 63.2266450197 2869.46059654 2123.9714371 -3.30000811335 3.99249600695
960533502 76265.4836851 42.407500543 2907.5703202 1339.56571849 16.7046362787 0.439645705475
841960017 252755.382437 63.2266450197 2866.49243526 2127.51476319 -3.29549475635 3.88176868819
960533502 76265.4836851 42.407500543 2922.59473528 1340.12497936 16.6816333965 0.802455861134
841960017 252755.382437 63.2266450197 2863.52943419 2130.95899706 -3.28855395091 3.77229568017
960533502 76265.4836851 42.407500543 2937.59492335 1341.00886584 16.6508404242 1.16102233925
841960017 252755.382437 63.2266450197 2860.5737433 2134.30527958 -3.27926259217 3.66410314388
960533502 76265.4836851 42.407500543 2952.56398562 1342.21352331 16.6125058274 1.51526824958
841960017 252755.382437 63.2266450197 2857.62744418 2137.55477385 -3.2676956513 3.5572142799

