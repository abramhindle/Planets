make planets && ./planets | perl run.pl | csound -dm6 -+rtaudio=jack  -o devaudio -b 400 -B 1200 -L stdin run.orc head.sco
