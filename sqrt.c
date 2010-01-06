#include <stdio.h>
#include <sys/time.h>
#include <math.h>

int main () {
    struct timeval tv1, tv2;
    struct timezone tz;
    
    int iterations = 10000;
    double i;
    double v = 1000.0;
    double y = 0.0;
   
    // First sqrt
    gettimeofday(&tv1,&tz);
    for(i = 0;i<iterations;i++) {
	y++;
	v = sqrt(y);
    }
    gettimeofday(&tv2,&tz);
    
    printf("Sqrt: %f us per sqrt\n", 
	   ((tv2.tv_sec - tv1.tv_sec) * 1000.0 * 1000.0 +
	    (tv2.tv_usec - tv1.tv_usec) )/iterations);
    

    // then exponent
    gettimeofday(&tv1,&tz);
    for(i = 0;i<iterations;i++) {
	y++;
	v = pow(y,0.51);
    }
    gettimeofday(&tv2,&tz);
    
    printf("Pow:  %f us per sqrt\n", 
	   ((tv2.tv_sec - tv1.tv_sec) * 1000.0 * 1000.0 +
	    (tv2.tv_usec - tv1.tv_usec) )/iterations);


    // then min
    gettimeofday(&tv1,&tz);
    for(i = 0;i<iterations;i++) {
	y++;
	v = (v < y ? v : y);
    }
    gettimeofday(&tv2,&tz);
    
    printf("Min:  %f us per sqrt\n", 
	   ((tv2.tv_sec - tv1.tv_sec) * 1000.0 * 1000.0 +
	    (tv2.tv_usec - tv1.tv_usec) )/iterations);

}
