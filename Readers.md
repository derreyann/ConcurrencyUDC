# Writers/Readers

## Readers
```c
//r2, r3 stop here
Lock(readers_m);
readers++;
if(readers==1) //meaning we 1st{
    //r1 goes here
    lock(lock); //lock shared area
}
unlock(readers_m);
read();
lock(readers_m);
readers--;
if(readers==0) //last one {
    unlock(lock);
}
unlock(readers_m);
```

## Writers
```c
//w1 decalred
lock(lock);
write();
unlock(lock);
```
## Code Mutex
```c
#include <pthread.h>

Mutex Lock; //protecting data
int Readers0;
mutex readers_m;
```
